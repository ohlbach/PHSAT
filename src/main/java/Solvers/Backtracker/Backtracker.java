package Solvers.Backtracker;

import Datastructures.Clause;
import Datastructures.ClauseList;
import Datastructures.Clauses.Quantifier;
import Datastructures.Literal;
import Datastructures.Results.Aborted;
import Datastructures.Results.Result;
import Datastructures.Results.Satisfiable;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Statistics.Statistic;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import Management.Parameter;
import Management.Parameters;
import Management.ProblemSupervisor;
import Datastructures.ValueType;
import Solvers.InterruptReason;
import Solvers.Solver;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.function.Consumer;

import static Utilities.Utilities.*;

/** The Backtracker tries to solve a QSat problem in the style of the Davis-Putnam procedure
 *  by iteratively selecting true predicates and backtracking when a false clause has been found.
 * <br>
 * Several backtrackers can work in parallel. <br>
 * The clauses are kept in a clauseList which is shared by all active backtrackers.
 * Once a globally true literal has been derived all backtrackers are interrupted,
 * the clauseList derives all consequences, and the backtrackers resume their work with the changed clauses.
 * <br>
 * The sequence of literals to be selected during the search is determined at the beginning and then kept unchanged.
 * The initial sequence depends on two parameters: predicateArrangement and seed.
 * If seed %gt;= 0 then the sequence is random (the random number generator depends on the vlaue of seed.)<br>
 * The other options are:<br>
 * - predicateArrangement == 1: just the sequence of natural numbers: 1,2,...<br>
 * - predicateArrangement == 2: just the inverse sequence of natural numbers: n,n-1,...<br>
 * - predicateArrangement == 3: predicates with more literal occurrence first<br>
 * - predicateArrangement == 4: predicates with less literal occurrence first.
 * <br>
 * A further parameter is firstSign (+1, or -1)<br>
 * It controls the sign of the selected predicates (positive or negative).
 * <br>
 * Each combination of the different parameters determines a particular strategy and therefore a particular backtracker.
 * Several of these combinations can work in parallel and exchange intermediate results (globally true predicates)
 * among themselves and with other solvers.
 * <br>
 * As soon as a new literal is selected and set locally to be true, there may be derived further locally true literals,
 * which in turn may derive even more locally true literals. This propagation of true literals is done in parallel in
 * separate threads. These threads are taken from a thread pool. Each such thread waits for a propagation job to be
 * executed. The propagation jobs may even come from different currently active backtrackers.
 * <br>
 * The derivation of new locally true literals and locally false clause can be verified with a model-based algorithm.
 * Each derived globally true literal can be labelled with an InferenceStep which describes the derivation of this literal.
 * <br>
 * A backtracker, once generated can work at a sequence of problems, by calling the methods: initialize and solveProblem.
 * initialize is called in the ProblemSupervisor's thread, which also maintains the clauseList.
 * solveProblem is called in a separate thread.
 */
public class Backtracker extends Solver {

    private static int arrangementDefault = 1;
    private static int seedDefault = 1;
    private static int firstSignDefault = 1;

    /** Generates the parameters for the GUI.
     *
     * @return A Parameters instance with a specification of the parameters to be presented in the GUI.
     */
    public static Parameters makeParameter() {
        Parameters parameters = new Parameters("Backtracker");
        Parameter selected = new Parameter("Select", Parameter.DisplayType.Button,new ValueType.Booleans(),false,
                "Select the Backtracker");
        parameters.add(selected);
        ValueType boolType = new ValueType.Booleans(false);
        boolean arrangement = arrangementDefault == 1;
        parameters.add(new Parameter("Up-sequence", Parameter.DisplayType.Boolean,boolType, arrangement,
                "Predicates in ascending order (1,2,3,...)"));
        arrangement = arrangementDefault == 2;
        parameters.add(new Parameter("Down-sequence", Parameter.DisplayType.Boolean,boolType, arrangement,
                "Predicates in decending order (...3,2,1)"));
        arrangement = arrangementDefault == 3;
        parameters.add(new Parameter("More-first", Parameter.DisplayType.Boolean,boolType, arrangement,
                "Predicates: more predicates first"));
        arrangement = arrangementDefault == 4;
        parameters.add(new Parameter("Less-first", Parameter.DisplayType.Boolean,boolType, arrangement,
                "Predicates: less predicates first"));
        Parameter seed = new Parameter("Seed", Parameter.DisplayType.String,
                new ValueType.Integers(-1,true),seedDefault,
                "Seed for random number generator (integer >= -1)");
        parameters.add(seed);
        parameters.add(new Parameter("PositiveFirst", Parameter.DisplayType.Boolean,
                boolType,firstSignDefault==1,
                "Try positive predicates first"));
        parameters.add(new Parameter("NegativeFirst", Parameter.DisplayType.Boolean,
                boolType,firstSignDefault==-1,
                "Try negative predicates first"));
        parameters.setDescription("Backtracking search (kind of Davis-Putnam Procedure)");
        return parameters;
    }

    /**
     * Generates and adds new backtrackers based on the provided parameters.
     *
     * @param parameters The parameters containing the values necessary to create the problem generators.
     * @param backtrackers The list of backtrackers to add the newly created walkers to.
     */
    public static void makeSolvers(Parameters parameters,ArrayList<Solver> backtrackers) {
        int index = 0;
        int solverNumber = 0;
        boolean arrangement1 = (Boolean) parameters.parameters.get(++index).value;
        boolean arrangement2 = (Boolean) parameters.parameters.get(++index).value;
        boolean arrangement3 = (Boolean) parameters.parameters.get(++index).value;
        boolean arrangement4 = (Boolean) parameters.parameters.get(++index).value;
        IntArrayList seeds   = (IntArrayList)parameters.parameters.get(++index).value;

        boolean positiveFirst = (Boolean) parameters.parameters.get(++index).value;
        boolean negativeFirst = (Boolean) parameters.parameters.get(++index).value;

        if (arrangement1) {
            if(positiveFirst) backtrackers.add(new Backtracker(++solverNumber, 1, -1,1));
            if(negativeFirst) backtrackers.add(new Backtracker(++solverNumber, 1, -1,-1));}
        if (arrangement2) {
            if(positiveFirst) backtrackers.add(new Backtracker(++solverNumber, 2, -1,1));
            if(negativeFirst) backtrackers.add(new Backtracker(++solverNumber, 2, -1,-1));}
        if (arrangement3) {
            if(positiveFirst) backtrackers.add(new Backtracker(++solverNumber, 3, -1,1));
            if(negativeFirst) backtrackers.add(new Backtracker(++solverNumber, 3, -1,-1));}
        if (arrangement4) {
            if(positiveFirst) backtrackers.add(new Backtracker(++solverNumber, 4, -1,1));
            if(negativeFirst) backtrackers.add(new Backtracker(++solverNumber, 4, -1,-1));}
        for(int seed : seeds) {
            if(seed >= 0) {
                if(positiveFirst) backtrackers.add(new Backtracker(++solverNumber, 4, seed,1));
                if(negativeFirst) backtrackers.add(new Backtracker(++solverNumber, 4, seed,-1));}
            }}


     /** The predicates are initially sorted as follows:<br>
     * - predicateArrangement == 1: just the sequence of natural numbers: 1,2,...<br>
     * - predicateArrangement == 2: just the inverse sequence of natural numbers: n,n-1,...<br>
     * - predicateArrangement == 3: predicates with more literal occurrences first<br>
     * - predicateArrangement == 4: predicates with less literal occurrences first.*/
    private final int predicateArrangement;

    /** if seek &gt;= 0 then the predicates to be selected are randomly sorted */
    private final int seed;

    /** +1: predicates are selected positively, -1: predicates are selected negatively.*/
    private int firstSign;

    /** the ClauseList which is generated by the Normalizer and shared between the differerent backtrackers */
    protected ClauseList clauseList;

    /** determines the sequence of predicates which are temporally set to true.*/
    protected int[] predicateSequence;

    /** maps each predicate to its position in the predicateSequence array- */
    protected int[] predicatePositions;

    /** The index of the current selected predicate in the predicateSequence array*/
    protected int selectedPredicatePosition;

    /** maps derived literals to the selected true literals which cause the derivation of the literal.
     * The element which is the last one in predicateSequence is the selected literal to which backtracking is necessary. */
    protected IntArrayList[] dependentSelections;

    /** contains the sequence of (0, selected literal) followed by the derived literals.*/
    protected IntArrayList currentlyTrueLiterals;

    /** maps a derived predicate to the clauses used to derive this predicate (if trackReasoning = true)*/
    protected ArrayList<Clause>[] usedClausesArray;

    /** keeps the local candidate model */
    protected byte[] localModel;

    /** keeps statistical information */
    public StatisticsBacktracker statistics;

    /** current recursions depth, for statistical purposes */
    private int recursionDepth;

    /** stores the threads which are used to propagateInThread derived true predicates. */
    public PropagatorPool propagatorPool;

    /** for monitoring the operations */
    public Consumer<String> monitor = null;

    /** is used to signal the end of the propagator jobs.
     * If a propagator founds a false clause, the false clause is inserted.
     * If all propagator jobs are finished, 'this' is inserted.
     * This way propagateSelectedLiteral can wait until all propagator jobs are finished. */
    private final BlockingQueue<Object> propagatorQueue = new LinkedBlockingQueue<>();

    /** counts the active propagator threads */
    protected int propagatorThreadCounter = 0;

    protected Clause falseClause = null;

    /** constructs a new Backtracker.
     *
     * @param solverNumber  for enumerating the solvers.
     * @param predicateArrangement for initializing the sequence of predicates to be selected.<br>
     *   -  1: just the sequence of natural numbers: 1,2,...<br>
     *   -  2: just the inverse sequence of natural numbers: n,n-1,...<br>
     *   -  3: predicates with more literal occurrences first<br>
     *   -  4: predicates with less literal occurrences first.
     * @param seed if seed &gt;= 0 then the predicates are sorted randomly, and predicateArrangement is ignored.
     * @param firstSign: +1 selected predicates are always true, -1: selected predicates are always false.
     */
    public Backtracker(int solverNumber, int predicateArrangement, int seed, int firstSign) {
        super(solverNumber);
        solverId                  = "Backtracker_" + solverNumber;
        this.predicateArrangement = predicateArrangement;
        this.seed                 = seed;
        this.firstSign            = firstSign;
        myThread                  = Thread.currentThread();} // for testing purposes.


    /** initializes the datastructures for a new problem to be solved.
     * <br>
     * The initialize methods for the solvers are called sequentially, whereas the solveProblem methods are called in parallel.
     *
     * @param problemSupervisor the problemSupervisor for the solver.
     */
    public void initialize(ProblemSupervisor problemSupervisor) {
        super.initialize(problemSupervisor);
        clauseList = problemSupervisor.clauseList;
        clauseList.addSolver(this);
        monitor = monitoring ? (message) -> super.monitor.println(solverId+"_" + solverNumber, message) : null;
        propagatorPool = problemSupervisor.propagatorPool;
        propagatorQueue.clear();
        propagatorThreadCounter = 0;
        if(dependentSelections == null || dependentSelections.length < predicates +1)
            dependentSelections = new IntArrayList[predicates+1];
        if(currentlyTrueLiterals == null) currentlyTrueLiterals = new IntArrayList(predicates);
        else                                currentlyTrueLiterals.ensureCapacity(predicates);
        currentlyTrueLiterals.clear();
        statistics = new StatisticsBacktracker(solverId);
        recursionDepth = -1;
        falseClause = null;
        if(trackReasoning) {
            if((usedClausesArray == null || usedClausesArray.length < predicates+1))
                usedClausesArray = new ArrayList[predicates+1];
            else {for(int i = 1; i <= predicates; ++i) {if(usedClausesArray[i] != null) usedClausesArray[i].clear();}}}}

    /**
     * Solves the QSat-problem by copying the normalized clauses and searching a model with a backtracking algorithm.
     * <br>
     * Predicates selected to be temporarily true cause parallel threads to perform truth-propagation.
     * A top-level selected predicate which derives a contradiction
     * causes its negation to become true, and this is used to simplify the clauses.
     * Since several solvers may run in parallel, they may exchange true literals.
     * These are taken into account even during the recursive search.
     *
     * @return The result of solving the problem.
     */
    @Override
     public Result solveProblem()  {
        long solverStartTime = System.nanoTime();
        System.out.println("Clauses\n"+ clauseList.toString("clauses",null));
        myThread = Thread.currentThread();
        initializeLocalModel();
        initializePredicateSequence(predicateArrangement,seed);
        selectedPredicatePosition = -1;
        int selectedLiteral;
        try{
        while(true){ // stopped by throw Satisfiable/Unsatisfiable or Aborted
            if(myThread.isInterrupted()) processInterrupt(null); // maybe changes in clauseList
            if(clauseList.isEmpty() || (selectedPredicatePosition = findNextPredicateIndex(selectedPredicatePosition + 1)) == 0) {
                model.exchangeModel(localModel);
                throw new Satisfiable(problemId,solverId, model);}
            int selectedPredicate = predicateSequence[selectedPredicatePosition];
            selectedLiteral = firstSign * selectedPredicate;
            ++statistics.selectedLiterals;
            System.out.println("SEL " + selectedLiteral);
            //if(monitoring) monitor.accept("Selected literal " + Symboltable.toString(selectedLiteral,symboltable));
            currentlyTrueLiterals.add(0); currentlyTrueLiterals.add(selectedLiteral);
            statistics.recursionDepth = Math.max(statistics.recursionDepth,++recursionDepth);
            clearDependencies(selectedPredicate).add(selectedPredicate);
            propagateSelectedLiteral(selectedLiteral);}}
        catch(Result result) {
            result.complete(problemId,solverId, solverStartTime);
            statistics.elapsedTime = System.nanoTime() - solverStartTime;
            System.out.println(statistics.toString());
            return result;}}

    /** increments the propagator counter */
    protected synchronized void incrementPropagatorCounter() {
        ++propagatorThreadCounter;}

    /** decrements the propagator counter.
     * <br>
     * If the counter is 0, 'this' is inserted into the propagatorQueue.
     * It means that no further true-literal propagation is going on.*/
    protected synchronized void decrementPropagatorCounter() {
        --propagatorThreadCounter;
        if (propagatorThreadCounter <= 0) {notifyAll();}}


    /** inserts a false clause into the propagatorQueue.
     *
     * @param clause a locally false clause
     * @return the false clause itself.
     */
    protected synchronized Clause falseClauseFound(Clause clause) {
        falseClause = clause;
        return clause;}


    /** used to stop the processing until a new true literal has been processed in the clauseList. */
    private final BlockingQueue<Boolean> waitingQueue = new LinkedBlockingQueue<>(1);

    /** called by clauseList to cause the solver to stop until a true literal has been processed.
     */
    @Override
    public void waitForTrueLiteralProcessing() {
        interruptReason = InterruptReason.TRUELITERALPROCESSING;
        System.out.println("INTER " + myThread.getName());
        myThread.interrupt();}

    /** called by clauseList after a new true literal has been processed.*/
    @Override
    public void continueProcessing() {
        waitingQueue.add(true);} // stops waiting waitingQueue.take and starts incorporating global changes

    /** processes different kinds of interrupts.
     * <br>
     * - TRUELITERALPROCESSING: all global changes are incorporated into the search structure.<br>
     * - PROBLEMSOLVED:  an Aborted exception is thrown.
     * - any other interrupt: StackTrace is printed and the system exits.
     *
     * @param exception null or an unexpected exception.
     * @throws Aborted when another solver has found a solution and has sent an interrupt.
     * */
    private void processInterrupt(Exception exception) throws Result {
        if(interruptReason != null) {
            switch(interruptReason) {
                case TRUELITERALPROCESSING:
                    waitingQueue.clear();
                    clauseList.acknowledgeWaiting();
                    interruptReason = null;
                    try {waitingQueue.take();} // clauseList has finished incorporating new true literals.
                    catch (InterruptedException exception1) {processInterrupt(exception1);} // maybe the entire process is aborted
                    incorporateGlobalChanges();
                    return;
                case PROBLEMSOLVED:
                    throw new Aborted(problemId,solverId, solverStartTime,"Interrupted by another thread");}}
        if(exception != null) { // any external exception or internal severe errors
            System.err.println(exception.getMessage());
            exception.printStackTrace();
            System.exit(1);}
    }

    /** finds from the given predicateIndex this one or the next index of the predicate without a global and local truth value
     * and with clauses containing such a predicate.
     *
     * @param predicateIndex the index of a predicate in predicateSequence
     * @return 0 or the next index of the predicate without a global and local truth value and with clauses containing this predicate
     */
    int findNextPredicateIndex(int predicateIndex)  {
        for(; predicateIndex <= predicates; ++predicateIndex) {
            int predicate = predicateSequence[predicateIndex];
            if (clauseList.isBothEmpty(predicate)) continue;
            if(model.status(predicate) == 0 && localStatus(predicate) == 0) return predicateIndex;}
        return 0;}

    /** propagates the truth of the selected literal.
     * <br>
     * Immediately derivable true literals are derived immediately.
     * Each newly derived literal causes a propagatorThread to be activated,
     * such that further true literals can be derived in parallel.
     * <br>
     * The method then waits until all propagator threads are finished.
     * If one of them found a false clause then the last selected predicate which caused the contradiction
     * is determined and selectedPredicatePosition is determined and the search backtracks to this position.
     *
     * @param selectedLiteral the selected literal
     * @throws Unsatisfiable if backtracked to the top-level and a contradiction was found.
     */
    protected void propagateSelectedLiteral(int selectedLiteral) throws Result {
        propagatorThreadCounter = 0;
        propagatorQueue.clear();
        makeLocallyTrue(selectedLiteral);
        int propagatorJobs = statistics.propagatorJobs;
        Clause myFalseClause = propagateLocally(selectedLiteral); // may start propagator threads
         if(myFalseClause == null) {
            if(propagatorJobs == statistics.propagatorJobs) return; // no propagation done
            synchronized (this) {
                try{wait();} // waits until all propagatorJobs are finished
                catch(InterruptedException exception){processInterrupt(exception); }
            }} // maybe another solver found a solution
        myFalseClause = falseClause;
        falseClause = null;
        if(myThread.isInterrupted()) processInterrupt(null);             // global changes incorporated
        if(myFalseClause == null) return;  // continue search, next selection
        int lastSelectedPredicate = getLastSelectedPredicate(myFalseClause);
         backtrackTo(lastSelectedPredicate);
        selectedPredicatePosition = predicatePositions[lastSelectedPredicate]; // this predicate must be false.
        if(trackReasoning) joinUsedClauses(myFalseClause,lastSelectedPredicate);
        joinDependencies(myFalseClause,lastSelectedPredicate);
        int negateLastSelectedPredicate = -firstSign * lastSelectedPredicate;
        if(currentlyTrueLiterals.isEmpty()) {                                  // the top-literal in the search must be false.
            InferenceStep step = trackReasoning ?
                    new InfSelectedPredicateNegated(negateLastSelectedPredicate,usedClausesArray[lastSelectedPredicate]) : null;
            if(monitoring && step != null) monitor.accept(step.toString(symboltable));
            selectedPredicatePosition = -1;
            model.add(myThread,negateLastSelectedPredicate,step);}
        else {
            makeLocallyTrue(negateLastSelectedPredicate);
            if(monitoring) monitor.accept(
                    "backtrack and negate selected predicate: " + Symboltable.toString(negateLastSelectedPredicate,symboltable));
            --selectedPredicatePosition;  // a new predicate must be selected from the previously selected predicate.
        }}


    /** propagates the local truth of the given literal.
     * <br>
     * Derived unit clauses cause a new Propagator thread to be activated.<br>
     * If a locally false clause is found, it is inserted into the propagatorQueue,
     * and the propagatorPool is instructed to deactivate all jobs. <br>
     * If the propagatorCounter is 0, then 'this' is inserted into the propagatorQueue.
     *
     * @param literal a locally true literal whose implications are to be computed.
     * @return true if a false clause has been found
     */
    boolean propagateInThread(int literal) {
        incrementPropagatorCounter();
        if(propagateLocally(literal) != null) { // false clause found
            //propagatorThreadCounter = 0;
            decrementPropagatorCounter();
            return true;}
        decrementPropagatorCounter();
        return false;}


    /** propagates the truth of the trueLiteral locally.
     * <br>
     * Derived true predicates activate a Propagator thread.<br>
     * A false clause is inserted into the propagatorQueue and causes backtracking<br>
     * The method may be called from the main thread and from the propagator threads.
     *
      *@param trueLiteral a locally true trueLiteral
     * @return null or a false clause.
     */
    protected Clause propagateLocally(int trueLiteral)  {
         for(int sign = 1; sign >= -1; sign -= 2) {
            Literal literalObject = clauseList.literalIndex.getFirstLiteral(sign*trueLiteral);
            while(literalObject != null && !myThread.isInterrupted()) {
                Clause clause = literalObject.clause;
                if((clause.quantifier != Quantifier.OR) || sign == -1) { // true trueLiteral in an OR: ignore clause
                    Clause falseClause = analyseClause(clause);          // may produce new Propagator jobs
                    if(falseClause != null) {falseClauseFound(clause); return clause;}}
                literalObject = (Literal)literalObject.nextItem;}}
        return null;}

    /** Analyzes a clause given the current local model (the global model is ignored).
     * <p>
     * The following cases are possible:<br>
     * - the clause is already true: return null; <br>
     * - the clause is already false: return the clause; <br>
     * - making an unsigned literal true causes the clause to become false: make the literal false;<br>
     * - making an unsigned literal false causes the clause to become false: make the literal true.
     *
     * @param clause The clause to be analyzed.
     * @return the clause if it is locally false already, otherwise null.
     */
    protected Clause analyseClause(Clause clause) {
        // Since disjunctions are frequent,
        // and only one passage through the literals is sufficient,
        // it is worth treating this case separately.
        if(clause.quantifier == Quantifier.OR) {
            Literal unsignedLiteral = null;
            for(Literal literalObject : clause.literals) {
                switch(localStatus(literalObject.literal)) {
                    case 0:
                        if(unsignedLiteral != null) return null;    // two unsigned literals: nothing to be done
                        unsignedLiteral = literalObject; break;
                    case 1: return null;}}                          // clause is true;
            if(unsignedLiteral == null) {
                if(verify) verifyFalseClause(clause,true);
                return clause;}            // all literals are false. backtrackTo
            if(makeLiteralLocallyTrue(clause,unsignedLiteral,1)) {  // all other literals are false
                if(verify) verifyFalseClause(clause,true);
                return clause;}
            return null;}

        // all other clause types.
        int trueLiterals = 0;
        int unsignedLiterals = 0;
        for(Literal literalObject : clause.literals) {
            switch(localStatus(literalObject.literal)) {
                case 0: unsignedLiterals += literalObject.multiplicity; break;
                case 1: trueLiterals += literalObject.multiplicity;}}
        int max = clause.max; int min = clause.min;

        // too many or not enough true literals.
        if(trueLiterals > max || trueLiterals + unsignedLiterals < min) {
            System.err.println("VF " + trueLiterals + " " + unsignedLiterals + "    " + clause.toString(null,0) +
                    "\n " + toStringLocalModel());
            if(verify) verifyFalseClause(clause,true);
            return clause;} // clause is false

        if(min <= trueLiterals) { // clause is already true.
            if(max < clause.expandedSize) {              // more true literals might be dangerous
                for(Literal literalObject : clause.literals) {
                    if(localStatus(literalObject.literal) == 0 &&
                            trueLiterals + literalObject.multiplicity > max){    // making it true causes too many true literals
                        if(makeLiteralLocallyTrue(clause,literalObject,-1)) { // literal must be false
                            if(verify) verifyFalseClause(clause,true);
                            return clause;}}}}
            return null;}

        if(min == 0) return null;
        // The clause is not yet true because there are not enough true literals.
        // We check if a making a particular literal true is sufficient to make the clause true.
        int candidates = 0;
        Literal candidateLiteral = null;
        for(Literal literalObject : clause.literals) {
            if(localStatus(literalObject.literal) == 0 && trueLiterals + literalObject.multiplicity >= min) {
                ++candidates;
                candidateLiteral = literalObject;}}
        if(candidates == 1) {
            if(makeLiteralLocallyTrue(clause,candidateLiteral,1)) {
                if(verify) verifyFalseClause(clause,true);
                return clause;}} // clause is false
        return null;}

    /**
     * Makes a literal locally true/false and adds the job to the propagatorPool
     * <br>
     * A propagator propagates the truth value of the literal to the other clauses.
     *
     * @param clause        The clause containing the literal.
     * @param literalObject The literal object to make true or false.
     * @param sign          The sign of the literal (-1 for literal is false, 1 for literal is true).
     * @return true if the clause turned out to be false.
     */
    synchronized boolean makeLiteralLocallyTrue(Clause clause, Literal literalObject, int sign) {
        int trueLiteral = sign*literalObject.literal;
        if(!makeLocallyTrue(trueLiteral)) return true;  // another thread may have found this out. Clause is false.
        if(verify) verifyTrueLiteral(clause,trueLiteral,true);
        currentlyTrueLiterals.add(trueLiteral);
        int truePredicate = Math.abs(trueLiteral);
        joinDependencies(clause,truePredicate);
        if(falseClause != null) return false;  // no further propagation necessary
        ++statistics.propagatorJobs;
        propagatorPool.addPropagatorJob(this,trueLiteral);
        return false;}

    /** performs a model-based check for the derivation of a true literal from a clause in the local model.
     *
     * @param clause a clause
     * @param literal a derived true literal.
     * @param stop if true then the system stops when the verification failed.
     * @return true if the verification succeeded.
     */
    protected boolean verifyTrueLiteral(Clause clause, int literal, boolean stop) {
        IntArrayList predicates = clause.predicates();
        int literalPosition = predicates.indexOf(Math.abs(literal));
        int nModels = 1 << predicates.size();
        int trueCases = 0;
        for (int model = 0; model < nModels; ++model) {
            if(compatibleLocally(model,predicates) &&
                    ((literal > 0) ? (model & (1 << literalPosition)) != 0 :
                                     (model & (1 << literalPosition)) == 0) &&
                    clause.isTrue(model,predicates)){++trueCases;}}
        if(trueCases != 1) {
            if(stop) {
                synchronized (this) {
                System.err.println("verifyTrueLiteral failed: " + clause.toString(symboltable,0) +
                        " derived literal: " + Symboltable.toString(literal,symboltable) +
                        "\nLocal Model: " + toStringLocalModel());
                System.err.println("Stack " + currentlyTrueLiterals);
                new Exception().printStackTrace();
                System.exit(1);}}
            return false;}
        return true;}

    /** verifies the derivation of a false clause in the local model.
     *
     * @param clause a locally false clause
     * @param stop if true then the system stops when the verification failed
     * @return true if the verification succeeded.
     */
    protected boolean verifyFalseClause(Clause clause, boolean stop) {
        System.out.println("VC " + clause.toString(null,0) + " " + toStringDependencies(clause) +
                "\n  " + currentlyTrueLiterals);
        IntArrayList predicates = clause.predicates();
        int nModels = 1 << predicates.size();
        for (int model = 0; model < nModels; ++model) {
            if(compatibleLocally(model,predicates) && clause.isTrue(model,predicates)){
                if(stop) {
                    synchronized (this) {
                    System.err.println("verifyFalseClause failed: " + clause.toString(symboltable,0) +
                        "   \nLocal Model: " + toStringLocalModel());
                    System.err.println("Falsifying Model: " +Clause.modelString(model,predicates,null));
                    System.err.println("Stack " + currentlyTrueLiterals);
                    new Exception().printStackTrace();
                    System.exit(1);}}
            return false;}}
        return true;}




    /** checks if the model (an int) is compatible with the local model.
     *  <br>
     *  If the local model is unassigned (= 0) for a predicate, it is ignored.<br>
     *  Otherwise if the model is 1 for a predicate, the local model must also be 1.<br>
     *  If the model is 0 for a predicate, the local model must be -1.
     *
     * @param model      a model as int-value
     * @param predicates the predicates which determine the bits in the model
     * @return true if the given model is true in the local model.
     */
    protected boolean compatibleLocally(int model, IntArrayList predicates) {
        for(int predicate : predicates) {
            if(localModel[predicate] == 0) continue;
            boolean isTrue = (model & (1 << predicates.indexOf(predicate))) != 0;
            if(isTrue) {if(localModel[predicate] == -1) return false;}
            else       {if(localModel[predicate] == 1)  return false;}}
         return true;}

    /** either clears an existing dependencies list for the given predicate, or creates a new empty list.
     *
     * @param predicate a predicate
     * @return the old cleared or new dependency list.
     */
    private IntArrayList clearDependencies(int predicate) {
        IntArrayList joinedDependencies = dependentSelections[predicate];
        if(joinedDependencies == null) {
            joinedDependencies = new IntArrayList();
            dependentSelections[predicate] = joinedDependencies;}
        else joinedDependencies.clear();
        return joinedDependencies;}

    /** either clears an existing usedClauses array  for the given predicate, or creates a new empty list.
     *
     * @param predicate a predicate
     * @return the old cleared or new usedClauses list.
     */
    private ArrayList<Clause> clearUsedClauses(int predicate) {
        ArrayList<Clause> usedClauses = usedClausesArray[predicate];
        if (usedClauses == null) {
            usedClauses = new ArrayList<>();
            usedClausesArray[predicate] = usedClauses;}
        else usedClauses.clear();
        return usedClauses;}

    /**Joins the dependencies and the usedClauses (if trackReasoning == true) of a given clause.
     * <br>
     * truePredicate is either a newly derived predicate from the given clause,
     * or a selected predicate which caused the clause to be locally false.
     * <br>
     * The dependencies are put into dependentSelections[truePredicate] and
     * the usedClauses are put into usedClauses[truePredicate]<br>
     * Predicates which are globally true/false are ignored.<br>
     *
     * @param clause The clause whose dependencies will be joined.
     * @param truePredicate the predicate which is to be made locally true
     * @return The IntArrayList containing the joined dependencies (maybe empty)
     */
    protected IntArrayList joinDependencies(Clause clause, int truePredicate) {
       IntArrayList joinedDependencies = clearDependencies(truePredicate);
        ArrayList<Clause> usedClauses = null;
        if(trackReasoning) {usedClauses = clearUsedClauses(truePredicate); usedClauses.add(clause);}
        for(Literal literalObject : clause.literals) {
            int predicate = Math.abs(literalObject.literal);
            if(predicate != truePredicate && model.status(predicate) == 0 && localStatus(predicate) != 0){
                addIfNotContained(joinedDependencies,dependentSelections[predicate]);
                if(trackReasoning) {addIfNotContained(usedClauses,usedClausesArray[predicate]);}}}
        return joinedDependencies;}

    /** joins all the clauses used to derive the falseClause and puts them into usedClauseArray[predicate]
     *
     * @param falseClause a locally false clause
     * @param predicate the selected predicate which is to be negated in backtracking.
     */
    protected void joinUsedClauses(Clause falseClause, int predicate) {
        ArrayList<Clause> usedClauses = usedClausesArray[predicate];
        if(usedClauses == null) {usedClauses = new ArrayList<>(); usedClausesArray[predicate] = usedClauses;}
        else usedClauses.clear();
        usedClauses.add(falseClause);
        for(Literal literalObject : falseClause.literals) {
            addIfNotContained(usedClauses,usedClausesArray[Math.abs(literalObject.literal)]);}}

    /** The selected predicate which has the last position in predicatePositions.
     * <br>
     * Literals with a global truth value are ignored.
     *
     * @param dependencies The list of selected predicates which caused the derivation of a true literal or an empty clause.
     * @return The selected predicate which has the last position in predicatePositions.
     */
    protected int getLastSelectedPredicate(IntArrayList dependencies) {
        if(dependencies == null) return 0;
        int lastPredicate = 0;
        int lastPosition  = 0;
        for(int i = 0; i < dependencies.size(); ++i) {
            int predicate = dependencies.getInt(i);
            if(model.status(predicate) != 0) continue;
            if(lastPredicate == 0 || predicatePositions[predicate] > lastPosition) {
                lastPredicate = predicate;
                lastPosition = predicatePositions[lastPredicate];}}
        return lastPredicate;}

    /** The selected predicate which has the last position in the clause's dependecies.
     *
     * @param clause A clause with some locally true/false predicates
     * @return The selected predicate which has the last position in predicatePositions.
     */
    protected int getLastSelectedPredicate(Clause clause) {
        int lastPredicate = 0;
        for(Literal literalObject : clause.literals) {
            int predicate = Math.abs(literalObject.literal);
            IntArrayList dependencies = dependentSelections[predicate];
            if(dependencies != null) {
                int lastPred = getLastSelectedPredicate(dependencies);
                if(lastPred != 0 &&  (lastPredicate == 0 || predicatePositions[lastPred] > predicatePositions[lastPredicate])) {
                    lastPredicate = lastPred;}}}
        return lastPredicate;}

    /** backtracks to the given predicate
     * <br>
     * The local model after lastSelectedPredicate is cleared,
     * currentlyTrueLiterals gets shrunk to the position of the lastSelectedPredicate
     *
     * @param lastSelectedPredicate to where backtrackTo
     */
    protected void backtrackTo(int lastSelectedPredicate)  {
        ++statistics.backtrackings;
        int backjumps = 0;
        for(int i = currentlyTrueLiterals.size()-1; i >= 0; --i) {
            int predicate = Math.abs(currentlyTrueLiterals.getInt(i));
            if(predicate == 0) {++backjumps; --recursionDepth; continue;}
            localModel[predicate] = 0;
            if(predicate == lastSelectedPredicate) {
                System.out.println("BACK " + lastSelectedPredicate + "\n"+toStringLocalModel());
                currentlyTrueLiterals.size(i-1);
                if(backjumps > 1) ++statistics.backtrackings;
                return;}}}

    // incorporation of global changes

    /** incorporates new globally true literals.
     * <br>
     * Selected and derived literals which have a new global truth value are removed from
     * the search. selectedPredicatePosition is might be changed.
     * If a locally derived literal is globally false then the last selected literal which
     * implies this derived literal is made false as well.
     *
     * @throws Unsatisfiable if the model discovered a contradiction.
     */
    protected void incorporateGlobalChanges() throws Unsatisfiable {
        ++statistics.incoporations;
        boolean selected = false;
        for(int i = 0; i < currentlyTrueLiterals.size(); ++i) {
            int literal = currentlyTrueLiterals.getInt(i);
            if(literal == 0) {selected = true; continue;}
            if(selected) {
                selected = false;
                switch(model.status(literal)) {
                    case 0:  continue;
                    case 1:  removeSelectedTrueLiteral(i); i -= 2; continue;
                    case -1: removeSelectedFalseLiteral(i); return;}}
            else {
                switch(model.status(literal)) {
                    case 0:  continue;
                    case 1:  removeDerivedTrueLiteral(i--); continue;
                    case -1: removeDerivedFalseLiteral(i); return;}}}}

    /** removes a selected literal, which is globally true, from the search.
     * <br>
     * The selected literal is removed from currentlyTrueLiterals and all further literals are shifted backwards by 2 positions.<br>
     * The selected literal is removed from all dependencies of the derived literals after position.
     * <br>
     * Example: currentlyTrueLiterals = 0,p,p1,...,pn, 0,q,q1,,,  and q ist true then<br>
     * currentlyTrueLiterals = 0,p,p1,...,pn,q1,,,  and the qi don't depend on q anymore.
     * <br>
     * The selectedPredicatePosition remains as it is.
     * <br>
     * If the true literal is the first selected literal then all literals which are derived from this literal are made true
     * (might not be necessary because clauseList should have done this already).
     * <br>
     * The incorporation loop over currentlyTrueLiterals must continue.
     *
     * @param position the position of a selected literal in currentlyTrueLiterals.
     * @throws Unsatisfiable if the model discovers a contradiction.
     */
    private void removeSelectedTrueLiteral(int position) throws Unsatisfiable {
        int selectedLiteral = currentlyTrueLiterals.getInt(position);

        if(position == 1) {
            for(int j = 2; j < currentlyTrueLiterals.size(); ++j) {
                int literal = currentlyTrueLiterals.getInt(j);
                if(literal == 0) {removeRange(currentlyTrueLiterals,0,j); break;}
                if(model.status(literal) == 0) incGlobChAddTrueLiteral(selectedLiteral,literal);} // might not be necessary

            for(int j = 2; j < currentlyTrueLiterals.size(); ++j) {
                int literal = currentlyTrueLiterals.getInt(j);
                if(literal != 0) {
                    IntArrayList dependencies = dependentSelections[Math.abs(literal)];
                    if(dependencies != null) dependencies.rem(selectedLiteral);}}
            return;}

        for(int j = position+1; j < currentlyTrueLiterals.size(); ++j) {
            int literal = currentlyTrueLiterals.getInt(j);
            if(literal == 0) continue;
            IntArrayList dependencies = dependentSelections[Math.abs(literal)];
            if(dependencies != null) dependencies.rem(selectedLiteral);}
        removeRange(currentlyTrueLiterals,position-1,2);} // remove 0,selectedLiteral

    /** makes the literal globally true, while adding the corresponding inference step, and prints to the monitor
     * <br>
     * This is called when the toplevel selected literal is actually globally true.
     * The all derived literals are also made true.
     *
     * @param selectedLiteral a selected true literal
     * @param literal         a locally derived literal
     * @throws Unsatisfiable  if the model discovers a contradiction.
     */
    private void incGlobChAddTrueLiteral(int selectedLiteral, int literal) throws Unsatisfiable {
        if(monitoring)
            monitor.accept("incorprateGlobalChanges: globally true selected literal " +
                    Symboltable.toString(selectedLiteral,symboltable) + " causes locally derived literal " +
                    Symboltable.toString(literal,symboltable) + " to become globally true");
        InferenceStep step = trackReasoning ?
            new InfIndirectInference(selectedLiteral,model.getInferenceStep(selectedLiteral),
                    literal, usedClausesArray[Math.abs(literal)]) : null;
        model.add(myThread,literal,step);}

    /** removes a selected literal, which is globally false, from the search.
     * <br>
     * Example: currentlyTrueLiterals = 0,p,p1,...,pn, 0,q,q1,,,  and q ist false then<br>
     * currentlyTrueLiterals = 0,p,p1,...,pn, and the local model of all remaining literals is zeroed.
     * <br>
     * The selectedPredicatePosition is set to the predicatePosition of the last selected predicate which has no global truth value.
     * Therefor the new selected predicate can be chosen after this predicate.
     * <br>
     * If the false literal was the very first selected literal, then currentlyTrueLiterals is emptied and
     * selectedPredicatePosition is set to the predicatePosition of the false literal.<br>
     * In the other case the selectedPredicatePosition is set to the predicatePosition of the last selected predicate
     * without a global truth value.
     * <br>
     * The incorporation loop over currentlyTrueLiterals stops here.
     *
     * @param position the position of a selected literal in currentlyTrueLiterals.
     */
    private void removeSelectedFalseLiteral(int position) {
        int selectedLiteral = currentlyTrueLiterals.getInt(position);
        for(int j = position+1; j < currentlyTrueLiterals.size(); ++j) { // the local truth value all literals after position are zeroed
            int literal = currentlyTrueLiterals.getInt(j);
            if(literal != 0) localModel[Math.abs(literal)] = 0;}

        currentlyTrueLiterals.size(position-1); // remove all items from this position on.

        if(currentlyTrueLiterals.isEmpty()) {
            selectedPredicatePosition = predicatePositions[Math.abs(selectedLiteral)];
            return;}
        for(int j = currentlyTrueLiterals.size()-1; j >= 0; --j) { // find the previously selected literal without global truth value.
            int literal = currentlyTrueLiterals.getInt(j);
            if(literal == 0) { // the next one is a selected literal
                selectedPredicatePosition = predicatePositions[Math.abs(currentlyTrueLiterals.getInt(j+1))];
                return;}}}

    /** removes a derived literal, which is globally true, from the search.
     * <br>
     * The derived literal is removed from currentlyTrueLiterals and all further literals are shifted backwards by 1 position.<br>
     * The derived literal's dependencies is cleared.
     *
     * @param position the position of a derived literal in currentlyTrueLiterals.
     */
    private void removeDerivedTrueLiteral(int position) {
        int derivedLiteral = currentlyTrueLiterals.getInt(position);
        int predicate = Math.abs(derivedLiteral);
        if(trackReasoning) clearUsedClauses(predicate);
        clearDependencies(predicate);
        removeRange(currentlyTrueLiterals,position,1);}


    /** removes a derived literal, which is globally false, from the search.
     * <br>
     * The last selected literal which implies the derived literal is determined.
     * If this is the top-level selected literal then
     * the negation of this literal is added to the global model,
     * Consequences of this new true literal are not yet determined.
     * In the other cases the last selected literal is negated and backtracking performed.
     *
     * @param position the position of a derived literal in currentlyTrueLiterals.
     * @throws Unsatisfiable if the model discovers a contradiction.
     */
    private void removeDerivedFalseLiteral(int position) throws Unsatisfiable {
        int derivedLiteral = currentlyTrueLiterals.getInt(position);
        IntArrayList dependencies = dependentSelections[Math.abs(derivedLiteral)];
        int lastSelectedPredicate = getLastSelectedPredicate(dependencies);
        selectedPredicatePosition = predicatePositions[lastSelectedPredicate];
        dependencies.clear();
        int newTrueLiteral = -firstSign*lastSelectedPredicate;
        if(trackReasoning) clearUsedClauses(Math.abs(newTrueLiteral));
        if(currentlyTrueLiterals.indexOf(lastSelectedPredicate) == 1) {
            if(monitoring) {
                monitor.accept("incorparateGlobalChanges: derived literal " + Symboltable.toString(derivedLiteral,symboltable) +
                        " which is globally false causes new false selected literal: " + Symboltable.toString(-newTrueLiteral,symboltable));}
            InferenceStep step = trackReasoning ?
                new InfIndirectInference(-derivedLiteral, model.getInferenceStep(-derivedLiteral),
                        newTrueLiteral, usedClausesArray[Math.abs(derivedLiteral)]) : null;
            currentlyTrueLiterals.clear();
            model.add(myThread,newTrueLiteral,step);}
        else {
            backtrackTo(lastSelectedPredicate);
            makeLocallyTrue(newTrueLiteral);}}



    /** initializes the predicate sequence.
         * <br>
         * The predicates are sorted as follows:<br>
         * - seed &gt;= 0:              randomly<br>
         * - predicateArrangement == 1: just the sequence of natural numbers: 1,2,...<br>
         * - predicateArrangement == 2: just the inverse sequence of natural numbers: n,n-1,...<br>
         * - predicateArrangement == 3: predicates with more literal occurrences first<br>
         * - predicateArrangement == 4: predicates with less literal occurrences first.<br>
         * The arrays may be reused for different problems.
         *
         * @param predicateArrangement see above
         * @param seed for the random number generator
         */
    protected void initializePredicateSequence(int predicateArrangement, int seed) {
        if(predicateSequence == null || predicateSequence.length < predicates+1) {
            predicateSequence  = new int[predicates+1];
            predicatePositions = new int[predicates+1];}
        if(seed >= 0) {initializePredicateSequenceRandomly(seed); return;}

        switch(predicateArrangement) {
            case 1:
                for (int predicate = 1; predicate <= predicates; ++predicate) {
                    predicateSequence[predicate] = predicate;
                    predicatePositions[predicate] = predicate;}
                return;
            case 2:
                for (int predicate = 1; predicate <= predicates; ++predicate) {
                    predicateSequence[predicates-predicate+1] = predicate;
                    predicatePositions[predicate] = predicates-predicate+1;}
                return;}
        int sign = (predicateArrangement == 3) ? -1 : 1;
        Integer[] sequence = new Integer[predicates +1]; // sorting with a comparator works only with Object-arrays.
        for (int predicate = 1; predicate <= predicates; predicate++) {sequence[predicate] = predicate;}
        Arrays.sort(sequence,1,predicates+1,
                Comparator.comparingInt(predicate ->
                        sign * (clauseList.literalIndex.size(predicate) + clauseList.literalIndex.size(-(int)predicate))));
        for(int predicate = 1; predicate <= predicates; ++ predicate) {
            predicateSequence[predicate] = sequence[predicate];
            predicatePositions[predicateSequence[predicate]] = predicate;}}

    /**
     * Initializes the predicate sequence randomly based on the given seed.
     *
     * @param seed The seed for the random number generator.
     */
    protected void initializePredicateSequenceRandomly(int seed) {
        for(int predicate = 1; predicate <= predicates; ++predicate) predicateSequence[predicate] = predicate;
        shuffleArray(predicateSequence,1,predicates, seed);
        for(int position = 1; position <= predicates; ++position) {
            predicatePositions[predicateSequence[position]] = position;}}

    /** initializes the local to be synchronous to the global model.
     */
    protected void initializeLocalModel() {
        if(localModel == null || localModel.length < predicates+1) localModel = new byte[predicates+1];
        model.copy(localModel);}

    /** sets the local truth status value of the literal.
     * <br>
     * The literal is assumed to be locally true.<br>
     * Although the method is synchronized, two different threads can set contradictory values.
     * This is checked and reported.
     *
     * @param literal  a derived true literal.
     * @return false if a contradiction is found, otherwise true;
     */
    protected synchronized boolean makeLocallyTrue(int literal) {
        if(literal > 0) {
            if(localModel[literal] == -1) return false;
            localModel[literal] = 1;}
        else {if(localModel[-literal] == 1) return false;
              localModel[-literal] = -1;}
        return true;}


    /**
     * Retrieves the status value of a literal in the local model.
     *
     * @param literal The literal to check.
     * @return The truth value of the literal in the local model.
     */
    protected byte localStatus(int literal) {
        return literal > 0 ? localModel[literal] : (byte)-localModel[-literal]; }


    /** Converts LocalModel to String representation.
     *
     * @return The String representation of LocalModel.
     */
    public String toStringLocalModel() {
        StringBuilder st = new StringBuilder();
        int counter = 0;
        for (int predicate = 1; predicate <= predicates; ++predicate) {
            int sign = localModel[predicate];
            if(sign != 0) {
                st.append(sign*predicate).append(",");
                ++counter;
                if(counter % 50 == 0) st.append("\n");}}
        if(counter == 0) return "";
        String str = st.toString();
        return str.substring(0,str.length()-1);}


    /** Retrieves the statistics for the current object.
     *
     * @return the statistics.
     */
    @Override
    public Statistic getStatistics() {
        return statistics;}

    /** Returns a string representation of the current state of the backtracker.
     *<br>
     * The returned string includes information about the Backtracker object's solver ID,
     * predicate sequence, predicate positions, clauses, recursion level, globally true predicates,
     * selected predicates, and derived predicates.
     *
     * @return a string representation of the Backtracker object
     */
    public String toString() {
        StringBuilder st = new StringBuilder();
        st.append("Backtracker_").append(solverId);
        st.append("\nPredicate Sequence\n   ").append(Arrays.toString(predicateSequence));
        st.append("\nPredicate Positions\n   ").append(Arrays.toString(predicatePositions));
        st.append("\nClauses:\n").append(clauseList.toString("clauses",symboltable));
        st.append("\nGlobal Model: ").append(model.toString(symboltable));
        st.append("\nLocal Model:  ").append(toStringLocalModel()).append("\n");

        if(!currentlyTrueLiterals.isEmpty()) {
            st.append("Selected and Derived Literals:\n");
            boolean selected = false;
            for(int i = 0; i < currentlyTrueLiterals.size(); ++i) {
                int literal = currentlyTrueLiterals.getInt(i);
                if(literal == 0) {selected = true; continue;}
                if(selected) {st.append("S:"); selected = false;}
                st.append(Symboltable.toString(literal,symboltable)).append(",");}}
        return st.toString();}

    /**
     * Converts the dependencies of a given clause into a string representation.
     *
     * @param clause the clause whose dependencies are to be converted
     * @return a string representation of the dependencies of the given clause
     */
    public String toStringDependencies(Clause clause) {
        StringBuilder sb = new StringBuilder();
        for(Literal literalObject : clause.literals) {
            int literal = literalObject.literal;
            IntArrayList dependencies = dependentSelections[Math.abs(literal)];
            sb.append("L: ").append(literal).append(": ").append(dependencies).append(", ");}
        return sb.toString();}
}
