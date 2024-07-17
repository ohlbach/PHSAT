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
import Management.Parameter;
import Management.Parameters;
import Management.ProblemSupervisor;
import Solvers.InterruptReason;
import Solvers.Solver;
import Utilities.Utilities;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.function.Consumer;

import static Utilities.Utilities.*;

public class Backtracker extends Solver {


    public static Parameters makeParameter() {
        Parameters parameters = new Parameters("Backtracker");
        Parameter selected = new Parameter("Select",Parameter.Type.Button,"false",false,
                "Select the Backtracker");
        parameters.add(selected);
        parameters.add(new Parameter("Up-sequence",Parameter.Type.Boolean,null,
                "Predicates in ascending order (1,2,3,...)"));
        parameters.add(new Parameter("Down-sequence",Parameter.Type.Boolean,null,
                "Predicates in decending order (...3,2,1)"));
        parameters.add(new Parameter("More-first",Parameter.Type.Boolean,null,
                "Predicates: more predicates first"));
        parameters.add(new Parameter("Less-first",Parameter.Type.Boolean,null,
                "Predicates: less predicates first"));
        Parameter seed = new Parameter("Seed", Parameter.Type.String, "-1",
                IntArrayList.wrap(new int[]{-1}),
                "Seed for random number generator (non-negative integer)");
        seed.setParser((String rangeString, StringBuilder errors) -> Utilities.parseIntRange(rangeString, -1, errors));
        parameters.add(seed);
        parameters.add(new Parameter("PositiveFirst",Parameter.Type.Boolean,"true",
                "Try positive predicates first"));
        parameters.add(new Parameter("NegativeFirst",Parameter.Type.Boolean,"false",
                "Try negative predicates first"));
        parameters.setDescription("Backtracking search (kind of Davis-Putnam Procedure)");
        return parameters;
    }

    /**
     * Generates and adds new problem generators based on the provided parameters.
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

    protected ClauseList clauseList;

     /** The predicates are initially sorted as follows:<br>
     * - predicateArrangement == 1: just the sequence of natural numbers: 1,2,...<br>
     * - predicateArrangement == 2: just the inverse sequence of natural numbers: n,n-1,...<br>
     * - predicateArrangement == 3: predicates with more literal occurrences first<br>
     * - predicateArrangement == 4: predicates with less literal occurrences first.*/
    private int predicateArrangement = 1;

    /** if seek &gt;= 0 then the predicates to be selected are randomly sorted */
    private int seed = -1;

    /** the final result */
    Result result = null;

    /** determines the sequence of predicates which are temporally set to true.*/
    protected int[] predicateSequence;
    /** maps each predicate to its position in the predicateSequence array- */
    protected int[] predicatePositions;

    /** +1: predicates are selected positively, -1: predicates are selected negatively.*/
    int firstSign = 1;

    /** stored the sequence of selected predicates. */
    int[] selectedPredicates;

    /** The index of the current selected predicate in the predicateSequence array*/
    int selectedPredicatePosition;

    /** the level of the recursion and the index of selectedPredicates */
    int recursionLevel = -1;

    /** maps derived literals to the selected true literals which cause the derivation of the literal.
     * The lists are kept sorted according to the predicateSequence,
     * such that the last element is the selected literal to which backtracking is necessary. */
    protected IntArrayList[] dependentSelections;

    /** contains the sequence of (0, selected literal) followed by the derived literals.*/
    protected IntArrayList currentlyTrueLiterals;

    /** maps a derived predicate to the clauses used to derive this predicate (if trackReasoning = true*/
    protected ArrayList<Clause>[] usedClausesArray;

    StatisticsBacktracker statistics;


    /** keeps the local candidate model */
    byte[] localModel;

    /** stores the threads which are used to propagateInThread derived true predicates. */
    public PropagatorPool propagatorPool;

    public Consumer<String> monitor = null;

    public boolean verify;

    /** is used to signal the end of the propagator jobs.
     * If a propagator founds a false clause, the false clause is inserted.
     * If all propagator jobs are finished, 'this' is inserted.
     * This way propagateSelectedLiteral can wait until all propagator jobs are finished. */
    private final BlockingQueue<Object> propagatorQueue = new LinkedBlockingQueue<>();

    /** counts the active propagator threads */
    private int propagatorThreadCounter = 0;

    /** increments the propagator counter */
    private synchronized void incrementPropagatorCounter() {++propagatorThreadCounter;}

    /** decrements the propagator counter.
     * <br>
     * If the counter is 0, 'this' is inserted into the propagatorQueue.
     * It means that no further true-literal propagation is going on.*/
    private synchronized void decrementPropagatorCounter() {
        --propagatorThreadCounter;
        if (propagatorThreadCounter == 0) {propagatorQueue.add(this);}}

    /** inserts a false clause into the propagatorQueue.
     *
     * @param clause a locally false clause
     * @return the false clause itself.
     */
    protected synchronized Clause falseClauseFound(Clause clause) {
        propagatorQueue.add(clause);
        return clause;}

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
     * @param myThread the calling thread
     * @param problemSupervisor the problemSupervisor for the solver.
     */
    public void initialize(Thread myThread, ProblemSupervisor problemSupervisor) {
        super.initialize(myThread,problemSupervisor);
        clauseList = problemSupervisor.clauseList;
        clauseList.addSolver(this);
        monitor = monitoring ? (message) -> super.monitor.println(solverId+"_" + solverNumber, message) : null;
        startTime = System.nanoTime();
        propagatorQueue.clear();
        recursionLevel = -1;
        if(selectedPredicates == null || selectedPredicates.length < predicates) selectedPredicates = new int[predicates+1];
        if(dependentSelections == null || dependentSelections.length < predicates +1)
            dependentSelections = new IntArrayList[predicates+1];
        if(currentlyTrueLiterals == null) currentlyTrueLiterals = new IntArrayList(predicates);
        else                                currentlyTrueLiterals.ensureCapacity(predicates);
        currentlyTrueLiterals.size(0);
        if(trackReasoning) {
            if((usedClausesArray == null || usedClausesArray.length < predicates+1)) usedClausesArray = new ArrayList[predicates+1];
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
        initializeLocalModel();
        initializePredicateSequence(predicateArrangement,seed);
        selectedPredicatePosition = -1;
        int selectedLiteral = 0;
        try{
        while(true){
            if(result != null) throw result; // found by a Propagator thread
            if(myThread.isInterrupted()) processInterrupt(null);
            if(clauseList.isEmpty() || (selectedPredicatePosition = findNextPredicateIndex(selectedPredicatePosition + 1)) == 0) {
                model.exchangeModel(localModel);
                throw new Satisfiable(problemId,solverId, model);}
            int selectedPredicate = predicateSequence[selectedPredicatePosition];
            selectedLiteral = firstSign * selectedPredicate;
            if(monitoring) monitor.accept("Selected literal " + Symboltable.toString(selectedLiteral,symboltable));
            currentlyTrueLiterals.add(0); currentlyTrueLiterals.add(selectedLiteral);
            IntArrayList dependencies = dependentSelections[selectedPredicate];
            if(dependencies == null) {dependencies = new IntArrayList(); dependentSelections[selectedPredicate] = dependencies;}
            else dependencies.clear();
            dependencies.add(selectedPredicate);
            propagateSelectedLiteral(selectedLiteral);}}
        catch(Result result) {
            result.complete(problemId,solverId,startTime);
            return result;}}

    /** used to stop the processing until a new true literal has been processed in the clauseList. */
    private BlockingQueue<Boolean> waitingQueue = new LinkedBlockingQueue<>(1);

    /** called by clauseList to cause the solver to stop until a true literal has been processed.
     */
    @Override
    public void waitForTrueLiteralProcessing() {
        interruptReason = InterruptReason.TRUELITERALPROCESSING;
        myThread.interrupt();}

    /** called by clauseList after a new true literal has been processed.*/
    @Override
    public void continueProcessing() {
        waitingQueue.add(true);}

    /** processes different kinds of interrupts.
     *
     * @param exception an unexpected exception or null.
     * @throws Aborted when another solver has found a solution and has sent an interrupt.
     * */
    private void processInterrupt(Exception exception) throws Result {
        if(interruptReason != null) {
            switch(interruptReason) {
                case TRUELITERALPROCESSING:
                    waitingQueue.clear();
                    clauseList.acknowledgeWaiting();
                    interruptReason = null;
                    try {waitingQueue.take();} // clauseList has incorporated new true literals.
                    catch (InterruptedException exception1) {processInterrupt(exception1);} // maybe the entire process is aborted
                    incorporateGlobalChanges();
                    return;
                case PROBLEMSOLVED:
                    throw new Aborted(problemId,solverId,startTime,"Interrupted by another thread");}}
        if(exception != null) {
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
     * is determined and returned as a result.
     * If no contradiction is detected then 0 is returned (and a further predicate has to be selected)
     *
     * @param selectedLiteral the selected literal
     * @return 0 (no false clause found or thread is interrupted) or the selected predicate to be backtracked to.
     * @throws Unsatisfiable if backtracked to the top-level and a contradiction was found.
     */
    protected void propagateSelectedLiteral(int selectedLiteral) throws Result {
        propagatorThreadCounter = 0;
        propagatorQueue.clear();
        makeLocallyTrue(selectedLiteral);
        Clause falseClause = propagateLocally(selectedLiteral); // may start propagator threads
        if(myThread.isInterrupted()) processInterrupt(null);
        if(falseClause == null) {
            try{Object object = propagatorQueue.take(); // waits until all propagatorJobs are finished, or one them found a false clause.
                if(object != this) { // object is a false clause
                    falseClause = (Clause)object;}}
            catch(InterruptedException exception){processInterrupt(exception); }}
        if(falseClause == null) return;
        int lastSelectedPredicate = getLastSelectedPredicate(falseClause);
        backtrackTo(lastSelectedPredicate);
        selectedPredicatePosition = predicatePositions[lastSelectedPredicate];
        if(currentlyTrueLiterals.isEmpty()) {
            model.add(myThread,-firstSign * lastSelectedPredicate);
            return;}
        if(trackReasoning) joinUsedClauses(falseClause,lastSelectedPredicate);}


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

    /** propagates the local truth of the given literal.
     * <br>
     * Derived unit clauses cause a new Propagator thread to be activated.<br>
     * If a locally false clause is found, it is inserted into the propagatorQueue,
     * and the propagatorPool is instructed to deactivate all jobs. <br>
     * If the propagatorCounter is 0, then 'this' is inserted into the propagatorQueue.
     */
    void propagateInThread(int literal) {
        incrementPropagatorCounter();
        if(propagateLocally(literal) != null) propagatorPool.jobFinished(this); // false clause found
        else decrementPropagatorCounter();}


    /** propagates the truth of the trueLiteral locally.
     * <br>
     * Derived true predicates activate a Propagator thread.<br>
     * A false clause is inserted into the propagatorQueue and causes backtracking<br>
     * The method may be called from the main thread and from the propagator threads.
     *
      *@param trueLiteral a locally true trueLiteral
     * @return true if a false clause has been found.
     */
    protected Clause propagateLocally(int trueLiteral)  {
        makeLocallyTrue(trueLiteral);
         Thread currentThread = Thread.currentThread(); // may be a Propagator thread
         for(int sign = 1; sign >= -1; sign -= 2) {
            Literal literalObject = clauseList.literalIndex.getFirstLiteral(sign*trueLiteral);
            while(literalObject != null && !currentThread.isInterrupted() && !myThread.isInterrupted()) {
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
            if(unsignedLiteral == null) {return clause;}            // all literals are false. backtrackTo
            makeLiteralLocallyTrue(clause,unsignedLiteral,1);  // all other literals are false
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
        if(trueLiterals > max || trueLiterals + unsignedLiterals < min) return clause; // clause is false

        if(min <= trueLiterals && trueLiterals <= max) { // clause is already true.
            if(max < clause.expandedSize) {              // more true literals might be dangerous
                for(Literal literalObject : clause.literals) {
                    if(localStatus(literalObject.literal) == 0 &&
                            trueLiterals + literalObject.multiplicity > max){    // making it true causes too many true literals
                        makeLiteralLocallyTrue(clause,literalObject,-1);}}} // literal must be false
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
        if(candidates == 1) makeLiteralLocallyTrue(clause,candidateLiteral,1);
        return null;}

    /**
     * Makes a literal locally true/false and adds the job to the propagatorPool
     * <br>
     * A propagator propagates the truth value of the literal to the other clauses.
     *
     * @param clause        The clause containing the literal.
     * @param literalObject The literal object to make true or false.
     * @param sign          The sign of the literal (-1 for literal is false, 1 for literal is true).
     */
    synchronized void makeLiteralLocallyTrue(Clause clause, Literal literalObject, int sign) {
        int trueLiteral = sign*literalObject.literal;
        if(verify) {
            if(!verifyTrueLiteral(clause,trueLiteral)) {
                System.err.println("verifyTrueLiteral failed: " + clause.toString(symboltable,0) +
                        " derived literal: " + Symboltable.toString(trueLiteral,symboltable) +
                        "\nLocal Model: " + toStringLocalModel());
                System.exit(1);}}
        currentlyTrueLiterals.add(trueLiteral);
        makeLocallyTrue(trueLiteral);
        int truePredicate = Math.abs(trueLiteral);
        IntArrayList depSelections = dependentSelections[truePredicate];
        if(depSelections == null) {depSelections = new IntArrayList(); dependentSelections[truePredicate] = depSelections;}
        dependentSelections[truePredicate] = joinDependencies(clause,truePredicate,depSelections);

        propagatorPool.addPropagatorJob(this,trueLiteral);}

    /** performs a model-based check for the derivation of a true literal from a clause in the local model.
     *
     * @param clause a clause
     * @param literal a derived true literal.
     * @return true if the verification succeeded.
     */
    protected boolean verifyTrueLiteral(Clause clause, int literal) {
        IntArrayList predicates = clause.predicates();
        int literalPosition = predicates.indexOf(Math.abs(literal));
        int nModels = 1 << predicates.size();
        int trueCases = 0;
        for (int model = 0; model < nModels; ++model) {
            if(compatibleLocally(model,predicates) &&
                    ((literal > 0) ? (model & (1 << literalPosition)) != 0 :
                                     (model & (1 << literalPosition)) == 0) &&
                    clause.isTrue(model,predicates)){++trueCases;}}
        return trueCases == 1;}


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


    /**Joins the dependencies of a given clause to the provided IntArrayList.
     * <br>
     * Predicates which are globally true/false are ignored.<br>
     * If trackReasing = true then all clauses used to derive truePredicate are joined and put into usedClauseArray[truePredicate]
     *
     * @param clause The clause whose dependencies will be joined.
     * @param truePredicate the predicate which is to be made locally true/false (0 if a false clause was found)
     * @param joinedDependencies The IntArrayList to which the dependencies will be joined.
     * @return The IntArrayList containing the joined dependencies (maybe empty)
     */
    protected IntArrayList joinDependencies(Clause clause, int truePredicate, IntArrayList joinedDependencies) {
        joinedDependencies.clear();
        ArrayList<Clause> usedClauses = null;
        if(trackReasoning && truePredicate != 0) {
            usedClauses = usedClausesArray[truePredicate];
            if(usedClauses == null) {usedClauses = new ArrayList<>(); usedClausesArray[truePredicate] = usedClauses;}
            else usedClauses.clear();
            usedClauses.add(clause);}
        for(Literal literalObject : clause.literals) {
            int predicate = Math.abs(literalObject.literal);
            if(predicate != truePredicate && model.status(predicate) == 0 && localStatus(predicate) != 0){
                addIfNotContained(joinedDependencies,dependentSelections[predicate]);
                if(trackReasoning && truePredicate != 0) {addIfNotContained(usedClauses,usedClausesArray[predicate]);}}}
        return joinedDependencies;}


    /** The selected predicate which has the last position in predicatePositions.
     * <br>
     * The last predicate is removed from the list.
     * Predicates with a global truth value are ignored.
     *
     * @param dependencies The list of selected predicates which caused the derivation of a true literal or an empty clause.
     * @return The selected predicate which has the last position in predicatePositions.
     */
    protected int getAndRemoveLastSelection(IntArrayList dependencies) {
        int lastPredicate = 0;
        int position = 0;
        int index = 0;
        for(int i = 0; i < dependencies.size(); ++i) {
            int predicate = dependencies.getInt(i);
            if(model.status(predicate) != 0) continue;
            if(position == 0 ||  predicatePositions[predicate] > position) {
                lastPredicate = predicate;
                position = predicatePositions[lastPredicate];
                index = i;}}
        dependencies.removeInt(index);
        return lastPredicate;}

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
     *
     * @param lastSelectedPredicate to where backtrackTo
     */
    protected void backtrackTo(int lastSelectedPredicate)  {
        for(int i = currentlyTrueLiterals.size()-1; i >= 0; --i) {
            int predicate = Math.abs(currentlyTrueLiterals.getInt(i));
            if(predicate == 0) continue;
            localModel[predicate] = 0;
            if(predicate == lastSelectedPredicate) {
                currentlyTrueLiterals.size(i-1);
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
        boolean selected = false;
        for(int i = 0; i < currentlyTrueLiterals.size(); ++i) {
            int literal = currentlyTrueLiterals.getInt(i);
            if(literal == 0) {selected = true; continue;}
            if(selected) {
                selected = false;
                switch(model.status(literal)) {
                    case 0:  continue;
                    case 1:  removeSelectedTrueLiteral(i); i-=2; continue;
                    case -1: removeSelectedFalseLiteral(i); return;
                }}
            else {
                switch(model.status(literal)) {
                    case 0:  continue;
                    case 1:  removeDerivedTrueLiteral(i--); continue;
                    case -1: removeDerivedFalseLiteral(i); return;}
            }
        }
    }

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
     * The incorporation loop over currentlyTrueLiterals must continue.
     *
     * @param position the position of a selected literal in currentlyTrueLiterals.
     */
    protected void removeSelectedTrueLiteral(int position) {
        int selectedLiteral = currentlyTrueLiterals.getInt(position);
        for(int j = position+1; j < currentlyTrueLiterals.size(); ++j) {
            int literal = currentlyTrueLiterals.getInt(j);
            if(literal == 0) continue;
            IntArrayList dependencies = dependentSelections[Math.abs(literal)];
            if(dependencies != null) dependencies.remove(selectedLiteral);}
        removeRange(currentlyTrueLiterals,position-1,2);} // remove 0,selectedLiteral

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
    protected void removeSelectedFalseLiteral(int position) {
        int selectedLiteral = currentlyTrueLiterals.getInt(position);
        for(int j = position+1; j < currentlyTrueLiterals.size(); ++j) {
            int literal = currentlyTrueLiterals.getInt(j);
            if(literal != 0) localModel[Math.abs(literal)] = 0;}

        currentlyTrueLiterals.size(position-2); // remove all items from this position on.

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
    protected void removeDerivedTrueLiteral(int position) {
        int derivedLiteral = currentlyTrueLiterals.getInt(position);
        IntArrayList dependencies = dependentSelections[Math.abs(derivedLiteral)];
        if(dependencies != null) dependencies.clear();
        removeRange(currentlyTrueLiterals,position,1);}


    /** removes a derived literal, which is globally false, from the search.
     * <br>
     * The last selected literal which implies the derived literal is determined.
     * The negation of this literal is added to the global model,
     * and incorporateGlobalChanges is called again.<br>
     * Consequences of this new true literal are not yet determined.
     *
     * @param position the position of a derived literal in currentlyTrueLiterals.
     */
    protected void removeDerivedFalseLiteral(int position) throws Unsatisfiable {
        int derivedLiteral = currentlyTrueLiterals.getInt(position);
        IntArrayList dependencies = dependentSelections[Math.abs(derivedLiteral)];
        int lastSelectedPredicate = getLastSelectedPredicate(dependencies);
        dependencies.clear();
        int newTrueLiteral = -firstSign*lastSelectedPredicate;
        if(monitoring) {
            monitor.accept("Globally false derived literal " + Symboltable.toString(derivedLiteral,symboltable) +
                    " causes new true selected literal " + Symboltable.toString(newTrueLiteral,symboltable));}
        model.add(myThread,newTrueLiteral,null);
        incorporateGlobalChanges();}


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
     * <br>
     * The local model may be reused for different problems.
     */
    protected void initializeLocalModel() {
        if(localModel == null || localModel.length < predicates+1) localModel = new byte[predicates+1];
        model.copy(localModel);}


    /** sets the local truth status value of the literal.
     * <br>
     * The literal is assumed to be locally true.
     *
     * @param literal  a derived true literal.
     */
    protected void makeLocallyTrue(int literal) {
        if(literal > 0) localModel[literal] = 1; else localModel[-literal] = -1;
    }

    /** resets the local status value of the literal and clears usedClauses[literal]
     *
     * @param literal  a derived literal.
     */
    protected void clearLocalStatus(int literal) {
        int predicate = Math.abs(literal);
        localModel[predicate] = 0;
        if(trackReasoning) {
            ArrayList<Clause> usedClauses = usedClausesArray[predicate];
            if(usedClauses != null) usedClauses.clear();}}


    /**
     * Retrieves the status value of a literal in the local model.
     *
     * @param literal The literal to check.
     * @return The truth value of the literal in the local model.
     */
    protected synchronized byte localStatus(int literal) {
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


    @Override
    public Statistic getStatistics() {
        return statistics;}

    /**
     * Returns a string representation of the current state of the backtracker.
     *
     * The returned string includes information about the Backtracker object's solver ID,
     * predicate sequence, predicate positions, clauses, recursion level, globally true predicates,
     * selected predicates, and derived predicates.
     *
     * @return a string representation of the Backtracker object
     */
    public String toString() {
        StringBuilder st = new StringBuilder();
        st.append("Backtracker ").append(solverId);
        st.append("\nPredicate Sequence\n   ").append(Arrays.toString(predicateSequence));
        st.append("\nPredicate Positions\n   ").append(Arrays.toString(predicatePositions));
        st.append("\nClauses:\n").append(clauseList.toString("clauses",symboltable));
        st.append("\nGlobal Model: ").append(model.toString(symboltable));
        st.append("\nLocal Model:  ").append(toStringLocalModel());

        if(recursionLevel >= 0) {
            st.append("\nRecursion Level: ").append(recursionLevel).append("\n");
            st.append("Selected Predicates: ");
            for(int i = 0; i <= recursionLevel; ++i)
                st.append(Symboltable.toString(firstSign*selectedPredicates[i],symboltable)).append(",");
            st.append("Derived Literals:\n");
            st.append(currentlyTrueLiterals.toString());}
        return st.toString();}
}
