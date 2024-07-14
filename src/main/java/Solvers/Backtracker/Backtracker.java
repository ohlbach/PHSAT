package Solvers.Backtracker;

import Datastructures.*;
import Datastructures.Clauses.Quantifier;
import Datastructures.Results.Result;
import Datastructures.Results.Satisfiable;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Statistics.Statistic;
import InferenceSteps.InferenceStep;
import Management.Parameter;
import Management.Parameters;
import Management.ProblemSupervisor;
import Solvers.Solver;
import Utilities.Utilities;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.function.Consumer;

import static Utilities.Utilities.addIfNotContained;
import static Utilities.Utilities.shuffleArray;

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

    /** the level of the recursion and the index of selectedPredicates */
    int recursionLevel = -1;

    /** maps derived literals to the selected true literals which cause the derivation of the literal.
     * The lists are kept sorted according to the predicateSequence,
     * such that the last element is the selected literal to which backtracking is necessary. */
    protected IntArrayList[] dependentSelections;

    /** contains the derived predicates.*/
    protected IntArrayList derivedPredicates;

    /** points to the end (first free index) of the derivedPredicates in the current recursion level */
    int derivedPredicatesEnd;

    /** maps the selected predicates to the first index in derivedPredicates where the derived predicates start. */
    int[] derivedPredicatesStarts;

    /** collects the globally true literals which are still to be processed. */
    private final IntArrayList globallyTrueLiterals = new IntArrayList();

    protected ArrayList<Clause>[] usedClausesArray;

    StatisticsBacktracker statistics;


    /** keeps the local candidate model */
    byte[] localModel;

    /** stores the threads which are used to propagateInThread derived true predicates. */
    public PropagatorPool propagatorPool;

    public Consumer<String> monitor = null;

    /** is used to signal the end of the propagator jobs.
     * If a propagator founds a false clause, the false clause is inserted.
     * If all propagator jobs are finished, 'this' is inserted.
     * This way propagateSelectedLiteral can wait until all propagator jobs are finished. */
    private final BlockingQueue<Object> propagatorQueue = new LinkedBlockingQueue<>(2);

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
    private synchronized Clause falseClauseFound(Clause clause) {
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
     */
    public void initialize(Thread myThread, ProblemSupervisor problemSupervisor) {
        super.initialize(myThread,problemSupervisor);
        clauseList = problemSupervisor.clauseList;
        model.addObserver(myThread, (literal,step) -> addGloballyTrueLiteral(literal));
        monitor = monitoring ? (message) -> super.monitor.println(solverId+"_" + solverNumber, message) : null;
        startTime = System.nanoTime();
        propagatorQueue.clear();
        globallyTrueLiterals.clear();
        recursionLevel = -1;
        if(selectedPredicates == null || selectedPredicates.length < predicates) selectedPredicates = new int[predicates+1];
        if(dependentSelections == null || dependentSelections.length < predicates +1)
            dependentSelections = new IntArrayList[predicates+1];
        if(derivedPredicates == null) derivedPredicates = new IntArrayList(predicates);
        else                          derivedPredicates.ensureCapacity(predicates);
        derivedPredicates.clear();
        derivedPredicatesEnd = 0;
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
        int nextPredicateIndex = 0;
        int selectedLiteral = 0;
        try{
        while(!myThread.isInterrupted()){
            if(result != null) throw result; // found by a Propagator thread

            if(myThread.isInterrupted()) return null;

            if(clauseList.isEmpty() || (nextPredicateIndex = findNextPredicateIndex(nextPredicateIndex+1)) == 0) {
                model.exchangeModel(localModel);
                throw new Satisfiable(problemId,solverId, model);}

            int selectedPredicate = predicateSequence[nextPredicateIndex];
            if(monitoring) monitor.accept("Selected predicate " + Symboltable.toString(selectedPredicate,symboltable));
            ++recursionLevel;
            selectedPredicates[recursionLevel] = selectedPredicate;
            derivedPredicatesStarts[selectedPredicate] = derivedPredicatesEnd; // initially 0, changed in backtrack
            derivedPredicates.add(selectedPredicate);
            selectedLiteral = firstSign * selectedPredicate;
            int predicate = propagateSelectedLiteral(selectedLiteral);
            if(predicate > 0) {nextPredicateIndex = predicateSequence[predicate]-1; }}}
        catch(Result result) {
            result.complete(problemId,solverId,startTime);
            return result;}
         return null;}



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
     * @param selectedLiteral
     * @return 0 (no false clause found or thread is interrupted) or the selected predicate to be backtracked to.
     * @throws Unsatisfiable if backtracked to the top-level and a contradiction was found.
     */
    int propagateSelectedLiteral(int selectedLiteral) throws Result {
        propagatorThreadCounter = 0;
        propagatorQueue.clear();
        setLocalStatus(selectedLiteral);
        int selectedPredicate = Math.abs(selectedLiteral);
        dependentSelections[selectedPredicate] = null;
        propagateLocally(selectedLiteral);
        try{Object object = propagatorQueue.take(); // waits until all propagatorJobs are finished, or one them found a false clause.
            if(object != this) { // object is a false clause
                Clause falseClause = (Clause)object;
                IntArrayList dependencies = joinDependencies(falseClause, 0,new IntArrayList());
                int lastSelection = getLastSelection(dependencies);
                if(trackReasoning) joinUsedClauses(falseClause,lastSelection);
                return backtrack(lastSelection,dependencies,false);}}
        catch(InterruptedException ignore) {return 0;}
        return 0;}

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
        if(propagateLocally(literal)) propagatorPool.jobFinished(this); // false clause found
        else decrementPropagatorCounter();}


    /** propagates the truth of the trueLiteral locally.
     * <br>
     * Derived true predicates activate a Propagator thread.<br>
     * A false clause is inserted into the propagatorQueue and causes backtracking
     *
      *@param trueLiteral a locally true trueLiteral
     * @return true if a false clause has been found.
     */
    boolean propagateLocally(int trueLiteral)  {
        setLocalStatus(trueLiteral);
         Thread currentThread = Thread.currentThread(); // may be a Propagator thread
         for(int sign = 1; sign >= -1; sign -= 2) {
            Literal literalObject = clauseList.literalIndex.getFirstLiteral(sign*trueLiteral);
            while(literalObject != null && !currentThread.isInterrupted() && !myThread.isInterrupted()) {
                Clause clause = literalObject.clause;
                if((clause.quantifier != Quantifier.OR) || sign == -1) { // true trueLiteral in an OR: ignore clause
                    Clause falseClause = analyseClause(clause); // may produce new Propagator jobs
                    if(falseClause != null) {falseClauseFound(clause); return true;}}
                literalObject = (Literal)literalObject.nextItem;}}
        return false;}

    /** Analyzes a clause given the current local model.
     * <p>
     * The following cases are possible:<br>
     * - the clause is already true: return null; <br>
     * - the clause is already false: return the clause; <br>
     * - making an unsigned literal true causes the clause to become false: make the literal false;<br>
     * - making an unsigned literal false causes the clause to become false: make the literal true.
     *
     * @param clause The clause to be analyzed.
     * @return the clause if it is false already (locally or globally), otherwise null.
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
                        if(unsignedLiteral != null) return null; // two unsigned literals: nothing to be done
                        unsignedLiteral = literalObject; break;
                    case 1: return null;}}                   // clause is true;
            if(unsignedLiteral == null) {return clause;} // all literals are false. backtrack
            makeLiteralLocallyTrue(clause,unsignedLiteral,1);        // all other literals are false
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

        if(min <= trueLiterals && trueLiterals <= max) return null; // clause is true.

        // try to derive new true or false literals
        for(Literal literalObject : clause.literals) {
            if(localStatus(literalObject.literal) == 0) {
                int trueLits = trueLiterals + literalObject.multiplicity;
                if(!(min <= trueLits && trueLits <= max)) { // making it true causes a contradiction
                    makeLiteralLocallyTrue(clause,literalObject,-1); // literal must be false
                    continue;}
                trueLits = trueLiterals + unsignedLiterals - literalObject.multiplicity;
                if(!(min <= trueLits && trueLits <= max)) { // making it false causes a contradiction
                    makeLiteralLocallyTrue(clause,literalObject,1);}}}
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
        int literal = sign*literalObject.literal;
        int predicate = Math.abs(literal);
        IntArrayList depSelections = dependentSelections[predicate];
        if(depSelections == null) {depSelections = new IntArrayList(); dependentSelections[predicate] = depSelections;}
        dependentSelections[predicate] = joinDependencies(clause,predicate,depSelections);
        setLocalStatus(literal);
        propagatorPool.addPropagatorJob(this,literal);}

    /** Adds a literal to the list of globally true predicates.
     *
     * @param literal The literal to be added.
     */
    public synchronized void addGloballyTrueLiteral(int literal) {
        globallyTrueLiterals.add(literal);}

    /** Retrieves a globally true literal from the list of globally true predicates.
     *
     * @return The globally true literal. If the list is empty, returns 0.
     */
    public synchronized int getGloballyTrueLiteral() {
        if(globallyTrueLiterals.isEmpty()) return 0;
        int literal = globallyTrueLiterals.getInt(globallyTrueLiterals.size()-1);
        globallyTrueLiterals.removeInt(globallyTrueLiterals.size()-1);
        return literal;}

    protected int backtrackPredicate;
    protected boolean backtrackTryAgain;
    protected boolean backtrackTryNegated;
    protected IntArrayList backtrackDependencies;



    /**Joins the dependencies of a given clause to the provided IntArrayList.
     * <br>
     * Predicates which are globally true/false are ignored.<br>
     * If trackReasing = true then all clauses used to derive truePredicate are joined and put into usedClauseArray[truePredicate]
     *
     * @param clause The clause whose dependencies will be joined.
     * @param truePredicate the predicate which is to be made locally true/false
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
            int literal = literalObject.literal;
            if(model.status(literal) == 0 && localStatus(literal) != 0){
                int predicate = Math.abs(literal);
                joinDependencies(joinedDependencies, dependentSelections[predicate]);
                if(trackReasoning && truePredicate != 0 && predicate != truePredicate) {
                    ArrayList<Clause> usedClp = usedClausesArray[predicate];
                    if(usedClp != null) {
                        for(Clause cl : usedClp) if(!usedClauses.contains(cl)) usedClauses.add(cl);}}}}
        return joinedDependencies;}

    /**
     * Joins two dependencies by adding all elements from dependencies2 to dependencies1, skipping duplicates.
     * <br>
     * Predicates which are globally true/false are ignored.
     *
     * @param dependencies1 The first dependency list.
     * @param dependencies2 The second dependency list to join.
     */
    protected void joinDependencies(IntArrayList dependencies1, IntArrayList dependencies2){
        for(int predicate : dependencies2) {
            if(model.status(predicate) == 0 && !dependencies1.contains(predicate)) dependencies1.add(predicate);}}

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
    protected int getLastSelection(IntArrayList dependencies) {
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
    protected int getLastSelection(Clause clause) {
        int lastPredicate = 0;
        for(Literal literalObject : clause.literals) {
            int predicate = Math.abs(literalObject.literal);
            IntArrayList dependencies = dependentSelections[predicate];
            if(dependencies != null) {
                int lastPred = getLastSelection(dependencies);
                if(lastPred != 0 &&  (lastPredicate == 0 || predicatePositions[lastPred] > predicatePositions[lastPredicate])) {
                    lastPredicate = lastPred;}}}
        return lastPredicate;}

    /** backtracks to the given predicate
     * <p>
     * If tryAgain is true then the same predicate is selected again. <br>
     * Otherwise its negation is made locally true.<br>
     * If the backtracking ends at the first selection, its negation is made globally true.<br>
     * The current point of the search is indicated by the last predicate in selectedPredicates.
     *
     * @param predicate to where backtrack
     * @param dependencies of the clause or predicate which caused a contradiction (or null if tryAgain is true)
     * @param tryAgain if true then the predicate is tried again (because of global interactions), otherwise its negation is tried.
     * @return the predicate to which it is backtracked.
     * @throws Result if backtracked to the top-selection and model.add(-selection) causes a contradiction
     */
    synchronized int backtrack(int predicate, IntArrayList dependencies, boolean tryAgain) throws Result {
        int backtrackings = 0;
        for(;recursionLevel >= 0; --recursionLevel) {
            ++backtrackings;
            int selectedPredicate = selectedPredicates[recursionLevel];
            if(selectedPredicate == predicate) {
                if(tryAgain) {
                    ++statistics.retries;
                    if(monitoring) monitor.accept("Trying again predicate " + Symboltable.toString(predicate,symboltable));
                    clearLocalStatus(predicate);} // findNextPredicateIndex will find this predicate again.
                else {
                    if(monitoring) monitor.accept("Backtracking to predicate " + Symboltable.toString(predicate,symboltable));
                    ++statistics.backtrackings;
                    if(backtrackings > 1) ++statistics.backjumps;
                    setLocalStatus(-firstSign*selectedPredicate);
                    if(recursionLevel == 0) {
                        InferenceStep step = trackReasoning ?
                            new InfSelectedPredicateNegated(-firstSign*selectedPredicate,
                                    usedClausesArray[selectedPredicate]) : null;
                        model.add(null,-firstSign*selectedPredicate,step);}
                    else {dependencies.rem(selectedPredicate);
                         dependentSelections[selectedPredicate] = dependencies;}}

                for(int i = derivedPredicatesStarts[selectedPredicate]; i < derivedPredicatesEnd; ++i) {
                    clearLocalStatus(derivedPredicates.getInt(i));}
                derivedPredicatesEnd = derivedPredicatesStarts[selectedPredicate];}}
        return predicate;
    }


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
    protected void setLocalStatus(int literal) {
        if(literal > 0) localModel[literal] = 1; else localModel[-literal] = -1;}

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
        if(!globallyTrueLiterals.isEmpty()) {
            st.append("Globally True Literals: ");
            for(int literal : globallyTrueLiterals)
                st.append(Symboltable.toString(globallyTrueLiterals.getInt(literal),symboltable)).append(",");
            st.append("\n");}
        if(recursionLevel >= 0) {
            st.append("\nRecursion Level: ").append(recursionLevel).append("\n");
            st.append("Selected Predicates: ");
            for(int i = 0; i <= recursionLevel; ++i)
                st.append(Symboltable.toString(firstSign*selectedPredicates[i],symboltable)).append(",");
            st.append("Derived Literals:\n");
            for(int i = 0; i <= recursionLevel; ++i) {
                st.append(Symboltable.toString(selectedPredicates[i],symboltable)).append(": ");
                int end = (i < recursionLevel) ? derivedPredicatesStarts[selectedPredicates[i+1]] : derivedPredicatesEnd;
                for(int j = derivedPredicatesStarts[selectedPredicates[i]]; j < end; ++i) {
                    st.append(Symboltable.toString(derivedPredicates.getInt(j),symboltable)).append(",");}}}
        return st.toString();}
}
