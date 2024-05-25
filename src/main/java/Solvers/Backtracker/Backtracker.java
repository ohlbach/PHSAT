package Solvers.Backtracker;

import Datastructures.Clauses.Quantifier;
import Datastructures.LinkedItemList;
import Datastructures.LiteralIndex;
import Datastructures.Results.Result;
import Datastructures.Results.Satisfiable;
import Datastructures.Results.UnsatClause;
import Datastructures.Statistics.Statistic;
import Datastructures.Symboltable;
import InferenceSteps.InfUnitClause;
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
                "Predicates: more literals first"));
        parameters.add(new Parameter("Less-first",Parameter.Type.Boolean,null,
                "Predicates: less literals first"));
        Parameter seed = new Parameter("Seed", Parameter.Type.String, "-1",
                IntArrayList.wrap(new int[]{-1}),
                "Seed for random number generator (non-negative integer)");
        seed.setParser((String rangeString, StringBuilder errors) -> Utilities.parseIntRange(rangeString, -1, errors));
        parameters.add(seed);
        parameters.add(new Parameter("PositiveFirst",Parameter.Type.Boolean,"true",
                "Try positive literals first"));
        parameters.add(new Parameter("NegativeFirst",Parameter.Type.Boolean,"false",
                "Try negative literals first"));
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

     /** The predicates are initially sorted as follows:<br>
     * - predicateArrangement == 1: just the sequence of natural numbers: 1,2,...<br>
     * - predicateArrangement == 2: just the inverse sequence of natural numbers: n,n-1,...<br>
     * - predicateArrangement == 3: predicates with more literal occurrences first<br>
     * - predicateArrangement == 4: predicates with less literal occurrences first.*/
    private int predicateArrangement = 1;

    private int seed = -1;

    /** the final result */
    Result result = null;

    /** determines the sequence of predicates which are temporally set to true.*/
    protected int[] predicateSequence;
    /** maps each predicate to its position in the predicateSequence array- */
    protected int[] predicatePositions;

    /** +1: predicates are selected positively, -1: predicates are selected negatively.*/
    int firstSign = 1;

    /** The list of all clauses. The clauses are changed when a globally true literal is derived. */
    protected  LinkedItemList<Clause> clauses;

    /** Maps (positive and negative) literals to the Literal objects containing the literal */
    protected  LiteralIndex<Literal> literalIndex;

    /** stored the sequence of selected literals. */
    int[] selectedPredicates;

    /** the level of the recursion and the index of selectedPredicates */
    int recursionLevel = 0;

    /** maps derived literals to the selected true literals which cause the derivation of the literal.
     * The lists are kept sorted according th the predicateSequence,
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

    BacktrackerStatistics statistics;


    /** keeps the local candidate model */
    byte[] localModel;

    /** stores the threads which are used to propagateInThread derived true literals. */
    public PropagatorPool propagatorPool;

    public Consumer<String> monitor = null;

    /** is used to signal the end of the propagator jobs.
     * If a propagator founds a false clause, the false clause is inserted.
     * If all propagator jobs are finished, 'this' is inserted.
     * This way propagateSelectedLiteral can wait until all propagator jobs are finished. */
    private final BlockingQueue<Object> propagatorQueue = new LinkedBlockingQueue<>(2);

    /** counts the active propagator threads */
    private int propagatorCounter = 0;

    /** increments the propagator counter */
    private synchronized void incrementPropagatorCounter() {++propagatorCounter;}

    /** decrements the propagator counter.
     * <br>
     * If the counter is 0, 'this' is inserted into the propagatorQueue.*/
    private synchronized void decrementPropagatorCounter() {
        --propagatorCounter;
        if (propagatorCounter == 0) {propagatorQueue.add(this);}}

    /** inserts a false clause into the propagatorQueue.
     *
     * @param clause a locally false clause
     */
    private synchronized void falseClauseFound(Clause clause) {
        propagatorQueue.add(clause);}

    /** constructs a new Backtracker.
     *
     * @param solverNumber  for enumerating the walkers.
     */
    public Backtracker(int solverNumber, int predicateArrangement, int seed, int firstSign) {
        super(solverNumber);
        solverId = "Backtracker";
        this.predicateArrangement = predicateArrangement;
        this.seed = seed;
        this.firstSign = firstSign;
        myThread = Thread.currentThread();} // for testing purposes.

    /** adds the literals which are already true in the model to the task queue.
     * Installs the observer in the model.
     */
    public void initialize(Thread myThread, ProblemSupervisor problemSupervisor) {
        super.initialize(myThread,problemSupervisor);
        problemSupervisor.model.addObserver(myThread,
                (literal,step) -> addGloballyTrueLiteral(literal));
        monitor = monitoring ? (message) -> super.monitor.println(solverId+"_" + solverNumber, message) : null;
        }

    /** initializes all problem-specific data.
     * <br>
     * If the solver is reused, the data are reused as far as possible.*/
    protected void initializeProblemSpecifics() {
        startTime = System.nanoTime();
        propagatorQueue.clear();
        globallyTrueLiterals.clear();
        recursionLevel = 0;
        myThread = Thread.currentThread();
        clauses = new LinkedItemList<>("Clauses");
        if(literalIndex == null) literalIndex = new LiteralIndex<>(predicates);
        else literalIndex.ensureCapacity(predicates);
        if(selectedPredicates == null || selectedPredicates.length < predicates) selectedPredicates = new int[predicates+1];
        if(dependentSelections == null || dependentSelections.length < predicates +1)
            dependentSelections = new IntArrayList[predicates+1];
        if(derivedPredicates == null) derivedPredicates = new IntArrayList(predicates);
        else                          derivedPredicates.ensureCapacity(predicates);
        derivedPredicatesEnd = 0;
    }

    @Override
    public Result solveProblem() {
        initializeProblemSpecifics();
        readInputClauses();
        initializeLocalModel();
        initializePredicateSequence(predicateArrangement,seed);
        try{searchModel();
            System.out.println(statistics);}
        catch(Result result) {
            result.complete(problemId,solverId,startTime);
            return result;}
        return null;}

    /** integrates the normalized input clauses into the clauses-list and the literal index.
     */
    public void readInputClauses() {
        Clause clause;
        Solvers.Normalizer.Clause normalizedClause = normalizer.clauses.firstLinkedItem;
        while(normalizedClause != null) {
            clause = new Clause(normalizedClause);
            insertClause(clause);
            normalizedClause = (Solvers.Normalizer.Clause)normalizedClause.nextItem;}}

    /** inserts int-array version of the inputClauses into the internal datastructures (for testing purposes)
     * <br>
     * The clauses are not simplified in any way. They should be redundancy-free.
     *
     * @param inputClauses a list of int-array representations of the clauses.
     */
    protected void readInputClauses(int[]... inputClauses ) {
        for(int[] inputClause : inputClauses) {
            Clause newClause = new Clause(inputClause);
            insertClause(newClause);}}

    /** inserts a clause into the clause list and the literal index.
     *
     * @param clause a clause.
     */
    void insertClause(Clause clause) {
        for(Literal literalObject : clause.literals) literalIndex.addToBack(literalObject);
        clauses.addToBack(clause);}

    /** removes the clause from the literal index and from the clauses list.
     *
     * @param clause a clause to be removed.
     */
    void removeClause(Clause clause) {
        for(Literal literalObject : clause.literals) literalIndex.remove(literalObject);
        clauses.remove(clause);}

    /** removes a (false) literal from an OR-clause.
     * <p>
     * If the resulting clause is a unit clause, it is inserted into the model.
     * The unit clause is entirely removed.
     *
     * @param literalObject to be removed.
     * @throws Result if inserting a unit clause into the model causes a contradiction.
     */
    void removeLiteral(Literal literalObject) throws Result{
        Clause clause = literalObject.clause;
        assert clause.quantifier == Quantifier.OR;
        literalIndex.remove(literalObject);
        if(clause.removeLiteral(literalObject)) {
            InferenceStep step = trackReasoning ? new InfUnitClause(clause) : null;
            if(monitoring) {monitor.accept("Unit Clause " + clause.toString(symboltable,0));}
            model.add(null,clause.literals.get(0).literal,step);
            removeClause(clause);}}



     void searchModel() throws Result {
        int nextPredicateIndex = 0;
        int selectedLiteral = 0;
        while(!myThread.isInterrupted()){
            if(result != null) throw result; // found by a Propagator thread

            processGloballyTrueLiterals(); // no Propagator thread is active
            if(myThread.isInterrupted()) return;

            if(clauses.isEmpty() || (nextPredicateIndex = findNextPredicateIndex(nextPredicateIndex+1)) == 0) {
                model.exchangeModel(localModel);
                throw new Satisfiable(problemId,solverId, model);}

            int selectedPredicate = predicateSequence[nextPredicateIndex];
            selectedPredicates[recursionLevel] = selectedPredicate;
            derivedPredicatesStarts[selectedPredicate] = derivedPredicates.size();
            derivedPredicates.add(selectedPredicate);
            selectedLiteral = firstSign * selectedPredicate;
            ++recursionLevel;
            IntArrayList depSelections = propagateSelectedLiteral(selectedLiteral);
            if(depSelections != null) {
                int lastPredicate = getAndRemoveLastSelection(depSelections);
                backtrack(lastPredicate,depSelections,false);}}}

    int findNextPredicateIndex(int predicateIndex) throws Result {
        for(; predicateIndex <= predicates; ++predicateIndex) {
            int predicate = predicateSequence[predicateIndex];
            if (literalIndex.isEmpty(predicate)) continue;
            byte status = model.status(predicate);
            if (status != 0) {
                removeGloballyTrueLiteral(predicate*status); continue;}
            return predicateIndex;}
        return 0;}

    IntArrayList propagateSelectedLiteral(int selectedLiteral) throws Result {
        propagatorCounter = 0;
        propagatorQueue.clear();
        setLocalStatus(selectedLiteral);
        int selectedPredicate = Math.abs(selectedLiteral);
        dependentSelections[selectedPredicate] = null;
        Clause falseClause = propagateLocally(selectedLiteral);
        if(falseClause != null) return joinDependencies(falseClause, new IntArrayList());
        try{Object object = propagatorQueue.take(); // waits until all propgatorJobs are finished, or one them found a false clause.
            if(object != this) return joinDependencies((Clause)object, new IntArrayList());}
        catch(InterruptedException ignore) {return null;}
        return null;}

    /** propagates the local truth of the given literal.
     * <br>
     * Derived unit clauses cause a new Propagator thread to be activated.<br>
     * If a locally false clause is found, it is inserted into the propagatorQueue,
     * and the propagatorPool is instructed to deactivate all jobs. <br>
     * If the propagatorCounter is 0, then 'this' is inserted into the propagatorQueue.
     */
    void propagateInThread(int literal) {
        incrementPropagatorCounter();
        Clause falseClause = propagateLocally(literal);
        if(falseClause != null) {
            falseClauseFound(falseClause);
            propagatorPool.jobFinished(this);}
        else decrementPropagatorCounter();}





    Clause propagateLocally(int literal)  {
         setLocalStatus(literal);
         Thread currentThread = Thread.currentThread(); // may be a Propagator thread
         for(int sign = 1; sign >= -1; sign -= 2) {
            Literal literalObject = literalIndex.getFirstLiteral(sign*literal);
            while(literalObject != null && !currentThread.isInterrupted() && !myThread.isInterrupted()) {
                Clause clause = literalObject.clause;
                if((clause.quantifier != Quantifier.OR) || sign == -1) { // true literal in an OR: ignore clause
                    Clause falseClause = analyseClauseLocally(clause); // may produce new Propagator jobs
                    if(falseClause != null) return falseClause;}
                literalObject = (Literal)literalObject.nextItem;}}
        return null;}

    /**Analyzes a clause given the current local model.
     * <p>
     * The following cases are possible:<br>
     * - the clause is already true: return null; <br>
     * - the clause is already false: return the clause; <br>
     * - making an unsigned literal true causes the clause to become false: make the literal false;<br>
     * - making an unsigned literal false causes the to become false: make the literal true.
     *
     * @param clause The clause to be analyzed.
     * @return the clause if it is false already (locally or globally), otherwise null.
     */
    Clause analyseClauseLocally(Clause clause) {
        // Since disjunctions are frequent,
        // and only one passage through the literals is sufficient,
        // it is worth treating this case separately.
        if(clause.quantifier == Quantifier.OR) {
            Literal unsignedLiteral = null;
            for(Literal literalObject : clause.literals) {
                switch(getLocalStatus(literalObject.literal)) {
                    case 0:
                        if(unsignedLiteral != null) return null; // two unsigned literals: nothing to be done
                        unsignedLiteral = literalObject; break;
                    case 1: return null;}}                   // clause is true;
            if(unsignedLiteral == null) {return clause;} // all literals are false
            makeLiteralLocallyTrue(clause,unsignedLiteral,(byte)1);
            return null;}

        // all other clause types.
        int trueLiterals = 0;
        int unsignedLiterals = 0;
        for(Literal literalObject : clause.literals) {
            switch(getLocalStatus(literalObject.literal)) {
                case 0: unsignedLiterals += literalObject.multiplicity; break;
                case 1: trueLiterals += literalObject.multiplicity;}}
        int max = clause.max; int min = clause.min;
        if(trueLiterals > max || trueLiterals + unsignedLiterals < min) return clause;
        // too many or not enough true literals.
        if(min <= trueLiterals && trueLiterals <= max) return null; // clause is true.
        // try to derive new true or false literals
        for(Literal literalObject : clause.literals) {
            if(getLocalStatus(literalObject.literal) == 0) {
                int trueLits = trueLiterals + literalObject.multiplicity;
                if(!(min <= trueLits && trueLits <= max)) { // making it true causes a contradiction
                    makeLiteralLocallyTrue(clause,literalObject,(byte)-1);
                    continue;}
                trueLits = trueLiterals + unsignedLiterals - literalObject.multiplicity;
                if(!(min <= trueLits && trueLits <= max)) { // making it false causes a contradiction
                    makeLiteralLocallyTrue(clause,literalObject,(byte)1);}}}
        return null;}

    /**
     * Makes a literal locally true and add the job to the propagatorPool
     *
     * @param clause        The clause containing the literal.
     * @param literalObject The literal object to make true.
     * @param sign          The sign of the literal (-1 for negation, 1 for positive).
     */
    synchronized void makeLiteralLocallyTrue(Clause clause, Literal literalObject, Byte sign) {
        int literal = sign*literalObject.literal;
        int predicate = Math.abs(literal);
        IntArrayList depSelections = dependentSelections[predicate];
        if(depSelections == null) {depSelections = new IntArrayList(); dependentSelections[predicate] = depSelections;}
        dependentSelections[predicate] = joinDependencies(clause,depSelections);
        setLocalStatus(literal);
        propagatorPool.addPropagatorJob(this,literal);}

    /** Adds a literal to the list of globally true literals.
     *
     * @param literal The literal to be added.
     */
    public synchronized void addGloballyTrueLiteral(int literal) {
        globallyTrueLiterals.add(literal);}

    /** Retrieves a globally true literal from the list of globally true literals.
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

    /** All globally true literals are integrated into the datastructures and the search structure.
     * <br>
     * The clauses are simplified with the true literals. <br>
     * If new true literals are derivable they are set to the model and thus inserted into globallyTrueLiterals.<br>
     * The search structure is updated if globally true literals interact with locally true literals.
     *
     * @throws Result
     */
    void processGloballyTrueLiterals() throws Result {
        backtrackPredicate = 0;
        backtrackTryAgain = false;
        backtrackTryNegated = false;
        int literal;
        while(!myThread.isInterrupted() && !clauses.isEmpty() &&
                (literal = getGloballyTrueLiteral()) != 0 && !literalIndex.isEmpty(literal)) { // literal is globally true
            int predicate = Math.abs(literal);
            switch(getLocalStatus(literal)) {  // compare with local truth
                case 0: break;
                case 1:
                    int selectedPredicate = getLastSelection(dependentSelections[predicate]);
                    if(backtrackPredicate == 0 || predicatePositions[selectedPredicate] < predicatePositions[backtrackPredicate]) {
                        backtrackPredicate = selectedPredicate;} // restart from the earliest selection
                    backtrackTryAgain = true;
                    break;
                case -1:
                    selectedPredicate = getLastSelection(dependentSelections[predicate]);
                    if(backtrackPredicate == 0 || predicatePositions[selectedPredicate] < predicatePositions[backtrackPredicate]) {
                        backtrackPredicate = selectedPredicate;} // contradiction caused by the earliest selection
                    backtrackTryNegated = true;
                    backtrackDependencies = dependentSelections[predicate];
                    break;}
            if(dependentSelections[predicate] != null) dependentSelections[predicate].clear();
            removeGloballyTrueLiteral(literal);}
        if(myThread.isInterrupted()) return;
        if(backtrackPredicate > 0) {
            if(backtrackTryAgain)   backtrack(backtrackPredicate,null, true); return;}
            if(backtrackTryNegated) backtrack(backtrackPredicate,backtrackDependencies, false);}

    /** removes all occurrences of the globally true literal.
     * <br>
     * The shortened clauses are simplified as far as possible.
     *
     * @param trueLiteral a globally true literal
     * @throws Result if a contradiction is encountered.
     */
    void removeGloballyTrueLiteral(int trueLiteral) throws Result {
        for(int sign = 1; sign >= -1; --sign) {
            int literal = sign*trueLiteral;
            Literal literalObject = literalIndex.getFirstLiteral(literal);
            while(literalObject != null) {
                Clause clause = literalObject.clause;
                if(clause.quantifier == Quantifier.OR) {
                    if(sign == 1) removeClause(clause); // clause is true
                    else {removeLiteral(literalObject); // new unit clause is inserted into the model.
                        if(clause.isInList) {
                            int lastPredicate = getLastSelection(clause);
                            if(lastPredicate != 0) { // local search may produce new facts if backtracked to  'backtrackPredicate'
                                if (backtrackPredicate == 0 || predicatePositions[lastPredicate] < predicatePositions[backtrackPredicate])
                                    backtrackPredicate = lastPredicate;
                                backtrackTryAgain = true;}}}
                    literalObject = (Literal)literalObject.nextItem;
                    continue;}

                clause.removeLiteral(literalObject,(sign == 1));
                switch(clause.simplify(trackReasoning,
                        (litObject -> literalIndex.remove(litObject)), // also called for irrelevant literals
                        ((lit, step) -> model.add(null,lit,step)),
                        monitor,symboltable)) {
                    case 0: // local search may produce new facts if backtracked to  'backtrackPredicate'
                        int lastPredicate = getLastSelection(clause);
                        if(lastPredicate != 0) {
                            if (backtrackPredicate == 0 || predicatePositions[lastPredicate] < predicatePositions[backtrackPredicate])
                                backtrackPredicate = lastPredicate;
                                backtrackTryAgain = true;}
                        break;
                    case 1: removeClause(clause); break;
                    case -1: throw new UnsatClause(problemId,solverId, clause);}
            literalObject = (Literal)literalObject.nextItem;}
        }}


    /**Joins the dependencies of a given clause to the provided IntArrayList.
     * <br>
     * Predicates which are globally true/false are ignored.
     *
     * @param clause The clause whose dependencies will be joined.
     * @param joinedDependencies The IntArrayList to which the dependencies will be joined.
     * @return The IntArrayList containing the joined dependencies (maybe empty)
     */
    protected IntArrayList joinDependencies(Clause clause, IntArrayList joinedDependencies) {
        joinedDependencies.clear();
        for(Literal literalObject : clause.literals) {
            int literal = literalObject.literal;
            if(model.status(literal) == 0 && getLocalStatus(literal) != 0)
                joinDependencies(joinedDependencies, dependentSelections[Math.abs(literal)]);}
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
     * @param clause A clause with some locally true/false literals
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
     * @param tryAgain if true then the predicate is tried again, otherwise its negation is tried.
     * @throws Result if backtracked to the top-selection and model.add(-selection) causes a contradiction
     */
    synchronized void backtrack(int predicate, IntArrayList dependencies, boolean tryAgain) throws Result {
        int backtrackings = 0;
        for(;recursionLevel >= 0; --recursionLevel) {
            ++backtrackings;
            int selectedPredicate = selectedPredicates[recursionLevel];
            if(selectedPredicate == predicate) {
                if(tryAgain) {
                    ++statistics.retries;
                    if(monitoring) monitor.accept("Trying again predicate " + Symboltable.toString(predicate,symboltable));}
                else {
                    if(monitoring) monitor.accept("Backtracking to predicate " + Symboltable.toString(predicate,symboltable));
                    ++statistics.backtrackings;
                    if(backtrackings > 1) ++statistics.backjumps;
                    if(recursionLevel == 0) {
                        model.add(null,-firstSign*selectedPredicate,null);
                        clearLocalStatus(predicate);}
                    else {
                        setLocalStatus(-firstSign*selectedPredicate);
                        dependencies.rem(predicate);
                        dependentSelections[selectedPredicate] = dependencies;}}

                for(int i = derivedPredicatesStarts[selectedPredicate]; i <= derivedPredicatesEnd; ++i) {
                    clearLocalStatus(derivedPredicates.getInt(i));}
                derivedPredicatesEnd = derivedPredicatesStarts[selectedPredicate];}}
    }


    /** initializes the predicate sequence.
     * <br>
     * The predicates are sorted as follows:<br>
     * - seed &gt;= 0:              randomly<br>
     * - predicateArrangement == 1: just the sequence of natural numbers: 1,2,...<br>
     * - predicateArrangement == 2: just the inverse sequence of natural numbers: n,n-1,...<br>
     * - predicateArrangement == 3: predicates with more literal occurrences first<br>
     * - predicateArrangement == 4: predicates with less literal occurrences first.
     *
     * @param predicateArrangement seed above
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
                Comparator.comparingInt(predicate -> sign * (literalIndex.size(predicate) + literalIndex.size(-(int)predicate))));
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
        if(localModel == null || localModel.length < predicates+1)
            localModel = new byte[predicates+1];
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            localModel[predicate] = model.status(predicate);}}


    /** sets the local truth status value of the literal.
     *
     * @param literal  a derived true literal.
     */
    protected void setLocalStatus(int literal) {
        if(literal > 0) localModel[literal] = 1; else localModel[-literal] = -1;}

    /** sets the local status value of the literal to 0.
     *
     * @param literal  a derived literal.
     */
    protected void clearLocalStatus(int literal) {
        if(literal > 0) localModel[literal] = 0; else localModel[-literal] = 0;}

    /**
     * Retrieves the status value of a literal in the local model.
     *
     * @param literal The literal to check.
     * @return The truth value of the literal in the local model.
     */
    protected synchronized byte getLocalStatus(int literal) {
        return literal > 0 ? localModel[literal] : (byte)-localModel[-literal]; }


    /** Converts Local Model to String representation.
     *
     * @return The String representation of Local Model.
     */
    public String toStringLocalModel() {
        StringBuilder st = new StringBuilder();
        for (int predicate = 1; predicate <= predicates; predicate++) {
            st.append(predicate).append(":").append(localModel[predicate]);
            if(predicate < predicates) st.append(",");
            if(predicate % 50 == 0) st.append("\n");}
        return st.toString();
    }


    @Override
    public Statistic getStatistics() {
        return statistics;}
}
