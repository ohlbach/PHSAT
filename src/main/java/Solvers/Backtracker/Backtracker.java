package Solvers.Backtracker;

import Datastructures.Clauses.Quantifier;
import Datastructures.LinkedItemList;
import Datastructures.LiteralIndex;
import Datastructures.Results.Result;
import Datastructures.Results.Satisfiable;
import Datastructures.Results.UnsatClause;
import Datastructures.Statistics.Statistic;
import InferenceSteps.InfUnitClause;
import InferenceSteps.InferenceStep;
import Management.Parameter;
import Management.Parameters;
import Management.ProblemSupervisor;
import Solvers.Solver;
import Utilities.BiConsumerWithUnsatisfiable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.function.Consumer;

public class Backtracker extends Solver {


    public static Parameters makeParameter() {
        Parameters parameters = new Parameters("Backtracker");
        Parameter selected = new Parameter("Select",Parameter.Type.Button,"false",false,
                "Select the Backtracker");
        parameters.add(selected);
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
        backtrackers.add(new Backtracker(1,1,1));}

     /** The predicates are initially sorted as follows:<br>
     * - predicateArrangement == 1: just the sequence of natural numbers: 1,2,...<br>
     * - predicateArrangement == 2: just the inverse sequence of natural numbers: n,n-1,...<br>
     * - predicateArrangement == 3: predicates with more literal occurrences first<br>
     * - predicateArrangement == 4: predicates with less literal occurrences first.*/
    private int predicateArrangement = 1;

    /** the final result */
    Result result = null;

    /** determines the sequence of predicates which are temporally set to true.*/
    int[] predicateSequence;
    /** maps each predicate to its position in the predicateSequence array- */
    int[] predicatePositions;

    /** +1: predicates are selected positively, -1: predicates are selected negatively.*/
    int firstSign = 1;

    /** The list of all clauses. The clauses are changed when a globally true literal is derived. */
    private LinkedItemList<Clause> clauses;

    /** Maps (positive and negative) literals to the Literal objects containing the literal */
    private LiteralIndex<Literal> literalIndex;

    /** stored the sequence of selected literals. */
    int[] selectedPredicates;

    /** the level of the recursion and the index of selectedPredicates */
    int recursionLevel = 0;

    /** maps derived literals to the selected true literals which cause the derivation of the literal.
     * The lists are kept sorted according th the predicateSequence,
     * such that the last element is the selected literal to which backtracking is necessary. */
    IntArrayList[] dependentSelections;

    /** contains the derived predicates.*/
    IntArrayList derivedPredicates;

    /** points to the end (first free index) of the derivedPredicates in the current recursion level */
    int derivedPredicatesEnd;

    /** maps the selected predicates to the first index in derivedPredicates where the derived predicates start. */
    int[] derivedPredicatesStarts;

    /** collects the globally true literals which are still to be processed. */
    private IntArrayList globallyTrueLiterals = new IntArrayList();

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
    private BlockingQueue<Object> propagatorQueue = new LinkedBlockingQueue<>(2);

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
    public Backtracker(int solverNumber, int predicateArrangement, int firstSign) {
        super(solverNumber);
        solverId = "Backtracker";
        this.predicateArrangement = predicateArrangement;
        this.firstSign = firstSign;}

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
        initializePredicateSequence();
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

    /** removes a (false) literal from the clause.
     * <p>
     * If the resulting clause is a unit clause, it is inserted into the model.
     * The clause is entirely removed.
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
            model.add(myThread,clause.literals.get(0).literal,step);
            removeClause(clause);};}

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
        int literal = globallyTrueLiterals.get(globallyTrueLiterals.size()-1);
        globallyTrueLiterals.remove(globallyTrueLiterals.size()-1);
        return literal;}

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
                int lastPredicate = depSelections.getInt(depSelections.size()-1);
                for(;recursionLevel >= 0; --recursionLevel) {
                    selectedPredicate = selectedPredicates[recursionLevel];
                    if(selectedPredicate == lastPredicate) {
                        setLocalTruth(-firstSign*selectedPredicate);
                        depSelections.removeLast();
                        dependentSelections[selectedPredicate] = depSelections;
                        for(int i = derivedPredicatesStarts[selectedPredicate]; i <= derivedPredicatesEnd; ++i) {
                            clearLocalTruth(derivedPredicates.getInt(i));}
                        derivedPredicatesEnd = derivedPredicatesStarts[selectedPredicate];}}}}}

    int findNextPredicateIndex(int predicateIndex) throws Result {
        for(; predicateIndex <= predicates; ++predicateIndex) {
            int predicate = predicateSequence[predicateIndex];
            if (literalIndex.isEmpty(predicate)) continue;
            byte status = model.status(predicate);
            if (status != 0) {removeTrueLiteral(predicate*status); continue;}
            return predicateIndex;}
        return 0;}

    IntArrayList propagateSelectedLiteral(int selectedLiteral) throws Result {
        propagatorCounter = 0;
        propagatorQueue.clear();
        setLocalTruth(selectedLiteral);
        int selectedPredicate = Math.abs(selectedLiteral);
        dependentSelections[selectedPredicate] = null;
        Clause falseClause = propagateLocally(selectedLiteral);
        if(falseClause != null) return joinDependencies(falseClause);
        try{Object object = propagatorQueue.take(); // waits until all propgatorJobs are finished, or one them found a false clause.
            if(object instanceof Clause) return joinDependencies((Clause)object);}
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
         setLocalTruth(literal);
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
                switch(getLocalTruth(literalObject.literal)) {
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
            switch(getLocalTruth(literalObject.literal)) {
                case 0: unsignedLiterals += literalObject.multiplicity; break;
                case 1: trueLiterals += literalObject.multiplicity;}}
        int max = clause.max; int min = clause.min;
        if(trueLiterals > max || trueLiterals + unsignedLiterals < min) return clause;
        // too many or not enough true literals.
        if(min <= trueLiterals && trueLiterals <= max) return null; // clause is true.
        // try to derive new true or false literals
        for(Literal literalObject : clause.literals) {
            if(getLocalTruth(literalObject.literal) == 0) {
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
        dependentSelections[Math.abs(literal)] = joinDependencies(clause);
        setLocalTruth(literal);
        propagatorPool.addPropagatorJob(this,literal);}



    void processGloballyTrueLiterals() throws Result {
        int literal;
        while(!myThread.isInterrupted() && !clauses.isEmpty() &&
                (literal = getGloballyTrueLiteral()) != 0) {
            removeTrueLiteral(literal);}
        if(myThread.isInterrupted()) return;
        updateSearch();}

    Consumer<Literal> literalRemover = (literalObject -> literalIndex.remove(literalObject));
    BiConsumerWithUnsatisfiable<Integer, InferenceStep> reportTruth = ((literal, step) -> model.add(null,literal,step));

    void removeTrueLiteral(int trueLiteral) throws Result {
        for(int sign = 1; sign >= -1; --sign) {
            int literal = sign*trueLiteral;
            Literal literalObject = literalIndex.getFirstLiteral(literal);
            while(literalObject != null) {
                Clause clause = literalObject.clause;
                if(clause.quantifier == Quantifier.OR) {
                    if(sign == 1) removeClause(clause); // clause is true
                    else          removeLiteral(literalObject);
                    literalObject = (Literal)literalObject.nextItem;
                    continue;}
                clause.removeLiteral(literalObject,(sign == 1));

                switch(clause.simplify(trackReasoning,literalRemover,reportTruth,monitor,symboltable)) {
                    case 1: removeClause(clause); break;
                    case -1: throw new UnsatClause(problemId,solverId, clause);}
            literalObject = (Literal)literalObject.nextItem;}
        }}

    protected void updateSearch() throws Result {
        for(int predicateIndex = 0; predicateIndex < selectedPredicateIndices.size(); ++predicateIndex) {
            int predicate = predicateSequence[selectedPredicateIndices.getInt(predicateIndex)];
             switch(model.status(predicate)) {
                 case 1:  makeDerivedLiteralsTrue(predicateIndex,predicate); break;
                 case -1: clearSelections(predicateIndex);}}}


    protected void makeDerivedLiteralsTrue(int predicateIndex, int predicate) throws Result {
        for(int i = derivedPredicatesEnd - 1; i >= 0; --i) {
            int literal = derivedPredicates.getInt(i);
            switch(model.status(literal)) {
                case 0: model.add(null,literal,null); break; // step
                case 1: dependentSelections[Math.abs(literal)].clear();
                        derivedPredicates[predicate].rem(i);
                        for(int predIndex = predicateIndex+1; i < selectedPredicateIndices.size(); ++i) {
                            int selectedPredicate = predicateSequence[selectedPredicateIndices.getInt(predIndex)];
                            dependentSelections[selectedPredicate].removeInt(predicate);}
                        removeTrueLiteral(literal);
                        break;
                case -1: clearSelections(predicateIndex);


            }
        }
    }




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
    void analyseClauseGlobally(Clause clause) throws Result {
       /* for(int i = clause.literals.size()-1; i >= 0; --i) { // remove all literals which are globally true/false.
            Literal literalObject = clause.literals.get(i);
            int literal = literalObject.literal;
            byte status = model.status(literal);
            if(status != 0) {
                literalIndex.remove(literalObject);
                if(clause.removeLiteral(literalObject,status == (byte)1));
                    throw new UnsatClause(problemId,solverId,clause);}}
        if(clause.literals.size() == 1) {
            model.add(null, clause.literals.get(0).literal,null);
            removeClause(clause);}

        if(clause.quantifier == Quantifier.OR) return;

        int expandedSize = clause.expandedSize;

        if(expandedSize < clause.min || expandedSize > clause.max)
            throw new UnsatClause(problemId,solverId,clause); // too many or not enough true literals.

        if(min <= trueLiterals && trueLiterals <= max) {
            removeClause(clause);
            return;} // clause is true

        // try to derive new true or false literals
        for(Literal literalObject : clause.literals) {
            int literal = literalObject.literal;
            switch(model.status(literal)) {
                case 1: clause.removeLiteral(literalObject,true);
            }
            if(model.status(literalObject.literal) == 0) {
                int trueLits = trueLiterals + literalObject.multiplicity;
                if(!(min <= trueLits && trueLits <= max)) { // making it true causes a contradiction
                    model.add(myThread,-literalObject.literal,null);

                    continue;}
                trueLits = trueLiterals + unsignedLiterals - literalObject.multiplicity;
                if(!(min <= trueLits && trueLits <= max)) { // making it false causes a contradiction
                    makeLiteralTrue.accept(clause,literalObject,(byte)1);}}} */
        return;}



    IntArrayList joinDependencies(Clause clause) {
        IntArrayList joinedDependencies = new IntArrayList();
        for(Literal literalObject : clause.literals) {
            int literal = literalObject.literal;
            if(getLocalTruth(literal) != 0)
                joinDependencies(joinedDependencies, dependentSelections[Math.abs(literal)]);}
        joinedDependencies.sort(Comparator.comparingInt(predicate -> predicatePositions[predicate]));
        return joinedDependencies;}

    void joinDependencies(IntArrayList dep1, IntArrayList dep2){
        if(dep1.isEmpty()) {dep1.addAll(dep2); return;}
        for(int predicate : dep2) {if(!dep1.contains(predicate)) dep1.add(predicate);}}




    /** initializes the (recursive) selection of a literal which is temporally set to true.
     * <p>
     * Called only by the main thread.
     *
     * @param literal the selected literal
     */
    private void initializeSelection(int literal) {
        int predicate = Math.abs(literal);
        selectedPredicates.add(predicate);
        derivedPredicates[predicate].clear();
        dependentSelections[predicate].clear();
        setLocalTruth(literal);}

    /** adds a derived literal to the corresponding lists.
     *
     * @param literal a derived literal
     * @param dependencies the list of selected literals which caused the derivation of this literal.
     */
    public synchronized void addDerivedLiteral(int literal, IntArrayList dependencies) {
        int predicate = Math.abs(literal);
        int lastSelection = dependencies.getInt(dependencies.size()-1); // must be positive
        derivedPredicates[lastSelection].add(literal);
        dependentSelections[predicate] = dependencies;
        setLocalTruth(literal);}

    /** backtracks to the last selection mentioned in dependencies.
     * <p>
     * If the backtracking ends at the first selection, its negation is made globally true.<br>
     * The current point of the search is indicated by the last predicate in selectedPredicates.
     *
     * @param dependencies the list of selections which caused a contradiction.
     * @throws Result if backtracked to the top-selection and model.add(-selection) causes a contradiction
     */
    synchronized void backtrack(IntArrayList dependencies) throws Result {
        int lastSelection = dependencies.getInt(dependencies.size()-1);
        int index = selectedPredicates.size();
        int selectedPredicate = 0;
        while((selectedPredicate = selectedPredicates.getInt(--index)) != lastSelection) {
            for(int literal : derivedPredicates[selectedPredicate])
                clearLocalTruth(literal);
            selectedPredicates.remove(selectedPredicates.size()-1);}
        if(selectedPredicates.isEmpty()) {
            model.add(myThread,-firstSign*lastSelection);}
        else {
            int derivedLiteral = -firstSign*lastSelection;
            int previousSelection = selectedPredicates.remove(selectedPredicates.size()-1);
            derivedPredicates[previousSelection].add(derivedLiteral);
            setLocalTruth(derivedLiteral);
            dependencies.remove(dependencies.size()-1);
            dependentSelections[lastSelection] = dependencies;
            propagateInThread(derivedLiteral);}}

    /** report the result to the main thread and sends an interrupt to myThread
     *  <p>
     *  It can be called from any other thread.
     * @param result a result
     */
    public synchronized void reportResult(Result result) {
        this.result = result;
        myThread.interrupt();}

    /** sets the local truth value of the literal.
     *
     * @param literal  a derived true literal.
     */
    private void setLocalTruth(int literal) {
        if(literal > 0) localModel[literal] = 1; else localModel[-literal] = -1;}

    /** sets the local truth value of the literal to 0.
     *
     * @param literal  a derived literal.
     */
    private void clearLocalTruth(int literal) {
        if(literal > 0) localModel[literal] = 0; else localModel[-literal] = 0;}

    /**
     * Retrieves the truth value of a literal in the local model.
     *
     * @param literal The literal to check.
     * @return The truth value of the literal in the local model.
     */
    private synchronized byte getLocalTruth(int literal) {
        return literal > 0 ? localModel[literal] : (byte)-localModel[-literal]; }


    /** initializes the predicate sequence.
     * <br>
     * The predicates are sorted as follows:<br>
     * - predicateArrangement == 1: just the sequence of natural numbers: 1,2,...<br>
     * - predicateArrangement == 2: just the inverse sequence of natural numbers: n,n-1,...<br>
     * - predicateArrangement == 3: predicates with more literal occurrences first<br>
     * - predicateArrangement == 4: predicates with less literal occurrences first.
     */
    void initializePredicateSequence() {
        predicateSequence  = new int[predicates+1];
        predicatePositions = new int[predicates+1];
        switch(predicateArrangement) {
            case 0:
                for (int predicate = 1; predicate <= predicates; ++predicate) {
                    predicateSequence[predicate] = predicate;
                    predicatePositions[predicate] = predicate;}
                return;
            case 1:
                for (int predicate = 1; predicate <= predicates; ++predicate) {
                    predicateSequence[predicates-predicate+1] = predicate;
                    predicatePositions[predicate] = predicates-predicate+1;}
                return;}
        int sign = (predicateArrangement == 3) ? -1 : 1;
        Integer[] predicateIndex = new Integer[predicates+1];
        for (int predicate = 1; predicate <= predicates; predicate++) {predicateIndex[predicate] = predicate;}
        Arrays.sort(predicateIndex,1,predicates+1,
                Comparator.comparingInt(i -> sign * (literalIndex.size(i) + literalIndex.size(-i))));
        for(int predicate = 1; predicate <= predicates; ++ predicate) {
            predicateSequence[predicate] = predicateIndex[predicate];
            predicatePositions[predicateSequence[predicate]] = predicate;
        }}

    /** initializes the local to be synchronous to the global model.
     */
    void initializeLocalModel() {
        localModel = new byte[predicates+1];
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            localModel[predicate] = model.status(predicate);}}


    @Override
    public Statistic getStatistics() {
        return statistics;}
}
