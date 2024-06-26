package Datastructures;

import Datastructures.Clauses.Quantifier;
import Datastructures.Results.Result;
import Datastructures.Results.Satisfiable;
import Datastructures.Results.UnsatClause;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Theory.Model;
import InferenceSteps.InfMergeResolution;
import InferenceSteps.InferenceStep;
import Management.ErrorReporter;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.concurrent.PriorityBlockingQueue;
import java.util.function.Consumer;

/**
 * Represents a list of clauses and provides operations for adding, removing, and processing clauses.
 * <br>
 * In particular the following operations are supported:<br>
 * - subsumption checks: subsumed clauses are removed<br>
 * - mergeResolution: resolution steps which shorten clauses are performed.<br>
 * - purity-checks: pure literals are identified and removed.
 * <br>
 * Once all clauses have been inserted, none of the operations adds new clauses.
 * The clauses may, however, be shortened and removed from the lists.
 * <br>
 * The instance of the class can be reused for a sequence of problems (without creating the datastructures anew)<br>
 * A single instance can be used when a number of problem solvers work in parallel threads at the same time.
 * Destructive changes, however, should be done by stopping the parallel threads, performing the changes and then
 * continuing the threads.
 */
public class ClauseList {
    // Problem independent parameters

    /** If true then inference steps are tracked. */
    private final boolean trackReasoning;

    /** If true then simplification steps are verify by checking all models.
     * For this it is necessary to have trackReasoning = true.*/
    private final boolean verify;

    /** monitors the inference steps */
    Consumer<String> monitor;

    /** to be used when a Result has been created */
    String solverId = "ClauseList";

    /** to prevent the model.add()-method of calling the ClauseList-observer */
    Thread myThread;

    // Problem-specific control-parameters

    /** The identifier for the problem */
    String problemId;

    /** The global model */
    Model model = null;

    /** the number of predicates */
    private int predicates;

    /** the doubly linked list of clauses */
    LinkedItemList<Clause> clauses;

    /** maps literals to the literal occurrences in the clauses */
    public LiteralIndex<Literal> literalIndex;

    /** contains pairs singleton-literal,clause. To be used when a model has to be completed. */
    protected final ArrayList<Object> singletons = new ArrayList<>();

    /** records some statistical values */
    public StatisticsClauseList statistics = null;

    /** a flag which indicates that all clauses are inserted and therefore purity-checks are possible. */
    boolean allClausesInserted = false;

    /** the symboltable for the predicates */
    Symboltable symboltable = null;

    /** supports efficient identification of subsumption and merge-resolution partners. */
    int timestamp = 0;

    /** A queue of newly derived unit predicates and binary equivalences.
     * The unit predicates are automatically put at the beginning of the queue.
     */
    protected final PriorityBlockingQueue<Task> queue =
            new PriorityBlockingQueue<>(100, Comparator.comparingInt(task->task.priority));


    /**
     * Creates a new instance of ClauseList with the problem-independent parameters.
     * <br>
     * The constructor is usually called once for a sequence of problems.
     *
     * @param trackReasoning specifies whether to track reasoning steps
     * @param verify specifies whether to verify the inference steps
     * @param monitor a {@link Consumer} that will be used to monitor progress or log messages
    */
    public ClauseList(boolean trackReasoning, boolean verify, Consumer<String> monitor) {
        this.trackReasoning = trackReasoning;
        this.monitor = monitor;
        this.verify = verify;
    }

    /** Initializes the instance for a new problem.
     * <br>
     * The method must be called for each problem separately
     *
     * @param problemId The ID of the problem.
     * @param model The model to be initialized.
     * @param symboltable The symbol table.
     */
    public void initialize(String problemId, Model model, Symboltable symboltable) {
        this.problemId = problemId;
        this.model = model;
        this.predicates = model.predicates;
        this.symboltable = symboltable;
        if(clauses == null) clauses = new LinkedItemList<>("Clauses");
        else                clauses.clear();
        if(literalIndex == null) literalIndex = new LiteralIndex<>(predicates);
        else                     literalIndex.ensureCapacity(predicates);
        allClausesInserted = false;
        timestamp = 0;
        statistics = new StatisticsClauseList();
        singletons.clear();
        queue.clear();
        myThread = Thread.currentThread();
        model.addObserver(Thread.currentThread(),
                (literal,inferenceStep) -> {
                        synchronized(this) {queue.add(new Task(Task.TaskType.TRUELITERAL, literal,inferenceStep));}});}

    /**
     * After having inserted all clauses, the following operations are performed: <br>
     * - true literals are applied to the clauses<br>
     * - subsumed clauses are removed<br>
     * - pure literals are identified and removed.
     * - merge resolutions are performed.
     *
     * @throws Result If the problem is satisfiable, throws a {@link Result} exception.
     */
    public void allClausesInserted() throws Result {
        IntArrayList trueLiterals = model.model; // derived true literals are automatically appended
        for(int i = 0; i < trueLiterals.size(); ++i) {
            int literal = trueLiterals.getInt(i);
            applyTrueLiteral(literal,model.getInferenceStep(literal));}

        this.allClausesInserted = true;
        Clause clause = clauses.firstLinkedItem;
        while(clause != null) {
            removeSubsumedClauses(clause);
            if(clause.quantifier == Quantifier.OR) mergeResolution(clause);
            clause = (Clause)clause.nextItem;}

        if(!removePureLiterals()) // new true literals generate tasks
            processTasks();

        if(clauses.isEmpty()) {
            extendModel();
            throw new Satisfiable(problemId,solverId,model);}}

    /**
     * Process the tasks in the queue.
     *
     * @throws Result if an error occurs during processing.
     */
    public void processTasks() throws Result {
        while (!queue.isEmpty()) {
            Task task = queue.poll();
            switch (task.taskType) {
                case TRUELITERAL:
                    applyTrueLiteral(task.literal, task.inferenceStep);
                    break;
                case SHORTENED_CLAUSE:
                    Clause clause = task.clause;
                    if(clause.isInList) {
                        removeSubsumedClauses(clause);
                        if(clause.quantifier == Quantifier.OR) mergeResolution(clause);}
                    break;
                case PURITY:
                    removePurePredicate(Math.abs(task.literal));
                    break;
                default:
                    break;}}}

    /** adds the clause to the clauses and to the literal index.
     * <br>
     * If allClausesInserted = true then a shortenedClause task is generated.
     *
     * @param clause to be added.
     */
    public void addClause(Clause clause) {
        if(allClausesInserted) addShortenedClauseTask(clause);
        clauses.addToBack(clause);
        addClauseToIndex(clause);}

    /** removes the clause from the clause list, but not from the index.
     *
     * @param clause a clause to be removed.
     */
    public void removeClause(Clause clause) {
        clauses.remove(clause);}

    /** adds the clause to the index (not to the clauses list).
     *
     * @param clause the clause to be added.
     */
    public void addClauseToIndex(Clause clause) {
        for(Datastructures.Literal literalObject : clause.literals) {
            literalIndex.addToBack(literalObject);}}

    /** removes the clause from the index.
     *
     * @param clause the clause to be removed.
     */
    public void removeClauseFromIndex(Clause clause) {
        for(Literal literalObject : clause.literals) {
            literalIndex.remove(literalObject);}}

    /** removes the literal from the index.
     *
     * @param literalObject the literal to be removed.
     */
    public void removeLiteralFromIndex(Literal literalObject) {
        literalIndex.remove(literalObject);
        if(allClausesInserted) addPurityTask(literalObject.literal);}

    /** checks if the clauses are empty
     *
     * @return true if there are no clauses anymore.
     */
    public boolean isEmpty() {
        return clauses.isEmpty();}

    /** applies the true literal to all clauses containing the literal.
     *
     * @param literal       a true literal
     * @param inferenceStep which caused the truth of the literal.
     * @throws Unsatisfiable if a contradiction is discovered.
     */
    void applyTrueLiteral(int literal, InferenceStep inferenceStep) throws Unsatisfiable {
        for(int sign = 1; sign >= -1; sign -=2) {
            Literal literalObject = literalIndex.getFirstLiteral(sign*literal);
            while(literalObject != null) {
                Clause clause = literalObject.clause;
                switch(clause.applyTrueLiteral(sign*literal,sign == 1, inferenceStep, trackReasoning,
                        monitor,this::removeLiteralFromIndex,this::addTrueLiteralTask, symboltable)){
                    case -1: throw new UnsatClause(problemId,solverId, clause);
                    case 1: removeClause(clause); removeClauseFromIndex(clause); break;
                    case 0: addShortenedClauseTask(clause);}
                literalObject = (Literal)literalObject.nextItem;
            }}}

    /** adds the literal to the model and geerates a TRUELITERAL task
     *
     * @param literal a true literal
     * @param inferenceStep which caused the derivation of the true literals
     * @throws Unsatisfiable if the model detects a contradiction.
     */
    public void addTrueLiteralTask(int literal, InferenceStep inferenceStep) throws Unsatisfiable{
        model.add(myThread,literal,inferenceStep);
        queue.add(new Task(Task.TaskType.TRUELITERAL, literal,inferenceStep));}

    /** adds a SHORTNED_CLAUSE task
     *
     * @param clause a shortened clause
     */
    void addShortenedClauseTask(Clause clause) {
        queue.add(new Task(Task.TaskType.SHORTENED_CLAUSE, clause));}

    /** adds a purity task
     *
     * @param literal a possibly pure literal.
     * */
    void addPurityTask(int literal) {
        queue.add(new Task(Task.TaskType.PURITY, literal, null));}

    /**
     * Removes all clauses subsumed by the given clause clauses from the clause list.
     * <br>
     * A subsumer subsumes a subsumee iff the subsumee clause is true in all models of the subsumer clause.
     * <br>
     * A necessary condition is that the subsumer's literals are a subset of the subsumee's literals.
     * This is checked with a timestamp mechanism for efficiently finding the subsumee candidates.
     * <br>
     * All subsumed clauses are removed from the datastructures.
     *
     * @param subsumer the clause that potentially subsumes other clauses.
     */
    public void removeSubsumedClauses (Clause subsumer) {
         ++timestamp;
        int size = subsumer.literals.size();
        Literal subsumerLiteral = subsumer.literals.get(0);
        Literal subsumeeLiteral = literalIndex.getFirstLiteral(subsumerLiteral.literal);
        while(subsumeeLiteral != null) { // the clauses with the first literal get the timestamp.
            Clause subsumee = subsumeeLiteral.clause;
            if (subsumee != subsumer) subsumee.timestamp = timestamp;
            subsumeeLiteral = (Literal)subsumeeLiteral.nextItem;}

        for(int i = 1; i < subsumer.literals.size(); ++i) {
            subsumerLiteral = subsumer.literals.get(i);
            subsumeeLiteral = literalIndex.getFirstLiteral(subsumerLiteral.literal);
            while(subsumeeLiteral != null) {
                Clause subsumee = subsumeeLiteral.clause;
                if ((subsumee.timestamp - timestamp) == size-2 && subsumes(subsumer,subsumee)) {
                    if(verify) verifySubsumption(subsumer,subsumee);
                    removeClause(subsumee);
                    removeClauseFromIndex(subsumee);
                    ++statistics.subsumedClauses;
                    if(monitor != null) {
                        monitor.accept("Clause " + subsumer.toString(symboltable,0) +
                                " subsumes " + subsumee.toString(symboltable,0));}}
                else ++subsumee.timestamp;
                subsumeeLiteral = (Literal)subsumeeLiteral.nextItem;}}
        ++timestamp;}

    /**
     * Determines whether the given clause is subsumed by some of the clauses in the list.
     *
     * @param subsumee The clause to be checked for subsumption.
     * @return the subsumer clause if the subsumee clause is subsumed by some clause in the list, otherwise null.
     */
    public Clause isSubsumed(Clause subsumee) {
        ++timestamp;
        for(Literal subsumeeLiteral : subsumee.literals) {
            Literal subsumerLiteral = literalIndex.getFirstLiteral(subsumeeLiteral.literal);
            while(subsumerLiteral != null) {
                Clause subsumer = subsumerLiteral.clause;
                if(subsumer == subsumee) {
                    subsumerLiteral = (Literal)subsumerLiteral.nextItem;
                    continue;}
                int subsumerTimestamp = subsumer.timestamp;
                if(subsumerTimestamp < timestamp) {
                    subsumer.timestamp = timestamp;
                    subsumerLiteral = (Literal)subsumerLiteral.nextItem;
                    continue;}
                if((subsumerTimestamp - timestamp) == subsumer.literals.size()-2 && subsumes(subsumer,subsumee)) {
                    timestamp += subsumee.literals.size();
                    if(verify) verifySubsumption(subsumer,subsumee);
                    return subsumer;}
                ++subsumer.timestamp;
                subsumerLiteral = (Literal)subsumerLiteral.nextItem;}}
        ++timestamp;
        return null;}

    /**
     * Determines whether a subsumer clause subsumes a subsumee clause.
     * <br>
     * Precondition:  The subsumer's literals must be a subset of the subsumee's literals
     * <br>
     * A subsumer clause subsumes a subsumee clause if the subsumee clause is true in all models of the subsumer clause.
     * In some cases this expensive check can be avoided by checking syntactic conditions on the clause.
     * In particular if the clauses are both OR-clauses then the subset condition is sufficient.
     * <br>
     * If both clauses have no multiple literals then the subsumer-interval must be smaller or equal the subsumee-interval.
     *
     * @param subsumer The clause that potentially subsumes other clauses
     * @param subsumee The clause to be potentially subsumed
     * @return true if the subsumer subsumes the subsumee, false otherwise
     */
    protected boolean subsumes(Clause subsumer, Clause subsumee) {
        assert Clause.isSubset(subsumer,subsumee);
        if(subsumer.quantifier == Quantifier.OR && subsumee.quantifier == Quantifier.OR) {return true;}
        if(!subsumer.hasMultipleLiterals) {
            if(!subsumee.hasMultipleLiterals) // the subsumer-interval must be smaller or equal the subsumee-interval
                return subsumee.min <= subsumer.min && subsumee.max >= subsumer.max;

            // The subsumer has no multiples, and the subsumee should not have multiples in the overlapping literals
            int extraLiterals = 0;
            boolean subsumeeMultiples = false;
            for(Literal subsumeeLiteral : subsumee.literals) {
                if(subsumer.findLiteral(subsumeeLiteral.literal) == null)
                    extraLiterals += subsumeeLiteral.multiplicity;
                else {
                    if(subsumeeLiteral.multiplicity > 1) {
                        subsumeeMultiples = true; break;}}}

            // the extraLiterals in the subsumee may all be false or true in the subsumer's models.
            if(!subsumeeMultiples) {
                return subsumee.min <= subsumer.min && subsumee.max >= subsumer.max + extraLiterals;}}
        return subsumesByModels(subsumer, subsumee);}


    /**
     * Determines whether a subsumer clause subsumes a subsumee clause by checking if the subsumer's models satisfy the subsumee.
     * <br>
     * In principle there are no special conditions, but by efficiency reasons the method should only be called
     * when there are multiple literals in the clause.
     *
     * @param subsumer The clause that potentially subsumes other clauses
     * @param subsumee The clause to be potentially subsumed
     * @return true if the subsumer subsumes the subsumee, false otherwise
     */
    protected static boolean subsumesByModels(Clause subsumer, Clause subsumee) {
        IntArrayList predicates = Clause.predicates(subsumee,subsumer);
        for(int model : subsumer.getModels(predicates)) {
            if(!subsumee.isTrue(model,predicates)) {return false;}}
        return true;}

    /**
     * Verifies if a clause is subsumed by another clause using model-based subsumption check.
     *
     * @param subsumer The clause that is potentially the subsumer.
     * @param subsumee The clause that is potentially the subsumee.
     */
    protected void verifySubsumption(Clause subsumer, Clause subsumee ) {
        if(subsumesByModels(subsumer,subsumee)) return;
        if(monitor!=null) {
            monitor.accept("Error: Subsumption Check: Clause " + subsumer.toString(symboltable,0) +
                    "is not subsumed by " + subsumee.toString(symboltable,0));}}

    /** searches pure predicates and process them.
     * <br>
     *  A literal p is pure in clauses which are NOT interval- or exactly-clauses if it occurs either <br>
     *  - only positively or only negatively in OR- or ATLEAST-clauses or<br>
     *  - only positively or only negatively in ATMOST-clauses<br>
     *  The literal p is singleton pure if it occurs only once in an interval- or exactly-clause.<br>
     *  A pure literal in  OR- or ATLEAST-clauses can be made true if occurs only positively,<br>
     *  or made false if it occurs only negatively. <br>
     *  In atmost clauses it is the other way round.<br>
     *  A singleton pure literal in an interval- or exactly-clause can be removed. <br>
     *  Its truth value can only be determined after a model for the entire clause set has been found.
     *
     * @return true if the clause set became empty.
     * @throws Unsatisfiable if a contradiction is discovered.
     * */
    protected boolean removePureLiterals()  throws Unsatisfiable{
        boolean purityFound = true;
        while(purityFound) {
            purityFound = false;
            for(int predicate = 1; predicate <= predicates; ++predicate) {
                purityFound |= removePurePredicate(predicate);
                if(clauses.isEmpty()) return true;}}
        return false;}

    /** checks if the predicate or its negation is pure.
     * <br>
     *  A literal p is pure in clauses which are NOT interval- or exactly-clauses if it occurs either <br>
     *  - only positively or only negatively in OR- or ATLEAST-clauses or<br>
     *  - only positively or only negatively in ATMOST-clauses<br>
     *  The literal p is singleton pure if it occurs only once in an interval- or exactly-clause.<br>
     *  A pure literal in  OR- or ATLEAST-clauses can be made true if occurs only positively,<br>
     *  or made false if it occurs only negatively. <br>
     *  In atmost clauses it is the other way round.<br>
     *  A singleton pure literal in an interval- or exactly-clause can be removed. <br>
     *  Its truth value can only be determined after a model for the entire clause set has been found.
     *
     * @param predicate a predicate.
     * @return true if a pure literal has been found.
     * @throws Unsatisfiable if a contradiction is discovered.
     * */
    protected boolean removePurePredicate(int predicate) throws Unsatisfiable{
        if(literalIndex.isBothEmpty(predicate)) return false;
        if(isPositivelyPure(predicate)) {
            if(monitor!=null) {monitor.accept("Predicate " + Symboltable.toString(predicate,symboltable) +
                    " is positively pure");}
            ++statistics.pureLiterals;
            model.add(myThread,predicate,null);
            applyTrueLiteral(predicate,null); // pure literals will never be part of a contradiction.
            return true;}
        if(isNegativelyPure(predicate)) {
            if(monitor!=null) {monitor.accept("Predicate " + Symboltable.toString(predicate,symboltable) +
                    " is negatively pure");}
            ++statistics.pureLiterals;
            model.add(myThread,-predicate,null);
            applyTrueLiteral(-predicate,null); // pure literals will never be part of a contradiction.
            return true;}

        int literal = isSingletonPure(predicate);
        if(literal != 0) {
            if(monitor != null) {monitor.accept("Literal " + Symboltable.toString(literal,symboltable) +
                    " is singleton pure");}
            ++statistics.singletonLiterals;
            Clause clause = literalIndex.getFirstLiteral(literal).clause;
            singletons.add(literal); singletons.add(clause.clone());
            switch(clause.removeLiteral(literal,trackReasoning,0,this::removeLiteralFromIndex, this::addTrueLiteralTask, monitor,symboltable)) {
                case -1: clauses.remove(clause); removeClauseFromIndex(clause); throw new UnsatClause(problemId,solverId, clause);
                case +1: removeClause(clause); removeClauseFromIndex(clause); return true;}
            return true;}
        return false;
    }

    /**
     * Determines whether a given predicate is positively pure in the clause list.
     * <br>
     * A literal p is positively pure if it can be made true without affecting the satisfiability or unsatisfiability of a clause set.<br>
     * This is the case when it occurs positively in OR- and Atleast clauses and negatively in Atmost clauses<br>
     * Examples: <br>
     * p,q,r
     * atleast p,a,b
     * atmost -p,c,d
     * <br>
     * Add a true literal to a true OR- or Atleast-clause does not change the truth of the clause. <br>
     * Adding a false literal to an Atmost-clause does not change the truth of the clause.
     *
     * @param predicate the predicate to check for positive purity
     * @return true if the predicate is positively pure, false otherwise
     */
    protected boolean isPositivelyPure(int predicate) {
        Literal positiveLiterals  = literalIndex.getFirstLiteral(predicate);
        Literal negativeLiterals = literalIndex.getFirstLiteral(-predicate);
        Clause clause;
        while (negativeLiterals != null) { // only negative literals in ATMOST-clauses are allowed
            clause = negativeLiterals.clause;
            if(clause.quantifier != Quantifier.ATMOST) return false;
            negativeLiterals = (Literal)negativeLiterals.nextItem;}

        while (positiveLiterals != null) {
            clause = positiveLiterals.clause; // only positive literals in Or- or Atleast clauses are allowed.
            if(!(clause.quantifier == Quantifier.OR || clause.quantifier == Quantifier.ATLEAST)) return false;
            positiveLiterals = (Literal)positiveLiterals.nextItem;}

        return true;}

    /**
     * Determines whether a given predicate is positively pure in the clause list.
     * <br>
     * A literal p is positively pure if it can be made true without affecting the satisfiability or unsatisfiability of a clause set.<br>
     * This is the case when it occurs positively in OR- and Atleast clauses and negatively in Atmost clauses<br>
     * Examples: <br>
     * p,q,r
     * atleast p,a,b
     * atmost -p,c,d
     * <br>
     * Add a true literal to a true OR- or Atleast-clause does not change the truth of the clause. <br>
     * Adding a false literal to an Atmost-clause does not change the truth of the clause.
     *
     * @param predicate the predicate to check for positive purity
     * @return true if the predicate is positively pure, false otherwise
     */
    protected boolean isNegativelyPure(int predicate) {
        Literal positiveLiterals = literalIndex.getFirstLiteral(predicate);
        Literal negativeLiterals = literalIndex.getFirstLiteral(-predicate);
        Clause clause;
        while (positiveLiterals != null) { // only positive literals in ATMOST-clauses are allowed
            clause = positiveLiterals.clause;
            if(clause.quantifier != Quantifier.ATMOST) return false;
            positiveLiterals = (Literal)positiveLiterals.nextItem;}

        while (negativeLiterals != null) {
            clause = negativeLiterals.clause; // only negative liteals in Or- or Atleast clauses are allowed.
            if(!(clause.quantifier == Quantifier.OR || clause.quantifier == Quantifier.ATLEAST)) return false;
            negativeLiterals = (Literal)negativeLiterals.nextItem;}

        return true;}

    /** checks if the predicate p is singleton pure.
     * <br>
     * If p occurs only once in an interval clause p is singleton pure.<br>
     * If -p occurs only once in an interval clause -p is singleton pure.
     *
     * @param predicate a predicate.
     * @return the predicate or its negation if it is singleton pure
     */
    protected int isSingletonPure(int predicate) {
        Literal positiveLiterals = literalIndex.getFirstLiteral(predicate);
        Literal negativeLiterals = literalIndex.getFirstLiteral(-predicate);
        if(positiveLiterals != null) {
            if(negativeLiterals == null &&
                (positiveLiterals.clause.quantifier == Quantifier.INTERVAL && positiveLiterals.nextItem == null)) return predicate;
            else return 0;}
        if(negativeLiterals.clause.quantifier == Quantifier.INTERVAL && negativeLiterals.nextItem == null) return -predicate;
        return 0;}


    /**
     * This method is used to perform a merge resolution between a shorter parent OR-clause and corresponding other clauses
     * <br>
     * The method uses timestamps to quickly identify candidate clauses for the merge-resolution.<br>
     * If shorterParent is shortened itself, the method is called recursively.<br>
     * If another clause is shortened a ShortenedClauseTask is inserted into the queue.
     *
     * @param shorterParent The shorter parent clause to merge with.
     * @throws Unsatisfiable If the merge resolution leads to unsatisfiability.
     */
    protected void mergeResolution(Clause shorterParent) throws Unsatisfiable {
        assert shorterParent.quantifier == Quantifier.OR;

        ArrayList<Literal> shorterLiterals = shorterParent.literals;
        int shorterSize = shorterLiterals.size();
        Clause longerParent;
        for(int i = 0; i < shorterSize; ++i) { // shorterLiterals may be changed.
            ++timestamp;
            Literal shorterLiteral1  = shorterLiterals.get(i);
            int resolutionLiteral = shorterLiteral1.literal;
            Literal longerLiteral = literalIndex.getFirstLiteral(-resolutionLiteral);
            while(longerLiteral != null) {
                longerParent = longerLiteral.clause;
                if(longerParent.quantifier == Quantifier.OR ||
                        (longerParent.min == longerLiteral.multiplicity && longerParent.literals.size() == shorterSize) ||
                        (longerParent.min == 2 && longerParent.literals.size() == shorterSize+1))
                    longerParent.timestamp = timestamp;
                longerLiteral = (Literal)longerLiteral.nextItem;}
            for(int j = 0; j < shorterSize; ++j) {
                if(j == i) continue;
                Literal shorterLiteral2 = shorterLiterals.get(j);
                longerLiteral = literalIndex.getFirstLiteral(shorterLiteral2.literal);
                while(longerLiteral != null) {
                    longerParent = longerLiteral.clause;
                    int longerTimestamp = longerParent.timestamp;
                    if((longerTimestamp - timestamp) == shorterSize-2) { // resolution partner found
                        int longerSize = longerParent.literals.size();
                        resolve(shorterParent,resolutionLiteral,longerParent);
                        ++statistics.mergedResolvents;
                        if(longerParent.isInList && longerParent.literals.size() < longerSize) addShortenedClauseTask(longerParent);
                        if(!shorterParent.isInList) {timestamp += shorterSize; return;}
                        if(shorterParent.literals.size() < shorterSize) {
                            timestamp += shorterSize;
                            removeSubsumedClauses(shorterParent);
                            mergeResolution(shorterParent);
                            return;}}
                    else ++longerParent.timestamp;
                    longerLiteral = (Literal)longerLiteral.nextItem;}}}
        timestamp += shorterSize;}


    /**
     * Resolves two clauses by performing merge resolution.
     * <br>
     * If the second clause is not an OR-clause, the situation is as in the following examples:<br>
     *  - p,q,r and atleast 2 -p,q^x,r^y,s =&gt; q,r (shorterParent shortened)<br>
     *  - p,q,r and atleast n -p^n,q^x,r^y =&gt; q,r (shorterParent shortened)
     *  <br>
     * If both OR-clauses are of equal size then the second ('longerClause') is removed and the shorter clause is
     * shortend and kept.<br>
     * If the result is a unit clause, a trueLiteralTask is generated and both clauses are removed.
     *
     * @param shorterParent   the shorter parent clause
     * @param literal         the literal used for resolution
     * @param longerParent    the longer parent clause
     * @throws Unsatisfiable if the resolution results in a contradicting model (maybe a unit-clause is derived)
     */
    protected void resolve(Clause shorterParent, int literal, Clause longerParent) throws Unsatisfiable {
        assert shorterParent.quantifier == Quantifier.OR;
        int[] shorterClone = null;
        int[] longerClone  = null;
        if(trackReasoning || monitor != null) {
            longerClone = longerParent.simpleClone();
            shorterClone = shorterParent.simpleClone();}
        InferenceStep step;

        if(longerParent.quantifier != Quantifier.OR) {
            if(shorterParent.removeLiteral(literal, trackReasoning, this::removeLiteralFromIndex, this::addTrueLiteralTask)) {
                removeClause(shorterParent);} // unit clause
            if(trackReasoning) {
                step = new InfMergeResolution(shorterClone, longerClone,shorterParent);
                if(verify) step.verify(monitor,symboltable);
                shorterParent.addInferenceStep(step);}
            if(monitor != null) {
                monitor.accept("Merge Resolution: " + Clause.toString(shorterClone, symboltable) + " + " +
                        Clause.toString(longerClone,symboltable) + " => " + shorterParent.toString(symboltable,0));}
            return;}

        if(shorterParent.literals.size() == longerParent.literals.size()) {
            if(shorterParent.removeLiteral(literal, trackReasoning, this::removeLiteralFromIndex, this::addTrueLiteralTask)) {
                removeClause(shorterParent);} // unit clause
            if(trackReasoning) {
                step = new InfMergeResolution(shorterClone, longerClone,shorterParent);
                if(verify) step.verify(monitor,symboltable);
                shorterParent.addInferenceStep(step);}
            removeClause(longerParent);
            removeClauseFromIndex(longerParent);
            if(monitor != null) {
                monitor.accept("Merge Resolution: " + Clause.toString(shorterClone, symboltable) + " + " +
                        Clause.toString(longerClone,symboltable) + " => " + shorterParent.toString(symboltable,0));}
            return;}

        if(longerParent.removeLiteral(-literal, trackReasoning, this::removeLiteralFromIndex, this::addTrueLiteralTask))
            removeClause(longerParent); // should not happen
        if(trackReasoning) {
            step = new InfMergeResolution(shorterClone, longerClone,longerParent);
            if(verify) step.verify(monitor,symboltable);
            longerParent.addInferenceStep(step);}
        if(monitor != null) {
            monitor.accept("Merge Resolution: " + Clause.toString(shorterClone, symboltable) + " + " +
                    Clause.toString(longerClone,symboltable) + " => " + longerParent.toString(symboltable,0));}
    }





    /** extends the model by determining the truth-value of singleton pure predicates in interval- and exactly-clauses.
         *
         * @throws Unsatisfiable should not happen.
         */
    public void extendModel() throws Unsatisfiable {
        for(int i = singletons.size()-2; i >= 0; i -=2) {
            Clause clause = (Clause)singletons.get(i+1);
            int trueLiterals = clause.trueLiterals(model::isTrue);
            if(clause.min <= trueLiterals &&  trueLiterals <= clause.max) { // enough true literals. All unsigned literals can become false.
                for(Datastructures.Literal literalObject : clause.literals) {
                    int unsignedLiteral = literalObject.literal;
                    if(model.status(unsignedLiteral) == 0) {
                        if(monitor!=null) monitor.accept("Extending model with " + Symboltable.toString(-unsignedLiteral,symboltable) +
                                " for clause " + clause.toString(symboltable,0));
                        model.add(null,-unsignedLiteral,null);}}
                continue;}

            if(trueLiterals > clause.max) { // this should not happen
                ErrorReporter.reportErrorAndStop("Normalizer.extendModel: too many true literals in clause " +
                        clause.toString(symboltable,0) + "\nnumber of true literal: " + trueLiterals +
                        "\nModel: " + model.toString(symboltable));}

            for(Datastructures.Literal literalObject : clause.literals) {
                int unsignedLiteral = literalObject.literal;
                if(model.status(unsignedLiteral) == 0) { // filling up the true literals up to clause.max
                    if(trueLiterals >= clause.max) unsignedLiteral = -unsignedLiteral;
                    if(monitor!=null) monitor.accept("Extending model with " + Symboltable.toString(unsignedLiteral,symboltable) +
                            " for clause " + clause.toString(symboltable,0));
                    model.add(null,unsignedLiteral,null);
                    if(model.status(literalObject.literal) == 1) trueLiterals += literalObject.multiplicity;}}


            if(trueLiterals < clause.min)  // should not happen
                ErrorReporter.reportErrorAndStop("Normalizer.extendModel: not enough true literals in clause " +
                        clause.toString(symboltable,0) + "\nnumber of true literals: " + trueLiterals  +
                        "\nModel: " + model.toString(symboltable));}}

    /**
     * Returns a string representation of the given version of the clause list.
     * <br>
     * - clauses:    all clauses<br>
     * - singletons: all singletons<br>
     * - index:      the clauses in the literal index
     * <br>
     * None of these keywords: all three lists.
     *
     * @param version the version of the object to generate the string representation of
     * @param symboltable the symbol table used for generating the string representation
     * @return the string representation of the clause list's version
     */
    public String toString(String version, Symboltable symboltable) {
        switch(version.toLowerCase()) {
            case "clauses":    return toStringClauses(symboltable);
            case "singletons": return toStringSingletons(symboltable);
            case "index" :     return literalIndex.toString(predicates,symboltable);
            case "queue":      return toStringQueue(symboltable);}
        String cl = clauses.isEmpty() ? "" :  "Clauses:\n"    + toStringClauses(symboltable);
        String sgt = (!singletons.isEmpty()) ? "Singletons:\n" + toStringSingletons(symboltable) : "";
        String ind = literalIndex.toString(predicates,symboltable);
        String qu = toStringQueue(symboltable);
        if (!qu.isEmpty()) qu  = "Queue:\n" + qu;
        if(!ind.isEmpty()) ind = "Index:\n" + ind;
        if(!cl.isEmpty() && (!sgt.isEmpty() || !ind.isEmpty())) cl +="\n\n";
        if(!sgt.isEmpty() && !qu.isEmpty()) sgt += "\n\n";
        if(!qu.isEmpty() && !ind.isEmpty()) qu += "\n\n";
        return  cl + sgt + qu + ind;}

    /** lists the singletons as string
     *
     * @param symboltable null or a symboltable.
     * @return the singletons as a string.*/
    public String toStringSingletons(Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        st.append("Singleton Literals:\n");
        for(int i = 0; i < singletons.size(); i += 2) {
            st.append(Symboltable.toString((int) singletons.get(i), symboltable)).append(" in clause ");
            st.append(((Clause) singletons.get(i+1)).toString(symboltable,0)).append("\n");}
        return st.toString();}

    /**
     * Returns a string representation of the clauses contained in this object.
     *
     * @param symboltable the symbol table used to resolve symbols in the clauses
     * @return a string representation of the clauses, or an empty string if there are no clauses
     */
    public String toStringClauses(Symboltable symboltable) {
        if(clauses.isEmpty()) return "";
        int size = (clauses.lastLinkedItem.id + "." + clauses.lastLinkedItem.version).length();
        StringBuilder sb = new StringBuilder();
        Clause clause = clauses.firstLinkedItem;
        while (clause != null) {
            sb.append(clause.toString(symboltable,size)).append("\n");
            clause = (Clause) clause.nextItem;}
        return sb.toString();}

    /**
     * Convert the queue of tasks to a string representation.
     * Each task is converted to a string using the provided symbol table.
     *
     * @param symboltable the symbol table used for converting tasks to strings
     * @return a string representation of the queue of tasks
     */
    public String toStringQueue(Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        for(Task task : queue) {
            st.append(task.toString(symboltable)).append("\n");}
        return st.toString();

    }
}










