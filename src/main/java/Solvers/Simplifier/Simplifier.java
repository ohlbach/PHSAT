package Solvers.Simplifier;

import Datastructures.Clauses.InputClauses;
import Datastructures.Results.Result;
import Datastructures.Results.Satisfiable;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Statistics.Statistic;
import Datastructures.Symboltable;
import Datastructures.Task;
import Datastructures.Theory.EquivalenceClasses.EquivalenceClasses;
import InferenceSteps.InfInputClause;
import InferenceSteps.InferenceStep;
import Management.Monitor.Monitor;
import Management.ProblemSupervisor;
import Solvers.Solver;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.concurrent.PriorityBlockingQueue;
import java.util.function.IntSupplier;

import static java.lang.Thread.interrupted;

/** This is an incomplete solver which tries to simplify the clauses as far as possible.
 *  - It tries to derive new true literals and send them to the model.<br>
 *  - It tries to derive new equivalences and send them to the equivalenceClasses.<br>
 *  <br>
 *  The following operations are preformed:<br>
 *  - all clauses are transformed to atleast normal form.<br>
 *  - subsumed clauses are removed. <br>
 *  - merge resolution between two clause, possibly with a two-literal clause between them, reduces the clauses.<br>
 *  - resolution between two-literal clauses generates its resolution completion.<br>
 *  - New true literals in the model are incorporated.<br>
 *  - New equivalences form the equivalenceClasses are incorporated.
 */

public class Simplifier extends Solver {


    /** the input clauses */
    protected InputClauses inputClauses;

    /** the equivalence classes */
    public EquivalenceClasses equivalenceClasses;

    /** The id of the current problem to be solved */
    private String problemId;

    public final String solverId = "Simplifier";

    /** for distinguishing the monitoring areas */
    private String monitorId;

    /** the simplifier's statistics */
    protected SimplifierStatistics statistics;

    /** contains a list of literalObjects for each literal in two-literal clauses.*/
    protected Literals literalIndexTwo;

    /** contains a list of literalObjects for each literal in longer clauses.*/
    protected Literals literalIndexMore;

    /** the list of clauses. */
    protected Clauses clauses;

    /** for generating an identifier for the new clauses. */
    private IntSupplier nextId;

    /** used in various algorithms. */
    private int timestamp = 1;

    public static void makeSolvers(HashMap<String,String> parameters, ArrayList<Solver> solvers,
                                   StringBuilder errors, StringBuilder warnings) {
        solvers.add(new Simplifier());
    }

    /** constructs a new Simplifier.
     * All internal data are taken form the supervisor.
     *
     */
    public Simplifier() {
        super(1,null);
    }

    /** this is a constructor for testing purposes (without ProblemSupervisor)
     *
     * @param predicates      the number of predicates.
     * @param monitor         null or a monitor.
     * @param trackReasoning  true if the reasoning is to be tracked.
     * @param nextId          for generating a new identifier for a clause.
     */
    public Simplifier(int predicates, Monitor monitor, boolean trackReasoning, IntSupplier nextId) {
        this.predicates = predicates;
        this.monitor = monitor;
        monitoring = monitor != null;
        this.monitorId = "Simplifier";
        this.trackReasoning = trackReasoning;
        this.nextId = nextId;
        equivalenceClasses = new EquivalenceClasses(null,monitor);
        literalIndexTwo = new Literals(predicates);
        literalIndexMore = new Literals(predicates);
        clauses = new Clauses();
        model = equivalenceClasses.model;
        statistics = new SimplifierStatistics(solverId);
    }


    /** specifies the task types in the priority queue.
     */
    private enum TaskType {
        /** a new true literal is obtained from the model */
        ProcessTrueLiteral,
        /** a new binary equivalence is found in the TwoLiteral module. */
        ProcessEquivalence,
        /** a new binary clause is available for simplifications */
        ProcessBinaryClause,
        /** a longer clause is available for simplification */
        ProcessLongerClause,
        /** an input clause should be simplified. */
        ProcessLongerInputClause,
        /** merge resolution with a binary clause in between */
        ProcessBinaryTriggeredMerging,
        /** partial merge resolution between 3-literal clauses */
        ProcessMergeResolutionPartial
    }

    /** gets the priority for the objects in the queue.
     *
     * @param task the task in the queue
     * @return the priority of the objects in the queue.
     */
    private int getPriority(Task<Simplifier.TaskType> task) {
        switch(task.taskType) {
            case ProcessTrueLiteral: return Math.abs((Integer)task.a);
            case ProcessEquivalence: return predicates + (Math.abs((Integer)task.a)) + 1; // this guarantees a deterministic sequence of the tasks
            case ProcessBinaryClause:           return 2*predicates + 100;
            case ProcessLongerClause:           return 2*predicates + 101;
            case ProcessLongerInputClause:      return 2*predicates + 102;
            case ProcessMergeResolutionPartial: return 2*predicates + 103;}
        return 0;}

    /** Installs the observer in the model and the equivalence classes.
     */
    @Override
    public void installCommunication(ProblemSupervisor problemSupervisor) {
        problemSupervisor.model.addObserver(this::addTrueLiteralTask);
        problemSupervisor.equivalenceClasses.addObserver(this::addEquivalenceTask);}

    /** A queue of newly derived unit literals and binary equivalences.
     * The unit literals are automatically put at the beginning of the queue.
     */
    private final PriorityBlockingQueue<Task<Simplifier.TaskType>> queue =
            new PriorityBlockingQueue<>(10, Comparator.comparingInt(this::getPriority));

    /** reads the disjunctions, the atleast, atmost, exactly and interval clauses from inputClauses and transforms them to atleast-clauses.
     * The clauses themselves are simplified as far as possible.<br>
     * New unit clauses are put into the model. <br>
     * Two-literal clauses generate a corresponding task.
     * 
     * @throws Result if a contradiction or the empty clause is derived.
     */
    public void readInputClauses() throws Result{
        try{
            for(int[] inputClause : inputClauses.disjunctions) {
                Clause clause = new Clause(inputClause);
                if(clause.removeComplementaryLiterals()) {++statistics.notInternalizedInputClauses; continue;}
                clause.removeDoubleLiterals();
                if(clause.size() == 1) {
                    ++statistics.derivedUnitClauses;
                    ++statistics.notInternalizedInputClauses;
                    model.add(clause.literals.get(0).literal,clause.inferenceStep);
                    continue;}
                insertClause(clause);
                if(clause.size() == 2) addBinaryClauseTask(clause);
                else {queue.add(new Task<>(TaskType.ProcessLongerInputClause,clause));}
                if(clause.size() == 3){synchronized(this){queue.add(new Task<>(TaskType.ProcessMergeResolutionPartial,clause));}}
            }

            for(int[] inputClause : inputClauses.atleasts) {
                insertNewClause(inputClause,new Clause(inputClause));}

            for(int[] atmostClause : inputClauses.atmosts) {
                int[] atleastClause = InputClauses.atmostToAtleast(atmostClause);
                Clause clause = new Clause(atleastClause);
                if(trackReasoning) clause.inferenceStep = new InfAtmostToAtleast(atmostClause,atleastClause);
                if(insertNewClause(atmostClause,clause)) {
                    if(monitoring) {
                        monitor.println(monitorId,"Atmost-clause: " +
                                InputClauses.toString(0,atmostClause,symboltable) + " turned to atleast-clause " +
                                clause.toString(symboltable,0));}}}

            for(int[] exactlyClause : inputClauses.exactlys) {
                int[][] atleastClauses = InputClauses.exactlyToAtleast(exactlyClause,nextId);
                for(int i = 0; i < 2; ++i) {
                    int[] atleastClause = atleastClauses[i];
                    Clause clause = new Clause(atleastClause);
                    if(trackReasoning) clause.inferenceStep = new InfExactlyToAtleast(exactlyClause,atleastClause);
                    if(insertNewClause(exactlyClause,clause)){
                        if(monitoring) {
                            monitor.println(monitorId,"Exactly-clause: " +
                                    InputClauses.toString(0,exactlyClause,symboltable) + " turned to atleast-clause " +
                                    clause.toString(symboltable,0));}}}}

            for(int[] intervalClause : inputClauses.intervals) {
                int[][] atleastClauses = InputClauses.intervalToAtleast(intervalClause,nextId);
                for(int i = 0; i < 2; ++i) {
                    int[] atleastClause = atleastClauses[i];
                    Clause clause = new Clause(atleastClause);
                    if(trackReasoning) clause.inferenceStep = new InfIntervalToAtleast(intervalClause,atleastClause);
                    if(insertNewClause(intervalClause,clause)){
                        if(monitoring) {
                            monitor.println(monitorId,"Interval-clause: " +
                                    InputClauses.toString(0,intervalClause,symboltable) + " turned to atleast-clause " +
                                    clause.toString(symboltable,0));}}}}
            checkAllPurities();
            statistics.orAndAtleastCLauses = clauses.size;
            if(clauses.isEmpty()) throw new Satisfiable(problemId,solverId, model);}
        catch(Result result) {
            result.solverId  = solverId;
            result.problemId = problemId;
            result.statistic = statistics;
            throw result;}
        }

    /** simplifies and inserts an atleast-clause derived from input clauses.
     * The clauses are put into the task queue.
     *
     * @param inputClause the original input-clause.
     * @param clause      the clause to be inserted.
     * @return            true if the clause survived the simplifications.
     * @throws Unsatisfiable     if a contradiction is encountered.
     */
    private boolean insertNewClause(int[] inputClause, Clause clause) throws Unsatisfiable {
        if(clause.isTrue())  {++statistics.notInternalizedInputClauses; return false;}
        if(clause.isFalse()) {throw new UnsatClause(problemId,solverId,inputClause);}
        int size = clause.size();
        if(clause.removeComplementaryLiterals())   {++statistics.notInternalizedInputClauses; return false;}
        if(clause.size() != size) {
            if(monitoring)
                monitor.println(monitorId,"Complementary literals removed in clause " +
                        InputClauses.toString(0,inputClause,symboltable) + " -> " + clause.toString(symboltable,0));}
        if(!clause.isDisjunction) {
            if(!simplifyClause(clause, true)) {++statistics.notInternalizedInputClauses; return false;}}
        if(clause.size() == 2 && binaryClauseIsSubsumedByBinaryClause(clause)) return false;
        insertClause(clause);
        ++statistics.orAndAtleastCLauses;
        if(clause.size() == 2) addBinaryClauseTask(clause);
        else {synchronized(this){queue.add(new Task<>(TaskType.ProcessLongerInputClause,clause));}}
        if(clause.size() == 3){synchronized(this){queue.add(new Task<>(TaskType.ProcessMergeResolutionPartial,clause));}}
        return true;}

    /** adds the literals which are already true in the model to the task queue.
     * Installs the observer in the model.
     */
    public void initialize() {
        for(int literal: model.model) {
            addTrueLiteralTask(literal,model.getInferenceStep(literal));}
        model.addObserver(this::addTrueLiteralTask);}

    /** checks all literals for purity.
     * Pure literals are inserted into the model.
     *
     * @throws Unsatisfiable should not happen.
     */
    protected void checkAllPurities() throws Unsatisfiable{
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            if(checkPurity(predicate)) continue;
            checkPurity(-predicate);}}

    /** checks the literal for purity. <br>
     * A literal l is pure if -l does not occur anymore. l can then be made true. <br>
     * A pure literal is put into the model.<br>
     * The literal is not checked if it is already in the model.
     *
     * @param literal a literal.
     * @return true if the literal is pure.
     * @throws Unsatisfiable should not happen.
     */
    protected boolean checkPurity(int literal) throws Unsatisfiable {
        if(model.status(literal) != 0) return false;
        if(literalIndexTwo.isEmpty(-literal) && literalIndexMore.isEmpty(-literal)) {
            model.add(literal, trackReasoning ? new InfPureLiteral(literal) : null);
            if(monitoring) monitor.println(monitorId,"Pure Literal: " + Symboltable.toString(literal,symboltable));
            ++statistics.pureLiterals;
            return true;}
        return false;}

    /** adds a true literal to the queue
     *
     * @param literal a true literal
     * @param inferenceStep which caused the truth
     */
    public void addTrueLiteralTask(int literal, InferenceStep inferenceStep) {
        if(monitoring) {
            monitor.print(monitorId,"In: True literal " +
                    Symboltable.toString(literal,symboltable));}
        synchronized (this) {queue.add(new Task<>(TaskType.ProcessTrueLiteral, literal, inferenceStep));}}

    /** adds a true literal to the queue
     *
     * @param literal a true literal
     * @param inferenceStep which caused the truth
     */
    public void addEquivalenceTask(int representative, int literal, InferenceStep inferenceStep) {
        if(monitoring) {
            monitor.print(monitorId,"In: Equivalence " + Symboltable.toString(representative,symboltable) + "="+
                    Symboltable.toString(literal,symboltable));}
        synchronized (this) {queue.add(new Task<>(TaskType.ProcessEquivalence, representative, literal, inferenceStep));}}

    private boolean printClauses = true;

    /** reads the next task from the task queue and processes it.
     *
     * @param n 0 or the maximum number of task before stopping the loop (0: unlimited) (> 0 for test purposes).
     * @throws Result if a contradiction is encountered or the clause set became empty.
     */
    public void processTasks(int n) throws Result {
        Task<Simplifier.TaskType> task;
        int counter = 0;
        Clause clause = null;
        while(!interrupted()) {
            try {
                if(monitoring) {monitor.print(monitorId,"Queue is waiting\n" + Task.queueToString(queue));}
                task = queue.take(); // waits if the queue is empty
                switch(task.taskType){
                    case ProcessTrueLiteral:
                        if(monitoring) {monitor.print(monitorId,"Next Task: " + task);
                            if(printClauses) System.out.println(clauses.toString());}
                        processTrueLiteral((Integer)task.a);
                        break;
                    case ProcessEquivalence:
                        if(monitoring) {monitor.print(monitorId,"Next Task: " + task);
                            if(printClauses) System.out.println(clauses.toString());}
                        processEquivalence((Integer)task.a,(Integer)task.b,(InferenceStep) task.c);
                        break;
                    case ProcessBinaryClause:
                        clause = (Clause)task.a;
                        if(clause.exists) {
                            if(monitoring) {monitor.print(monitorId,"Next Task: " + task);
                                if(printClauses) System.out.println(clauses.toString());}
                            processBinaryClause(clause);}
                        break;
                    case ProcessLongerClause:
                        clause = (Clause)task.a;
                        if(clause.exists) {
                            if(monitoring) {monitor.print(monitorId,"Next Task: " + task);
                                if(printClauses) System.out.println(clauses.toString());}
                            processLongerClause(clause);}
                        break;
                    case ProcessLongerInputClause:
                        clause = (Clause)task.a;
                        if(clause.exists) {
                            if(monitoring) {monitor.print(monitorId,"Next Task: " + task);
                                if(printClauses) System.out.println(clauses.toString());}
                            processLongerInputClause(task);}
                        break;
                   
                    case ProcessMergeResolutionPartial:
                        clause = (Clause)task.a;
                        if(clause.exists) {
                            if(monitoring) {monitor.print(monitorId,"Next Task: " + task);
                                if(printClauses)  System.out.println(clauses.toString());}
                            mergeResolutionPartial(clause);}
                        break;
                }
                if(clauses.isEmpty()) {throw new Satisfiable(problemId,solverId,model);}}
            catch(InterruptedException ex) {return;}
            if(n > 0 && ++counter == n) return;}}

    /** The method applies a true literal to the clause.
     * For a disjunction this means that the clause is true and can therefore be deleted.<br>
     * For a quantified clause this means that the literal can be deleted and the limit
     * must be reduced by the literal's multiplicity.
     * <br>
     * The resulting clause must be checked for the following phenomena: <br>
     *  - if the resulting limit is &lt;= 0, the clause is true and can be deleted.<br>
     *  - if the resulting limit is 1, the clause became a disjunction. <br>
     *  - if the limit is still &gt; 1, new true literals might be derived.<br>
     *  Example: atleast 4 p,q^2,r^2 and p is true<br>
     *  The clause is then: atleast 3 q^2,r^2. <br>
     *  Both q and r must now be true.
     *
     * @param literal a true literal.
     * @throws Unsatisfiable if a contradiction is found.
     *  */
    protected void processTrueLiteral(int literal) throws Unsatisfiable {
        processTrueLiteralTwo(literal);
        processTrueLiteralMore(literal);}

    /** applies a true literal to all two-literal clauses containing this literal.
     * Clauses containing this literal are removed.<br>
     * Clauses containing -literal yield a new true literal, which is put into the model.<br>
     * The clause is removed as well.
     *
     * @param literal a true literal.
     * @throws Unsatisfiable if a contradiction is encountered.
     */
    protected void processTrueLiteralTwo(int literal) throws Unsatisfiable{
        Literal literalObject = literalIndexTwo.getFirstLiteralObject(literal);
        while(literalObject != null) {
            Clause clause = literalObject.clause;
            if(clause == null) {literalObject = literalObject.nextLiteral; continue;}
            removeClause(clause,true); // literals may become pure.
            literalObject = literalObject.nextLiteral;}

        literalObject = literalIndexTwo.getFirstLiteralObject(-literal);
        while(literalObject != null) {
            Clause clause = literalObject.clause;
            if(clause == null) {literalObject = literalObject.nextLiteral; continue;}
            Literal otherLiteral = (clause.literals.get(0) == literalObject) ? clause.literals.get(1) : clause.literals.get(0);
            if(monitoring) monitor.println(monitorId,clause.toString(symboltable,0) + " and true(" +
                    Symboltable.toString(literal,symboltable) + ") -> " +
                    "true("+Symboltable.toString(otherLiteral.literal,symboltable)+")");
            model.add(otherLiteral.literal,
                    trackReasoning ?
                            new InfUnitResolutionTwo(clause,literal,model.getInferenceStep(literal),otherLiteral.literal) :
                            null);
            ++statistics.derivedUnitClauses;
            removeClause(clause,true);
            literalObject = literalObject.nextLiteral;}

        literalIndexTwo.removePredicate(literal);}

    /** applies a true literal to all longer clauses containing this literal.
     * All literals with a truth value in the model are removed.<br>
     * Clauses which become true in this step are entirely removed.<br>
     * The empty clause causes an UnsatEmptyClause exception to be thrown.<br>
     * Derived unit clauses are put into the model <br>
     * Shortened clauses cause new tasks to be inserted into the task queue.<br>

     * @param literal a true (or false) literal.
     * @throws Unsatisfiable if a contradiction is encountered.
     */
    protected void processTrueLiteralMore(int literal) throws Unsatisfiable{
        for(int sign = 1; sign >= -1; sign -= 2) {
            literal *= sign;   // the clauses containing the predicate are selected. The literals to be removed depend on the model.
            Literal literalObject = literalIndexMore.getFirstLiteralObject(literal);
            while(literalObject != null) {
                 Clause clause = literalObject.clause;
                 if(clause == null) {literalObject = literalObject.nextLiteral; continue;}
                 boolean removed = false;
                 for(int i = 0; i < clause.literals.size(); ++i){
                     Literal litObject = clause.literals.get(i);
                     int status = model.status(litObject.literal);
                     if(status == 0) continue; // literal must not be removed.
                     boolean isTrue = status == 1;
                     String clauseBefore = (trackReasoning || monitoring) ? clause.toString(symboltable,0) : null;
                     boolean isDisjunction = clause.isDisjunction;
                     if(!removeLiteralFromClause(litObject,isTrue)) {removed = true; break;} // clause is true now.

                     if(monitoring) monitor.println(monitorId,clauseBefore + " and " +
                                (isTrue ? "true":"false")+ "(" +
                                Symboltable.toString(litObject.literal,symboltable) + ") -> " + clause.toString(symboltable,0));
                     if(trackReasoning) {
                         clause.inferenceStep = new InfUnitResolution(clauseBefore,clause.inferenceStep,isDisjunction,
                                 IntArrayList.wrap(new int[]{litObject.literal}),isTrue,
                                 clause.toString(symboltable,0),model);}
                     --i;}
                    if(removed) continue;
                    if(clause.limit > clause.expandedSize) throw new UnsatClause(problemId,solverId,clause);
                    switch(clause.size()) {
                        case 0: throw new UnsatEmptyClause(problemId,solverId, clause.id, clause.inferenceStep);
                        case 1:
                            int trueLiteral =  clause.literals.get(0).literal;
                            model.add(trueLiteral,clause.inferenceStep);
                            ++statistics.derivedUnitClauses;
                            if(monitoring)
                                monitor.println(monitorId,"New true literal " +
                                        Symboltable.toString(trueLiteral,symboltable) + " derived from clause " + clause.id);
                            removeClause(clause,false);
                            break;
                        case 2:  addBinaryClauseTask(clause);
                                 addBinaryMergeTask(clause);
                                 break;
                        default: if(simplifyClause(clause,false)) addShortenedClauseTask(clause);
                                else removeClause(clause,true);}
            literalObject = literalObject.nextLiteral;}}
        literalIndexMore.removePredicate(literal);}

    /** moves a longer clause which has become a binary clause to the literalIndexTwo.
     * To this end the literals are copied.
     * Therefor an iteration over the old literals can still proceed.
     *
     * @param clause a binary clause.
     */
    protected void moveToIndexTwo(Clause clause) {
        assert(clause.size() == 2);
        for(int i = 0; i < 2; ++i) {
            Literal oldLiteralObject = clause.literals.get(i);
            Literal newLiteralObject = new Literal(oldLiteralObject.literal,oldLiteralObject.multiplicity);
            newLiteralObject.clause = clause;
            literalIndexMore.removeLiteral(oldLiteralObject);
            literalIndexTwo.addLiteral(newLiteralObject);
            clause.literals.set(i,newLiteralObject);}}





    /** adds a just shortened clause to the task queue, either a BinaryClauseTask and a BinaryMergeTask, or a LongerClauseTask.
     *
     * @param clause a just shortened clause
     */
    protected void addShortenedClauseTask(Clause clause) {
        if(clause.size() == 2) {addBinaryClauseTask(clause);
                                addBinaryMergeTask(clause);}
        else                   addLongerClauseTask(clause);}

    /** processes the BinaryClause task.
     * - clauses subsumed by the binary clause are removed. <br>
     * - merge resolution with the binary clause is performed.<br>
     * - equivalences are detected. <br>
     * - all binary resolvents with the other binary clauses are generated.
     *
     * @param clause         a two-literal clause
     * @throws Unsatisfiable if a contradiction is discovered.
     */
    protected void processBinaryClause(Clause clause) throws Unsatisfiable {
        assert(clause.size() == 2);
        int literal1 = clause.literals.get(0).literal;
        int literal2 = clause.literals.get(1).literal;
        removeClausesSubsumedByBinaryClause(clause);
        binaryMergeResolutionAndEquivalence(clause,literal1,literal2,true);
        if(!clause.exists) return;
        binaryMergeResolutionAndEquivalence(clause,literal2,literal1,false);
        if(!clause.exists) return;
        mergeResolutionWithBinaryClause(clause,literal1,literal2);
        if(!clause.exists) return;
        mergeResolutionWithBinaryClause(clause,literal2,literal1);
        if(!clause.exists) return;
        saturateBinaryClausesWithBinaryCLause(clause,literal1);
        if(!clause.exists) return;
        saturateBinaryClausesWithBinaryCLause(clause,literal2);
        saturateLongerClausesWithBinaryClause(clause);}


    /** removes all longer disjunctions which are subsumed by a binary subsumer.
     *
     * @param subsumer a binary clause.
     * @throws Unsatisfiable should not happen.
     */
    protected void removeClausesSubsumedByBinaryClause(Clause subsumer) throws Unsatisfiable {
        assert(subsumer.size() == 2);
        Literal subsumerLiteral = literalIndexMore.getFirstLiteralObject(subsumer.literals.get(0).literal);
        while(subsumerLiteral != null) {
            Clause subsumee = subsumerLiteral.clause;
            if(subsumee == null) {subsumerLiteral = subsumerLiteral.nextLiteral; continue;}
            if(subsumee.exists && subsumee.isDisjunction && subsumee != subsumer) subsumee.timestamp = timestamp;
            subsumerLiteral = subsumerLiteral.nextLiteral;}

        Literal subsumeeLiteral = literalIndexMore.getFirstLiteralObject(subsumer.literals.get(1).literal);
        while(subsumeeLiteral != null) {
            Clause subsumee = subsumeeLiteral.clause;
            if(subsumee == null) {subsumeeLiteral = subsumeeLiteral.nextLiteral; continue;}
            if(subsumee.timestamp == timestamp) {
                removeClause(subsumee,true);
                ++statistics.subsumedClauses;}
            subsumeeLiteral = subsumeeLiteral.nextLiteral;}
        ++timestamp;}

    /** removes all clauses subsumed by the given clause.
     *  A clause atleast n    p_1^k1 ... p_n^kn subsumes <br>
     *  a clause atleast n-.. p_1^(k1+..) ... p_n^(kn+..) phi <br>
     *  where - means smaller or equal and + means larger or equal.<br>
     *  This is a sufficient, but not necessary condition. <br>
     *  A case not detected is: atleast 4 p^2,q^2,r subsumes atleast 3 p^2,q^3,r^2.
     *
     * @param subsumer       a clause,
     * @throws Unsatisfiable should not happen.
     */
    protected void removeClausesSubsumedByLongerClause(Clause subsumer) throws Unsatisfiable {
        boolean candidatesFound = false;
        int subsumerSize = subsumer.literals.size();
        int sumsumerLimit = subsumer.limit;
        Literal subsumerLiteral = subsumer.literals.get(0);
        Literal subsumeeLiteral = literalIndexMore.getFirstLiteralObject(subsumerLiteral.literal);
        while(subsumeeLiteral != null) { // mark all candidates with a timestamp
            Clause subsumee = subsumeeLiteral.clause;
            if(subsumee != subsumer && subsumee.limit <= sumsumerLimit &&
                    subsumee.literals.size() >= subsumerSize &&
                    subsumeeLiteral.multiplicity >= subsumerLiteral.multiplicity) {
                candidatesFound = true;
                subsumee.timestamp = timestamp;}
            subsumeeLiteral = subsumeeLiteral.nextLiteral;}

        if(!candidatesFound) return;
        for(int i = 1; i < subsumer.literals.size(); ++i) { // find candidates.
            subsumerLiteral = subsumer.literals.get(i);
            subsumeeLiteral = literalIndexMore.getFirstLiteralObject(subsumerLiteral.literal);
            while(subsumeeLiteral != null) {
                Clause subsumee = subsumeeLiteral.clause;
                if(subsumee == null) {subsumeeLiteral = subsumeeLiteral.nextLiteral; continue;}
                if(subsumee.exists && (subsumee.timestamp - timestamp) == i - 1&&
                        subsumeeLiteral.multiplicity >= subsumerLiteral.multiplicity) {
                    ++subsumee.timestamp;
                    if(subsumee.timestamp - timestamp == subsumerSize-1){
                        removeClause(subsumee,true);}}
                subsumeeLiteral = subsumeeLiteral.nextLiteral;}}
        timestamp += subsumerSize + 1;}

        /** performs merge resolution between binary clauses and equivalence recognition, if possible.
         * Binary MergeResolution:  p,q and -p,q -&gt; true(q).<br>
         * Equivalence Recognition: p,q and -p,-q -&gt; p == q.<br>
         * A derived true literal is inserted into the model.<br>
         * A derived equivalence is sent to the equivalence classes.<br>
         * In both cases the two clauses are removed and the search stops immediately.
         *
         * @param clause1   the binary clause.
         * @param literal1  either the first or the second literal.
         * @param literal2  the other literal.
         * @param checkEquivalence if true then equivalence check is done.
         * @throws Unsatisfiable if inserting a derived unit clause into the model causes a contradiction.
         */
    protected void binaryMergeResolutionAndEquivalence(Clause clause1, int literal1, int literal2,
                                                       boolean checkEquivalence) throws Unsatisfiable {
        assert(clause1.size() == 2);
        Literal literalObject = literalIndexTwo.getFirstLiteralObject(-literal1);
        while(literalObject != null) { // all clauses with -literal1 are marked.
            Clause clause = literalObject.clause;
            if(clause != null && clause != clause1) clause.timestamp = timestamp;
            literalObject = literalObject.nextLiteral;}

        literalObject = literalIndexTwo.getFirstLiteralObject(literal2);
        while(literalObject != null) {
            Clause clause2 = literalObject.clause;
            if(clause2 == null) {literalObject = literalObject.nextLiteral; continue;}
            if(clause2.timestamp == timestamp) { // a partner clause is found
                if(monitoring) monitor.println(monitorId,clause1.toString(symboltable,0) + " and " +
                        clause2.toString(symboltable,0) + " -> " + "true("+ Symboltable.toString(literal2,symboltable)+")");
                model.add(literal2,trackReasoning ? new InfMergeResolutionTwo(clause1,clause2,literal2) : null);
                removeClause(clause1,true);
                removeClause(clause2,true);
                ++timestamp;
                return;}
            literalObject = literalObject.nextLiteral;}

        if(checkEquivalence) {
            literalObject = literalIndexTwo.getFirstLiteralObject(-literal2);
            while(literalObject != null) {
                Clause clause2 = literalObject.clause;
                if(clause2 == null) {literalObject = literalObject.nextLiteral; continue;}
                if(clause2.timestamp == timestamp) { // a partner clause is found.
                    if(monitoring) monitor.println(monitorId,clause1.toString(symboltable,0) + " and " +
                            clause2.toString(symboltable,0) + " -> " +
                            Symboltable.toString(literal1,symboltable)+" == " + Symboltable.toString(-literal2,symboltable));
                    equivalenceClasses.addEquivalenceTask(literal1,-literal2,trackReasoning ? new InfEquivalence(clause1,clause2) : null);
                    removeClause(clause1,false);
                    ++timestamp;
                    return;}
                literalObject = literalObject.nextLiteral;}}
        ++timestamp;}

    /** performs resolution between the given clause and other binary clauses.
     * Subsumed resolvents are ignored. <br>
     * If the resolvent's literals are identical it is inserted into the model.<br>
     * Other resolvents generate a new BinaryClauseTask.
     *
     * @param clause     a binary clause,
     * @param literal1   a literal of the clause.
     * @throws Unsatisfiable  if the model detects a contradiction.
     */
    protected void saturateBinaryClausesWithBinaryCLause(Clause clause, int literal1) throws Unsatisfiable {
        assert(clause.size() == 2);
        Literal literalObject = literalIndexTwo.getFirstLiteralObject(-literal1);
        while(literalObject != null) {
            Clause clause1 = literalObject.clause;
            if(clause1 == null) {literalObject = literalObject.nextLiteral; continue;}
            Clause resolvent = resolveBetweenBinaryClauses(clause,clause1);
            if(!clause.exists) return; // unit clause derived
            if(resolvent != null) {
                insertClause(resolvent);
                addShortenedClauseTask(resolvent);}
            literalObject = literalObject.nextLiteral;}}


    /** generates a resolvent between two binary clauses.
     * p,q and -p,r -> q,r.<br>
     * p,q and -p,q -> q.<br>
     * A subsumed resolvent is not returned.<br>
     * If the resolvent merges to a unit clauses, both clauses are removed.
     *
     * @param clause1 a binary disjunction.
     * @param clause2 a binary disjunction.
     * @throws Unsatisfiable if a contradiction is found.
     * @return the resolvent or null if either the resolvent is subsumed, or a true literal is derived.
     */
    protected Clause resolveBetweenBinaryClauses(Clause clause1, Clause clause2) throws Unsatisfiable{
        assert(clause1.size() == 2);
        assert(clause2.size() == 2);
        int literala1 = clause1.literals.get(0).literal;
        int literala2 = clause1.literals.get(1).literal;
        int literalb1 = clause2.literals.get(0).literal;
        int literalb2 = clause2.literals.get(1).literal;
        int literal1,literal2;
        if(literala1 == -literalb1) {literal1 = literala2; literal2 = literalb2;}
        else {
            if(literala1 == -literalb2) {literal1 = literala2; literal2 = literalb1;}
            else {
                if(literala2 == -literalb1) {literal1 = literala1; literal2 = literalb2;}
                else {literal1 = literala1; literal2 = literalb1;}}}

        if(literal1 == -literal2) return null; // tautology
        if(literal1 == literal2) {
            ++statistics.derivedUnitClauses;
            model.add(literal1,trackReasoning ? new InfMergeResolutionTwo(clause1,clause2,literal1) : null);
            if(monitoring) {
                monitor.println(monitorId,clause1.toString(symboltable,0) + " and " +
                        clause2.toString(symboltable,0) + " -> " + Symboltable.toString(literal1,symboltable));}
            removeClause(clause1,true);
            removeClause(clause2,true);
            ++statistics.binaryResolvents;
            return null;}
        Clause resolvent = new Clause(nextId.getAsInt(), literal1, literal2);
        if(binaryClauseIsSubsumedByBinaryClause(resolvent)) return null;
        ++statistics.binaryResolvents;
        if(trackReasoning) resolvent.inferenceStep = new InfBinaryResolution(clause1,clause2,resolvent,symboltable);
        return resolvent;}



    /** checks if the binary clause is subsumed by another binary clause.
     *
     * @param clause a binary clause
     * @return true if the clause is subsumed.
     */
    protected boolean binaryClauseIsSubsumedByBinaryClause(Clause clause) {
        return binaryClauseIsSubsumedByBinaryClause(clause.literals.get(0).literal, clause.literals.get(1).literal);}

    /** checks if the binary clause is subsumed by another binary clause.
     * Since binary clauses are always disjunctions, quantification plays no role.
     *
     * @param literal1 the first literal of a binary clause
     * @param literal2 the second literal of a binary clause.
     * @return true if the clause is subsumed.
     */
    protected boolean binaryClauseIsSubsumedByBinaryClause(int literal1, int literal2) {
        Literal literalObject = literalIndexTwo.getFirstLiteralObject(literal1);
        while(literalObject != null) {
            Clause clause = literalObject.clause;
            if(clause.exists) clause.timestamp = timestamp;
            literalObject = literalObject.nextLiteral;}

        literalObject = literalIndexTwo.getFirstLiteralObject(literal2);
        while(literalObject != null) {
            if(literalObject.clause.timestamp == timestamp) {++timestamp; return true;}
            literalObject = literalObject.nextLiteral;}
        ++timestamp;
        return false;}

    /** checks if the longer clause is subsumed by a binary clause.
     *  Only Or-clauses can be subsumed by binary clauses.
     * 
     * @param clause a longer clause
     * @return null or the binary subsumer clause. 
     */
    Clause longerClauseIsSubsumedByBinaryClause(Clause clause) {
        if(clause.limit > 1) return null;
        for(Literal literalObject : clause.literals) {
            int literal = literalObject.literal;
            Literal literalObjectTwo = literalIndexTwo.getFirstLiteralObject(literal);
            while(literalObjectTwo != null) {
                ArrayList<Literal> binaryLiterals = literalObjectTwo.clause.literals;
                int otherBinaryLiteral = (literal == binaryLiterals.get(0).literal) ? binaryLiterals.get(1).literal :
                                                                                      binaryLiterals.get(0).literal;
                if(clause.findLiteral(otherBinaryLiteral) != null) return literalObjectTwo.clause;
                literalObjectTwo = literalObjectTwo.nextLiteral;}}
        return null;}

    /** checks if the longer clause is subsumed by another longer clause.
     *
     * @param subsumee a longer clause
     * @return null or the binary subsumer clause.
     */
    Clause longerClauseIsSubsumedByLongerClause(Clause subsumee) {
        int size = subsumee.size();
        int limit = subsumee.limit;
        for(Literal literalObject1 : subsumee.literals) {
            int literal1 = literalObject1.literal;
            Literal literalObject2 = literalIndexMore.getFirstLiteralObject(literal1);
            while(literalObject2 != null) {
                Clause subsumer = literalObject2.clause;
                if(subsumer.timestamp < timestamp) {
                    if(subsumer.size() <= size && subsumer.limit >= limit &&
                            literalObject2.multiplicity <= literalObject1.multiplicity) subsumer.timestamp = timestamp;}
                else {
                    if(subsumer.timestamp - timestamp == subsumer.size()-2 &&
                            literalObject2.multiplicity <= literalObject1.multiplicity){
                        timestamp += size+2; return subsumer;}
                    if(literalObject2.multiplicity <= literalObject1.multiplicity) ++subsumer.timestamp;}
                literalObject2 = literalObject2.nextLiteral;}}
        timestamp += size+2;
        return null;}


    /** computes all resolvents between the given binary clause and the longer clauses.
     *
     * @param clause         a binary clause.
     * @throws Unsatisfiable if a contradiction is discovered.
     */
    void saturateLongerClausesWithBinaryClause(Clause clause) throws Unsatisfiable {
        assert(clause.size() == 2);
        for(Literal literalObject : clause.literals) {
            Literal negLiteralObject = literalIndexMore.getFirstLiteralObject(-literalObject.literal);
            while(negLiteralObject != null) {
                Clause resolvent = negLiteralObject.clause.resolve(nextId,negLiteralObject,literalObject);
                if(resolvent != null) {
                    if(trackReasoning) resolvent.inferenceStep = 
                            new InfResolution(literalObject.clause,clause,resolvent,inputClauses.symboltable);
                    if(simplifyClause(resolvent,true) && !isSubsumed(resolvent)){
                            insertClause(resolvent);
                        addShortenedClauseTask(resolvent);}}
                negLiteralObject = negLiteralObject.nextLiteral;}}}

    /** checks if the clause is subsumed by another clause.
     *
     * @param clause a clause
     * @return true if the clause is subsumed by another clause.
     */
    boolean isSubsumed(Clause clause) {
        if(clause.size() == 2) return binaryClauseIsSubsumedByBinaryClause(clause);
        if(longerClauseIsSubsumedByBinaryClause(clause) != null) return true;
        if(longerClauseIsSubsumedByLongerClause(clause) != null) return true;
        return false;}

    /** removes all subsumed clauses and performs merge resolution with the longer clauses.
     *
     * @param clause a longer clause.
     * @throws Unsatisfiable if a contradiction is discovered.
     */
    protected void processLongerClause(Clause clause) throws Unsatisfiable {
        if(clause.size() < 3) return;
        removeClausesSubsumedByLongerClause(clause);
        mergeResolutionWithLongerClauseDirect(clause);}

    /** performs forward subsumption and merge resolution with the given longer input clause.
     *  After this, a new ProcessLongerInputClause for the next input clause is added to the queue.
     *
     * @param task    a ProcessLongerInputClause task.
     * @throws Unsatisfiable a contradiction is discovered.
     */
    protected void processLongerInputClause(Task<Simplifier.TaskType> task) throws Unsatisfiable {
        Clause clause = (Clause)task.a;
        while(clause != null) {
            if (!clause.exists || clause.size() <= 2) {
                clause = clause.nextClause;
                continue;}
            removeClausesSubsumedByLongerClause(clause);
            mergeResolutionWithLongerClauseDirect(clause);

            clause = clause.nextClause;
            while(clause != null) {
                if(clause.exists && clause.size() > 2) {
                    task.a = clause; // reuse the task
                    synchronized (this) {queue.add(task);}
                    break;}
                else {clause = clause.nextClause;}}
            break;}}

    /** performs merge resolution between a binary clause and a longer clause.
     *  atleast n p,q^n,phi<br>
     *  -p,q<br>
     *  ----------------------<br>
     *  atleast n q^n,phi<br>
     *  The resolvent is simplified further.
     *
     * @param clause   a binary clause
     * @param literal1 a literal of the binary clause
     * @param literal2 the other literal of the binary clause
     * @throws Unsatisfiable  if a contradiction is encountered.
     */
    protected void mergeResolutionWithBinaryClause(Clause clause, int literal1, int literal2) throws Unsatisfiable {
        assert(clause.size() == 2);
        boolean candidateClausesFound = false;
        Literal literalObject1 = literalIndexMore.getFirstLiteralObject(-literal1);
        while(literalObject1 != null) {
            Clause clause1 = literalObject1.clause;
            if(clause1 == null) {literalObject1 = literalObject1.nextLiteral; continue;}
            if(literalObject1.multiplicity == 1) {
                clause1.timestamp = timestamp;
                candidateClausesFound = true;}
            literalObject1 = literalObject1.nextLiteral;}

        if(!candidateClausesFound) return;
        Literal literalObject2 = literalIndexMore.getFirstLiteralObject(literal2);
        while(literalObject2 != null) {
            Clause clause2 = literalObject2.clause;
            if(clause2 == null) {literalObject2 = literalObject2.nextLiteral; continue;}
            if(clause2.timestamp == timestamp && clause2.limit == literalObject2.multiplicity) {
                String clause2String = (trackReasoning || monitoring) ? clause2.toString(symboltable,0) : null;
                ++statistics.mergedClauses;
                if(removeLiteralFromClause(clause2.findLiteral(-literal1),false)) {
                    if(trackReasoning) clause2.inferenceStep = new InfMergeResolutionMore(clause,clause2String,clause2,symboltable);
                    if(monitoring) monitor.println(monitorId,"Merge Resolution between " +
                            clause.toString(symboltable,0) + " and " + clause2String + " -> " + clause2.toString(symboltable,0));
                    if(simplifyClause(clause2,false)) addShortenedClauseTask(clause2);
                    else {removeClause(clause2,true);}}}
            literalObject2 = literalObject2.nextLiteral;}
        ++timestamp;
    }


    /** performs merge resolution between longer clauses.
     * atleast n p^n',q_1^k_1,...,q_l^k_l and<br>
     * atleast m -p^n,q_1^m,...,q_l^m, phi<br>
     * ----------------------------------<br>
     * atleast m       q_1^m,...,q_l^m, phi<br>
     * If phi is empty and n = 1 then the first clause is removed entirely.<br>
     * The shortened clause is simplified further and a new task is generated.
     *
     * @param clauseP a clause to be tested as parent clause for a merge resolution step.
     * @return        true if the clause itself has survived.
     * @throws Unsatisfiable if the simplification causes an Unsatisfiable exception.
     */
    protected boolean mergeResolutionWithLongerClauseDirect(Clause clauseP) throws Unsatisfiable {
        boolean candidateClausesFound;
        int clausePSize = clauseP.literals.size();
        int limitP = clauseP.limit;
        for(Literal literalObjectP : clauseP.literals) {
            candidateClausesFound = false;
            int literalObjectPNeg = -literalObjectP.literal;
            Literal literalObjectS = literalIndexMore.getFirstLiteralObject(literalObjectPNeg);
            while(literalObjectS != null) { // mark potential candidates with timestamp
                Clause clauseS = literalObjectS.clause;
                if(clauseS == null) {literalObjectS = literalObjectS.nextLiteral; continue;}
                if(clauseS.literals.size() >= clausePSize &&clauseS.limit >= limitP && literalObjectS.multiplicity == limitP) {
                    clauseS.timestamp = timestamp;
                    candidateClausesFound = true;}
                literalObjectS = literalObjectS.nextLiteral;}

            if(!candidateClausesFound) continue;
            int i = 0;
            for(Literal literalObjectPi : clauseP.literals) {
                if(literalObjectPi == literalObjectP) continue;
                ++i;
                Literal literalObjectSi = literalIndexMore.getFirstLiteralObject(literalObjectPi.literal);
                while(literalObjectSi != null) {
                    Clause clauseS = literalObjectSi.clause;  // this is the potential merge partner.
                    if(clauseS == null) {literalObjectSi = literalObjectSi.nextLiteral; continue;}
                    if((clauseS.timestamp -timestamp) == i-1 && literalObjectSi.multiplicity == clauseS.limit) {
                        ++clauseS.timestamp;
                        if(clauseS.timestamp - timestamp == clausePSize-1) { // mergepartner found
                            String resolventBefore = trackReasoning ? clauseS.toString(symboltable,0) : null;
                            if(clauseS.size() == clausePSize) {// both are equally long
                                ++statistics.mergedClauses;
                                if(removeLiteralFromClause(clauseS.findLiteral(literalObjectPNeg),false)) {
                                    clauseS.reduceToDisjunction();  // clauseS becomes a disjunction (implicit GCD-reduction)
                                    addShortenedClauseTask(clauseS);
                                    if(trackReasoning) {
                                        clauseS.inferenceStep =
                                                new InfMergeResolutionMore(clauseP,resolventBefore,clauseS,symboltable);}
                                    if(monitoring) {
                                        monitor.println(monitorId,
                                                clauseP.toString(symboltable,0) + " and " +
                                                        resolventBefore + " -> " + clauseS.toString(symboltable,0));}}
                                if(limitP == 1) {
                                    ++statistics.mergedClauses;
                                    removeClause(clauseP,true);
                                    timestamp += clausePSize + 1;
                                    return false;}}
                            else {
                                ++statistics.mergedClauses;
                                if(removeLiteralFromClause(clauseS.findLiteral(literalObjectPNeg),false)){
                                    if(trackReasoning) {
                                        clauseS.inferenceStep =
                                            new InfMergeResolutionMore(clauseP,resolventBefore,clauseS,symboltable);}
                                    if(monitoring) {
                                        monitor.println(monitorId, resolventBefore + " and " +
                                            clauseS.toString(symboltable,0) + " -> " + clauseS.toString(symboltable,0));}

                                    if(simplifyClause(clauseS,false)) addShortenedClauseTask(clauseS);
                                    else removeClause(clauseS,true);}}}}
                    literalObjectSi = literalObjectSi.nextLiteral;}}
            timestamp += clausePSize + 1;}
        return true;}



   
    /** creates resolvents between 3-literal clauses such that the resolvent has again 3 literals.
     *
     * @param clause        a 3-literal clause.
     * @throws Unsatisfiable if a contradiction is discovered.
     */
    void mergeResolutionPartial(Clause clause) throws Unsatisfiable{
        if(clause.size() != 3) return;
        mergeResolutionPartialBinary(clause);
        for(Literal literalObject : clause.literals) { // example: p,q,r
            int posLiteral = literalObject.literal;
            boolean found = false;
            Literal negLiteralObject = literalIndexMore.getFirstLiteralObject(-literalObject.literal);
            while(negLiteralObject != null) {          // example: -p,q',s
                if(negLiteralObject.clause.size() == 3) {negLiteralObject.clause.timestamp = timestamp; found = true;}
                negLiteralObject = negLiteralObject.nextLiteral;}
            if(!found) continue;

            for(Literal literalObject1 : clause.literals) {
                if(literalObject1 == literalObject) continue;
                Literal otherLiteralObject = literalIndexMore.getFirstLiteralObject(literalObject1.literal);
                while(otherLiteralObject != null) {                        // Example: otherLiteralObject = q
                    if(otherLiteralObject.clause.timestamp == timestamp) { // example: -p,q,s
                        resolve(literalObject,otherLiteralObject.clause.findLiteral(-posLiteral));}
                        otherLiteralObject = otherLiteralObject.nextLiteral;}}
            ++timestamp;}}

    void mergeResolutionPartialBinary(Clause clause) throws Unsatisfiable{
        if(!clause.exists || clause.size() != 3) return;
        for(Literal literalObject : clause.literals) { // example: p,q,r
            int posLiteral = literalObject.literal;
            Literal negLiteralObject = literalIndexTwo.getFirstLiteralObject(-literalObject.literal);
            while(negLiteralObject != null) {          // example: -p,s
                resolve(literalObject,negLiteralObject.clause.findLiteral(-posLiteral));
                negLiteralObject = negLiteralObject.nextLiteral;}
        }}

    /** creates a resolvent between the clauses with the two literals.
     * The resolvent is checked for binary subsumption, simplified and inserted into the internal data structures.
     * A new task is inserted into the task queue.
     *
     * @param posLiteral     a parent literal
     * @param negLiteral     a parent literal
     * @throws Unsatisfiable if a contradiction is discovered.
     */
    void resolve(Literal posLiteral, Literal negLiteral) throws Unsatisfiable {
        Clause resolvent = posLiteral.clause.resolve(nextId,posLiteral,negLiteral);
        if(resolvent == null) return;
        if(binaryClauseIsSubsumedByBinaryClause(resolvent)) return;
        if(trackReasoning) resolvent.inferenceStep =
                new InfResolution(posLiteral.clause,negLiteral.clause, resolvent, inputClauses.symboltable);
        if(simplifyClause(resolvent,false)) {
            insertClause(resolvent);
            if(monitoring) monitor.println(monitorId,
                    "Partial Resolution: " + posLiteral.clause.toString(inputClauses.symboltable,0) + " and " +
                            negLiteral.clause.toString(inputClauses.symboltable,0) + " -> " +
                    resolvent.toString(inputClauses.symboltable,0));
            addShortenedClauseTask(resolvent);}}


    /** adds a two-literal clause as ProcessBinaryClause task to the task queue.
     *
     * @param clause a two-literal clause.
     */
    protected void addBinaryClauseTask(Clause clause) {
        assert(clause.size() == 2);
        synchronized (this) {queue.add(new Task<>(TaskType.ProcessBinaryClause, clause));}}

    /** adds a two-literal clause as ProcessBinaryTriggeredMerging task to the task queue.
     *
     * @param clause a two-literal clause.
     */
    protected void addBinaryMergeTask(Clause clause) {
        assert(clause.size() == 2);
        synchronized (this) {queue.add(new Task<>(TaskType.ProcessBinaryTriggeredMerging, clause));}}

    /** adds a two-literal clause as ProcessLongerClause task to the task queue.
     *
     * @param clause a longer clause.
     */
    protected void addLongerClauseTask(Clause clause) {
        assert(clause.size() > 2);
        synchronized (this) {queue.add(new Task<>(TaskType.ProcessLongerClause, clause));}}


    /** This method replaces in all clauses the given literal by the given representative.
     * The new clauses are simplified as far as possible.<br>
     * Derived true literals are inserted into the model.
     *
     * @param representative  the representative of an equivalence class.
     * @param literal         the literal of the equivalence class.
     * @param equivalenceStep which caused the equivalence.
     * @throws Unsatisfiable         if a contradiction is encountered.
     */
    protected void processEquivalence(int representative, int literal, InferenceStep equivalenceStep) throws Unsatisfiable {
        for(int sign = 1; sign >= -1; sign -=2) {
            representative *= sign;
            literal *= sign;
            Literal literalObject = literalIndexTwo.getFirstLiteralObject(literal);
            while(literalObject != null) {  // check all two-literal clauses
                Clause  clause = literalObject.clause;
                if(clause == null) {literalObject = literalObject.nextLiteral; continue;}
                String clauseString = (trackReasoning | monitoring) ? clause.toString(symboltable,0) : null;

                if(clause.findLiteral(-representative) != null) { // new clause would be a tautology
                    ++statistics.equivalenceReplacements;
                    removeClause(clause,true);
                    literalObject = literalObject.nextLiteral; continue;}

                Literal representativeObject = clause.findLiteral(representative);
                if(representativeObject != null) { // the two literals merge into one.
                    InferenceStep step = trackReasoning ? new InfEquivalenceMerge(clause,representative, literal, equivalenceStep) : null;
                    if(monitoring) {
                        monitor.println(monitorId,"Equivalence Replacement: " +
                                clauseString + " and " + Symboltable.toString(literal,symboltable) + " = " +
                                Symboltable.toString(representative,symboltable) + " -> true(" +
                                Symboltable.toString(representative,symboltable)+ ","+
                                Symboltable.toString(literal,symboltable)+")");}
                    model.add(representative, step);
                    model.add(literal, step);
                    removeClause(clause,true);
                    return;} // true literals need no further replacements.
                else {  // the two-literal clause needs to been changed.
                    literalIndexTwo.removeLiteral(literalObject);
                    representativeObject = new Literal(representative,1);
                    representativeObject.clause = clause;
                    literalIndexTwo.addLiteral(representativeObject);
                    clause.replaceLiteral(literalObject,representativeObject);
                    ++statistics.equivalenceReplacements;
                    if(trackReasoning) {clause.inferenceStep =
                            new InfEquivalenceReplacement(clauseString,clause,representative,literal,equivalenceStep, symboltable);}

                    addBinaryClauseTask(clause);
                    addBinaryMergeTask(clause);}
                literalObject = literalObject.nextLiteral;}

            literalObject = literalIndexMore.getFirstLiteralObject(literal);  // we check the longer clauses.
            while(literalObject != null) {
                Clause  clause = literalObject.clause;
                if(clause == null) {literalObject = literalObject.nextLiteral; continue;}
                String clauseString = (trackReasoning | monitoring) ? clause.toString(symboltable,0) : null;
                Literal representativeObject = clause.findLiteral(representative);
                if(representativeObject != null) { // the two literals merge into one.
                    literalIndexMore.removeLiteral(literalObject);
                    clause.removeLiteral(literalObject,false);
                    representativeObject.multiplicity += literalObject.multiplicity;
                    clause.adjustMultiplicitiesToLimit();
                    ++statistics.equivalenceReplacements;
                    if (trackReasoning) clause.inferenceStep =
                            new InfEquivalenceReplacement(clauseString, clause, representative, literal, equivalenceStep, symboltable);
                    if(simplifyClause(clause,false)) {
                        if(clause.size() == 2) moveToIndexTwo(clause);
                        if(monitoring) monitor.print(monitorId,"\n  Clause " + clauseString + ": literal " +
                                Symboltable.toString(literal,symboltable) + " replaced by equivalent literal " +
                                Symboltable.toString(representative,symboltable) + " new clause: " + clause.toString(symboltable,0));
                        addShortenedClauseTask(clause);}
                    else removeClause(clause,true);}
                else {
                    literalIndexMore.removeLiteral(literalObject);
                    representativeObject = new Literal(representative,literalObject.multiplicity);
                    representativeObject.clause = clause;
                    literalIndexMore.addLiteral(representativeObject);
                    clause.replaceLiteral(literalObject,representativeObject);
                    ++statistics.equivalenceReplacements;
                    if (trackReasoning) clause.inferenceStep =
                            new InfEquivalenceReplacement(clauseString, clause, representative, literal, equivalenceStep, symboltable);
                    if(clause.removeComplementaryLiterals()) {
                        removeClause(clause,true);
                        literalObject = literalObject.nextLiteral; continue;}
                    if(simplifyClause(clause,false)) {
                        if(monitoring) monitor.print(monitorId,"\n  Clause " + clauseString + ": literal " +
                                Symboltable.toString(literal,symboltable) + " replaced by equivalent literal " +
                                Symboltable.toString(representative,symboltable) + " new clause: " + clause.toString(symboltable,0));
                        addShortenedClauseTask(clause);}
                    else removeClause(clause,true);}
                literalObject = literalObject.nextLiteral;}
            }
    }

    /** inserts a clause into the internal lists.
     *
     * @param clause a clause.
     */
    protected void insertClause(Clause clause) {
        Literals literalIndex = (clause.size() == 2) ? literalIndexTwo : literalIndexMore;
        for(Literal literalObject : clause.literals) literalIndex.addLiteral(literalObject);
        clauses.addClause(clause);}


    /** removes the clause from the internal lists.
     *
     * @param clause  a clause to be removed.
     * @param checkPurity if true then the clause's literals are checked for purity.
     * @throws Unsatisfiable should not happen.
     */
    protected void removeClause(Clause clause, boolean checkPurity) throws Unsatisfiable {
        clauses.removeClause(clause);
        Literals literalIndex = (clause.size() == 2) ? literalIndexTwo :literalIndexMore;
        for(Literal literalObject : clause.literals) {
            literalIndex.removeLiteral(literalObject);
            if(checkPurity) {checkPurity(literalObject.literal); checkPurity(-literalObject.literal);}}}



    /** removes the Literal from the corresponding index.
     *
     * @param literalObject a Literal
     */
    protected void removeLiteralFromIndex(Literal literalObject) {
        Clause clause = literalObject.clause;
        Literals literalIndex = (clause.size() == 2) ? literalIndexTwo : literalIndexMore;
        literalIndex.removeLiteral(literalObject);}



    /** removes the literal from the clause and from the corresponding index.
     * If the literal becomes pure, it is inserted into the model.<br>
     * If the limit is reduced to 0, the entire clause is removed.<br>
     * If the clause becomes a two-literal clause, it is moved to the two-literal index.
     *
     * @param literalObject the literal object to be removed.
     * @param reduceLimit if true then the clause's limit is reduced by the literal's multiplicity.
     * @return true if the clause still exists.
     * @throws Unsatisfiable if inserting a pure literal into the model causes a contradiction.
     */
    protected boolean removeLiteralFromClause(Literal literalObject, boolean reduceLimit) throws Unsatisfiable {
        Clause clause = literalObject.clause;
        removeLiteralFromIndex(literalObject);
        if(clause.removeLiteral(literalObject,reduceLimit)){
            if(clause.size() == 2) {moveToIndexTwo(clause);}
            checkPurity(literalObject.literal); // pure literals are just added to the model.
            return true;}
        removeClause(clause,true);
        return false;}

    /** used in simplifyClause */
    private final ArrayList<Literal> removedLiterals = new ArrayList<>(5);

    /** simplifies an atleast-clause with multiplicities &gt; 1.
     * 1. True literals are extracted.<br>
     * Example: atleast 5 p^2,q^2,r,s -&gt; true(p,q) and r,s (disjunction).<br>
     * 2. Clause is reduced to essential literals.<br>
     * Example: atleast 2 p^2,q^2,r -&gt; p,q (disjunction).<br>
     * 3. Numbers are divided by the greatest common divisor (GCD). <br>
     * Example: atleast 6 p^4,q^4,r^4 -> atleast 3 p^2,q^2,r^2.
     *
     * @param clause         the clause to be simplified.
     * @param isNewClause  if false then the removed literals are removed from the index and checked for purity.
     * @return               true if the clause still exists.
     * @throws Unsatisfiable        if the model finds a contradiction.
     */
    protected boolean simplifyClause(Clause clause, boolean isNewClause) throws Unsatisfiable {
        if(clause.isDisjunction || !clause.hasMultiplicities) return true; // nothing to be simplified.
        String clauseBefore = (trackReasoning || monitoring) ? clause.toString(symboltable,0) : null;
        boolean reducedByGCD;
        int sizeBefore = clause.size();
        removedLiterals.clear();
        try {
            ArrayList<Literal> trueLiterals = clause.reduceByTrueLiterals(removedLiterals);
            if (trueLiterals != null) {
                if(monitoring) {
                    String literalNames = "";
                    for (Literal literalObject : trueLiterals) {
                        literalNames += Symboltable.toString(literalObject.literal, symboltable) + ",";}
                    String newClause = (clause.limit > 0) ? ". new clause: " + clause.toString(symboltable,0) : "";
                    monitor.println(monitorId, "True literals " + literalNames + " extracted from clause " +
                            clauseBefore + newClause);}

                for (Literal literalObject : trueLiterals) {
                    int literal = literalObject.literal;
                    ++statistics.derivedUnitClauses;
                    model.add(literal, trackReasoning ? new InfTrueLiteral(clauseBefore, clause.id, literal, clause.inferenceStep) : null);}

                if(clause.limit <= 0) return false;}

            if (clause.isDisjunction || !clause.hasMultiplicities) return true; // nothing to be simplified.
            if (clause.reduceToEssentialLiterals(removedLiterals)) {
                if (monitoring && clause.limit > 0)
                    monitor.println(monitorId, "Clause  " + clauseBefore + " reduced to essential literals:  " +
                            clause.toString(symboltable, 0));
                if (clause.limit <= 0) return false;
                if (clause.size() == 1) {
                    int literal = clause.literals.get(0).literal;
                    model.add(literal, clause.inferenceStep);
                    return false;
                }
                if (clause.limit <= 1) return clause.limit == 1;}

            reducedByGCD = clause.divideByGCD();
            if (reducedByGCD && monitoring) {
                monitor.println(monitorId, "Clause " + clauseBefore + " divided by gcd  to " +
                        clause.toString(symboltable, 0));}
            if (clause.size() == 1) {
                model.add(clause.literals.get(0).literal, new InfInputClause(clause.id));
                return false;}}
        finally {
            if(!isNewClause) {
                Literals literalIndex = (sizeBefore == 2) ? literalIndexTwo : literalIndexMore;
                for(Literal literalObject : removedLiterals) literalIndex.removeLiteral(literalObject);
                if(sizeBefore > 2 && clause.size() == 2) moveToIndexTwo(clause);
                for(Literal literalObject : removedLiterals) checkPurity(literalObject.literal);}}
        if(reducedByGCD) return simplifyClause(clause,isNewClause);
        return true;}

    /** removes all clauses from the internal datastructures.
     * This is mainly for testing purposes.
     */
    public void clear() {
        if(inputClauses != null) inputClauses.clear();
        timestamp = 1;
        literalIndexTwo.clear();
        literalIndexMore.clear();
        clauses.clear();
        model.clear();
        statistics.clear();}

    /** reads the input clauses and processes the tasks in the task queue.
     * The simplifier tries to derive unit clause, pure literals and equivalences.
     *
     * @return null or a result (unsatisfiable or satisfiable)
     */
    @Override
    public Result solveProblem(ProblemSupervisor problemSupervisor) {
        long startTime = System.nanoTime();
        model                  = problemSupervisor.model;
        inputClauses           = problemSupervisor.inputClauses;
        predicates             = inputClauses.predicates;
        monitor                = problemSupervisor.monitor;
        monitoring             = monitor != null;
        monitorId              = "Simplifier";
        problemId              = problemSupervisor.problemId;
        equivalenceClasses     = problemSupervisor.equivalenceClasses;
        literalIndexTwo        = new Literals(predicates);
        literalIndexMore       = new Literals(predicates);
        clauses                = new Clauses();
        statistics             = new SimplifierStatistics(solverId);
        trackReasoning         = problemSupervisor.globalParameters.trackReasoning;
        nextId                 = problemSupervisor::nextClauseId;
        equivalenceClasses     = problemSupervisor.equivalenceClasses;
        try{
            readInputClauses();
            processTasks(0);}
        catch(Result result) {
            result.statistic = statistics;
            result.solverId = "Simplifier";
            result.problemId = problemId;
            result.startTime = startTime;
            return result;}
        return null;}

    /** returns the statistics.
     *
     * @return the statistics.
     */
    @Override
    public Statistic getStatistics() {
        return statistics;}
}
