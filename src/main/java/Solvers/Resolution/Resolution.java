package Solvers.Resolution;

import Datastructures.Results.Result;
import Datastructures.Results.Satisfiable;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Statistics.Statistic;
import Datastructures.Symboltable;
import Datastructures.Task;
import Datastructures.Theory.Model;
import InferenceSteps.InfInputClause;
import InferenceSteps.InferenceStep;
import Management.Monitor.Monitor;
import Management.ProblemSupervisor;
import Solvers.Normalizer.Normalizer;
import Solvers.Solver;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.concurrent.PriorityBlockingQueue;
import java.util.function.IntConsumer;
import java.util.function.IntSupplier;

/** This is an incomplete solver which tries to simplify the clauses as far as possible.
 *  - It tries to derive new true literals and send them to the model.<br>
 *  - It tries to derive new equivalences and send them to the equivalenceClasses.<br>
 *  <br>
 *  The following operations are preformed:<br>
 *  - all clauses are transformed to atleast normal form.<br>
 *  - the clause set is partially saturated by resolution in such a way that the clause length does not increase.<br>
 *  - subsumed clauses are removed. <br>
 *  - merge resolution between two clause, reduces the clause length.<br>
 *  - New true literals in the model are incorporated.<br>
 *  - New equivalences form the equivalenceClasses are incorporated.<br>
 *  - pure literals are sent to the model.<br>
 *  - after saturation partially pure literals are made true.<br>
 *  - when there are only 2-literal clauses left (after saturation)
 *    the model is completed by choosing the first literal as true literal.
 */

public class Resolution extends Solver {

    /** provides the normalized clauses */
    Normalizer normalizer;

    /** The id of the current problem to be solved */
    private String problemId;

    public final String solverId = "Resolution";

    /** for distinguishing the monitoring areas */
    private String monitorId;

    /** the simplifier's statistics */
    protected ResolutionStatistics statistics;

    /** contains a list of literalObjects for each literal in two-literal clauses.*/
    protected Literals literalIndexTwo;

    /** contains a list of literalObjects for each literal in longer clauses.*/
    protected Literals literalIndexMore;

    /** the list of clauses. */
    protected Clauses clauses;

    /** a timestamp to be used by inference algorithms. */
    private int timestamp = 1;

    /** a timestamp to be used by subsumption algorithms */
    private int timestampSubsumption = 1;

    /** is used by the clause.removeComplementaryLiterals to increment the statistics.*/
    private final IntConsumer addComplementaries = (n -> statistics.complementaryLiterals += n);

    /** controls if the consistency is to be checked (for testing purposes) */
    private final boolean checkConsistency = true;

    /** the thread which runs the simplifier. */
    private Thread myThread;

    /** collects the equivalences for later updating the model.
     * Entries: representative_1, literal_1, ...*/
    IntArrayList equivalences = new IntArrayList();
    /** collects the corresponding inference steps.
     * Entries: InfEquivalence,null,InfEquivalence,null,...*/
    ArrayList<InfEquivalence> equivalenceSteps = new ArrayList<>();

    /** The local model may contain true literals derived from pure literals.
     * These need not be true in all models, and therefore cannot be sent to the global model. */
    private byte[] localModel;

    /** just creates a single Resolution solver,.
     *
     * @param parameters not used.
     * @param solvers    to add the new simplifier.
     * @param errors     not used.
     * @param warnings   not used.
     */
    public static void makeSolvers(HashMap<String,String> parameters, ArrayList<Solver> solvers,
                                   StringBuilder errors, StringBuilder warnings) {
        HashMap<String,Object> solverParameters = new HashMap<>();
        solverParameters.put("name", "Resolution");
        solvers.add(new Resolution(1,solverParameters));
    }

    /** constructs a new Simplifier.
     */
    public Resolution(int solverNumber, HashMap<String,Object> solverParameters) {
        super(solverNumber,solverParameters);}

    /** this is a constructor for testing purposes (without ProblemSupervisor)
     *
     * @param predicates      the number of predicates.
     * @param monitor         null or a monitor.
     * @param trackReasoning  true if the reasoning is to be tracked.
     * @param nextId          for generating a new identifier for a clause.
     */
    public Resolution(int predicates, Monitor monitor, boolean trackReasoning, IntSupplier nextId) {
        this.predicates = predicates;
        this.monitor = monitor;
        this.nextId = nextId;
        monitoring = monitor != null;
        this.monitorId = "Resolution";
        this.trackReasoning = trackReasoning;
        literalIndexTwo = new Literals("TWO",predicates);
        literalIndexMore = new Literals("MORE",predicates);
        localModel = new byte[predicates+1];
        clauses = new Clauses();
        model = new Model(predicates);
        statistics = new ResolutionStatistics(solverId);
        myThread = Thread.currentThread();
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
        /** for a clause, backward subsumption, merge resolution and saturation is done. */
        ProcessClauseFirstTime,
        /** partial merge resolution between 3-literal clauses */
        ProcessMergeResolutionPartial,
        /** pure literal elmination and elimination of predicates */
        ProcessElimination
    }

    /** gets the priority for the objects in the queue.
     * <br>
     * The smaller the priority the earlier the task is executed.
     *
     * @param task the task in the queue
     * @return the priority of the objects in the queue.
     */
    private int getPriority(Task<Resolution.TaskType> task) {
        switch(task.taskType) {
            case ProcessTrueLiteral: return Math.abs((Integer)task.a);
            case ProcessEquivalence: return predicates + (Math.abs((Integer)task.a)) + 1; // this guarantees a deterministic sequence of the tasks
            case ProcessBinaryClause:           return 2*predicates + 100;
            case ProcessLongerClause:           return 2*predicates + 101;
            case ProcessClauseFirstTime:        return 2*predicates + 102;
            case ProcessMergeResolutionPartial: return 2*predicates + 103;
            case ProcessElimination:            return 2*predicates + 104;}
        return 0;}


    /** A queue of newly derived unit literals and binary equivalences.
     * The unit literals are automatically put at the beginning of the queue.
     */
    private final PriorityBlockingQueue<Task<Resolution.TaskType>> queue =
            new PriorityBlockingQueue<>(10, Comparator.comparingInt(this::getPriority));

    /** Installs the observer in the model.<br>
     * The method is called before the threads are started.
     * */
    public void initialize(Thread myThread, ProblemSupervisor problemSupervisor) {
        this.problemSupervisor = problemSupervisor;
        this.myThread = myThread;
        problemSupervisor.model.addObserver(myThread, this::addExternalTrueLiteralTask);}

    /** reads the input clauses and processes the tasks in the task queue.
     * <br>
     * The resolution simplifier tries to derive unit clause, pure literals and equivalences.
     *
     * @return null or a result (unsatisfiable or satisfiable)
     */
    @Override
    public Result solveProblem() {
        super.initialize(Thread.currentThread(),problemSupervisor);
        monitorId              = "Resolution";
        statistics             = new ResolutionStatistics(solverId);
        makeReusable();
        synchronized (this) {
            for(int literal: model.model) {
                addExternalTrueLiteralTask(literal,model.getInferenceStep(literal));}}
        try{
            readInputClauses();
            synchronized (this) {queue.add(new Task(TaskType.ProcessElimination,null));}
            processTasks(0);}
        catch(Result result) {
            System.out.println("Result " + result.getClass().getName());
            if(result instanceof Satisfiable) {
                completeModel();
                completeModelForEquivalences((byte) 1);
                model.exchangeModel(localModel);}
            result.statistic = statistics;
            result.solverId = solverId;
            result.problemId = problemId;
            result.startTime = startTime;
            System.out.println(statistics);
            return result;}
        return null;}

    /** The solver may be reused for more than one problem.
     * <br>
     * The method initializes internal datastructures and updates them for reusing them.
     */
    public void makeReusable() {
        timestamp = 1;
        timestampSubsumption = 1;
        statistics.clear();
        queue.clear();
        equivalences.clear();
        if(literalIndexTwo == null)  literalIndexTwo = new Literals("TWO",predicates);
        else literalIndexTwo.clear(predicates);
        if(literalIndexMore == null) literalIndexMore = new Literals("MORE",predicates);
        else literalIndexMore.clear(predicates);
        if(clauses == null) clauses = new Clauses(); else clauses.clear();
        if(localModel == null) localModel = new byte[predicates+1];
        else if(localModel.length <= predicates) localModel = new byte[predicates+1];
        else for(int predicate = 1; predicate <= predicates; ++predicate) localModel[predicate] = 0;
    }

    /** removes all clauses from the internal datastructures.
     * <br>
     * To be used only for testing purposes.
     */
    public void clear() {
        timestamp = 1;
        timestampSubsumption = 1;
        literalIndexTwo.clear(predicates);
        literalIndexMore.clear(predicates);
        clauses.clear();
        statistics.clear();
        queue.clear();
        model.clear();
        for(int predicate = 1; predicate <= predicates; ++predicate) localModel[predicate] = 0;
    }

    /** returns the local truth-status of the literal: +1 = true, -1 = false, 0 = undefined. */
    byte localStatus(int literal) {
        return (literal > 0) ? localModel[literal] : (byte)-localModel[-literal];}

    /** sets the local truth status of the literal
     *
     * @param literal a literal which is expected to be true.
     */
    void makeLocallyTrue(int literal) {
        if(literal > 0) localModel[literal] = 1; else localModel[-literal] = -1;}

    private int[] clauseId = new int[]{0};

    IntSupplier nextId = () -> ++clauseId[0];

    /** reads the disjunctions, the atleast, atmost, exactly and interval clauses from the normalizer and transfroms them to Clauses.
     * 
     * @throws Result if a contradiction or the empty clause is derived.
     */
    public void readInputClauses() throws Result {
        ArrayList<IntArrayList> normalizedClauses = problemSupervisor.normalizer.clauses;
        clauseId[0] = normalizedClauses.get(normalizedClauses.size()-1).getInt(0);
        Clause clause; int subIdentifier = 0;
        for(IntArrayList normalizedClause : problemSupervisor.normalizer.clauses) {
            switch(Normalizer.getQuantifier(normalizedClause)) {
                case OR:
                case ATLEAST: insertClause(clause = new Clause(normalizedClause,0));
                    if(clause.size() == 2) addBinaryClauseTask(clause); break;
                case ATMOST:  insertClause(clause = new Clause(Normalizer.atmost2Atleast(normalizedClause),0));
                    if(clause.size() == 2) addBinaryClauseTask(clause); break;
                case EXACTLY: subIdentifier = 0;
                    for(IntArrayList atleastClause : Normalizer.exactlyToAtleast(normalizedClause)) {
                    insertClause(clause = new Clause(atleastClause,++subIdentifier));
                    if(clause.size() == 2) addBinaryClauseTask(clause);} break;
                case INTERVAL: subIdentifier = 0;
                    for(IntArrayList atleastClause : Normalizer.intervalToAtleast(normalizedClause)) {
                        insertClause(clause = new Clause(atleastClause,++subIdentifier));
                        if(clause.size() == 2) addBinaryClauseTask(clause);}}}
        if(checkConsistency) checkConsistency();
        statistics.initialClauses = clauses.size;
        if(clauses.isEmpty()) throw new Satisfiable(problemId,solverId, model);
        if(printClauses) {
            System.out.println("INPUT CLAUSES: " + literalIndexMore.size());
            if(monitoring) printSeparated();}
        synchronized(this){queue.add(new Task<>(TaskType.ProcessClauseFirstTime,clauses.firstClause));}}

    /** counts the number of true literal processes in the queue */
    private int trueLiteralsInQueue = 0;

    private int binaryClausesInQueue = 0;

    /** adds a true literal to the queue and to the model.
     * <br>
     *  Derived literals (as unit clauses) are added to the local and to the global model.<br>
     *  Pure literals are only added to the local model.<br>
     *
     * @param literal a true literal
     * @param globallyTrue if true then the literal is derived as true literal.
     * @param inferenceStep which caused the truth
     */
    public void addInternalTrueLiteralTask(int literal, boolean globallyTrue, InferenceStep inferenceStep) throws Unsatisfiable {
       if(monitoring) {
            monitor.print(monitorId,"True literal added: " +
                    Symboltable.toString(literal,symboltable));}
        makeLocallyTrue(literal);
        synchronized (this) {
            queue.add(new Task<>(TaskType.ProcessTrueLiteral, literal, inferenceStep));
            ++trueLiteralsInQueue;}
        if(globallyTrue) model.add(myThread,literal,inferenceStep);}


    /** adds a true literal to the queue. It is called by other threads for globally true literals.
     * <br>
     * If the queue is empty a new ProcessElimination task is added to the queue as well.
     *
     * @param literal a true literal.
     * @param inferenceStep which caused the truth.
     */
    public void addExternalTrueLiteralTask(int literal, InferenceStep inferenceStep) {
        if(monitoring) {
            monitor.print(monitorId,"In: True literal from model " +
                    Symboltable.toString(literal,symboltable));}
        if(trackReasoning && inferenceStep == null) inferenceStep = new InfExternal(literal);
        makeLocallyTrue(literal);
        synchronized (this) {
            if(queue.isEmpty()) queue.add(new Task<>(TaskType.ProcessElimination,null));
            queue.add(new Task<>(TaskType.ProcessTrueLiteral, literal, inferenceStep));
            ++trueLiteralsInQueue;}}


    /** controls that all clauses are printed after each task has been changed something (for testing purposes).*/
    private final boolean printClauses = true;

    /** reads the next task from the task queue and processes it.
     *
     * @param n 0 or the maximum number of task before stopping the loop (0: unlimited) (> 0 for test purposes).
     * @throws Result if a contradiction is encountered or the clause set became empty.
     */
    public void processTasks(int n) throws Result {
        Task<Resolution.TaskType> task;
        int counter = 0;
        Clause clause;
        while(!myThread.isInterrupted()) {
            try {
                if(monitoring) {monitor.println(monitorId,"Queue is waiting\n" + Task.queueToString(queue));}
                task = queue.take(); // waits if the queue is empty
                boolean changed = false;
                switch(task.taskType){
                    case ProcessTrueLiteral:
                        if(monitoring) {monitor.println(monitorId,"Next Task: " + task);}
                        changed = true;
                        --trueLiteralsInQueue;
                        processTrueLiteral((Integer)task.a, (InferenceStep)task.b);
                        break;
                    case ProcessEquivalence:
                        if(monitoring) {monitor.println(monitorId,"Next Task: " + task);}
                        changed = true;
                        processEquivalence((Integer)task.a,(Integer)task.b,(InferenceStep) task.c);
                        break;
                    case ProcessBinaryClause:
                        clause = (Clause)task.a;
                        if(clause.exists) {
                            if(monitoring) {monitor.println(monitorId,"Next Task: " + task);}
                            changed = true;
                            processBinaryClause(clause);}
                        --binaryClausesInQueue;
                        break;
                    case ProcessLongerClause:
                        clause = (Clause)task.a;
                        if(clause.exists) {
                            if(monitoring) {monitor.println(monitorId,"Next Task: " + task);}
                            changed = true;
                            processLongerClause(clause);}
                        break;
                    case ProcessClauseFirstTime:
                        if(((Clause)task.a).exists) {
                            if(monitoring) {monitor.println(monitorId,"Next Task: " + task);}
                            changed = true;
                            processClauseFirstTime(task);}
                        break;
                    case ProcessElimination:
                        processElimination(task);
                        break;}
                if(clauses.isEmpty()) {
                    completeModel();
                    completeModelForEquivalences((byte) 1);
                    throw new Satisfiable(problemId,solverId,model);}
                if(trueLiteralsInQueue == 0 && clauses.status != 0) generatePositiveOrNegativeModel();
                if(queue.isEmpty()) {
                    System.out.println("Empty Queue " + clauses.size + " " + model.toString());
                    //throw new Aborted(problemId,solverId,"Empty Queue");
                }
                if(monitoring  && printClauses && changed) {
                    System.out.println("Model: " + model.toString());
                    printSeparated();}
                if(trueLiteralsInQueue == 0 && binaryClausesInQueue == 0 && literalIndexMore.size() == 0) {
                    if(monitoring) {
                        monitor.println(monitorId, "No longer clauses any more. 2-Literal Clauses are Saturated.\n" );
                        printSeparated();}
                    addInternalTrueLiteralTask(clauses.firstClause.literals.get(0).literal,false,null);}

                }
            catch(InterruptedException ex) {
                ex.printStackTrace();
                return;}
            if(n > 0 && ++counter == n){
                System.out.println("Stopped " + counter);
                return;}}}


    /** performs forward subsumption, merge resolution and saturation with the given clause.
     * <br>
     *  After this, a new ProcessClauseFirstTime for the next clause in the clauses list is added to the queue.<br>
     *  The task is reused for this purpose.<br>
     *  Since derived clauses are put at the end of the clauses list, all clauses will eventually be processed the first time.
     *
     * @param task    a ProcessClauseFirstTime task.
     * @throws Unsatisfiable a contradiction is discovered.
     */
    void processClauseFirstTime(Task<Resolution.TaskType> task) throws Unsatisfiable {
        Clause clause = (Clause)task.a;
        while(clause != null) { // if the clause we removed, we look for the next clause which is not removed.
            if(!clause.exists) {clause = clause.nextClause; continue;}
            break;}
        if(clause == null) return;
        int size = clause.size();
        assert size >= 2;
        if(size == 2) processBinaryClause(clause);
        else          processLongerClause(clause);
        clause = clause.nextClause;
        while(clause != null) {
            if(!clause.exists) {clause = clause.nextClause; continue;}
            break;}
        if(clause != null) {
            task.a = clause;
            synchronized (this) {queue.add(task);}}}

    /** processes the binary clause.
     * <br>
     * - if the clause is subsumed, it is removed.<br>
     * - clauses subsumed by the binary clause are removed. <br>
     * - merge resolution with the binary clause is performed.<br>
     * - equivalences are detected. <br>
     * - all resolvents with the binary clauses are generated.<br>
     * - saturation with the binary clause is performed.
     *
     * @param clause         a two-literal clause
     * @throws Unsatisfiable if a contradiction is discovered.
     */
    protected void processBinaryClause(Clause clause) throws Unsatisfiable {
        assert(clause.size() == 2);
        if(binaryClauseIsSubsumed(clause) != null) {
            removeClause(clause,true);
            return;}
        removeBinaryClausesSubsumedByBinaryClause(clause);
        removeLongerClausesSubsumedByBinaryClause(clause);
        int literal1 = clause.literals.get(0).literal;
        int literal2 = clause.literals.get(1).literal;
        binaryMergeResolutionAndEquivalence(clause,literal1,literal2,true);
        if(!clause.exists) return;
        binaryMergeResolutionAndEquivalence(clause,literal2,literal1,false);
        if(!clause.exists) return;
        saturateBinaryClausesWithBinaryClause(clause);
        if(!clause.exists) return;
        saturateLongerClausesWithBinaryClause(clause);}

    /** processes the longer clause.
     * - if the clause is subsumed, it is removed.<br>
     * - other clauses subsumed by the clause are removed. <br>
     * - merge resolution with the clause is performed.<br>
     * - all resolvents with the binary clauses are generated.<br>
     *
     * @param clause a longer clause.
     * @throws Unsatisfiable if a contradiction is discovered.
     */
    protected void processLongerClause(Clause clause) throws Unsatisfiable {
        if(clause.size() < 3) {
            processBinaryClause(clause);
            return;}
        if((longerClauseIsSubsumedByBinaryClause(clause) != null) ||
           (longerClauseIsSubsumedByLongerClause(clause) != null)) {
            removeClause(clause,true);
            return;}
        removeClausesSubsumedByLongerClause(clause);
        if(!mergeResolutionWithLongerClause(clause)) return;
        saturateBinaryClausesWithLongerClause(clause);
        if(clause.exists && clause.size()==3) mergeResolutionPartial(clause);
    }

    /** The method applies a true literal to the clauses and the equivalences.
     * <br>
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
     *  <br>
     *  For an equivalence p = q where one of the literals is true, the other literal is also made true.
     *
     * @param oldTrueLiteral a true literal.
     * @throws Unsatisfiable if a contradiction is found.
     *  */
    protected void processTrueLiteral(int oldTrueLiteral, InferenceStep inferenceStep) throws Unsatisfiable {
        processTrueLiteralTwo(oldTrueLiteral,inferenceStep);
        processTrueLiteralMore(oldTrueLiteral);
        int oldTruePredicate = Math.abs(oldTrueLiteral);
        for(int i = 0; i < equivalences.size(); i += 2) {
            int newTrueLiteral = 0;
            if(equivalences.getInt(i) == oldTruePredicate) {
                newTrueLiteral = equivalences.getInt(i+1);
                if(oldTrueLiteral < 0) newTrueLiteral *= -1;}
            else {
                int literal = equivalences.getInt(i+1);
                if(Math.abs(literal) == oldTruePredicate) {
                    newTrueLiteral = equivalences.getInt(i);
                    if(oldTrueLiteral != literal) newTrueLiteral *= -1;}}
            if(newTrueLiteral != 0) {
                if(monitoring) monitor.println(monitorId, "Equivalent literal " +
                        Symboltable.toString(newTrueLiteral,symboltable) + " added to model.");
                makeLocallyTrue(newTrueLiteral);
                if(trackReasoning) {inferenceStep = new InfEquivalentTruth(newTrueLiteral,oldTrueLiteral,equivalenceSteps.get(i),inferenceStep);}
                model.add(myThread,newTrueLiteral,inferenceStep);
                ++statistics.derivedTrueLiterals;}}}

    /** applies a true literal to all two-literal clauses containing this literal.
     * <br>
     * Clauses containing this literal are removed.<br>
     * Clauses containing -literal yield a new true literal, which is put into the model.<br>
     * The clause is removed as well.
     *
     * @param oldTrueLiteral a true literal.
     * @throws Unsatisfiable if a contradiction is encountered.
     */
    protected void processTrueLiteralTwo(int oldTrueLiteral, InferenceStep inferenceStep) throws Unsatisfiable{
        Literal literalObject = literalIndexTwo.getFirstLiteralObject(oldTrueLiteral);
        while(literalObject != null) { // remove all binary clauses containing the literal.
            Clause clause = literalObject.clause;
            if(clause == null) {literalObject = literalObject.nextLiteral; continue;}
            removeClause(clause,true); // literals may become pure.
            literalObject = literalObject.nextLiteral;}

        literalObject = literalIndexTwo.getFirstLiteralObject(-oldTrueLiteral);
        while(literalObject != null) { // unit resolution between literal and all binary clauses -literal,p.
            Clause clause = literalObject.clause;
            if(clause == null) {literalObject = literalObject.nextLiteral; continue;}
            int otherLiteral = clause.otherLiteral(literalObject).literal;
            if(monitoring) monitor.println(monitorId,clause.toString(symboltable,0) + " and true(" +
                    Symboltable.toString(oldTrueLiteral,symboltable) + ") -> " +
                    "true("+Symboltable.toString(otherLiteral,symboltable)+")");
            addInternalTrueLiteralTask(otherLiteral, model.isTrue(oldTrueLiteral),
                    trackReasoning ? new InfUnitResolutionTwo(clause,oldTrueLiteral,inferenceStep,otherLiteral) : null);
            ++statistics.derivedTrueLiterals;
            removeClause(clause,true);
            literalObject = literalObject.nextLiteral;}

        literalIndexTwo.removePredicate(oldTrueLiteral);}

    /** applies a true literal to all longer clauses containing this literal.
     * <br>
     * All literals with a truth value in the model are removed.<br>
     * Clauses which become true in this step are entirely removed.<br>
     * The empty clause causes an UnsatEmptyClause exception to be thrown.<br>
     * Derived unit clauses are put into the model <br>
     * Shortened clauses cause new tasks to be inserted into the task queue.<br>

     * @param oldTrueLiteral a true (or false) literal.
     * @throws Unsatisfiable if a contradiction is encountered.
     */
    void processTrueLiteralMore(int oldTrueLiteral) throws Unsatisfiable{
        IntArrayList removedLiterals = new IntArrayList();
        for(int sign = 1; sign >= -1; sign -= 2) {
            oldTrueLiteral *= sign;   // the clauses containing the predicate are selected. The literals to be removed depend on the model.
            Literal literalObject = literalIndexMore.getFirstLiteralObject(oldTrueLiteral);
            while(literalObject != null) {
                 Clause clause = literalObject.clause;
                 if(clause == null) {literalObject = literalObject.nextLiteral; continue;}
                 String clauseBefore = (trackReasoning || monitoring) ? clause.toString(symboltable,0) : null;
                 removedLiterals.clear();
                 byte status = clause.removeLiterals(litObject->(int)localStatus(litObject.literal),
                         litObject -> {literalIndexMore.removeLiteral(litObject);
                                       removedLiterals.add(localStatus(litObject.literal)*litObject.literal);},null);
                 if(status == 1) {removeClause(clause,true, literalIndexMore); continue;}
                 if(monitoring) monitor.println(monitorId, clauseBefore + " and true" + removedLiterals +
                         " -> " + clause.toString(symboltable,0));
                 InferenceStep step = null;
                 if(trackReasoning) {
                     ArrayList<InferenceStep> steps = new ArrayList<>();
                     steps.add(clause.inferenceStep);
                     for(int literal : removedLiterals) {
                         step = model.getInferenceStep(literal);
                         if(step == null) step = new InfExternal(literal);
                         steps.add(step);}
                     step = new InfTrueLiterals(clauseBefore, clause.toString(symboltable,0), removedLiterals,steps);
                    clause.inferenceStep = step;}
                 if(status == -1) {throw new UnsatEmptyClause(problemId,solverId,clause.identifier(), step);}
                switch(clause.size()) {
                     case 1: ++statistics.derivedTrueLiterals;
                            addInternalTrueLiteralTask(clause.literals.get(0).literal,model.status(oldTrueLiteral) != 0,step);
                             removeClause(clause,false);
                             break;
                     case 2: moveToIndexTwo(clause); addBinaryClauseTask(clause); break;
                     default: synchronized (this) {queue.add(new Task(TaskType.ProcessLongerClause,clause));}}
                if(checkConsistency) checkConsistency();
            literalObject = literalObject.nextLiteral;}}
        literalIndexMore.removePredicate(oldTrueLiteral);
    }

    /** moves a longer clause which has become a binary clause to the literalIndexTwo.
     * <br>
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
    protected void addDerivedClauseTask(Clause clause) {
        if(clause.size() == 2) addBinaryClauseTask(clause);
        else                   addLongerClauseTask(clause);}



    /** removes all binary clauses which are subsumed by a binary subsumer.
     *
     * @param subsumer a binary clause.
     * @throws Unsatisfiable if a contradiction is found (should not happen)
     */
    protected void removeBinaryClausesSubsumedByBinaryClause(Clause subsumer) throws Unsatisfiable {
        assert(subsumer.size() == 2);
        int literal1 = subsumer.literals.get(0).literal;
        int literal2 = subsumer.literals.get(1).literal;
        Literal subsumeeLiteral = literalIndexTwo.getFirstLiteralObject(literal1);
        while(subsumeeLiteral != null) {
            Clause subsumee = subsumeeLiteral.clause;
            if(subsumee == null || subsumee == subsumer) {subsumeeLiteral = subsumeeLiteral.nextLiteral; continue;}
            if(subsumee.otherLiteral(subsumeeLiteral).literal == literal2) {
                removeClause(subsumee,true);}
            subsumeeLiteral = subsumeeLiteral.nextLiteral;}}


    /** removes all longer disjunctions which are subsumed by a binary subsumer.
     *
     * @param subsumer a binary clause.
     * @throws Unsatisfiable should not happen.
     */
    protected void removeLongerClausesSubsumedByBinaryClause(Clause subsumer) throws Unsatisfiable {
        assert(subsumer.size() == 2);
        Literal subsumeeLiteral = literalIndexMore.getFirstLiteralObject(subsumer.literals.get(0).literal);
        while(subsumeeLiteral != null) {
            Clause subsumee = subsumeeLiteral.clause;
            if(subsumee == null) {subsumeeLiteral = subsumeeLiteral.nextLiteral; continue;}
            if(subsumee.exists && subsumee.isDisjunction && subsumee != subsumer) subsumee.timestamp1 = timestamp;
            subsumeeLiteral = subsumeeLiteral.nextLiteral;}

        subsumeeLiteral = literalIndexMore.getFirstLiteralObject(subsumer.literals.get(1).literal);
        while(subsumeeLiteral != null) {
            Clause subsumee = subsumeeLiteral.clause;
            if(subsumee == null) {subsumeeLiteral = subsumeeLiteral.nextLiteral; continue;}
            if(subsumee.timestamp1 == timestamp) {
                removeClause(subsumee,true);
                ++statistics.subsumedClauses;}
            subsumeeLiteral = subsumeeLiteral.nextLiteral;}
        ++timestamp;}

    /** removes all clauses subsumed by the given clause.
     * <br>
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
            if(subsumee == null) {subsumeeLiteral = subsumeeLiteral.nextLiteral; continue;}
            if(subsumee != subsumer && subsumee.limit <= sumsumerLimit &&
                    subsumee.literals.size() >= subsumerSize &&
                    subsumeeLiteral.multiplicity >= subsumerLiteral.multiplicity) {
                candidatesFound = true;
                subsumee.timestamp1 = timestamp;}
            subsumeeLiteral = subsumeeLiteral.nextLiteral;}

        if(!candidatesFound) return;
        for(int i = 1; i < subsumer.literals.size(); ++i) { // find candidates.
            subsumerLiteral = subsumer.literals.get(i);
            subsumeeLiteral = literalIndexMore.getFirstLiteralObject(subsumerLiteral.literal);
            while(subsumeeLiteral != null) {
                Clause subsumee = subsumeeLiteral.clause;
                if(subsumee == null) {subsumeeLiteral = subsumeeLiteral.nextLiteral; continue;}
                if(subsumee.exists && (subsumee.timestamp1 - timestamp) == i - 1&&
                        subsumeeLiteral.multiplicity >= subsumerLiteral.multiplicity) {
                    ++subsumee.timestamp1;
                    if(subsumee.timestamp1 - timestamp == subsumerSize-1){
                        removeClause(subsumee,true);}}
                subsumeeLiteral = subsumeeLiteral.nextLiteral;}}
        timestamp += subsumerSize + 1;}

    /** performs merge resolution between binary clauses and equivalence recognition, if possible.
     * <br>
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
        ++timestamp; // just to be sure.
        try{
            Literal literalObject = literalIndexTwo.getFirstLiteralObject(-literal1);
            while(literalObject != null) { // all clauses with -literal1 are marked.
                Clause clause = literalObject.clause;
                if(clause != null && clause != clause1) clause.timestamp1 = timestamp;
                literalObject = literalObject.nextLiteral;}

            literalObject = literalIndexTwo.getFirstLiteralObject(literal2);
            while(literalObject != null) {
                Clause clause2 = literalObject.clause;
                if(clause2 == null) {literalObject = literalObject.nextLiteral; continue;}
                if(clause2.timestamp1 == timestamp) { // a partner clause is found
                    if(monitoring) monitor.println(monitorId,clause1.toString(symboltable,0) + " and " +
                            clause2.toString(symboltable,0) + " -> " + "true("+ Symboltable.toString(literal2,symboltable)+")");
                    addInternalTrueLiteralTask(literal2,true, trackReasoning ? new InfMergeResolutionTwo(clause1,clause2,literal2) : null);
                    removeClause(clause1,true);
                    removeClause(clause2,true);
                    return;}
                literalObject = literalObject.nextLiteral;}

            if(checkEquivalence) {
                literalObject = literalIndexTwo.getFirstLiteralObject(-literal2);
                while(literalObject != null) {
                    Clause clause2 = literalObject.clause;
                    if(clause2 == null) {literalObject = literalObject.nextLiteral; continue;}
                    if(clause2.timestamp1 == timestamp) { // a partner clause is found.
                        removeClause(clause1,false);
                        removeClause(clause2,true);
                        if(Math.abs(literal1) > Math.abs(literal2)) {int dummy = literal2; literal2 = literal1; literal1 = dummy;}
                        if(literal1 < 0) {literal1 *= -1; literal2 *= -1;}
                        if(monitoring) monitor.println(monitorId,clause1.toString(symboltable,0) + " and " +
                                clause2.toString(symboltable,0) + " -> " +
                                Symboltable.toString(literal1,symboltable)+" == " + Symboltable.toString(-literal2,symboltable));
                        int status = localStatus(clause2.literals.get(0).literal); // literal may have become pure.
                        if(status != 0) {addInternalTrueLiteralTask(-status*clause2.literals.get(1).literal,false,null); return;}
                        status = localStatus(clause2.literals.get(1).literal);
                        if(status != 0) {addInternalTrueLiteralTask(-status*clause2.literals.get(0).literal,false,null); return;}
                        equivalences.add(literal1); equivalences.add(-literal2);
                        InfEquivalence step = null;
                        if(trackReasoning) {
                            step = new InfEquivalence(clause1,clause2);
                            equivalenceSteps.add(step); equivalenceSteps.add(null);}
                        processEquivalence(literal1,-literal2,step);
                        return;}
                    literalObject = literalObject.nextLiteral;}}}
            finally {++timestamp;}}

    /** performs resolution between the given clause and all other binary clauses.
     * <br>
     * Subsumed resolvents are ignored. <br>
     * If the resolvent's literals are identical it is inserted into the model.<br>
     * Other resolvents generate a new BinaryClauseTask.
     *
     * @param parentClause1     a binary clause,
     * @throws Unsatisfiable  if the model detects a contradiction.
     */
    protected void saturateBinaryClausesWithBinaryClause(Clause parentClause1) throws Unsatisfiable {
        assert(parentClause1.size() == 2);
        for(Literal parentLiteralObject1 : parentClause1.literals) {
            Literal parentLiteralObject2 = literalIndexTwo.getFirstLiteralObject(-parentLiteralObject1.literal);
            while(parentLiteralObject2 != null) {
                Clause parentClause2 = parentLiteralObject2.clause;
                if(parentClause2 == null) {parentLiteralObject2 = parentLiteralObject2.nextLiteral; continue;}
                Clause resolvent = resolveBetweenBinaryClauses(parentClause1,parentClause2);
                if(resolvent != null) {
                    if(trackReasoning) resolvent.inferenceStep = new InfResolution(parentClause1, parentClause2, resolvent, symboltable);
                    insertClause(resolvent);
                    addDerivedClauseTask(resolvent);}
                parentLiteralObject2 = parentLiteralObject2.nextLiteral;}}}


    /** generates a resolvent between two binary clauses.
     * <br>
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
            ++statistics.derivedTrueLiterals;
            addInternalTrueLiteralTask(literal1,true,trackReasoning ? new InfMergeResolutionTwo(clause1,clause2,literal1) : null);
            if(monitoring) {
                monitor.println(monitorId,clause1.toString(symboltable,0) + " and " +
                        clause2.toString(symboltable,0) + " -> " + Symboltable.toString(literal1,symboltable));}
            removeClause(clause1,true);
            removeClause(clause2,true);
            ++statistics.binaryResolvents;
            return null;}
        Clause resolvent = new Clause(nextId.getAsInt(), literal1, literal2);
        if(binaryClauseIsSubsumed(resolvent) != null) return null;
        ++statistics.binaryResolvents;
        if(trackReasoning) resolvent.inferenceStep = new InfResolution(clause1,clause2,resolvent,symboltable);
        return resolvent;}

    /** checks if the clause is subsumed by another clause.
     *
     * @param clause a clause
     * @return null or the subsumer clause.
     */
    Clause isSubsumed(Clause clause) {
        boolean isBinaryClause = clause.size() == 2;
        Clause subsumer = isBinaryClause ? binaryClauseIsSubsumed(clause) :
                                           longerClauseIsSubsumedByBinaryClause(clause);
        if(subsumer == null && !isBinaryClause) subsumer = longerClauseIsSubsumedByLongerClause(clause);
        if(subsumer != null) ++statistics.subsumedClauses;
        return subsumer;}

    /** checks if the binary clause is subsumed by another binary clause.
     *
     * @param subsumee a binary clause
     * @return null or the subsumer clause.
     */
    protected Clause binaryClauseIsSubsumed(Clause subsumee) {
        int literal1 = subsumee.literals.get(0).literal;
        int literal2 = subsumee.literals.get(1).literal;
        Literal literalObject = literalIndexTwo.getFirstLiteralObject(literal1);
        while(literalObject != null) {
            Clause subsumer = literalObject.clause;
            if(subsumer == null) {literalObject = literalObject.nextLiteral;continue;}
             if(subsumer != subsumee &&
                     (subsumer.literals.get(0).literal == literal2 || subsumer.literals.get(1).literal == literal2)) {
                 return subsumer;}
            literalObject = literalObject.nextLiteral;}
        return null;}

    /** checks if the longer subsumee is subsumed by a binary subsumer.
     * <br>
     *  Only Or-clauses can be subsumed by binary clauses.
     * 
     * @param subsumee a longer clause
     * @return null or the binary subsumer clause.
     */
    Clause longerClauseIsSubsumedByBinaryClause(Clause subsumee) {
        assert(subsumee.size() > 2);
        if(subsumee.limit > 1) return null; // binary clauses cannot subsume longer atleast-clauses
        for(Literal literalObject : subsumee.literals) {
            int literal = literalObject.literal;
            Literal literalObjectTwo = literalIndexTwo.getFirstLiteralObject(literal);
            while(literalObjectTwo != null) {
                Clause subsumer = literalObjectTwo.clause;
                if(subsumer == null) {literalObjectTwo = literalObjectTwo.nextLiteral; continue;}
                int otherSubsumerLiteral = subsumer.otherLiteral(literalObjectTwo).literal;
                if(subsumee.findLiteral(otherSubsumerLiteral) != null) return subsumer;
                literalObjectTwo = literalObjectTwo.nextLiteral;}}
        return null;}

    /** checks if the longer clause is subsumed by another longer clause.
     * <br>
     *  atleast n p^a,... subsumes atleast n- p^a+,...<br>
     *  where n- means &lt;= n and a+ means &gt;=a.<br>
     *  Notice that the timestamp is changed!
     *
     * @param subsumee a longer clause
     * @return null or the longer subsumer clause.
     */
    Clause longerClauseIsSubsumedByLongerClause(Clause subsumee) {
        int size = subsumee.size();
        int limit = subsumee.limit;
        for(Literal literalObject1 : subsumee.literals) {
            int literal1 = literalObject1.literal;
            Literal literalObject2 = literalIndexMore.getFirstLiteralObject(literal1);
            while(literalObject2 != null) {
                Clause subsumer = literalObject2.clause;
                if(subsumer == null || subsumer == subsumee) {literalObject2 = literalObject2.nextLiteral; continue;}
                if(subsumer.timestamp2 < timestampSubsumption) {
                    if(subsumer.size() <= size && subsumer.limit >= limit &&
                            literalObject2.multiplicity <= literalObject1.multiplicity) subsumer.timestamp2 = timestampSubsumption;}
                else {
                    if(subsumer.timestamp2 - timestampSubsumption == subsumer.size()-2 &&
                            literalObject2.multiplicity <= literalObject1.multiplicity){
                        timestampSubsumption += size+2; return subsumer;}
                    if(literalObject2.multiplicity <= literalObject1.multiplicity) ++subsumer.timestamp2;}
                literalObject2 = literalObject2.nextLiteral;}}
        timestampSubsumption += size+2;
        return null;}


    /** computes all resolvents between the given binary parent clause and the longer clauses.
     * <br>
     * The resolvent is simplified and checked for subsumption.
     *
     * @param  binaryParentClause a binary clause.
     * @throws Unsatisfiable if a contradiction is discovered.
     */
    void saturateLongerClausesWithBinaryClause(Clause binaryParentClause) throws Unsatisfiable {
        assert(binaryParentClause.size() == 2);
        String clauseBefore = null; String binaryClause = null;
        if(trackReasoning || monitoring) binaryClause = binaryParentClause.toString(symboltable,0);
        for(Literal binaryParentLiteralObject : binaryParentClause.literals) {
            int otherBinaryParentLiteral = binaryParentClause.otherLiteral(binaryParentLiteralObject).literal;
            Literal longerParentLiteralObject = literalIndexMore.getFirstLiteralObject(-binaryParentLiteralObject.literal);
            while(longerParentLiteralObject != null) {
                Clause longerParentClause = longerParentLiteralObject.clause;
                 if(longerParentClause == null) {longerParentLiteralObject = longerParentLiteralObject.nextLiteral; continue;}
                if(longerParentClause.isDisjunction) {
                    Literal otherLongerParentLiteralObject = longerParentClause.findLiteral(otherBinaryParentLiteral);
                     if(otherLongerParentLiteralObject != null) {
                        if(trackReasoning || monitoring) clauseBefore = longerParentClause.toString(symboltable,0);
                        removeLiteralFromClause(longerParentLiteralObject,false);
                        if(monitoring) monitor.println(monitorId,"MergeResolution: "+ binaryClause + " and " +
                                clauseBefore + " -> " + longerParentClause.toString(symboltable,0));
                        if(trackReasoning)
                            longerParentClause.inferenceStep = new InfResolution(binaryParentClause, binaryClause, longerParentClause,
                                    clauseBefore,longerParentClause,symboltable);
                         addModifiedClauseTask(longerParentClause);
                        continue;}}
                resolve(binaryParentLiteralObject,longerParentLiteralObject);
                longerParentLiteralObject = longerParentLiteralObject.nextLiteral;}}}

    /** computes all resolvents between the given longer parent clause and the binary clauses.
     *
     * @param longerParentClause a longer clause
     * @throws Unsatisfiable     if a contradiction is discovered.
     */
    void saturateBinaryClausesWithLongerClause(Clause longerParentClause) throws Unsatisfiable {
        for(Literal literalObject : longerParentClause.literals) {
            Literal negLiteralObject = literalIndexTwo.getFirstLiteralObject(-literalObject.literal);
            while(negLiteralObject != null) {
                if(negLiteralObject.clause == null) {negLiteralObject = negLiteralObject.nextLiteral;continue;}
                resolve(literalObject,negLiteralObject);
                negLiteralObject = negLiteralObject.nextLiteral;}}}



    /** performs merge resolution between longer clauses.
     * <br>
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
    protected boolean mergeResolutionWithLongerClause(Clause clauseP) throws Unsatisfiable {
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
                    clauseS.timestamp1 = timestamp;
                    candidateClausesFound = true;}
                literalObjectS = literalObjectS.nextLiteral;}

            if(!candidateClausesFound)  {timestamp += clausePSize + 1;continue;}
            int i = 0;
            for(Literal literalObjectPi : clauseP.literals) {
                if(literalObjectPi == literalObjectP) continue;
                ++i;
                Literal literalObjectSi = literalIndexMore.getFirstLiteralObject(literalObjectPi.literal);
                while(literalObjectSi != null) {
                    Clause clauseS = literalObjectSi.clause;  // this is the potential merge partner.
                    if(clauseS == null) {literalObjectSi = literalObjectSi.nextLiteral; continue;}
                    if((clauseS.timestamp1 -timestamp) == i-1 && literalObjectSi.multiplicity == clauseS.limit) {
                        ++clauseS.timestamp1;
                        if(clauseS.timestamp1 - timestamp == clausePSize-1) { // mergepartner found
                            String resolventBefore = trackReasoning ? clauseS.toString(symboltable,0) : null;
                            if(clauseS.size() == clausePSize) {// both are equally long
                                ++statistics.mergedClauses;
                                if(removeLiteralFromClause(clauseS.findLiteral(literalObjectPNeg),false)) {
                                    clauseS.reduceToDisjunction();  // clauseS becomes a disjunction (implicit GCD-reduction)
                                    addDerivedClauseTask(clauseS);
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

                                    if(simplifyClause(clauseS,true)) addDerivedClauseTask(clauseS);
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
        mergeResolutionPartialBinary(clause);
        for(Literal literalObject : clause.literals) { // example: p,q,r
            int posLiteral = literalObject.literal;
            boolean found = false;
            Literal negLiteralObject = literalIndexMore.getFirstLiteralObject(-literalObject.literal);
            while(negLiteralObject != null) {          // example: -p,q',s
                if(negLiteralObject.clause == null) {negLiteralObject = negLiteralObject.nextLiteral; continue;}
                if(negLiteralObject.clause.size() == 3) {negLiteralObject.clause.timestamp1 = timestamp; found = true;}
                negLiteralObject = negLiteralObject.nextLiteral;}
            if(!found) {++timestamp; continue;}

            for(Literal literalObject1 : clause.literals) {
                if(literalObject1 == literalObject) continue;
                Literal otherLiteralObject = literalIndexMore.getFirstLiteralObject(literalObject1.literal);
                while(otherLiteralObject != null) {                        // Example: otherLiteralObject = q
                    if(otherLiteralObject.clause == null) {otherLiteralObject = otherLiteralObject.nextLiteral;continue;}
                    if(otherLiteralObject.clause.timestamp1 == timestamp) { // example: -p,q,s
                        resolve(literalObject,otherLiteralObject.clause.findLiteral(-posLiteral));}
                        otherLiteralObject = otherLiteralObject.nextLiteral;}}
            ++timestamp;}

    }

    /** creates all resolvents between the given 3-literal clause and all binary clauses.
     *
     * @param clause         a 3-literal clause
     * @throws Unsatisfiable if a contradiction is discovered.
     */
    void mergeResolutionPartialBinary(Clause clause) throws Unsatisfiable{
        if(!clause.exists || clause.size() != 3) return;
        for(Literal literalObject : clause.literals) { // example: p,q,r
            int posLiteral = literalObject.literal;
            Literal negLiteralObject = literalIndexTwo.getFirstLiteralObject(-literalObject.literal);
            while(negLiteralObject != null) {          // example: -p,s
                Clause negClause = negLiteralObject.clause;
                if(negClause == null) {negLiteralObject = negLiteralObject.nextLiteral; continue;}
                resolve(literalObject,negClause.findLiteral(-posLiteral));
                negLiteralObject = negLiteralObject.nextLiteral;}
        }}

    private final IntArrayList trueLiterals = new IntArrayList();
    /** creates a resolvent between the clauses with the two literals.
     * <br>
     * The resolvent is checked for subsumption, simplified and inserted into the internal data structures.
     * A new task is inserted into the task queue.
     *
     * @param posLiteral     a parent literal
     * @param negLiteral     a parent literal
     * @throws Unsatisfiable if a contradiction is discovered.
     */
    void resolve(Literal posLiteral, Literal negLiteral) throws Unsatisfiable {
        trueLiterals.clear();
        Clause resolvent = Clause.resolve(posLiteral,negLiteral,nextId,(literal -> trueLiterals.add(literal)));
        for(int literal : trueLiterals) {
            ++statistics.derivedTrueLiterals;
            addInternalTrueLiteralTask(literal,true,
                    trackReasoning ? new InfResolutionTrueLiteral(posLiteral.clause,negLiteral.clause,literal,symboltable): null);
            if(monitoring) monitor.println(monitorId, posLiteral.clause.toString(symboltable,0) + " and " +
            negLiteral.clause.toString(symboltable,0) + " -> true(" + Symboltable.toString(literal,symboltable) + ")");}
        if(resolvent == null) return;
        if(isSubsumed(resolvent) != null) return;
        if(trackReasoning) resolvent.inferenceStep =
                new InfResolution(posLiteral.clause,negLiteral.clause, resolvent, symboltable);
        insertClause(resolvent);
        if(resolvent.size() == 2) ++statistics.binaryResolvents; else ++statistics.longerResolvents;
        if(monitoring) monitor.println(monitorId,
                    "Resolution: " + posLiteral.clause.toString(symboltable,0) + " and " +
                            negLiteral.clause.toString(symboltable,0) + " -> " +
                            resolvent.toString(symboltable,0));
        addDerivedClauseTask(resolvent);}


    void addModifiedClauseTask(Clause clause) {
        TaskType type = (clause.size() == 2) ? TaskType.ProcessBinaryClause: TaskType.ProcessLongerClause;
        if(type == TaskType.ProcessBinaryClause) ++binaryClausesInQueue;
        synchronized (this) {queue.add(new Task<>(type, clause));}}

    /** adds a two-literal clause as ProcessBinaryClause task to the task queue.
     *
     * @param clause a two-literal clause.
     */
    protected void addBinaryClauseTask(Clause clause) {
        assert(clause.size() == 2);
        ++binaryClausesInQueue;
        synchronized (this) {queue.add(new Task<>(TaskType.ProcessBinaryClause, clause));}}



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
                    removeClause(clause,false);
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
                    addInternalTrueLiteralTask(representative, true, step);
                    addInternalTrueLiteralTask(literal, true, step);
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
                    if(monitoring) monitor.println(monitorId,"\n  Clause " + clauseString + ": literal " +
                            Symboltable.toString(literal,symboltable) + " replaced by equivalent literal " +
                            Symboltable.toString(representative,symboltable) + " new clause: " + clause.toString(symboltable,0));
                    if(binaryClauseIsSubsumed(clause) != null) removeClause(clause,false);
                        else addBinaryClauseTask(clause);}
                literalObject = literalObject.nextLiteral;}

            literalObject = literalIndexMore.getFirstLiteralObject(literal);  // we check the longer clauses.
            while(literalObject != null) {
                Clause  clause = literalObject.clause;
                if(clause == null) {literalObject = literalObject.nextLiteral; continue;}
                String clauseString = (trackReasoning | monitoring) ? clause.toString(symboltable,0) : null;
                Literal representativeObject = clause.findLiteral(representative);
                if(representativeObject != null) { // the two literals merge into one.
                    removeLiteralFromClause(literalObject,false);
                    representativeObject.multiplicity += literalObject.multiplicity;
                    clause.adjustMultiplicitiesToLimit();
                    ++statistics.equivalenceReplacements;
                    if (trackReasoning) clause.inferenceStep =
                            new InfEquivalenceReplacement(clauseString, clause, representative, literal, equivalenceStep, symboltable);
                    if(simplifyClause(clause,true)) {
                        if(monitoring) monitor.println(monitorId,"\n  Clause " + clauseString + ": literal " +
                                Symboltable.toString(literal,symboltable) + " replaced by equivalent literal " +
                                Symboltable.toString(representative,symboltable) + " new clause: " + clause.toString(symboltable,0));
                        if(isSubsumed(clause) != null) removeClause(clause,false); else  addDerivedClauseTask(clause);}
                    else removeClause(clause,true);}
                else { // just replace literal by representative
                    literalIndexMore.removeLiteral(literalObject);
                    representativeObject = new Literal(representative,literalObject.multiplicity);
                    representativeObject.clause = clause;
                    literalIndexMore.addLiteral(representativeObject);
                    clause.replaceLiteral(literalObject,representativeObject);
                    ++statistics.equivalenceReplacements;
                    if (trackReasoning) clause.inferenceStep =
                            new InfEquivalenceReplacement(clauseString, clause, representative, literal, equivalenceStep, symboltable);
                    if(clause.removeComplementaryLiterals((n -> statistics.complementaryLiterals += n), this::removeLiteralFromIndex)) {
                        removeClause(clause,false);
                        literalObject = literalObject.nextLiteral; continue;}
                    if(simplifyClause(clause,true)) {
                        if(monitoring) monitor.println(monitorId,"Clause " + clauseString + ": literal " +
                                Symboltable.toString(literal,symboltable) + " replaced by equivalent literal " +
                                Symboltable.toString(representative,symboltable) + " new clause: " + clause.toString(symboltable,0));
                        if(isSubsumed(clause) != null) removeClause(clause,false); else addDerivedClauseTask(clause);}
                    else removeClause(clause,false);}
                literalObject = literalObject.nextLiteral;}
            }}

    boolean isTotallyEmpty(int literal) {
        return  literalIndexTwo.isEmpty(literal) && literalIndexMore.isEmpty(literal) &&
                literalIndexTwo.isEmpty(-literal) && literalIndexMore.isEmpty(-literal);}

    int isPure(int literal) {
        if(literalIndexTwo.isEmpty(-literal) && literalIndexMore.isEmpty(-literal)) return 1;
        if(literalIndexTwo.isEmpty(literal) && literalIndexMore.isEmpty(literal)) return -1;
        return 0;}

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
     * @param literalIndex where the literals are.
     * @throws Unsatisfiable should not happen.
     */
    protected void removeClause(Clause clause, boolean checkPurity,Literals literalIndex) throws Unsatisfiable {
        clauses.removeClause(clause);
        for(Literal literalObject : clause.literals) {
            literalIndex.removeLiteral(literalObject);
            if(checkPurity) {checkPurity(literalObject.literal); checkPurity(-literalObject.literal);}}
        if(checkConsistency) checkConsistency();}

    /** removes the clause from the internal lists.
     *
     * @param clause  a clause to be removed.
     * @param checkPurity if true then the clause's literals are checked for purity.
     * @throws Unsatisfiable should not happen.
     */
    protected void removeClause(Clause clause, boolean checkPurity) throws Unsatisfiable {
        removeClause(clause,checkPurity,(clause.size() == 2) ? literalIndexTwo :literalIndexMore);}


        void removeClauses (int literal) throws Unsatisfiable {
        Literal literalObject = literalIndexMore.getFirstLiteralObject(literal);
        while(literalObject != null) {
            Clause clause = literalObject.clause;
            if(clause == null) {literalObject = literalObject.nextLiteral; continue;}
            removeClause(clause,false);
            literalObject = literalObject.nextLiteral;}}


    /** removes the Literal from the corresponding index.
     *
     * @param literalObject a Literal
     */
    protected void removeLiteralFromIndex(Literal literalObject) {
        Clause clause = literalObject.clause;
        if(clause.size() < 2) return;
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
        trueLiterals.clear();
        clauses.updateClauseNumbers(clause,-1);
        Literals literalIndex = (clause.size() == 2) ? literalIndexTwo : literalIndexMore;
        byte status = clause.removeLiterals((l-> (l == literalObject) ? (reduceLimit ? +1:-1) : 0),
                                           (l -> literalIndex.removeLiteral(literalObject)),
                                           (l -> trueLiterals.add(l)));
        if(status == -1) throw new UnsatEmptyClause(problemId,solverId,clause.identifier(),null);
        for(int literal :trueLiterals) addInternalTrueLiteralTask(literal,true,null);
        if(status == 1) return false;
        if(clause.size() == 2) {moveToIndexTwo(clause);}
        checkPurity(literalObject.literal); // pure literals are just added to the model.
        if(checkConsistency) checkConsistency();
        clauses.updateClauseNumbers(clause,+1);
        return true;}


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
     * @param clause                the clause to be simplified.
     * @param isAlreadyIntegrated   if true then the removed literals are removed from the index and checked for purity.
     * @return                      true if the clause still exists.
     * @throws Unsatisfiable        if the model finds a contradiction.
     */
    protected boolean simplifyClause(Clause clause, boolean isAlreadyIntegrated) throws Unsatisfiable {
        if(clause.isDisjunction || !clause.hasMultiplicities) return true; // nothing to be simplified.
        String clauseBefore = (trackReasoning || monitoring) ? clause.toString(symboltable,0) : null;
        boolean reducedByGCD;
        int sizeBefore = clause.size();
        Literals literalIndex = sizeBefore == 2 ? literalIndexTwo : literalIndexMore;
        removedLiterals.clear();
        try {
            byte status = clause.reduceByTrueLiterals(literalObject -> removedLiterals.add(literalObject),null);
            if(!removedLiterals.isEmpty()) {
                if(monitoring) {
                    String literalNames = "";
                    for (Literal literalObject : removedLiterals) {
                        literalNames += Symboltable.toString(literalObject.literal, symboltable) + ",";}
                    String newClause = (clause.limit > 0) ? ". new clause: " + clause.toString(symboltable,0) : "";
                    monitor.println(monitorId, "True literals " + literalNames + " extracted from clause " +
                            clauseBefore + newClause);}

                for (Literal literalObject : removedLiterals) {
                    if(isAlreadyIntegrated) literalIndex.removeLiteral(literalObject);
                    int literal = literalObject.literal;
                    ++statistics.derivedTrueLiterals;
                    addInternalTrueLiteralTask(literal, true,
                            trackReasoning ? new InfTrueLiteral(clauseBefore, clause.identifier, literal, clause.inferenceStep) : null);}

                if(status == 1) {removeClause(clause,true,literalIndex); return false;}}

            if (clause.isDisjunction || !clause.hasMultiplicities) return true; // nothing to be simplified.
            removedLiterals.clear();
            if (clause.reduceToEssentialLiterals(removedLiterals)) {
                if (monitoring && clause.limit > 0)
                    monitor.println(monitorId, "Clause  " + clauseBefore + " reduced to essential literals:  " +
                            clause.toString(symboltable, 0));
                if (clause.limit <= 0) return false;
                if (clause.size() == 1) {
                    int literal = clause.literals.get(0).literal;
                    addInternalTrueLiteralTask(literal, true, clause.inferenceStep);
                    return false;
                }
                if (clause.limit <= 1) return clause.limit == 1;}

            reducedByGCD = clause.divideByGCD();
            if (reducedByGCD && monitoring) {
                monitor.println(monitorId, "Clause " + clauseBefore + " divided by gcd  to " +
                        clause.toString(symboltable, 0));}
            if (clause.size() == 1) {
                addInternalTrueLiteralTask(clause.literals.get(0).literal, true, new InfInputClause(clause.identifier));
                return false;}}
        finally {
            if(isAlreadyIntegrated) {
                if(sizeBefore > 2 && clause.size() == 2) moveToIndexTwo(clause);}}
        if(reducedByGCD) return simplifyClause(clause,isAlreadyIntegrated);
        return true;}


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
        if(localStatus(literal) != 0) return false;
        if(literalIndexTwo.isEmpty(literal) && literalIndexTwo.isEmpty(-literal) &&
                literalIndexMore.isEmpty(literal) && literalIndexMore.isEmpty(-literal)) return false;
        if(literalIndexTwo.isEmpty(-literal) && literalIndexMore.isEmpty(-literal)) {
            if(monitoring) monitor.println(monitorId,"Pure Literal: " + Symboltable.toString(literal,symboltable));
            addInternalTrueLiteralTask(literal, false,trackReasoning ? new InfPureLiteral(literal,false) : null);
            ++statistics.pureLiterals;
            return true;}
        return false;}




    /** takes the literalObjects of the clauses whose predicates have been eliminated by exhaustive resolution.*/
    ArrayList<ArrayList<Literal>> eliminatedPredicates = new ArrayList<>();

    /** goes through the predicates to find the first one which can be eliminated.
     * <br>
     * The method can only be called after the two-literal clauses are saturated! <br>
     * The following eliminations are done: <br>
     * - all clauses with literals which are pure in the longer clauses are eliminated. <br>
     *   There may still be such literals in the saturated two-literal clauses.<br>
     * - If number of all resolvents with a literal does not exceed the number of clauses with this literal,
     *   then the resolvents are generated and the parent clauses are removed.<br>
     * If there are still predicates with non-empty literals in the index, the task is added to the queue again.
     *
     * @param task         the processElimination task.
     * @throws Unsatisfiable if the resolvent is unsatisfiable.
     */
    void processElimination(Task task) throws Unsatisfiable {
        boolean atEnd = false;
        try{
            for(int predicate = 1; predicate <= predicates; ++predicate) {
                if(localModel[predicate] != 0) continue;
                int sizeP = literalIndexMore.size(predicate,2);
                int sizeN = literalIndexMore.size(-predicate,2);
                if(sizeP == 0 && sizeN == 0) continue;
                if(sizeP == 0) {
                    removeClauses(-predicate);
                    if(literalIndexTwo.isEmpty(-predicate)) makeLocallyTrue(-predicate);
                    else {
                        ++statistics.partiallyPureLiterals;
                        addInternalTrueLiteralTask(-predicate,false,
                            trackReasoning ? new InfPureLiteral(-predicate,true) : null);
                        return;}}
                if(sizeN == 0) {
                    removeClauses(predicate);
                    if(literalIndexTwo.isEmpty(predicate)) makeLocallyTrue(predicate);
                    else {
                        ++statistics.partiallyPureLiterals;
                        addInternalTrueLiteralTask(predicate,false,
                                trackReasoning ? new InfPureLiteral(-predicate,true) : null);
                        return;}}
                if(sizeP < 0 || sizeN < 0) continue;

                // resolution between one or two clauses with positive predicate and one or two clauses with negative predicate.
                ArrayList<Literal> literalsP = new ArrayList<>();
                ArrayList<Literal> literalsN = new ArrayList<>();
                Literal literalObject = literalIndexMore.getFirstLiteralObject(predicate);
                while(literalObject != null) {literalsP.add(literalObject); literalObject = literalObject.nextLiteral;}
                literalObject = literalIndexMore.getFirstLiteralObject(-predicate);
                while(literalObject != null) {literalsN.add(literalObject); literalObject = literalObject.nextLiteral;}
                for(Literal literalP : literalsP) {for(Literal literalN: literalsN) {
                    ++statistics.longerResolvents;
                    resolve(literalP,literalN);}}
                for(Literal literalP : literalsP) removeClause(literalP.clause,false);
                for(Literal literalN : literalsN) removeClause(literalN.clause,false);
                eliminatedPredicates.add(literalsP); eliminatedPredicates.add(literalsN);
                if(monitoring) monitor.println(monitorId,"Predicate Eliminated by Resolution: " +
                        Symboltable.toString(predicate,symboltable));
                return;}
            atEnd = true;}
        finally {if(!atEnd) synchronized (this) {queue.add(task);}}}

    /** completes the local model for the clauses whose predicates have been eliminated with exhaustive resolution.
     */
    void completeModel() {
        for(int i = eliminatedPredicates.size()-1; i >= 0; --i) {
            for(Literal literalObject : eliminatedPredicates.get(i)) {
                Clause clause = literalObject.clause;
                int literal = literalObject.literal;
                if(localStatus(literal) != 0) continue;
                int trueLiterals = 0;
                for(Literal litObject : literalObject.clause.literals) {
                    if(localStatus(litObject.literal) == 1) ++trueLiterals;}
                if(trueLiterals < clause.limit) makeLocallyTrue(literal);}}
    }

    /** extends the local model to the equivalences.
     * <br>
     * If both literals of an equivalence are undefined then the status defines their truth value.
     *
     * @param status +1 or -1, for defining the truth value of literal pairs with undefined truth value.
     */
    void completeModelForEquivalences(byte status) {
        for(int i = 0; i < equivalences.size(); i += 2) {
            int literal1 = equivalences.getInt(i);
            int literal2 = equivalences.getInt(i+1);
            byte localStatus1 = localStatus(literal1);
            byte localStatus2 = localStatus(literal2);
            if(localStatus1 != 0) {
                if(localStatus2 != 0) assert localStatus1 == localStatus2;
                else makeLocallyTrue(localStatus1*literal2);}
            else {
                if(localStatus2 != 0) makeLocallyTrue(localStatus2*literal1);
                else {
                    makeLocallyTrue(status*literal1);
                    makeLocallyTrue(status*literal2);}}}}

    /** generates a local model from either the positive and mixedPositive literals or
     * from the negative and mixedNegative literals in the clauses.
     *
     * @throws Satisfiable after the model has been generated.*/
    void generatePositiveOrNegativeModel() throws Satisfiable {
        byte status = clauses.status;
        if(status != 0) {
            if (status == +1) generatePositiveModel();
            if (status == -1) generateNegativeModel();
            completeModel();
            completeModelForEquivalences(status);
            model.exchangeModel(localModel);
            throw new Satisfiable(problemId,solverId,model);}}

    /** generates a local model from the positive and mixed positive literals in the clauses.*/
    void generatePositiveModel() {
        Clause clause = clauses.firstClause;
        while(clause != null) {
            if(!clause.exists || clause.clauseType == ClauseType.NEGATIVE || clause.clauseType == ClauseType.MIXEDNEGATIVE) {
                clause = clause.nextClause;}
                int trueLiterals = 0;
            for(Literal literalObject : clause.literals) {
                 if(localStatus(literalObject.literal) == 1) ++trueLiterals;}
            if(trueLiterals < clause.limit) {
                for(Literal literalObject : clause.literals) {
                    int literal = literalObject.literal;
                    if(literal < 0 || localStatus(literal) != 0) continue;
                    makeLocallyTrue(literal);
                    if(++trueLiterals == clause.limit) break;}
                assert trueLiterals >= clause.limit;}
            clause = clause.nextClause;}}


    /** generates a local model from the negative and mixed negative literals in the clauses.*/
    void generateNegativeModel() {
        Clause clause = clauses.firstClause;
        while(clause != null) {
            if(!clause.exists || clause.clauseType == ClauseType.POSITIVE || clause.clauseType == ClauseType.MIXEDPOSITIVE) {
                clause = clause.nextClause;}
            int trueLiterals = 0;
            for(Literal literalObject : clause.literals) {
                if(localStatus(literalObject.literal) == 1) ++trueLiterals;}
            if(trueLiterals < clause.limit) {
                for(Literal literalObject : clause.literals) {
                    int literal = literalObject.literal;
                    if(literal > 0 || localStatus(literal) != 0) continue;
                    makeLocallyTrue(literal);
                    if(++trueLiterals == clause.limit) break;}
                assert trueLiterals >= clause.limit;}
            clause = clause.nextClause;}}

    /** lists the local model as string.
     *
     * @return the local model as string.
     */
    public String localModelString() {
        StringBuilder st = new StringBuilder();
        int predicate = 1;
        for(; predicate <= predicates; ++predicate) {
            byte status = localModel[predicate];
            if(status != 0) {st.append(status*predicate);break;}}
        ++predicate;
        for(; predicate <= predicates; ++predicate) {
            byte status = localModel[predicate];
            if(status != 0) st.append(",").append(status* predicate);}
        return st.toString();}


    /** checks the consistency of the clauses and the literal index.
     *  An error is reported and the system exits.
     */
    void checkConsistency() {
        Clause clause = clauses.firstClause;
        while(clause != null) {
            for(Literal literalObject: clause.literals) {
                if(literalObject.clause == null) {
                    System.out.println("Literal " + Symboltable.toString(literalObject.literal,symboltable) +
                            " of clause " + clause.toString(symboltable,0)+ " has no clause any more.");
                    new Exception().printStackTrace();
                    System.exit(1);}
                if(literalObject.clause != clause) {
                    System.out.println("Literal " + Symboltable.toString(literalObject.literal,symboltable) +
                            " of clause " + clause.toString(symboltable,0)+
                            " is in the wrong clause " + literalObject.clause.toString());
                    new Exception().printStackTrace();
                    System.exit(1);}
                if(clause.size() == 2) {
                    if(!literalIndexTwo.contains(literalObject)) {
                        System.out.println("Literal " + Symboltable.toString(literalObject.literal,symboltable) +
                            " of clause " + clause.toString(symboltable,0) +
                            " is not in literalIndexTwo.");
                        if(literalIndexMore.contains(literalObject)) {
                            System.out.println("Literal " + Symboltable.toString(literalObject.literal,symboltable) +
                                        " is instead in literalIndexMore.");}
                        new Exception().printStackTrace();
                        System.exit(1);}}
                else {
                    if(!literalIndexMore.contains(literalObject)) {
                        System.out.println("Literal " + Symboltable.toString(literalObject.literal,symboltable) +
                            " of clause " + clause.toString(symboltable,0) +
                            " is not in literalIndexMore.");
                        if(literalIndexTwo.contains(literalObject)) {
                            System.out.println("Literal " + Symboltable.toString(literalObject.literal,symboltable) +
                                    " is instead in literalIndexTwo.");}
                        new Exception().printStackTrace();
                        System.exit(1);}}}
            clause = clause.nextClause;}
        literalIndexTwo.checkConsistency(2,"literalIndexTwo");
        literalIndexMore.checkConsistency(0,"literalIndexMore");
        }

    /** the monitor prints the clauses separated by their size.
     */
    void printSeparated() {
        for(int size = 2; size <=  problemSupervisor.inputClauses.maxClauseLength; ++size) {
            System.out.println("Size " + size);
            Clause clause = clauses.firstClause;
            while(clause != null) {
                if(clause.size() == size) {monitor.println(clause.toString());}
                clause = clause.nextClause;}}}

    /** returns the statistics.
     *
     * @return the statistics.
     */
    @Override
    public Statistic getStatistics() {
        return statistics;}
}
