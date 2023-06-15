package Solvers.Resolution;

import Datastructures.Results.Result;
import Datastructures.Results.Satisfiable;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Statistics.Statistic;
import Datastructures.Symboltable;
import Datastructures.Task;
import Datastructures.Theory.Model;
import InferenceSteps.InferenceStep;
import Management.Monitor.Monitor;
import Management.ProblemSupervisor;
import Solvers.Normalizer.Normalizer;
import Solvers.Solver;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.concurrent.PriorityBlockingQueue;
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

    /** controls if the consistency is to be checked (for testing purposes) */
    private final boolean checkConsistency = false;

    /** the thread which runs the simplifier. */
    private Thread myThread;

    /** collects the equivalences for later updating the model.
     * Entries: representative_1, literal_1, ...*/
    IntArrayList equivalenceList = new IntArrayList();

    /** The local model may contain true literals derived from pure literals.
     * These need not be true in all models, and therefore cannot be sent to the global model. */
    private byte[] localModel;

    Equivalences equivalences = new Equivalences();

    Task<TaskType> equivalenceTask =  new Task<>(TaskType.ProcessEquivalence,null);

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
        /** some new equivalences are found. */
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
            case ProcessTrueLiteral:            return Math.abs((Integer)task.a);
            case ProcessEquivalence:            return 2*predicates + 101;
            case ProcessLongerClause:           return 2*predicates + 102;
            case ProcessClauseFirstTime:        return 2*predicates + 103;
            case ProcessMergeResolutionPartial: return 2*predicates + 104;
            case ProcessElimination:            return 2*predicates + 105;}
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
        problemId  = problemSupervisor.problemId;
        monitorId  = "Resolution";
        statistics = new ResolutionStatistics(solverId);
        makeReusable();
        synchronized (this) {
            for(int literal: model.model) {
                addExternalTrueLiteralTask(literal,model.getInferenceStep(literal));}}
        try{
            readInputClauses();
            synchronized (this) {queue.add(new Task<>(TaskType.ProcessElimination,null));}
            processTasks(0);}
        catch(Result result) {
            System.out.println("Result " + result.getClass().getName());
            //printSeparated();
            if(result instanceof Satisfiable) {
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
        equivalences.equivalences.clear();
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
        equivalences.clear();
        for(int predicate = 1; predicate <= predicates; ++predicate) localModel[predicate] = 0;
    }

    /** returns the local truth-status of the literal: +1 = true, -1 = false, 0 = undefined. */
    byte localStatus(final int literal) {
        return (literal > 0) ? localModel[literal] : (byte)-localModel[-literal];}

    /** sets the local truth status of the literal
     *
     * @param literal a literal which is expected to be true.
     */
    void makeLocallyTrue(final int literal) {
        if(literal > 0) localModel[literal] = 1; else localModel[-literal] = -1;}

    private final int[] clauseId = new int[]{0};

    IntSupplier nextId = () -> ++clauseId[0];

    /** reads the disjunctions, the atleast, atmost, exactly and interval clauses from the normalizer and transfroms them to Clauses.
     * 
     * @throws Result if a contradiction or the empty clause is derived.
     */
    public void readInputClauses() throws Result {
        ArrayList<IntArrayList> normalizedClauses = problemSupervisor.normalizer.clauses;
        clauseId[0] = normalizedClauses.get(normalizedClauses.size()-1).getInt(0);
        Clause clause; int subIdentifier;
        for(IntArrayList normalizedClause : problemSupervisor.normalizer.clauses) {
            switch(Normalizer.getQuantifier(normalizedClause)) {
                case OR:
                case ATLEAST: insertClause(clause = new Clause(normalizedClause,0));
                    if(clause.size() == 2) addClauseTask(clause); break;
                case ATMOST:  insertClause(clause = new Clause(Normalizer.atmost2Atleast(normalizedClause),0));
                    if(clause.size() == 2) addClauseTask(clause); break;
                case EXACTLY:
                    subIdentifier = 0;
                    for(IntArrayList atleastClause : Normalizer.exactlyToAtleast(normalizedClause)) {
                    insertClause(clause = new Clause(atleastClause,++subIdentifier));
                    if(clause.size() == 2) addClauseTask(clause);} break;
                case INTERVAL:
                    subIdentifier = 0;
                    for(IntArrayList atleastClause : Normalizer.intervalToAtleast(normalizedClause)) {
                        insertClause(clause = new Clause(atleastClause,++subIdentifier));
                        if(clause.size() == 2) addClauseTask(clause);}}}
        if(checkConsistency) checkConsistency();
        statistics.initialClauses = clauses.size;
        if(clauses.isEmpty()) throw new Satisfiable(problemId,solverId, model);
        if(printClauses) {
            System.out.println("INPUT CLAUSES: " + clauses.size());
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
    public void addInternalTrueLiteralTask(final int literal, final boolean globallyTrue, final InferenceStep inferenceStep) throws Unsatisfiable {
        if(trackReasoning && inferenceStep == null) {
            new Exception().printStackTrace();
            System.exit(1);
        }
        if(model.status(literal) == 1) return;
        if(monitoring) {
            String globally = globallyTrue ? "Globally True " : "Locally True ";
            monitor.print(monitorId,globally+"literal added: " + Symboltable.toString(literal,symboltable));}
        if(literal == 0) {
            System.out.println("INFERENCES");
            Result.printInferenceSteps(inferenceStep);
            new Exception().printStackTrace();
            System.exit(1);}
        makeLocallyTrue(literal);
        synchronized (this) {
            queue.add(new Task<>(TaskType.ProcessTrueLiteral, literal, inferenceStep));
            ++trueLiteralsInQueue;}
        if(globallyTrue) model.add(myThread,literal,inferenceStep);
        ++statistics.derivedTrueLiterals;
        //if(checkConsistency) problemSupervisor.intermediateModelCheck(model,null); // ()->{printSeparated(); return null;});
    }


    /** adds a true literal to the queue. It is called by other threads for globally true literals.
     * <br>
     * If the queue is empty a new ProcessElimination task is added to the queue as well.
     *
     * @param literal a true literal.
     * @param inferenceStep which caused the truth.
     */
    public void addExternalTrueLiteralTask(final int literal, InferenceStep inferenceStep) {
        if(monitoring) {
            monitor.print(monitorId,"In: True literal from model " +
                    Symboltable.toString(literal,symboltable));}
        if(trackReasoning && inferenceStep == null) inferenceStep = new InfExternal(literal);
        makeLocallyTrue(literal);
        synchronized (this) {
            if(queue.isEmpty()) queue.add(new Task<>(TaskType.ProcessElimination,null));
            queue.add(new Task<>(TaskType.ProcessTrueLiteral, literal, inferenceStep));
            ++trueLiteralsInQueue;}}

    /** adds the equivalence task to the queue, if it is not already there.
     */
    void addEquivalenceTask() {
        if(equivalenceTask.a == null) {
            equivalenceTask.a = true;
            synchronized (this){queue.add(equivalenceTask);}}}


    /** controls that all clauses are printed after each task has been changed something (for testing purposes).*/
    private final boolean printClauses = true;

    /** reads the next task from the task queue and processes it.
     *
     * @param n 0 or the maximum number of task before stopping the loop (0: unlimited) (> 0 for test purposes).
     * @throws Result if a contradiction is encountered or the clause set became empty.
     */
    public void processTasks(final int n) throws Result {
        Task<Resolution.TaskType> task;
        int counter = 0;
        Clause clause;
        while(!myThread.isInterrupted()) {
            try {
                //if(monitoring) {monitor.println(monitorId,"Queue is waiting\n" + Task.queueToString(queue));}
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
                        task.a = null;
                        processEquivalences();
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
                    throw new Satisfiable(problemId,solverId,model);}
                if(trueLiteralsInQueue == 0 && clauses.status != 0) generatePositiveOrNegativeModel();
                if(queue.isEmpty()) {
                    System.out.println("Empty Queue Clauses: " + clauses.size + ", Local Model: " + localModelString());
                    printSeparated();
                }
                if(monitoring  && printClauses && changed) {
                    System.out.println("Local Model: " + localModelString());
                    //printSeparated();
                }
                if(trueLiteralsInQueue == 0 && binaryClausesInQueue == 0 && literalIndexMore.size() == 0) {
                    if(monitoring) {
                        monitor.println(monitorId, "No longer clauses any more. 2-Literal Clauses are Saturated.\n" );
                        monitor.println(monitorId,"Local Model: " + localModelString());
                        printSeparated();}
                    addInternalTrueLiteralTask(clauses.firstClause.literals.get(0).literal,false,
                            trackReasoning ? new InfSaturatedTwoLiteralClauses(clauses.firstClause) : null);}
                }
            catch(InterruptedException ex) {
                ex.printStackTrace();
                return;}
            if(n > 0 && ++counter == n){
                System.out.println("Stopped because task counter > limit: " + counter);
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
    void processClauseFirstTime(final Task<Resolution.TaskType> task) throws Unsatisfiable {
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
    protected void processBinaryClause(final Clause clause) throws Unsatisfiable {
        assert clause.size() == 2;
        if(binaryClauseIsSubsumed(clause) != null) {
            removeClause(clause,true,literalIndexTwo,true);
            return;}
        removeBinaryClausesSubsumedByBinaryClause(clause);
        removeLongerClausesSubsumedByBinaryClause(clause);
        if(binaryClauseIsLocallyTrue(clause)){
            removeClause(clause,true,true);
            return;}
        int literal1 = clause.literals.get(0).literal;
        int literal2 = clause.literals.get(1).literal;
        mergeResolutionAndEquivalenceTwoTwo(clause,literal1,literal2,true);
        if(!clause.exists) return;
        mergeResolutionAndEquivalenceTwoTwo(clause,literal2,literal1,false);
        if(!clause.exists) return;
        mergeResolutionAndEquivalenceTwoMore(clause,literal1,literal2,true);
        mergeResolutionAndEquivalenceTwoMore(clause,literal2,literal1,false);
        if(checkPurity(literal1) || checkPurity(literal2)) {
            removeClause(clause,false,literalIndexTwo,true); return;}
        saturateBinaryClausesWithBinaryClause(clause);
        if(!clause.exists) return;
        saturateLongerClausesWithBinaryClause(clause);}

    /** checks if the binary clause is locally true
     *
     * @param clause a binary clause
     * @return true if it is locally true.
     */
    boolean binaryClauseIsLocallyTrue(Clause clause) {
        return localStatus(clause.literals.get(0).literal) == 1 ||  localStatus(clause.literals.get(1).literal) == 1;}

    /** processes the longer clause.
     * - if the clause is subsumed, it is removed.<br>
     * - other clauses subsumed by the clause are removed. <br>
     * - merge resolution with the clause is performed.<br>
     * - triggered equivalences with 3-literal disjunctions is searched. <br>
     * - all resolvents with the binary clauses are generated.<br>
     *
     * @param clause a longer clause.
     * @throws Unsatisfiable if a contradiction is discovered.
     */
    protected void processLongerClause(final Clause clause) throws Unsatisfiable {
        if(clause.size() < 3) {
            processBinaryClause(clause);
            return;}
        if((longerClauseIsSubsumedByBinaryClause(clause) != null) ||
           (longerClauseIsSubsumedByLongerClause(clause) != null)) {
            removeClause(clause,true,true);
            return;}
        removeClausesSubsumedByLongerClause(clause);
        if(!mergeResolutionMoreMore(clause)) return;
        if(clause.isDisjunction && clause.size() == 3 && findTriggeredEquivalence(clause) > 0) return;
        saturateBinaryClausesWithLongerClause(clause);
        if(clause.exists) mergeResolutionPartial(clause);
    }

    ArrayList<InferenceStep> inferenceSteps = new ArrayList<>();

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
    protected void processTrueLiteral(final int oldTrueLiteral, final InferenceStep inferenceStep) throws Unsatisfiable {
        processTrueLiteralTwo(oldTrueLiteral,inferenceStep);
        processTrueLiteralMore(oldTrueLiteral);
        trueLiterals.clear();
        inferenceSteps.clear();
        equivalences.applyTrueLiteral(oldTrueLiteral,inferenceStep,
                (trueLiteral,step) -> {trueLiterals.add((int)trueLiteral); inferenceSteps.add(step);});
        if(!equivalences.isEmpty()) addEquivalenceTask();
        for(int i = 0; i < trueLiterals.size(); ++i) {
            int newTrueLiteral = trueLiterals.getInt(i);
            InferenceStep step = inferenceSteps.get(i);
            if(localStatus(newTrueLiteral) == 0) {
                if(monitoring) monitor.println(monitorId, "Equivalent literal " +
                        Symboltable.toString(newTrueLiteral,symboltable) + " added to model.");
                addInternalTrueLiteralTask(newTrueLiteral,true,
                        trackReasoning ? new InfEquivalentTruth(newTrueLiteral,oldTrueLiteral,step) : null);
                ++statistics.derivedTrueLiterals;}}}

    /** applies a true literal to all two-literal clauses containing this literal.
     * <br>
     * Clauses containing this literal are removed.<br>
     * Clauses containing -literal yield a new true literal, which is put into the model.<br>
     * The clause is removed as well.
     *
     * @param oldTrueLiteral a true literal.
     * @param inferenceStep  which caused the derivation of oldTrueLiteral.
     * @throws Unsatisfiable if a contradiction is encountered.
     */
    protected void processTrueLiteralTwo(final int oldTrueLiteral, final InferenceStep inferenceStep) throws Unsatisfiable {
        literalIndexTwo.forAllLiterals(oldTrueLiteral,null, // remove all two-literal clauses with oldTrueLiteral
                (literalObject -> {removeClause(literalObject.clause,true,true); return false;}));

        literalIndexTwo.forAllLiterals(-oldTrueLiteral,null,
                (literalObject -> { // all two-literal clauses with -oldTrueLiteral yield a new true literal.
                    Clause clause = literalObject.clause;
                    int otherLiteral = clause.otherLiteral(literalObject).literal;
                    InfUnitResolutionTwo step = (trackReasoning || monitoring) ?
                            new InfUnitResolutionTwo(clause,oldTrueLiteral,inferenceStep,otherLiteral) : null;
                    if(monitoring) monitor.println(monitorId,step.info(symboltable));
                    addInternalTrueLiteralTask(otherLiteral, model.isTrue(oldTrueLiteral),step);
                    ++statistics.derivedTrueLiterals;
                    removeClause(clause,false,true);
                    return false;}));}


    /** applies a true literal to all longer clauses containing this literal.
     * <br>
     * All literals with a truth value in the model are removed and the clause is simplified.<br>
     * Clauses which become true in this step are entirely removed.<br>
     * The empty clause causes an UnsatClause exception to be thrown.<br>
     * Derived unit clauses are put into the model. <br>
     * Shortened clauses cause new tasks to be inserted into the task queue.<br>

     * @param oldTrueLiteral a true (or false) literal.
     * @throws Unsatisfiable if a contradiction is encountered.
     */
    void processTrueLiteralMore(final int oldTrueLiteral) throws Unsatisfiable{
        removedLiterals.clear();
        boolean globallyTrue = model.status(oldTrueLiteral) != 0;
        for(int sign = 1; sign >= -1; sign -= 2) {
            literalIndexMore.forAllLiterals(sign*oldTrueLiteral,null,
                    literalObject -> {
                        Clause clause = literalObject.clause;
                        clauses.updateClauseNumbers(clause,-1);
                        String clauseBefore =  monitoring    ? clause.toString(symboltable,0) : null;
                        if(trackReasoning || monitoring) {collectTrueLiterals(clause);}
                        switch(clause.removeLiterals(
                                litObject -> (int)localStatus(litObject.literal),
                                litObject -> {literalIndexMore.removeLiteral(litObject);
                                            removedLiterals.add(localStatus(litObject.literal)*litObject.literal);},
                                trueLiteral -> {
                                    ++statistics.derivedTrueLiterals;
                                    InfTrueLiteral step  = (trackReasoning || monitoring) ?
                                        new InfTrueLiteral(clauseBefore, clause, trueLiterals, trueLiteral, symboltable) : null;
                                    if(monitoring) monitor.println(monitorId,step.info(symboltable));
                                    addInternalTrueLiteralTask(trueLiteral,globallyTrue,step);
                                    trueLiterals.add(trueLiteral);})) {
                            case  1: removeClause(clause,false,false); return false;
                            case -1: throw new UnsatClause(clause,problemId,solverId);}

                        clauses.updateClauseNumbers(clause,1);
                        if(clause.size() == 2) moveToIndexTwo(clause);
                        addClauseTask(clause);
                        return false;});}
                if(checkConsistency) checkConsistency();
        literalIndexMore.removePredicate(oldTrueLiteral);
        for(int removedLiteral :removedLiterals) checkPurity(removedLiteral);
    }

    /** removes all binary clauses which are subsumed by a binary subsumer.
     *
     * @param subsumer a binary clause.
     * @throws Unsatisfiable if a contradiction is found (should not happen)
     */
    protected void removeBinaryClausesSubsumedByBinaryClause(final Clause subsumer) throws Unsatisfiable {
        assert(subsumer.size() == 2);
        int literal2 = subsumer.literals.get(1).literal;
        literalIndexTwo.forAllLiterals(subsumer.literals.get(0).literal,
                (subsumeeLiteral -> {
                    Clause subsumee = subsumeeLiteral.clause;
                    return subsumee != subsumer && subsumee.otherLiteral(subsumeeLiteral).literal == literal2;}),
                (subsumee -> {
                    removeClause(subsumee.clause,true,true);
                    ++statistics.subsumedClauses;
                    return false;}));}


    /** removes all longer disjunctions which are subsumed by a binary subsumer.
     *
     * @param subsumer a binary clause.
     * @throws Unsatisfiable should not happen.
     */
    protected void removeLongerClausesSubsumedByBinaryClause(final Clause subsumer) throws Unsatisfiable {
        assert(subsumer.size() == 2);
        literalIndexMore.timestampClauses(subsumer.literals.get(0).literal,
                (subsumeeLiteral -> {
                    Clause subsumee = subsumeeLiteral.clause;
                    return subsumee.isDisjunction || subsumee.limit == subsumeeLiteral.multiplicity;}),
                timestampSubsumption,false);

        literalIndexMore.forAllLiterals(subsumer.literals.get(1).literal,
                (subsumeeLiteral -> {
                    Clause subsumee = subsumeeLiteral.clause;
                    return subsumee.timestamp2 == timestampSubsumption &&
                            (subsumee.isDisjunction || subsumee.limit == subsumeeLiteral.multiplicity);}),
                (subsumeeLiteral -> {
                    removeClause(subsumeeLiteral.clause,true,true);
                    ++statistics.subsumedClauses;
                    return false;}));
        ++timestampSubsumption;}

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
    protected void removeClausesSubsumedByLongerClause(final Clause subsumer) throws Unsatisfiable {
        int subsumerSize = subsumer.literals.size();
        int sumsumerLimit = subsumer.limit;
        final Literal firstLiteral = subsumer.literals.get(0);
        if(!literalIndexMore.timestampClauses(firstLiteral.literal,
                (subsumeeLiteral -> {
                    Clause subsumee = subsumeeLiteral.clause;
                    return (subsumee != subsumer && subsumee.limit <= sumsumerLimit &&
                        subsumee.literals.size() >= subsumerSize &&
                        subsumeeLiteral.multiplicity >= firstLiteral.multiplicity);}),
                timestampSubsumption , false)) return;

        for(int i = 1; i < subsumer.literals.size(); ++i) { // find candidates.
            final int im = i-1;
            Literal subsumerLiteral = subsumer.literals.get(i);
            literalIndexMore.forAllLiterals(subsumerLiteral.literal,
                    (subsumeeLiteral -> {
                        Clause subsumee = subsumeeLiteral.clause;
                        if((subsumee.timestamp2 - timestampSubsumption) == im &&
                            subsumeeLiteral.multiplicity >= subsumerLiteral.multiplicity) {
                            ++subsumee.timestamp2; return true;}
                        return false;}),
                    (subsumeeLiteral -> {
                        Clause subsumee = subsumeeLiteral.clause;
                        if(subsumee.timestamp2 - timestampSubsumption == subsumerSize-1){
                            removeClause(subsumee,true,true);}
                    return false;}));}
        timestampSubsumption += subsumerSize + 1;}

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
    void mergeResolutionAndEquivalenceTwoTwo(final Clause clause1, int literal1, int literal2,
                                             final boolean checkEquivalence) throws Unsatisfiable {
        assert(clause1.size() == 2);
        ++timestamp; // just to be sure.
        try{
            literalIndexTwo.timestampClauses(-literal1,null,timestamp,true);

            literalIndexTwo.forAllLiterals(literal2,
                    (literalObject -> literalObject.clause.timestamp1 == timestamp),
                    (literalObject -> {
                        Clause clause2 = literalObject.clause;
                        if(monitoring) monitor.println(monitorId,clause1.toString(symboltable,0) + " and " +
                                    clause2.toString(symboltable,0) + " -> " + "true("+ Symboltable.toString(literal2,symboltable)+")");
                        addInternalTrueLiteralTask(literal2,true, trackReasoning ? new InfMergeResolutionTwo(clause1,clause2,literal2) : null);
                        removeClause(clause1,true,true);
                        removeClause(clause2,true,true);
                        ++statistics.mergeResolutionTwoTwo;
                        return true;}));

            if(checkEquivalence) {
                literalIndexTwo.forAllLiterals(-literal2,
                        (literalObject -> literalObject.clause.timestamp1 == timestamp),
                        (literalObject -> {
                            Clause clause2 = literalObject.clause;
                            removeClause(clause1,false,true);
                            removeClause(clause2,true,true);
                            InfEquivalence step = (trackReasoning || monitoring) ? new InfEquivalence(clause1,clause2) : null;
                            if(monitoring) monitor.println(monitorId,step.info(symboltable));
                            ++statistics.binaryEquivalences;
                            equivalences.add(0,literal1,-literal2, step);
                            equivalenceTask.a = true;
                            synchronized (this) {queue.add(equivalenceTask);}
                            return true;}));}}
        finally {++timestamp;}}

    /** performs merge resolution between a binary clause and a longer clause, and triggered equivalence recognition, if possible.
     * <br>
     * Binary MergeResolution with disjunctions:  p,q and -p,q,phi -&gt; q,phi. (destructively) <br>
     * Binary MergeResolution with atleast:       p,q and &gt;= m -p^n, q^k,phi and k = m+1-n -&gt; &gt;= m q^k,phi (destructively)<br>
     * If the condition k = m+1-n does not hold then the merge resolvent is added to the clauses.<br>
     * Triggered Equivalence Recognition: p,q and -p,-q,r -&gt; r -&gt; p == q. (only with 3-literal disjunctions)  <br>
     * Derived true literals are inserted into the model.<br>
     * A derived equivalence is sent to the equivalence classes.<br>
     *
     * @param clause1   the binary clause.
     * @param literal1  either the first or the second literal.
     * @param literal2  the other literal.
     * @param checkEquivalence if true then equivalence check is done.
     * @throws Unsatisfiable if inserting a derived unit clause into the model causes a contradiction.
     */
    void mergeResolutionAndEquivalenceTwoMore(final Clause clause1, int literal1, int literal2,
                                             final boolean checkEquivalence) throws Unsatisfiable {
        assert(clause1.size() == 2);
        ++timestamp; // just to be sure.
        try{
            Literal literalObject2 = literalIndexMore.getFirstLiteralObject(-literal1);
            while(literalObject2 != null) { // all clauses with -literal1 are marked.
                Clause clause = literalObject2.clause;
                if(clause != null) clause.timestamp1 = timestamp;
                literalObject2 = literalObject2.nextLiteral;}

            literalObject2 = literalIndexMore.getFirstLiteralObject(literal2);
            while(literalObject2 != null) { // clauses with literal2 can merge with clause1
                Clause clause2 = literalObject2.clause;
                if(clause2 == null) {literalObject2 = literalObject2.nextLiteral; continue;}
                if(clause2.timestamp1 == timestamp) { // a partner clause is found: p,q and -p,q,phi -> merge
                    Literal negLiteralObject1 = clause2.findLiteral(-literal1); // find -p
                    if(literalObject2.multiplicity == clause2.limit+1-negLiteralObject1.multiplicity) { // condition for destructive merge
                        String clause2Before = null;
                        if(monitoring || trackReasoning) {clause2Before = clause2.toString(symboltable,0);}
                        removeLiteralFromClause(negLiteralObject1,false);
                        InfMergeResolutionMore step = (monitoring || trackReasoning) ? new InfMergeResolutionMore(clause1,clause2Before,clause2,symboltable) : null;
                        if(trackReasoning) clause2.inferenceStep = step;
                        if(monitoring) monitor.println(monitorId,step.info());}
                    else {resolve(negLiteralObject1,clause1.findLiteral(literal1)); } // non-destructive merge
                    ++statistics.mergeResolutionTwoMore;}
                literalObject2 = literalObject2.nextLiteral;}

            if(checkEquivalence) { // p,q and -p,-q,r  -> triggered equivalence: -r -> p == -q
                literalObject2 = literalIndexTwo.getFirstLiteralObject(-literal2);
                while(literalObject2 != null) {
                    Clause clause2 = literalObject2.clause;
                    if(clause2 == null) {literalObject2 = literalObject2.nextLiteral; continue;}
                    if(clause2.timestamp1 == timestamp && clause2.isDisjunction && clause2.size() == 3) { // a partner clause is found.
                        removeClause(clause2,true,true);
                        int triggerLiteral = -clause2.findThirdLiteral(literal1,-literal2).literal;
                        InfEquivalence step = (monitoring || trackReasoning) ?
                                new InfEquivalence(triggerLiteral,clause1.findLiteral(literal1),literalObject2,symboltable) : null;
                        if(monitoring) monitor.println(monitorId,step.info(symboltable));
                        ++statistics.triggeredEquivalences;
                        equivalences.add(triggerLiteral,literal1,-literal2,step);
                        equivalenceTask.a = true;
                        synchronized (this) {queue.add(equivalenceTask);}}
                    literalObject2 = literalObject2.nextLiteral;}}}
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
    protected void saturateBinaryClausesWithBinaryClause(final Clause parentClause1) throws Unsatisfiable {
        assert(parentClause1.size() == 2);
        for(final Literal parentLiteralObject1 : parentClause1.literals) {
            Literal parentLiteralObject2 = literalIndexTwo.getFirstLiteralObject(-parentLiteralObject1.literal);
            while(parentLiteralObject2 != null) {
                Clause parentClause2 = parentLiteralObject2.clause;
                if(parentClause2 == null) {parentLiteralObject2 = parentLiteralObject2.nextLiteral; continue;}
                Clause resolvent = resolveBetweenBinaryClauses(parentClause1,parentClause2);
                if(resolvent != null) {
                    if(trackReasoning) resolvent.inferenceStep = new InfResolution(parentClause1, parentClause2, resolvent, symboltable);
                    if(monitoring) monitor.println(monitorId,parentClause1.toString(symboltable,0) + " and " +
                            parentClause2.toString(symboltable,0) + " -> " + resolvent.toString(symboltable,0));
                    insertClause(resolvent);
                    addClauseTask(resolvent);}
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
    protected Clause resolveBetweenBinaryClauses(final Clause clause1, final Clause clause2) throws Unsatisfiable{
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
            removeClause(clause1,true,true);
            removeClause(clause2,true,true);
            ++statistics.binaryResolvents;
            return null;}
        final Clause resolvent = new Clause(nextId.getAsInt(), literal1, literal2);
        if(binaryClauseIsSubsumed(resolvent) != null) return null;
        ++statistics.binaryResolvents;
        if(trackReasoning) resolvent.inferenceStep = new InfResolution(clause1,clause2,resolvent,symboltable);
        return resolvent;}

    /** checks if the clause is subsumed by another clause.
     *
     * @param clause a clause
     * @return null or the subsumer clause.
     */
    Clause isSubsumed(final Clause clause) {
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
    Clause binaryClauseIsSubsumed(final Clause subsumee) {
        assert subsumee.size() == 2;
        int literal1 = subsumee.literals.get(1).literal;
        Literal subsumerLiteral =
                literalIndexTwo.findLiteral(subsumee.literals.get(0).literal,
                    literalObject -> {
                        Clause subsumer = literalObject.clause;
                        return subsumer != subsumee &&
                            (subsumer.literals.get(0).literal == literal1 || subsumer.literals.get(1).literal == literal1);});
        return (subsumerLiteral != null) ?  subsumerLiteral.clause : null;}



    /** checks if the longer subsumee is subsumed by a binary subsumer.
     * <br>
     *  Only Or-clauses can be subsumed by binary clauses.
     * 
     * @param subsumee a longer clause
     * @return null or the binary subsumer clause.
     */
    Clause longerClauseIsSubsumedByBinaryClause(final Clause subsumee) {
        assert(subsumee.size() > 2);
        if(subsumee.limit > 1) return null; // binary clauses cannot subsume longer atleast-clauses
        for(Literal literalObject : subsumee.literals) {
            Literal subsumerLiteral = literalIndexTwo.findLiteral(literalObject.literal,
                    literalObjectTwo -> subsumee.findLiteral(literalObjectTwo.clause.otherLiteral(literalObjectTwo).literal) != null);
            if (subsumerLiteral != null) return subsumerLiteral.clause;}
        return null;}

    /** checks if the longer clause is subsumed by another longer clause.
     * <br>
     *  atleast n p^a,... subsumes atleast n- p^a+,...<br>
     *  where n- means &lt;= n and a+ means &gt;=a.<br>
     *  Notice that the timestampSubsumption is changed!
     *
     * @param subsumee a longer clause
     * @return null or the longer subsumer clause.
     */
    Clause longerClauseIsSubsumedByLongerClause(final Clause subsumee) {
        int size = subsumee.size();
        int limit = subsumee.limit;
        for(final Literal literalObject1 : subsumee.literals) {
            int multiplicity1 = literalObject1.multiplicity;
            Literal subsumerLiteral = literalIndexMore.findLiteral(literalObject1.literal,
                    literalObject2->{
                        Clause subsumer = literalObject2.clause;
                        if(subsumer == subsumee) return false;
                        int multiplicity2 = literalObject2.multiplicity;
                        if(subsumer.timestamp2 < timestampSubsumption) { // first candidate literal
                            if(subsumer.size() <= size && subsumer.limit >= limit && multiplicity2 <= multiplicity1) subsumer.timestamp2 = timestampSubsumption;
                            return false;}
                        else {
                            if(subsumer.timestamp2 - timestampSubsumption == subsumer.size()-2 && multiplicity2 <= multiplicity1) return true;
                            if(multiplicity2 <= multiplicity1) ++subsumer.timestamp2;}
                        return false;});
            if(subsumerLiteral != null) {timestampSubsumption += size+2; return subsumerLiteral.clause;}}
        timestampSubsumption += size+2;
        return null;}


    /** computes all resolvents between the given binary parent clause and the longer clauses.
     * <br>
     * If merge resolution is possible then merge resolutionis done:  p,q and -p,q,r,s -&gt; q,r,s<br>
     * The resolvent is simplified and checked for subsumption.<br>
     * A new task is generated.
     *
     * @param  binaryClause a binary clause.
     * @throws Unsatisfiable if a contradiction is discovered.
     */
    void saturateLongerClausesWithBinaryClause(final Clause binaryClause) throws Unsatisfiable {
        assert(binaryClause.size() == 2);
        String binaryClauseString = (trackReasoning || monitoring) ? binaryClause.toString(symboltable,0) : null;
        for(final Literal binaryLiteralObject : binaryClause.literals) {
            int otherBinaryLiteral = binaryClause.otherLiteral(binaryLiteralObject).literal;
            literalIndexMore.forAllLiterals(-binaryLiteralObject.literal,null,
                    longerLiteralObject -> {
                        Clause longerClause = longerLiteralObject.clause;
                        if(longerClause.isDisjunction) {
                            Literal otherLongerLiteralObject = longerClause.findLiteral(otherBinaryLiteral);
                            if(otherLongerLiteralObject != null) { // merge resolution is possible.
                                String longerClauseString = (trackReasoning || monitoring) ? longerClause.toString(symboltable,0) : null;
                                removeLiteralFromClause(longerLiteralObject,false);
                                InfResolution step = (trackReasoning || monitoring) ?
                                    new InfResolution(binaryClause, binaryClauseString, longerClause, longerClauseString, longerClause, symboltable) : null;
                                longerClause.inferenceStep = step;
                                if(monitoring) monitor.println(monitorId,step.info());
                                addClauseTask(longerClause);
                                ++statistics.mergeResolutionTwoMore;
                                return false;}}
                        resolve(binaryLiteralObject,longerLiteralObject);
                        return false;});}}


    /** computes all resolvents between the given longer parent clause and the binary clauses.
     * <br>
     * If merge resolution with the longer clause is possible, then merge resolution is done and the iteration stops.
     *
     * @param longerClause   a longer clause
     * @throws Unsatisfiable if a contradiction is discovered.
     */
    void saturateBinaryClausesWithLongerClause(final Clause longerClause) throws Unsatisfiable {
        String longerClauseString = (trackReasoning || monitoring) ? longerClause.toString(symboltable,0) : null;
        for(int i = 0; i < longerClause.size(); ++i) {
            Literal literalObject = longerClause.literals.get(i);
            if(literalIndexTwo.forAllLiterals(-literalObject.literal,null,
                    negLiteralObject-> {
                        Clause binaryClause = negLiteralObject.clause;
                        int otherBinaryLiteral = binaryClause.otherLiteral(negLiteralObject).literal;
                        if(longerClause.isDisjunction) {
                            Literal longerLiteralObject = longerClause.findLiteral(otherBinaryLiteral);
                            if(longerLiteralObject != null) { // merge resolution possible.
                                removeLiteralFromClause(literalObject,false);
                                InfResolution step = (trackReasoning || monitoring) ?
                                        new InfResolution(binaryClause, binaryClause.toString(symboltable,0), longerClause, longerClauseString, longerClause, symboltable) : null;
                                longerClause.inferenceStep = step;
                                if(monitoring) monitor.println(monitorId,step.info());
                                addClauseTask(longerClause);
                                ++statistics.mergeResolutionTwoMore;
                                return true;}}
                        resolve(literalObject,negLiteralObject);
                        return false;}))
                return;}}


    /** performs merge resolution between longer clauses.
     * <br>
     * P = atleast n  p^a,q^n,... and<br>
     * S = atleast m -p^b,q^n',...<br>
     * ----------------------------------<br>
     * atleast n+m-max(a,b) q^n+n',...
     * <br>
     * Destructive for S iff |S| &gt;= |P| and n' = n+m-max(a,b) for all literals in P <br>
     * Destructive for P iff |P| &gt;= |S| and n  = n+m-max(a,b) for all literals in S <br>
     *
     * @param clauseP a clause to be tested as parent clause for a merge resolution step.
     * @return        true if the clause itself has survived.
     * @throws Unsatisfiable if the simplification causes an Unsatisfiable exception.
     */
    protected boolean mergeResolutionMoreMore(final Clause clauseP) throws Unsatisfiable {
        int clausePSize = clauseP.literals.size();
        int limitP = clauseP.limit;
        try{
            for(final Literal literalObjectP : clauseP.literals) { // mark potential candidates with timestamp
                timestamp += clausePSize + 1;
                int literalPNeg = -literalObjectP.literal;
                literalIndexMore.timestampClauses(literalPNeg,
                        (literalObjectS -> literalObjectS.clause.size() <= clausePSize),timestamp,true);
                int i = 0;
                for(final Literal literalObjectPi : clauseP.literals) {
                    if(literalObjectPi == literalObjectP) continue;
                    int im = i++;
                    if(literalIndexMore.forAllLiterals(literalObjectPi.literal,
                        literalObjectSi -> literalObjectSi.clause.timestamp1 - timestamp == im,
                        literalObjectSi -> {
                                Clause clauseS = literalObjectSi.clause;  // this is the potential merge partner.
                                Literal literalObjectSNeg = clauseS.findLiteral(literalPNeg);
                                int newLimit = clauseP.limit + clauseS.limit -
                                    Math.max(literalObjectP.multiplicity, literalObjectSNeg.multiplicity);
                                if(literalObjectSi.multiplicity == newLimit) {
                                    ++clauseS.timestamp1;
                                    if(clauseS.timestamp1 - timestamp == clausePSize-1) { // mergepartner found
                                        ++statistics.mergeResolutionMoreMore;
                                        boolean destructive = clauseP.isDisjunction && clauseS.isDisjunction;
                                        if(!destructive) {
                                            destructive = true;
                                            for(final Literal litObjectSi : clauseS.literals) {
                                                int litSi = litObjectSi.literal;
                                                if(litSi != literalPNeg && clauseP.findLiteral(litSi) != null) {
                                                    destructive &= litObjectSi.multiplicity == newLimit;}}}
                                        String resolventBefore = trackReasoning ? clauseS.toString(symboltable,0) : null;
                                        boolean removeP = limitP == 1 && clauseS.size() == clausePSize;
                                        if(destructive) {
                                            if(removeLiteralFromClause(literalObjectSNeg,false)) {
                                                addClauseTask(clauseS);
                                                InfMergeResolutionMore step = (trackReasoning || monitoring) ?
                                                        new InfMergeResolutionMore(clauseP,resolventBefore,clauseS,symboltable) : null;
                                                if(trackReasoning) clauseS.inferenceStep = step;
                                                if(monitoring) monitor.println(monitorId,step.info());}
                                            if(removeP) { // only a disjunction is definitely subsumed and can be removed.
                                                removeClause(clauseP,true,true); return true;}}
                                        else resolve(literalObjectP,literalObjectSNeg);
                                        return false;}}
                                return false;})) return clauseP.exists;}}
                return clauseP.exists;}
        finally{timestamp += clausePSize + 1;}}

    /** This method tries to find a triggered equivalence with the given three-literal disjunction.
     * <br>
     *  Clause: p, q, r <br>
     *  Clause: p,-q,-r<br>
     *  -------------------
     *  -p -&gt; q == -r
     *
     * @param clause a three-literal disjunction.
     * @return the number of discovered equivalences.
     * @throws Unsatisfiable if the equivalence contradicts another equivalence.
     */
    int findTriggeredEquivalence(Clause clause) throws Unsatisfiable {
        assert clause.isDisjunction && clause.size() == 3;
        try{int counter = 0;
            for(Literal triggerLiteralObject : clause.literals) {
                timestamp += 2;
                int triggerLiteral = triggerLiteralObject.literal;
                Literal literalObject1 = null; Literal literalObject2 = null;
                for(Literal literalObject : clause.literals) { // identify the three literals in the clause
                    if(literalObject != triggerLiteralObject) {
                        if(literalObject1 == null) literalObject1 = literalObject;
                        else literalObject2 = literalObject;}}

                // timestamp all candidate clauses with the trigger literal.
                if(!literalIndexMore.timestampClauses(triggerLiteral,
                       (candidateLiteral -> {
                           Clause candidateClause = candidateLiteral.clause;
                           return candidateClause.isDisjunction && candidateClause.size() == 3;}),
                       timestamp,true)) continue; // there can't be an equivalence with this trigger literal.

                // timestamp the first equivalence literal.
                if(!literalIndexMore.timestampClauses(-literalObject1.literal,
                        (candidateLiteral -> candidateLiteral.clause.timestamp1 == timestamp),
                        timestamp+1,true)) continue; // there can't be an equivalence with this trigger literal.

                Literal literalObjectFirst = literalObject1;
                if(literalIndexMore.forAllLiterals(-literalObject2.literal,
                        literalObjectSecond -> literalObjectSecond.clause.timestamp1 == timestamp+1,
                        literalObjectSecond -> {
                            InfEquivalence step = (trackReasoning || monitoring) ?
                                    new InfEquivalence(-triggerLiteral,literalObjectFirst,literalObjectSecond, symboltable) : null;
                            if(monitoring) monitor.println(monitorId,step.info(symboltable));
                            ++statistics.triggeredEquivalences;
                            equivalences.add(-triggerLiteral,literalObjectFirst.literal , literalObjectSecond.literal,step);
                            addEquivalenceTask();
                            return true;})) ++counter;}
            return counter;}
        finally {timestamp += 2;}}

   
    /** creates all resolvents with the given clause which are not longer than the clause itself.
     * <br>
     * This may generate a lot of clauses, but their length is bounded by the length of the existing clauses.
     *
     * @param clause        a 3-literal clause.
     * @throws Unsatisfiable if a contradiction is discovered.
     */
    void mergeResolutionPartial(final Clause clause) throws Unsatisfiable{
        int size = clause.size();
        for(final Literal resolutionLiteralObject : clause.literals) {
            literalIndexTwo.forAllLiterals(-resolutionLiteralObject.literal,null, // resolution with binary clauses.
                    negLiteralObject -> {resolve(resolutionLiteralObject,negLiteralObject); return false;});
            timestamp += size+2;
            int posLiteral = resolutionLiteralObject.literal;
            literalIndexMore.timestampClauses(-posLiteral,  // timestamp all potential resolution partners.
                    (litObject -> litObject.clause.size() <= size), timestamp,true);

            for(final Literal literalObject1 : clause.literals) {
                if(literalObject1 == resolutionLiteralObject) continue;
                literalIndexMore.forAllLiterals(literalObject1.literal,null,
                        (litObject -> { // all literals except two of them must merge.
                            Clause otherClause = litObject.clause;
                            if(otherClause.timestamp1 - timestamp == otherClause.size()-3)
                                resolve(resolutionLiteralObject,otherClause.findLiteral(-posLiteral));
                            else ++otherClause.timestamp1;
                            return false;}));}}
            timestamp += size+2;}


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
    void resolve(final Literal posLiteral,final Literal negLiteral) throws Unsatisfiable {
        trueLiterals.clear();
        final Clause resolvent = Clause.resolve(posLiteral,negLiteral,nextId,(trueLiterals::add));
        for(int literal : trueLiterals) {
            ++statistics.derivedTrueLiterals;
            addInternalTrueLiteralTask(literal,true,
                    trackReasoning ? new InfResolutionTrueLiteral(posLiteral.clause,negLiteral.clause,literal,symboltable): null);
            if(monitoring) monitor.println(monitorId, posLiteral.clause.toString(symboltable,0) + " and " +
            negLiteral.clause.toString(symboltable,0) + " -> true(" + Symboltable.toString(literal,symboltable) + ")");}
        if(resolvent == null || isSubsumed(resolvent) != null) return;
        if(trackReasoning) resolvent.inferenceStep =
                new InfResolution(posLiteral.clause,negLiteral.clause, resolvent, symboltable);
        insertClause(resolvent);
        clauses.updateClauseNumbers(resolvent,1);
        if(resolvent.size() == 2) ++statistics.binaryResolvents; else ++statistics.longerResolvents;
        if(monitoring) monitor.println(monitorId,
                    "Resolution: " + posLiteral.clause.toString(symboltable,0) + " and " +
                            negLiteral.clause.toString(symboltable,0) + " -> " +
                            resolvent.toString(symboltable,0));
        addClauseTask(resolvent);}


    /** adds a ClauseTask (ProcessBinaryClause or ProcessLongerClause) depending on the clause's size.
     *
     * @param clause a clause.
     */
    void addClauseTask(final Clause clause) {
        final TaskType type = (clause.size() == 2) ? TaskType.ProcessBinaryClause: TaskType.ProcessLongerClause;
        if(type == TaskType.ProcessBinaryClause) ++binaryClausesInQueue;
        synchronized (this) {queue.add(new Task<>(type, clause));}}



    /** for collecting literals.*/
    private final IntArrayList removedLiterals = new IntArrayList();
    Unsatisfiable[] unsatisfiables = new Unsatisfiable[1];

    /** replaces for each equivalence and triggered equivalence all occurrences of the equivalent literals with their representative.
     * <br>
     * The changed clauses are simplified as far as possible. <br>
     * The equivalences are put into the backup array such that they can be used to complete a model.
     *
     * @throws Unsatisfiable if inconsistent true literals are derived.
     */
    private void processEquivalences() throws Unsatisfiable {
        removedLiterals.clear();
        unsatisfiables[0] = null;
        equivalences.equivalences.forEach((triggerLiteral,equivalenceList) -> {
            try{
                for(Equivalence eqv : equivalenceList) {
                    for(int i = 0; i < eqv.literals.size(); ++i) {
                        int literal = eqv.literals.getInt(i);
                        InferenceStep step = eqv.inferenceSteps.get(i);
                        if(triggerLiteral == 0)
                              processUntriggerdEquivalence(eqv.representative,literal,step);
                        else  processTriggeredEquivalence(triggerLiteral,eqv.representative,literal,step);}}}
            catch(Unsatisfiable unsatisfiable) {unsatisfiables[0] = unsatisfiable;}});
        if(unsatisfiables[0] != null) throw unsatisfiables[0];
        for(int removedLiteral : removedLiterals) checkPurity(removedLiteral);
        equivalences.backupEquivalences();}


    /** This method replaces in all clauses the given literal by the given representative.
     * The new clauses are simplified as far as possible.<br>
     * Derived true literals are inserted into the model.
     *
     * @param representative  the representative of an equivalence class.
     * @param literal         the literal of the equivalence class.
     * @param equivalenceStep which caused the equivalence.
     * @throws Unsatisfiable         if a contradiction is encountered.
     */
    void processUntriggerdEquivalence(int representative, int literal, final InferenceStep equivalenceStep) throws Unsatisfiable {
        for(int sign = 1; sign >= -1; sign -=2) {
            Literals literalIndex = literalIndexTwo;
            while(literalIndex != null) {
                Literals litIndex = literalIndex; int sgn = sign;
                literalIndex.forAllLiterals(sign*literal,null,
                        (literalObject -> {
                            ++statistics.equivalenceReplacements;
                            replaceLiteral(literalObject,sgn*representative,litIndex,equivalenceStep);
                            return false;}));
                literalIndex = (literalIndex == literalIndexTwo) ? literalIndexMore : null;}}}

    /** replaces the given literal by the given representative in clauses containing the negated triggerLiteral.
     * <br>
     * All possible simplifications are done in the changed clauses.
     *
     * @param triggerLiteral  any literal.
     * @param representative  any literal != triggerLiteral.
     * @param literal         any literal != representative and /= triggerLiteral
     * @param equivalenceStep null or the inference step that caused the triggered equivalence
     * @throws Unsatisfiable  if the replacement causes true literals to be derived which contradict the model.
     */
    void processTriggeredEquivalence(final int triggerLiteral, final int representative, final int literal, final InferenceStep equivalenceStep) throws Unsatisfiable {
        literalIndexMore.forAllLiterals(-triggerLiteral,null,
                (literalObject -> {
                    Clause clause = literalObject.clause;
                    int sign = 0;
                    for(Literal litObject : clause.literals) {
                        if(litObject.literal == literal)           {sign = 1;}
                        else if(literalObject.literal == -literal) {sign = -1;}
                        if(sign != 0) {
                            ++statistics.equivalenceReplacementsTriggered;
                            replaceLiteral(litObject,sign*representative,literalIndexMore, equivalenceStep);
                            break;}}
                    return false;}));}

    /** replaces the literal (in literalObject) by another literal (representative), typically equivalence replacement.
     * <br>
     * If the clause becomes a tautology or is subsumed, it is removed.<br>
     * If the clause is not a disjunction, it is simplified after replacement. <br>
     * True literals may be derived, which are put into the global model,<br>
     * and generate a trueLiteral task.<br>
     * If the clause became a two-literal clause, it is put into the literalIndexTwo index.<br>
     * The modified clause generates a derivedClause task.<br>
     * The clause numbers are updated.
     *
     * @param literalObject    any literalObject
     * @param representative   a literal
     * @param literalIndex     the corresponding literal index.
     * @param equivalenceStep  null or the inference step that caused the equivalence of the literal with the representative.
     * @throws Unsatisfiable   if derived literals cause a contradiction.
     */
    void replaceLiteral(final Literal literalObject, final int representative, final Literals literalIndex, final InferenceStep equivalenceStep) throws Unsatisfiable {
        int literal = literalObject.literal;
        Clause clause = literalObject.clause;
        clauses.updateClauseNumbers(clause,-1);
        int size = clause.size();
        String clauseString = (trackReasoning | monitoring) ? clause.toString(symboltable,0) : null;
        trueLiterals.clear();
        int status = Clause.replaceLiteral(literalObject,representative,
                (l->{literalIndex.removeLiteral(l);
                    if(!removedLiterals.contains(l.literal)) removedLiterals.add(l.literal);}),
                literalIndex::addLiteral, trueLiterals::add);
        if(status == 1 && trueLiterals.isEmpty()) {removeClause(clause,false,false); return;} // new clause is a tautology
        for(int trueLiteral : trueLiterals) { // the representative in the changed clause becomes true, the literal itself must also be true.
            if(trueLiteral == representative && !trueLiterals.contains(literal)) trueLiterals.add(literal);}
        InfEquivalenceReplacement step = (trackReasoning || monitoring) ?
                new InfEquivalenceReplacement(clauseString, clause, trueLiterals, representative, literal, equivalenceStep, symboltable) : null;
        if(monitoring) {monitor.println(monitorId,step.info(symboltable));}
        for(int trueLiteral : trueLiterals) addInternalTrueLiteralTask(trueLiteral,true,step);
        if(status == 1) {removeClause(clause,false,false); return;}
        if(status == 0 && isSubsumed(clause) != null) {
            ++statistics.subsumedClauses;
            removeClause(clause,false,false);
            return;}
        if(size > 2 && clause.size() == 2) moveToIndexTwo(clause);
        clauses.updateClauseNumbers(clause,+1);
        addClauseTask(clause);}

    /** inserts a clause into the internal lists.
     *
     * @param clause a clause.
     */
    protected void insertClause(final Clause clause) {
        Literals literalIndex = (clause.size() == 2) ? literalIndexTwo : literalIndexMore;
        for(Literal literalObject : clause.literals) literalIndex.addLiteral(literalObject);
        clauses.addClause(clause);}


    /** removes the clause from the internal lists.
     *
     * @param clause  a clause to be removed.
     * @param checkPurity if true then the clause's literals are checked for purity.
     * @param literalIndex where the literals are.
     * @param updateNumbers if true then the clause numbers are updated.
     * @throws Unsatisfiable should not happen.
     */
    protected void removeClause(final Clause clause, final boolean checkPurity, final Literals literalIndex, final boolean updateNumbers) throws Unsatisfiable {
        clauses.removeClause(clause);
        for(Literal literalObject : clause.literals) {
            literalIndex.removeLiteral(literalObject);
            if(checkPurity) {checkPurity(literalObject.literal); checkPurity(-literalObject.literal);}}
        if(updateNumbers) clauses.updateClauseNumbers(clause,-1);
        if(checkConsistency) checkConsistency();}

    /** removes the clause from the internal lists.
     *
     * @param clause  a clause to be removed.
     * @param checkPurity if true then the clause's literals are checked for purity.
     * @param updateNumbers if true then the clause numbers are updated.
     * @throws Unsatisfiable should not happen.
     */
    protected void removeClause(final Clause clause, final boolean checkPurity, final boolean updateNumbers) throws Unsatisfiable {
        removeClause(clause,checkPurity,(clause.size() == 2) ? literalIndexTwo :literalIndexMore, updateNumbers);}


    void removeClauses (final int predicate) throws Unsatisfiable {
        for(int sign = 1; sign >= -1; sign-=2) {
            int literal = sign*predicate;
            Literals literalIndex = literalIndexTwo;
            while(literalIndex != null) {
                Literal literalObject = literalIndex.getFirstLiteralObject(literal);
                while(literalObject != null) {
                    Clause clause = literalObject.clause;
                    if(clause == null) {literalObject = literalObject.nextLiteral; continue;}
                    removeClause(clause,false,true);
                    literalObject = literalObject.nextLiteral;}
                literalIndex = (literalIndex == literalIndexTwo) ? literalIndexMore : null;}}}

    /** removes the literal from the clause and from the corresponding index.
     * If the literal becomes pure, it is inserted into the model.<br>
     * If the limit is reduced to 0, the entire clause is removed.<br>
     * If the clause becomes a two-literal clause, it is moved to the two-literal index<br>
     * Clauses.updateClauseNumbers is called.
     *
     * @param literalObject the literal object to be removed.
     * @param reduceLimit if true then the clause's limit is reduced by the literal's multiplicity.
     * @return true if the clause still exists.
     * @throws Unsatisfiable if inserting a pure literal into the model causes a contradiction.
     */
    protected boolean removeLiteralFromClause(final Literal literalObject, final boolean reduceLimit) throws Unsatisfiable {
        Clause clause = literalObject.clause;
        trueLiterals.clear();
        clauses.updateClauseNumbers(clause,-1);
        Literals literalIndex = (clause.size() == 2) ? literalIndexTwo : literalIndexMore;
        byte status = clause.removeLiterals((l-> (l == literalObject) ? (reduceLimit ? +1:-1) : 0),
                                           (l -> literalIndex.removeLiteral(literalObject)),
                                           (trueLiterals::add));
        if(status == -1) throw new UnsatEmptyClause(problemId,solverId,clause.identifier(),null);
        for(int literal :trueLiterals) addInternalTrueLiteralTask(literal,true,null);
        if(status == 1) {clauses.removeClause(clause); return false;}
        if(clause.size() == 2) {moveToIndexTwo(clause);}
        checkPurity(literalObject.literal); // pure literals are just added to the model.
        if(checkConsistency) checkConsistency();
        clauses.updateClauseNumbers(clause,+1);
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
    protected boolean checkPurity(final int literal) throws Unsatisfiable {
        if(localStatus(literal) != 0) return false;
        if(literalIndexTwo.isEmpty(literal) && literalIndexTwo.isEmpty(-literal) &&
                literalIndexMore.isEmpty(literal) && literalIndexMore.isEmpty(-literal)) return false;
        if(literalIndexTwo.isEmpty(-literal) && literalIndexMore.isEmpty(-literal)) {
            if(monitoring) monitor.println(monitorId,"Pure Literal: " + Symboltable.toString(literal,symboltable));
            addInternalTrueLiteralTask(literal, false,trackReasoning ? new InfPureLiteral(literal,false) : null);
            ++statistics.pureLiterals; return true;}
        return false;}


    /** adds the literals in the clause which are locally true to the trueLiterals array.
     *
     * @param clause a clause.
     */
    void collectTrueLiterals(Clause clause) {
        trueLiterals.clear();
        for(Literal literalObject : clause.literals) {
            int literal = literalObject.literal;
            switch(localStatus(literal)) {
                case  1: trueLiterals.add(literal);  break;
                case -1: trueLiterals.add(-literal); break;}}}

    /** moves a longer clause which has become a binary clause to the literalIndexTwo.
     * <br>
     * To this end the literals are copied.
     * Therefor an iteration over the old literals can still proceed.
     *
     * @param clause a binary clause.
     */
    protected void moveToIndexTwo(final Clause clause) {
        assert(clause.size() == 2);
        for(int i = 0; i < 2; ++i) {
            Literal oldLiteralObject = clause.literals.get(i);
            Literal newLiteralObject = new Literal(oldLiteralObject.literal,oldLiteralObject.multiplicity);
            newLiteralObject.clause = clause;
            literalIndexMore.removeLiteral(oldLiteralObject);
            literalIndexTwo.addLiteral(newLiteralObject);
            clause.literals.set(i,newLiteralObject);}}




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
    void processElimination(final Task task) throws Unsatisfiable {
        if(monitoring) monitor.println(monitorId, "Starting ProcessElimination ");
        boolean atEnd = false;
        try{
            for(int predicate = 1; predicate <= predicates; ++predicate) {
                if(localModel[predicate] != 0) continue;
                int sizeP = literalIndexMore.size(predicate,2);
                int sizeN = literalIndexMore.size(-predicate,2);
                if(sizeP == 0 && sizeN == 0) continue;
                if(sizeP == 0) {
                    removeClauses(predicate);
                    ++statistics.partiallyPureLiterals;
                    if(monitoring) monitor.println(monitorId,"Literal " +
                                Symboltable.toString(-predicate,symboltable) + " is pure in the longer clauses.");
                    makeLocallyTrue(-predicate);
                    continue;}
                if(sizeN == 0) {
                    removeClauses(predicate);
                    ++statistics.partiallyPureLiterals;
                    if(monitoring) monitor.println(monitorId,"Literal " +
                                Symboltable.toString(predicate,symboltable) + " is pure in the longer clauses.");
                    makeLocallyTrue(predicate);
                    continue;}
                if(sizeP < 0 || sizeN < 0) continue; // too many occurrences

                // resolution between one or two clauses with positive predicate and one or two clauses with negative predicate.
                ArrayList<Literal> literalsP = new ArrayList<>();
                ArrayList<Literal> literalsN = new ArrayList<>();
                Literal literalObject = literalIndexMore.getFirstLiteralObject(predicate);
                while(literalObject != null) {literalsP.add(literalObject); literalObject = literalObject.nextLiteral;}
                literalObject = literalIndexMore.getFirstLiteralObject(-predicate);
                while(literalObject != null) {literalsN.add(literalObject); literalObject = literalObject.nextLiteral;}
                for(Literal literalP : literalsP) {
                    for(Literal literalN: literalsN) {
                        ++statistics.longerResolvents;
                        resolve(literalP,literalN);}}
                for(Literal literalP : literalsP) {Clause clause = literalP.clause; removeClause(clause,false,false); literalP.clause = clause;}
                for(Literal literalN : literalsN) {Clause clause = literalN.clause; removeClause(literalN.clause,false,false);literalN.clause = clause;}
                eliminatedPredicates.add(literalsP); eliminatedPredicates.add(literalsN);
                if(monitoring) monitor.println(monitorId,"Predicate Eliminated by Resolution: " +
                        Symboltable.toString(predicate,symboltable));
                return;}
            atEnd = true;}
        finally {
            if(monitoring) monitor.println(monitorId, "Ending ProcessElimination ");
            if(!atEnd) synchronized (this) {queue.add(task);}}}

    /** completes the local model for the clauses whose predicates have been eliminated with exhaustive resolution.
     */
    void completeModel() {
        for(int i = eliminatedPredicates.size()-1; i >= 0; --i) {
            for(Literal literalObject : eliminatedPredicates.get(i)) {
                Clause clause = literalObject.clause;
                int trueLiterals = 0;
                for(Literal litObject : clause.literals) {
                    if(localStatus(litObject.literal) == 1) ++trueLiterals;}
                if(trueLiterals < clause.limit)
                    for(Literal  litObject : clause.literals) {
                        if(localStatus(litObject.literal) == 0) {
                            makeLocallyTrue(litObject.literal);
                            if(++trueLiterals == clause.limit) break;}}}}
        Unsatisfiable unsatisfiable = equivalences.completeModel(literal -> (int)localStatus(literal),this::makeLocallyTrue);
        if(unsatisfiable != null) {
            System.out.println("Contradiction in completed model. Should not happen!");
            System.out.println(Arrays.toString(localModel));
            System.out.println(unsatisfiable.description(symboltable));
            System.exit(1);}
    }


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
            model.exchangeModel(localModel);
            throw new Satisfiable(problemId,solverId,model);}}

    /** generates a local model from the positive and mixed positive literals in the clauses.*/
    void generatePositiveModel() {
        if(monitoring) monitor.println(monitorId, "Generating Positive Model");
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
        if(monitoring) monitor.println(monitorId,"Generating Negative Model");
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
                if(clause.size() == size) {System.out.println(clause.toString(symboltable,0));}
                clause = clause.nextClause;}}}

    /** returns the statistics.
     *
     * @return the statistics.
     */
    @Override
    public Statistic getStatistics() {
        return statistics;}
}
