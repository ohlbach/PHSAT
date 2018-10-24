package Coordinator;

import Algorithms.Algorithms;
import Datastructures.Clauses.BasicClauseList;
import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseList;
import Datastructures.Literals.CLiteral;
import Datastructures.Results.Result;
import Datastructures.Results.Satisfiable;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Statistics.DataStatistics;
import Datastructures.Statistics.Statistic;
import Datastructures.Theory.DisjointnessClasses;
import Datastructures.Theory.EquivalenceClasses;
import Datastructures.Theory.ImplicationDAG;
import Datastructures.Theory.Model;
import Management.GlobalParameters;
import Management.Monitor;
import Management.ProblemSupervisor;

import java.util.*;
import java.util.concurrent.PriorityBlockingQueue;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

/**This is the superclass for PreProcessor, CentralProcessor and all Solver classes.<br>
 * It provides the following common functionalities: <br>
 *     - data which is common to all processors <br>
 *     - monitors for the central data<br>
 *     - tasks for simplifying clauses <br>
 *     - the simplifications on new clauses.
 * <p> The system distinguishes the <strong>core data structures</strong>
 *     and the <strong> supplementary data structures.</strong> <br>
 *     The core data structures are: model (unit clauses), implicationDAG (binary clauses) and clauses (longer clauses)<br>
 * Among the supplementary data structures are the equivalence classes and the disjointness classes.
 * They are constructed by the PreProcessor and maintained by the CentralProcessor.
 * <p>
 *     An important component is the <strong>taskQueue</strong>.
 *     It keeps tasks in a priority list.<br>
 *     Tasks are: <br>
 *         - processing unit clauses<br>
 *         - processing binary clauses <br>
 *         - simplifying longer clauses (subsumption and replacement resolution) <br>
 *         - processing pure literals <br>
 *         - reporting results (satisfiability, unsatisfiablity, errors)
 * <p>
 * Created by ohlbach on 10.10.2018.
 */
public abstract class Processor {
    /** the processor id */
    public String id;
    /** the problemId, mainly for printing information */
    public String problemId;
    /** the number of predicates in the system */
    public int predicates;
    /** the supervisor for the problem */
    public ProblemSupervisor supervisor;
    /** general parameters for the overall management */
    public GlobalParameters globalParameters;
    /** either the problemParameters or the solverParameters */
    public HashMap<String,Object> applicationParameters;
    /** the input clauses */
    public BasicClauseList basicClauseList = null;
    /** the clauses (disjunctions) for the processor */
    public ClauseList clauses = null;
    /** the local model */
    public Model model = null;
    /** the local implication DAG */
    public ImplicationDAG      implicationDAG = null;
    /** the equivalence classes (optinally) */
    public EquivalenceClasses  equivalences   = null;
    /** the disjointness classes (optionally) */
    public DisjointnessClasses disjointnesses = null;
    /** the monitor (optionally) */
    public Monitor monitor = null;
    /** indicates if monitoring is enabled */
    public boolean monitoring = false;
    /** the statistics for the processor*/
    public Statistic statistics = null;

    /** constructs a processor and initializes the common fields
     *
     * @param supervisor          which manages the problem
     * @param globalParameters    for overall management
     * @param applicationParameters either the problemParameters or the solverParameters
     * @param basicClauseList     the input clauses (n particular for checking a candidate model)
     */
    public Processor(ProblemSupervisor supervisor, GlobalParameters globalParameters,
                     HashMap<String,Object> applicationParameters,BasicClauseList basicClauseList) {
        id = (String)applicationParameters.get("name");
        problemId = supervisor.problemId;
        this.predicates            = basicClauseList.predicates;
        this.supervisor            = supervisor;
        this.globalParameters      = globalParameters;
        this.applicationParameters = applicationParameters;
        this.basicClauseList       = basicClauseList;
        monitor                    = globalParameters.monitor;
        monitoring                 = monitor.monitoring();}


    protected Consumer<CLiteral>            longClauseObserver = cLiteral    -> addTask(makeShortenedClauseTask(cLiteral.clause));
    protected Consumer<Integer>             oneLiteralObserver = literal     -> addTask(new Task.OneLiteral(literal,this));
    protected BiConsumer<Integer,Integer>  implicationObserver = (from,to)   -> addTask(new Task.TwoLiteral(-from,to,this));
    protected Consumer<int[]>              equivalenceObserver = equivalence -> addTask(new Task.Equivalence(equivalence,this));
    protected Consumer<Unsatisfiable> unsatisfiabilityObserver = unsat       -> addTask(new Task.Unsatisfiability(unsat,this));
    protected Consumer<Satisfiable>     satisfiabilityObserver = sat         -> addTask(new Task.Satisfiability(sat,this));

    protected Consumer<CLiteral> literalRemovalMonitor = cLiteral ->
            monitor.print(id,"Literal " + cLiteral.literal + " removed from clause " + cLiteral.clause.id);
    protected Consumer<Integer> trueLiteralMonitorDAG =  literal ->
            monitor.print(id,"Literal " + literal + " became true in the implication DAG.");
    protected BiConsumer<Integer,Integer> implicationMonitor =  (from, to) ->
            monitor.print(id,"New implication " + from + " -> " + to + " derived.");
    protected Consumer<int[]> equivalenceMonitor = equivalence ->
            monitor.print(id,"Equivalent literals " + Arrays.toString(equivalence) + " derived.");
    protected Consumer<Integer> trueLiteralMonitorEQV =  literal ->
            monitor.print(id,"Literal " + literal + " became true in the equivalence classes.");
    protected Consumer<Unsatisfiable> unsatisfiabilityMonitorEQV =   unsat ->
            monitor.print(id,"Unsatisfiability in equivalences detected.");
    protected Consumer<Unsatisfiable> unsatisfiabilityMonitorDIS =   unsat ->
            monitor.print(id,"Unsatisfiability in disjointnessesd detected.");
    protected Consumer<Integer> trueLiteralMonitorDIS =  literal ->
            monitor.print(id,"True literal " + literal + " in disjointnesses derived.");

    /** This method adds the monitors to the core data structures (clauses,implicationDAG,
     * disjointness classes (if there are some) and equivalence classes (if there are some).
     *
      * @param info for later printout.
     */
    protected void addMonitors(String info) {
        if(!monitoring) {return;}
        monitor.addThread(id,info);
        clauses.addLiteralRemovalObserver(literalRemovalMonitor);
        implicationDAG.addTrueLiteralObserver(trueLiteralMonitorDAG);
        implicationDAG.addImplicationObserver(implicationMonitor);
        implicationDAG.addEquivalenceObserver(equivalenceMonitor);
        if(equivalences != null) {
            equivalences.addTrueLiteralObserver( trueLiteralMonitorEQV);
            equivalences.addUnsatisfiabilityObserver(unsatisfiabilityMonitorEQV);}
        if(disjointnesses != null) {
            disjointnesses.addUnsatisfiabilityObserver(unsatisfiabilityMonitorDIS);
            disjointnesses.addTrueLiteralObserver(trueLiteralMonitorDIS);}}

    /** This method removes the monitors from the core data structures. <br>
     *  It needs to be called unless the core data structures become garbage anyway. */
    protected void removeMonitors() {
        if(!monitoring) {return;}
        clauses.removeLiteralRemovalObserver(literalRemovalMonitor);
        implicationDAG.removeTrueLiteralObserver(trueLiteralMonitorDAG);
        implicationDAG.removeImplicationObserver(implicationMonitor);
        implicationDAG.removeEquivalenceObserver(equivalenceMonitor);
        if(equivalences != null) {
            equivalences.removeTrueLiteralObserver( trueLiteralMonitorEQV);
            equivalences.removeUnsatisfiabilityObserver(unsatisfiabilityMonitorEQV);}
        if(disjointnesses != null) {
            disjointnesses.removeUnsatisfiabilityObserver(unsatisfiabilityMonitorDIS);
            disjointnesses.removeTrueLiteralObserver(trueLiteralMonitorDIS);}}


    /** The taskQueue maintains the tasks according to their priority */
    protected PriorityBlockingQueue<Task> taskQueue =
            new PriorityBlockingQueue<Task>(10,Comparator.comparingInt(task->task.priority));

    /** adds a task to the task queue
     *
     * @param task the task to be added
     */
    public void addTask(Task task) {taskQueue.add(task);}

    /** generates a task according to the clause's length (empty clauses, unit clauses, binary clauses and longer ones).<br>
     *
     * @param clause a clause (maybe empty)
     * @return  the corresponding task to be added to the task queue
     */
    protected Task makeShortenedClauseTask(Clause clause) {
        return makeShortenedClauseTask(clause,this);}

    /** generates a task according to the clause's length (empty clauses, unit clauses, binary clauses and longer ones).<br>
     *
     * @param clause a clause (maybe empty)
     * @param processor which has to process the task.
     * @return  the corresponding task to be added to the task queue
     */
    protected Task makeShortenedClauseTask(Clause clause, Processor processor) {
        switch(clause.size()) {
            case 0:  return new Task.Unsatisfiability(new Unsatisfiable("Clause " + clause.id + " became empty"),processor);
            case 1: clauses.removeClause(clause);
                return new Task.OneLiteral(clause.getLiteral(0),processor);
            case 2: clauses.removeClause(clause);
                return new Task.TwoLiteral(clause.getLiteral(0),clause.getLiteral(1),processor);
            default: return new Task.ShortenedClause(clause,processor);}
    }


    /** processes all task until a result is obtained or the queue becomes empty
     *
     * @return  Un/Satisfiable if there was a result, otherwise null.
     */
    public Result processTasks() {
        while(!taskQueue.isEmpty()) {
            Task task = taskQueue.poll();
            if(task.ignore) {continue;}
            Result result = task.execute();
            if(result != null) {return result;}}
        return null;}

    /** generates a task for processing pure literals */
    protected Consumer<Integer> purityObserver = literal -> {
        if(implicationDAG.isEmpty(literal)) {taskQueue.add(new Task.Purity(literal,this));}};

    /* Here we have the process-methods
       ******************************** */

    /** computes the consequences of a new unit clause in the clauses and the implicationDAG.<br>
     *  Equivalence classes need not be changed because it is assumed that all their literals<br>
     *  are already replaced by their representatives<br>
     *  Disjointness classes need not be changed because the information is also in the implicationDAG
     *
     * @param literal a newly derived unit clause.
     * @return Unsatisfiable if a contradiction has been discovered, otherwise null
     */
    public Result processOneLiteralClause(int literal) {
        int status = model.add(literal);
        if(status == -1) {
            Unsatisfiable result = new Unsatisfiable(model,literal);
            taskQueue.add(new Task.Unsatisfiability(result,this)); return result;}
        if(status == 1) {return null;}
        clauses.makeTrue(literal);
        implicationDAG.newTrueLiteral(literal);
        return processQueue(literal);}

    private Result processQueue(int literal) {
        ArrayList<Task> tasks = new ArrayList<>();
        for(Task task : taskQueue) {if(task.makeTrue(literal,tasks)) {return new Unsatisfiable(model,literal);}}
        for(Task task : tasks) {taskQueue.add(task);}
        return null;}


    /** simplifies all clauses with the new two-literal clause and inserts it into the implicationDAG<br>
     * This may cause new unit clauses, binary clauses and other shortened causes to be generated
     * and put into the task queue.
     *
     * @param literal1 a literal
     * @param literal2 a literal
     * @return null
     */
    public Result processTwoLiteralClause(int literal1, int literal2){
        int status = model.status(literal1);
        if(status == 1) {return null;}  // is already true
        if(status == -1) {taskQueue.add(new Task.OneLiteral(literal2,this)); return null;}
        status = model.status(literal2);
        if(status == 1) {return null;}  // is already true
        if(status == -1) {taskQueue.add(new Task.OneLiteral(literal1,this)); return null;}
        Algorithms.simplifyWithImplication(-literal1,literal2,clauses,implicationDAG);
        implicationDAG.addClause(literal1,literal2);
        return null;}

    /** performs simplifications with new or shortened clauses:<br>
     * - all subsumed clauses are removed <br>
     * - all replacement resolutions (with the implicationDAG) are performed.<br>
     * This may generate further tasks in the task queue.
     *
     * @param clause a new or simplified clause.
     * @return null
     */
    public Result processLongerClause(Clause clause){
        if(clause.removed) {return null;}
        int subsumed = Algorithms.subsume(clause,clauses,implicationDAG);
        int resolved = Algorithms.resolve(clause,clauses,implicationDAG);
        ((DataStatistics)statistics).TSK_subsumed += subsumed;
        ((DataStatistics)statistics).TSK_resolved += resolved;
        return null;}

    /**
     *
     * @param equivalents  an equivalence class [representative, literal_1,...]
     * @return null
     */
    public Result processEquivalence(int[] equivalents) {
        Clause eqClass = equivalences.addEquivalence(equivalents);
        if(eqClass == null) {return null;}
        int representative = eqClass.getLiteral(0);
        int start = representative == equivalents[0] ? 1: 0;
        for(int i = start; i < equivalents.length; ++i) {
            clauses.replaceByRepresentative(representative,equivalents[i]);}
        return null;}

    /** makes pure literals true and removes them from the clauses and the implicationDAG.
     *
     * @param literal a pure literal
     * @return null or Satisfiable
     */
    public Result processPurity(int literal) {
        int status = model.add(literal);
        if(status == -1) {
            Unsatisfiable result = new Unsatisfiable(model,literal);
            taskQueue.add(new Task.Unsatisfiability(result,this));
            return result;}
        if(status == 1) {return null;}
        clauses.removeLiteral(literal);
        implicationDAG.newTrueLiteral(literal);
        if(clauses.isEmpty()) {return Result.makeResult(model,basicClauseList);}
        return null;}



}
