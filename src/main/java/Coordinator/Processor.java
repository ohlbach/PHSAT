package Coordinator;

import Algorithms.Algorithms;
import Datastructures.Clauses.BasicClauseList;
import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseList;
import Datastructures.Clauses.ClauseStructure;
import Datastructures.Literals.CLiteral;
import Datastructures.Results.Result;
import Datastructures.Results.Satisfiable;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Statistics.DataStatistics;
import Datastructures.Statistics.Statistic;
import Datastructures.Theory.*;
import Management.GlobalParameters;
import Management.Monitor;
import Management.ProblemSupervisor;

import java.util.*;
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
 * Created by ohlbach on 10.10.2018.
 * Created by ohlbach on 10.10.2018.
 */
public abstract class Processor {
    /** the combined id of processor and problem*/
    public String id;
    /** the problemId, mainly for printing information */
    public String problemId;
    /** the processor id */
    public String processorId;
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

    /** turns the current state of the data into a string
     *
     * @return the current state of the data.
     */
    public String toString() {
        StringBuilder st = new StringBuilder();
        if(model != null && !model.isEmpty()) {
            st.append("Model: ").append(model.toString()).append("\n");}
        if(clauses == null || clauses.isEmpty()) {st.append("Clauses: EMPTY\n");}
        if(clauses != null && !clauses.isEmpty()) {
            st.append("Clauses:\n").append(clauses.toString()).append("\n");}
        if(implicationDAG != null && !implicationDAG.isEmpty()) {
            st.append("Implication DAG:\n").append(implicationDAG.toString()).append("\n");}
        if(equivalences != null && !equivalences.isEmpty()) {
            st.append(equivalences.toString()).append("\n");}
        if(disjointnesses != null && !disjointnesses.isEmpty()) {
            st.append(disjointnesses.toString());}
        if(statistics != null) {
            st.append(statistics.toString());}
        return st.toString();}


    /** constructs a processor and initializes the common fields
     *
     * @param processorId         the processor's identifier
     * @param supervisor          which manages the problem
     * @param applicationParameters either the problemParameters or the solverParameters
     * @param basicClauseList     the input clauses (n particular for checking a candidate model)
     */
    public Processor(String processorId,ProblemSupervisor supervisor,
                     HashMap<String,Object> applicationParameters,BasicClauseList basicClauseList) {
        this.processorId = processorId;
        problemId = supervisor.problemId;
        id = processorId+"@"+problemId;
        this.predicates            = basicClauseList.predicates;
        this.supervisor            = supervisor;
        this.globalParameters      = supervisor.globalParameters;
        this.applicationParameters = applicationParameters;
        this.basicClauseList       = basicClauseList;
        monitor                    = globalParameters.monitor;
        monitoring                 = monitor.monitoring();}


    protected Consumer<CLiteral>            longClauseObserver = cLiteral    -> addTask(makeShortenedClauseTask(cLiteral.clause,this));
    protected Consumer<Integer>            trueLiteralObserver = literal     -> addTask(new Task.TrueLiteral(literal,this));
    protected BiConsumer<ImplicationNode,ImplicationNode>  implicationObserver = (from,to)   -> addTask(new Task.BinaryClause(-from.literal,to.literal,this));
    protected Consumer<int[]>              equivalenceObserver = equivalence -> addTask(new Task.Equivalence(equivalence,this));
    protected Consumer<Unsatisfiable> unsatisfiabilityObserver = unsat       -> addTask(new Task.Unsatisfiability(unsat,this));
    protected Consumer<Satisfiable>     satisfiabilityObserver = sat         -> addTask(new Task.Satisfiability(sat,this));

    protected Consumer<CLiteral> literalRemovalMonitor = cLiteral ->
            monitor.print(id,"Literal " + cLiteral.literal + " removed from clause " + cLiteral.clause.id);
    protected Consumer<Clause> clauseRemovalMonitor = clause ->
            monitor.print(id,"Clause " + clause.toString() + " removed");
    protected Consumer<Integer> trueLiteralMonitorDAG =  literal ->
            monitor.print(id,"Literal " + literal + " became true in the implication DAG.");
    protected BiConsumer<ImplicationNode,ImplicationNode> implicationMonitor =  (from, to) ->
            monitor.print(id,"New implication " + from.literal + " -> " + to.literal + " derived.");
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
        clauses.addClauseRemovalObserver(clauseRemovalMonitor);
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
        clauses.removeClauseRemovalObserver(clauseRemovalMonitor);
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
    protected PriorityQueue<Task> taskQueue =
            new PriorityQueue<Task>(10,Comparator.comparingInt(task->task.priority));

    /** adds a task to the task queue.
     * It notifies a getTask-method in the central processor, which might be waiting for new tasks
     *
     * @param task the task to be added
     */
    public synchronized void addTask(Task task) {
        if(globalParameters.debug) {
            System.out.println("TASK Added");
            System.out.println(task.toString());
            System.out.println("TASKS");
            System.out.println(tasksString());
            System.out.println("TASKS-END");}
        taskQueue.add(task);
        notify();}

    /** turns the entire taskQueue into a string */
    public String tasksString() {
        StringBuilder st = new StringBuilder();
        taskQueue.stream().sorted(Comparator.comparingInt(t->t.priority)).filter(task->!task.ignore).forEach(task->st.append(task.toString()).append("\n"));
        return st.toString();}

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
                return new Task.TrueLiteral(clause.getLiteral(0),processor);
            case 2: clauses.removeClause(clause);
                return new Task.BinaryClause(clause.getLiteral(0),clause.getLiteral(1),processor);
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
    protected BiConsumer<Integer,ClauseStructure> purityObserver = (predicate,version) -> {
        int literal = isReallyPure(predicate,version);
        if(literal  != 0) {addTask(new Task.Purity(literal,this));}};

    protected int isReallyPure(int predicate,ClauseStructure version) {
        int literal = 0;
        switch(version) {
            case POSITIVE: if(implicationDAG.positiveMixed(predicate)) {literal = predicate;} break;
            case NEGATIVE: if(implicationDAG.negativeMixed(predicate)) {literal = -predicate;} break;
            case BOTH: if(!implicationDAG.contains(predicate)) {
                literal = clauses.literalIndex.preferredPurityStatus(predicate);}
            else {if(implicationDAG.positiveMixed(predicate)) {literal = predicate;}
            else {if(implicationDAG.negativeMixed(predicate)) {literal = -predicate;}}}}
        return literal;}

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
            addTask(new Task.Unsatisfiability(result,this)); return result;}
        if(status == 1) {return null;}
        clauses.makeTrue(literal);
        implicationDAG.newTrueLiteral(literal,false);
        disjointnesses.newTrueLiteral(literal);
        return trueLiteralInQueue(literal);}

    /** This method traverses the task queue to implement the consequences of a true literal on the remaining tasks
     *
     * @param literal a true literal
     * @return Unsatisfiable or null
     */
    private Result trueLiteralInQueue(int literal) {
        ArrayList<Task> tasks = new ArrayList<>();
        for(Task task : taskQueue) {if(task.makeTrue(literal,tasks)) {return new Unsatisfiable(model,literal);}}
        for(Task task : tasks) {addTask(task);}
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
        literal1 = equivalences.mapToRepresentative(literal1);
        literal2 = equivalences.mapToRepresentative(literal2);
        if(literal1 == -literal2) {return null;}
        if(literal1 == literal2) {addTask(new Task.TrueLiteral(literal1,this)); return null;}
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
        if(clause.size() < 3) {return null;}
        int subsumed = Algorithms.subsume(clause,clauses,implicationDAG);
        int resolved = Algorithms.resolve(clause,clauses,implicationDAG);
        ((DataStatistics)statistics).TSK_subsumed += subsumed;
        ((DataStatistics)statistics).TSK_resolved += resolved;
        return null;}

    /** The method replaces all members of the equivalence class by their representative.
     * If equivalences is defined then the new equivalence class is first merged with the older ones.
     *
     * @param equivalents  an equivalence class [representative, literal_1,...]
     * @return null
     */
    public Result processEquivalence(int[] equivalents) {
        int start = 1;
        int representative = equivalents[0];
        if(equivalences != null) {
            Clause eqClass = equivalences.addEquivalence(equivalents);
            if(eqClass == null) {return null;}  // nothing new
            representative = eqClass.getLiteral(0);
            start = (representative == equivalents[0]) ? 1: 0;}
        for(int i = start; i < equivalents.length; ++i) {
            clauses.replaceByRepresentative(representative,equivalents[i]);
            if(disjointnesses != null) {disjointnesses.replaceByRepresentative(representative,equivalents[i]);}}
        return null;}

    /** makes pure literals true and removes them from the clauses and the implicationDAG.<br>
     * A literal which seemed to be pure may in fact not be pure if an equivalence has been detected
     * and removed from the implicationDAG, but is still in the taskQueue This has to be checked anew.
     *
     * @param literal a pure literal
     * @return null
     */
    public Result processPurity(int literal) {
        if(model.status(literal) != 0) {return null;}
        if(clauses.isEmpty()) {
            implicationDAG.completeModel(model);
            equivalences.completeModel();
            return Result.makeResult(model,basicClauseList);}
        if(clauses.isPure(literal) && implicationDAG.isEmpty(literal)) {
            ((DataStatistics)statistics).CLS_Purities++;
            model.add(literal);
            clauses.removeLiteral(literal);
            implicationDAG.newTrueLiteral(literal,true);
            disjointnesses.newTrueLiteral(literal);}
        return null;}

    /** This method removes a literal from a clause if one of the solvers has discovered that the literal can be removed
     *
     * @param clauseId  if the clause containing the literal
     * @param literal   the literal to be removed
     * @return null
     */
    public Result processLiteralRemoval(String clauseId, int literal) {
        Clause clause = clauses.getClause(clauseId);
        if(clause == null) {return null;}
        int position = clause.contains(literal);
        if(position < 0) {return null;}
        clauses.removeLiteral(clause.getLiteral(position));
        return null;}



}
