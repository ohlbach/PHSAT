package Solvers.Resolution;

import Datastructures.Clauses.InputClauses;
import Datastructures.Results.Result;
import Datastructures.Results.Satisfiable;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Statistics.Statistic;
import Datastructures.Symboltable;
import Datastructures.Theory.Model;
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
import java.util.function.IntSupplier;

/** This is an incomplete solver which tries to simplify the clauses as far as possible and to derive true literals
 * to be sent to other solvers.
 * <br><br>
 * All input clauses are transformed to disjunctions or atleast-clauses.
 * <br><br>
 * The solver distinguishes three phases:<br>
 * 1. Simplification of new clauses (input clauses or new resolvents) <br>
 *    - subsumption (subsumed clauses are removed)<br>
 *    - true literal removal (true/false literals are removed from clauses) <br>
 *    - merge resolution (e.g. p,q and -p,q,phi -> q,phi)<br>
 *    - equivalence detection and replacement (p,-q, and -p,q -&gt; p == q)
 *    <br><br>
 * 2. Simplification of other clauses triggered by new clauses <br>
 *    - removing of subsumed clauses <br>
 *    - true literal removal in all other clauses
 *    - merge resolution of other clauses <br>
 *    - equivalence replacement
 *    <br><br>
 * 3. Limited generation of new resolvents <br>
 *   - saturation of two-literal clauses (all resolvents between two-literal clauses are generated) <br>
 *   - resolution between two-literal clauses and other clauses (their length does not change)<br>
 *   - partial merge resolution such that the length of the resolvent is not increased.
 *   - elimination of literals. Generate all resolvents for a predicate if the number of resolvents does not increase the
 *   number of clauses with this predicate.<br>
 *   The new resolvents are immediately used to simplify the other clauses.
 *   <br> <br>
 *   Further simplifications: <br>
 *   - purity deletion: literal occurring only positively/negatively can be made true and the clauses are deleted.<br>
 *   - partial purity deletion: after the two-literal clauses are saturated, literals occurring only
 *   positively/negatively in the longer clauses can be made true. <br>
 *   <br><br>
 *   Final decisions: <br>
 *   - an empty clause set indicates satisfiability of the clauses.<br>
 *   - an empty clause indicates unsatisfiability of the clause set.<br>
 *   - from a clause set consisting of only positive/negative and mixed clauses a model can be derived.
 *   <br><br>
 *   The class distinguishes the global model (derived true literals) and the local model (pure literals). <br>
 *   The true literals in the global model can be sent to other solvers.
 *   The locally true literals may be part of a model, but there may be other models where these literals are false.
 *   <br><br>
 *   The methods try to treat disjunctions as efficient as possible while dealing also with atleast-clauses.
 *   <br><br>
 *   There is an important distinction between two-literal clauses and longer clauses.
 *   Two-literal clauses are always disjunctions and can be treated much more efficiently than longer clauses. <br>
 *   The literals of two-literal clause are put into the index literalIndexTwo, and the literals of the
 *   longer clauses are put into the index literalIndexMore.<br>
 *   The iterators for the clauses and these indices (e.g. forAllLiterals) ignore removed literals/clauses,
 *   such that one can remove literals/clauses while iterating over them.
 *   <br><br>
 *   Tools for tracking the reasoning <br>
 *   - monitor: all operations can be documented in the monitor. <br>
 *   - trackReasoning: all deductions of new information (clauses, true literals) can be tagged with
 *   InferenceSteps explaining the reason for the deduction (displayed for unsatisfiable clause sets).
 *   <br><br>
 *   Tools for checking the soundness of the algorithms:<br>
 *   - checkConsistency checks the consistency of the indices, of the clause types, and of the model with the input clauses.<br>
 *   - printSeparated shows the current state of the clauses.
 */

public class Resolution extends Solver {
    /** controls if the consistency is to be checked (for testing purposes) */
    private final boolean checkConsistency = false;
    /** controls that all clauses are printed after each task has been changed something (for testing purposes).*/
    private final boolean printClauses = false;

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
    private IntArrayList timestamps = new IntArrayList(3);

    /** the recursion level for the timestamps */
    private int timestampLevel = 0;

    /** a timestamp to be used by subsumption algorithms */
    private int timestampSubsumption = 1;

    /** the thread which runs the simplifier. */
    private Thread myThread;

    /** The local model may contain true literals derived from pure literals.
     * These need not be true in all models, and therefore cannot be sent to the global model. */
    private byte[] localModel;

    private InferenceStep[] inferenceSteps = null;

    /** for local usage */
    private ArrayList<InferenceStep> steps = new ArrayList<>();

    Equivalences equivalences = new Equivalences();

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
        if(trackReasoning) inferenceSteps = new InferenceStep[predicates+1];
        clauses = new Clauses();
        model = new Model(predicates);
        statistics = new ResolutionStatistics(solverId);
        myThread = Thread.currentThread();
    }



    /** A queue of newly derived unit literals and binary equivalences.
     * The unit literals are automatically put at the beginning of the queue.
     */
    private final PriorityBlockingQueue<Task> queue =
            new PriorityBlockingQueue<>(1000, Comparator.comparingInt(task->task.priority));

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
            processTasks(0);}
        catch(Result result) {
            System.out.println("Result " + result.getClass().getName());
            //printSeparated();
            if(result instanceof Satisfiable) {model.exchangeModel(localModel);}
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
        timestampLevel = 0;
        for(int i = 0; i < timestamps.size(); ++i) timestamps.set(i,1);
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
        if(trackReasoning) {
            if(inferenceSteps == null || inferenceSteps.length < predicates+1) inferenceSteps = new InferenceStep[predicates+1];
            else for(int predicate = 1; predicate <= predicates; ++predicate) inferenceSteps[predicate] = null;}
    }

    /** removes all clauses from the internal datastructures.
     * <br>
     * To be used only for testing purposes.
     */
    public void clear() {
        timestampLevel = 0;
        for(int i = 0; i < timestamps.size(); ++i) timestamps.set(i,1);
        timestampSubsumption = 1;
        literalIndexTwo.clear(predicates);
        literalIndexMore.clear(predicates);
        clauses.clear();
        statistics.clear();
        queue.clear();
        model.clear();
        equivalences.clear();
        for(int predicate = 1; predicate <= predicates; ++predicate) localModel[predicate] = 0;
        if(inferenceSteps != null) {
            for(int predicate = 1; predicate <= predicates; ++predicate) inferenceSteps[predicate] = null;}
    }

    /** returns the local truth-status of the literal: +1 = true, -1 = false, 0 = undefined. */
    byte localStatus(final int literal) {
        return (literal > 0) ? localModel[literal] : (byte)-localModel[-literal];}

    /** sets the local truth status of the literal
     *
     * @param literal a literal which is expected to be true.
     */
    void makeLocallyTrue(final int literal, InferenceStep inferenceStep) {
        if(literal > 0) localModel[literal] = 1; else localModel[-literal] = -1;
        if(trackReasoning) inferenceSteps[Math.abs(literal)] = inferenceStep;}

    private final int[] clauseId = new int[]{0};

    IntSupplier nextId = () -> ++clauseId[0];

    /** reads the disjunctions, the atleast, atmost, exactly and interval clauses from the normalizer and transfroms them to Clauses.
     * 
     * @throws Result if a contradiction or the empty clause is derived.
     */
    public void readInputClauses() throws Result {
        ArrayList<IntArrayList> normalizedClauses = problemSupervisor.normalizer.clauses;
        clauseId[0] = normalizedClauses.get(normalizedClauses.size()-1).getInt(0);
        int subIdentifier;
        for(IntArrayList normalizedClause : problemSupervisor.normalizer.clauses) {
            switch(Normalizer.getQuantifier(normalizedClause)) {
                case OR:
                case ATLEAST: insertNewClause(new Clause(normalizedClause,0)); break;
                case ATMOST:  insertNewClause(new Clause(Normalizer.atmost2Atleast(normalizedClause),0));break;
                case EXACTLY:
                    subIdentifier = 0;
                    for(IntArrayList atleastClause : Normalizer.exactlyToAtleast(normalizedClause)) {
                        insertNewClause(new Clause(atleastClause,++subIdentifier));} break;
                case INTERVAL:
                    subIdentifier = 0;
                    for(IntArrayList atleastClause : Normalizer.intervalToAtleast(normalizedClause)) {
                        insertNewClause(new Clause(atleastClause,++subIdentifier));}}}
        statistics.initialClauses = clauses.size;
        if(clauses.isEmpty()) throw new Satisfiable(problemId,solverId, model);
        if(printClauses) {
            System.out.println("INPUT CLAUSES: " + clauses.size());
            printSeparated();}}

    /** simplifies a new clause, inserts it into the internal data structures and generates a ClauseTask.
     * <br>
     * If there are already true literals in the global model then the true/false literals are removed from the clause.
     * This may cause new true literals to be derived, which are inserted into the global model and generate a
     * True-Literal task.<br>
     * Clauses which became true in this process are not inserted.
     *
     * @param clause         a new clause
     * @throws Unsatisfiable if the clause is unsatisfiable.
     */
    void insertNewClause(Clause clause) throws Unsatisfiable {
        if(!model.isEmpty()) {
            collectTrueLiterals(clause);
            if(!trueLiterals.isEmpty()) {
                String clauseBefore = (trackReasoning || monitoring) ? clause.toString(symboltable,0) : null;
                int status = clause.removeLiterals((literalObject -> model.status(literalObject.literal)), null,
                        (literal -> {
                            InfTrueLiteral step = (trackReasoning || monitoring) ?
                                    new InfTrueLiteral(clauseBefore,clause,trueLiterals,steps,literal,symboltable) : null;
                            if(monitoring) monitor.println(monitorId,step.info(symboltable));
                            addInternalTrueLiteralTask(literal,true,step);}));
                ++statistics.trueLiteralRemovals;
                if(status == 1) return;
                InfTrueLiteralRemoval step = (trackReasoning || monitoring) ?
                        new InfTrueLiteralRemoval(clauseBefore,trueLiterals,steps,clause,symboltable) : null;
                clause.inferenceStep = step;
                if(monitoring) monitor.println(monitorId,step.info(symboltable));
                if(status == -1) throw new UnsatClause(clause,problemId,solverId);}}
        insertClause(clause);
        addSimplificationTask(clause);}

    /** adds a true literal to the queue and to the model.
     * <br>
     *  Derived literals (as unit clauses) are added to the local and to the global model.<br>
     *  Pure literals are only added to the local model.<br>
     *
     * @param literal a true literal
     * @param globallyTrue if true then the literal is derived as true literal.
     * @param inferenceStep which caused the truth
     */
    void addInternalTrueLiteralTask(final int literal, final boolean globallyTrue, final InferenceStep inferenceStep) throws Unsatisfiable {
        if(localStatus(literal) == 1) return;
        if(monitoring) {
            String globally = globallyTrue ? "Globally True " : "Locally True ";
            monitor.print(monitorId,globally+"literal added: " + Symboltable.toString(literal,symboltable));}
        makeLocallyTrue(literal,inferenceStep);
        if(globallyTrue) model.add(myThread,literal,inferenceStep);
        synchronized (this) {queue.add(Task.popTask(literal, inferenceStep,null,TaskType.TRUELITERAL));}
        ++statistics.derivedTrueLiterals;}


    /** adds a true literal to the queue. It is called by other threads for globally true literals.
     * <br>
     * If the queue is empty a new ProcessElimination task is added to the queue as well.
     *
     * @param literal a true literal.
     * @param inferenceStep which caused the truth.
     */
    void addExternalTrueLiteralTask(final int literal, InferenceStep inferenceStep) {
        if(monitoring) {
            monitor.print(monitorId,"In: True literal from model " +
                    Symboltable.toString(literal,symboltable));}
        if(trackReasoning && inferenceStep == null) inferenceStep = new InfExternal(literal);
        makeLocallyTrue(literal,trackReasoning ? inferenceStep : null);
        ++statistics.importedTrueLiterals;
        synchronized (this) {queue.add(Task.popTask(literal, inferenceStep,null,TaskType.TRUELITERAL));}}



    /** reads the next task from the task queue and processes it.
     *
     * @param n 0 or the maximum number of task before stopping the loop (0: unlimited) (> 0 for test purposes).
     * @throws Result if a contradiction is encountered or the clause set became empty.
     */
    public void processTasks(final int n) throws Result {
        Task task;
        int counter = 0;
        while(!myThread.isInterrupted()) {
            try {
                //if(monitoring) {monitor.println(monitorId,"Queue is waiting\n" + Task.queueToString(queue));}
                task = queue.take(); // waits if the queue is empty
                if(task.literal == 0 && task.clause == null) continue;
                if(monitoring) monitor.print(monitorId,task.toString(symboltable));
                Clause clause = task.clause;
                Task.pushTask(task); // to be reused.
                if(clause != null && !clause.exists) continue;
                switch(task.taskType) {
                    case TRUELITERAL:
                        processTrueLiteral(task.literal, task.inferenceStep);
                        break;
                    case SIMPLIFYSELF:
                        processSimplifySelf(clause,true);
                        break;
                    case SIMPLIFYOTHERS:
                        //processSimplifyOthers(clause);
                        break;
                    case EXPANDBINARY:
                        //processExpandBinary(clause);
                        break;
                    case PARTIALPURITY:
                        //processPartialPurity();
                        break;
                    case TRIPLERESOLUTION:
                        //processTripleResolution(clause);
                        break;}
                if(checkConsistency) checkConsistency();
                boolean changed = false;
                if(clauses.isEmpty()) {
                    if(monitoring) monitor.print(monitorId,"Clause set is empty");
                    completeModel();
                    throw new Satisfiable(problemId,solverId,model);}
                if(clauses.status != 0) generatePositiveOrNegativeModel();
                if(queue.isEmpty()) {
                    System.out.println("Empty Queue Clauses: " + clauses.size + ", Local Model: " + localModelString());
                    if(printClauses) printSeparated();
                    System.out.println(statistics.toString());
                }
                if(monitoring  && printClauses && changed) {
                    //System.out.println("Local Model: " + localModelString());
                    //printSeparated();
                }
                if(false) {// binaryClausesInQueue == 0 && literalIndexMore.size() == 0) {
                    if(monitoring) {
                        monitor.println(monitorId, "No longer clauses any more. 2-Literal Clauses are Saturated.\n" );
                        monitor.println(monitorId,"Local Model: " + localModelString());
                        if(printClauses) printSeparated();}
                    addInternalTrueLiteralTask(clauses.firstClause.literals.get(0).literal,false,
                            trackReasoning ? new InfSaturatedTwoLiteralClauses(clauses.firstClause) : null);}
                }
            catch(InterruptedException ex) {
                ex.printStackTrace();
                return;}
            if(n > 0 && ++counter == n){
                System.out.println("Stopped because task counter > limit: " + counter);
                return;}}}

    /** performs certain expansive resolution steps.
     * <br>
     * Binary clause: all resolvents with other binary clauses and with the longer clauses are generated.<br>
     * Longer clause: all resolvents with all binary clauses are generated. <br>
     *                Triple resolutions between 3-literal clauses are generated.<br>
     * The generated resolvents are simplified and immediately used to simplify other clauses.
     *
     * @param clause a clause
     * @throws Result
     */
    void processExpansions(Clause clause) throws Result{
        if(clause.size() == 2) {
            expandBinaryClauseWithBinaryClauses(clause);
            if(clause.exists) expandBinaryClauseWithLongerClauses(clause);}
        else  {
            saturateBinaryClausesWithLongerClause(clause);
            if(clause.exists) tripleResolution(clause);}}

    /** processes all simplifying tasks in the queue.
     *
     * @throws Result if a contradiction is encountered or the clause set became empty.
     */
    public void processSimplifyingTasks() throws Result {
        Task task;
        ++timestampLevel;
        while(!myThread.isInterrupted() && ((task = queue.peek()) != null)) {
            if (task.literal == 0 && task.clause == null)  {queue.poll(); continue;}
            if(task.taskType == TaskType.SIMPLIFYSELF) {--timestampLevel; return;}
            queue.poll();
            if (monitoring) monitor.print(monitorId, task.toString(symboltable));
            if (task.literal != 0) {
                int literal = task.literal;
                InferenceStep step = task.inferenceStep;
                Task.pushTask(task); // to be reused.
                processTrueLiteral(literal, step);}
            else {
                Clause clause = task.clause;
                Task.pushTask(task); // to be reused.
                if(!clause.exists) continue;
                processSimplifications(clause);}}
        --timestampLevel;}


    // METHODS FOR PROCESSING TRUE LITERALS

    /** The method applies a true literal to the clauses and the equivalences.
     * <br>
     * For a disjunction this means that the clause is true and can therefore be deleted.<br>
     * For a quantified clause this means that the literal can be deleted and the limit
     * must be reduced by the literal's multiplicity.
     * <br>
     * The resulting clause must be checked for the following phenomena: <br>
     *  - if the resulting limit is &lt;= 0, the clause is true and can be deleted.<br>
     *  - if the resulting limit is 1, the clause became a disjunction. <br>
     *  - if the limit is still &gt; 1, new true literals might be derived.
     *  <br>
     *  Example: atleast 4 p,q^2,r^2 and p is true<br>
     *  The clause is then: atleast 3 q^2,r^2. <br>
     *  Both q and r must now be true.
     *  <br>
     *  For an equivalence p = q where one of the literals is true, the other literal is also made true.<br>
     *
     * @param trueLiteral a true literal.
     * @throws Unsatisfiable if a contradiction is found.
     *  */
    void processTrueLiteral(final int trueLiteral, final InferenceStep inferenceStep) throws Result {
        applyTrueLiteralToBinaryClauses(trueLiteral,inferenceStep);
        applyTrueLiteralToLongerClauses(trueLiteral);
        equivalences.applyTrueLiteral(this::localStatus,inferenceStep,
                (newTrueLiteral,step) -> {
                    if(monitoring) monitor.println(monitorId, "Equivalent literal " +
                            Symboltable.toString(newTrueLiteral,symboltable) + " added to model.");
                    addInternalTrueLiteralTask(newTrueLiteral,true,
                            trackReasoning ? new InfEquivalentTruth(newTrueLiteral,newTrueLiteral,step) : null);});}

    /** applies a true literal to all two-literal clauses containing this literal.
     * <br>
     * Clauses containing this literal are removed.<br>
     * Clauses containing -literal yield a new true literal, which is put into the model.<br>
     * The clause is removed as well.
     *
     * @param trueLiteral a true literal.
     * @param inferenceStep  which caused the derivation of oldTrueLiteral.
     * @throws Unsatisfiable if a contradiction is encountered.
     */
    protected void applyTrueLiteralToBinaryClauses(final int trueLiteral, final InferenceStep inferenceStep) throws Result {
        literalIndexTwo.forAllLiterals(trueLiteral,null, // remove all two-literal clauses with oldTrueLiteral
                (literalObject -> {removeClause(literalObject.clause,true,true); return false;}));

        literalIndexTwo.forAllLiterals(-trueLiteral,null,
                (literalObject -> { // all two-literal clauses with -oldTrueLiteral yield a new true literal.
                    Clause clause = literalObject.clause;
                    int otherLiteral = clause.otherLiteral(literalObject).literal;
                    InfUnitResolutionTwo step = (trackReasoning || monitoring) ?
                            new InfUnitResolutionTwo(clause,trueLiteral,inferenceStep,otherLiteral) : null;
                    if(monitoring) monitor.println(monitorId,step.info(symboltable));
                    addInternalTrueLiteralTask(otherLiteral, model.isTrue(trueLiteral),step);
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
     * They are not immediately processed because processing all true literals first is more effective.

     * @param trueLiteral a true (or false) literal.
     * @throws Unsatisfiable if a contradiction is encountered.
     */
    void applyTrueLiteralToLongerClauses(final int trueLiteral) throws Result{
        removedLiterals.clear(); // for later checking purity
        for(int sign = 1; sign >= -1; sign -= 2) {
            literalIndexMore.forAllLiterals(sign*trueLiteral,null, // map over all clauses with true/false "trueLiteral"
                    literalObject -> {
                        Clause clause = literalObject.clause;
                        clauses.updateClauseNumbers(clause,-1);
                        String clauseBefore =  (monitoring || trackReasoning) ? clause.toString(symboltable,0) : null;
                        boolean globallyTrue = true;
                        collectTrueLiterals(clause);
                        for(int literal : trueLiterals) if(model.status(literal) == 0) {globallyTrue = false; break;}
                        final boolean globTrue = globallyTrue;
                        switch(clause.removeLiterals(
                                litObject -> localStatus(litObject.literal),
                                litObject -> {literalIndexMore.removeLiteral(litObject);
                                    removedLiterals.add(localStatus(litObject.literal)*litObject.literal);},
                                newTrueLiteral -> {
                                    ++statistics.derivedTrueLiterals;
                                    InfTrueLiteral step  = (trackReasoning || monitoring) ?
                                            new InfTrueLiteral(clauseBefore, clause, trueLiterals, steps, newTrueLiteral,symboltable ) : null;
                                    if(monitoring) monitor.println(monitorId,step.info(symboltable));
                                    addInternalTrueLiteralTask(newTrueLiteral,globTrue,step);
                                    trueLiterals.add(newTrueLiteral);})) {
                            case  1: removeClause(clause,false,false); return false;
                            case -1: {if(trackReasoning) {
                                clause.inferenceStep = new InfTrueLiteralRemoval(clauseBefore,trueLiterals,steps, clause,symboltable);}
                                throw new UnsatClause(clause,problemId,solverId);}}
                        InfTrueLiteralRemoval step = null;
                        if(trackReasoning || monitoring) step = new InfTrueLiteralRemoval(clauseBefore,trueLiterals,steps,clause,symboltable);
                        if(monitoring) monitor.println(monitorId,step.info(symboltable));
                        clause.inferenceStep = step;
                        clauses.updateClauseNumbers(clause,1);
                        if(clause.size() == 2) moveToIndexTwo(clause);
                        addSimplificationTask(clause);
                        return false;});}
        literalIndexMore.removePredicate(trueLiteral);
        for(int removedLiteral :removedLiterals) checkPurity(removedLiteral);
    }

    /** simplifies the clause by subsumption, merge resolution, equivalence recognition.
     *
     * @param clause a clause to be simplified.
     * @param inserted if true then the clause is arleady inserted and the clause numbers must be updated.
     * @throws Result
     */
    void processSimplifySelf(Clause clause,boolean inserted) throws Result {
        if(clause.size() == 2) simplifyBinaryClause(clause,inserted); // subsumption, merge resolution, equivalence recognition
        else {
            try {
                if(clause.isDisjunction) {
                    if(simplifyLongerDisjunctionByBinaryClauses(clause))
                        simplifyLongerDisjunctionByLongerClauses(clause);}
                else {if(simplifyAtleastClauseByBinaryClauses(clause))
                    simplifyAtleastClauseByLongerClauses(clause);}}
            finally{clause.determineClauseType();}}
        if(clause.exists) synchronized (this) {queue.add(Task.popTask(0,null,clause,TaskType.SIMPLIFYOTHERS));}
        }


    /** simplifies the given binary clause.
     * <br>
     * The clause may or may not be inserted into the internal data structures.<br>
     * If the clause is subsumed, it is removed (if inserted)<br>
     * The other operations are:<br>
     * Merge Resolution:  p,q and -p,q -&gt; true(q). <br>
     * Equivalence detection: p,q and -p,-q -&gt; p == -q. <br>
     * If this succeeds then the other parent clause is removed as well.<br>
     * A new equivalence is immediately processed.<br>
     * New true literals are inserted into the models and the queue.
     *
     * @param clause1 a binary clause.
     * @param inserted if it is true then the clause has already been inserted into the internal datastructures.
     * @return true if the clause survived.
     * @throws Unsatisfiable if the new true literal contradicts the model.
     */
    boolean simplifyBinaryClause(Clause clause1, boolean inserted) throws Result {
        assert clause1.size() == 2;
        if(inserted) clauses.updateClauseNumbers(clause1,-1);
        ArrayList<Literal> literals = clause1.literals;
        for(Literal literalObject : literals) {
            int literal1 = literalObject.literal;
            int literal2 = clause1.otherLiteral(literalObject).literal;
            if(literalIndexTwo.forAllLiterals(literal1, // now we look for a clause literal1,+-literal2
                    literalObject1Other -> literalObject1Other.clause != clause1,
                    (literalObject1Other -> {
                        Clause clause2 = literalObject1Other.clause;
                        int otherLiteral = clause2.otherLiteral(literalObject1Other).literal;
                        if(otherLiteral == literal2) { // subsumed
                            if(inserted) removeClause(clause1,true,false);
                            ++statistics.subsumedClauses;
                            return true;}
                        if(otherLiteral == -literal2) { // merge resolution
                            InfMergeResolutionTwo step = (trackReasoning || monitoring) ?
                                    new InfMergeResolutionTwo(clause1,clause2,literal1) : null;
                            if(monitoring) monitor.println(monitorId,step.info(symboltable));
                            if(inserted) removeClause(clause1,false,false);
                            removeClause(clause2,true,true);
                            ++statistics.mergeResolutionTwoTwo;
                            addInternalTrueLiteralTask(literal1,true, step);
                            return true;}
                        return false;}))) return false;} // clause1 is superfluous

        // Now we look for equivalences.
        int literal1 = literals.get(0).literal;
        int literal2 = literals.get(1).literal;
        if(literalIndexTwo.forAllLiterals(-literal1, // now we look for a clause -literal1,-literal2
                (literalObject1Neg -> literalObject1Neg.clause.otherLiteral(literalObject1Neg).literal == -literal2),
                (literalObject1Neg -> {              // an equivalence is found.
                    Clause clause2 = literalObject1Neg.clause;
                    InfEquivalence step = (trackReasoning || monitoring) ?
                            new InfEquivalence(clause1,clause2,literal1,-literal2,symboltable) : null;
                    if(monitoring) monitor.println(monitorId,step.info(symboltable));
                    if(inserted) removeClause(clause1,false,false);
                    removeClause(clause2,false,true);
                    Equivalence equivalence = new Equivalence(literal1,-literal2,step);
                    ++statistics.equivalences;
                    equivalences.add(equivalence);
                    processEquivalence(equivalence);
                    return true;}))) return false; // clause1 is superfluous
        else {if(inserted) clauses.updateClauseNumbers(clause1,+1);
            return true;}}

    /** removes longer subsumed clauses and performs mergeResolution between the binary clause and all longer clauses.
     * <br>
     * Simplified longer clauses cause a new clause task.<br>
     * At the end a new expand task is generated for the given binary clause.
     *
     * @param clause
     * @throws Result with the clause set can be decided.
     */
    void simplifyLongerClausesByBinaryClause(Clause clause) throws Result{
        assert(clause.size() > 2);
        removeLongerClausesSubsumedByBinaryClause(clause);
        int literal1 = clause.literals.get(0).literal;
        int literal2 = clause.literals.get(1).literal;
        mergeResolutionBinaryClauseWithLongerClauses(clause,literal1,literal2);
        if(clause.exists) mergeResolutionBinaryClauseWithLongerClauses(clause,literal2,literal1);
        }


    /** removes literals from a longer atleast clause by merge resolution with longer and binary clauses.
     * <br>
     * atleast m  p^l,q_1^m,...,  q_i^m, phi <br>
     * atleast n -p^k,q_1^k_1,...,q_i^k_i   and   n = max(k,l)<br>
     * ----------------------------------<br>
     * atleast m  q_1^m,...,  q_i^m, phi
     * <br><br>
     * Example: <br>
     * atleast 3  p^2,q^3,r^3,phi  <br>
     * atleast 4 -p^4,q^2,r^2 <br>
     * ---------------------- <br>
     * atleast 3 q^3,r^3,phi
     * <br>
     * After the literal is removed from the clause, it is simplified further.<br>
     * If the simplified clause is binary then it is further simplified by binary clauses.<br>
     * Notice that timestamp is modified.
     *
     * @param clauseM a longer disjunction.
     * @return true if the clause survived.
     * @throws Unsatisfiable if derived true literals cause a contradiction.
     */
    boolean simplifyAtleastClauseByLongerClauses(Clause clauseM) throws Result {
        int limitM = clauseM.limit;
        ArrayList<Literal> literalsM = clauseM.literals;
        int size = clauseM.size();
        trueLiterals.clear();
        for(int i = 0; i < literalsM.size(); ++i) {
            int timestamp = increaseTimestamp(size + 2);
            Literal literalObjectP = literalsM.get(i);
            int literalPNeg = -literalObjectP.literal;
            int multL = literalObjectP.multiplicity;
            if(!literalIndexMore.timestampClauses(literalPNeg,
                    (literalObjectPNeg -> {
                        Clause clauseN = literalObjectPNeg.clause;
                        int multK = literalObjectPNeg.multiplicity;
                        return (literalObjectPNeg.clause.size() <= clauseM.size() && clauseN.limit == Math.max(multL,multK));}),
                    timestampLevel, timestamp)) continue;
            int im = 0;
            for(Literal literalObjectQ : clauseM.literals) {
                if(literalObjectQ == literalObjectP) continue;
                int imp = im++;
                if(!literalIndexMore.forAllLiterals(literalObjectQ.literal,
                        (literalObjectQC-> literalObjectQC.clause.getTimestamp(timestampLevel) == getTimestamp() + imp),
                        (literalObjectQC-> {
                            Clause clauseN = literalObjectQC.clause;
                            if(clauseN.getTimestamp(timestampLevel) - getTimestamp() == clauseN.size()-2) {
                                for(Literal literalObjectN : clauseN.literals) {
                                    if(literalObjectN.literal == literalPNeg) continue;
                                    if(limitM != clauseM.findLiteral(literalObjectN.literal).multiplicity) return false;}
                                String clauseBefore = (trackReasoning || monitoring) ? clauseM.toString(symboltable,0) : null;
                                int status = clauseM.removeLiteral(literalObjectP.literal, (literal->trueLiterals.add(literal)));
                                InfResolution step = (trackReasoning || monitoring) ?
                                        new InfResolution(clauseM,clauseBefore,clauseN,
                                                clauseN.toString(symboltable,0),clauseM,
                                                symboltable, "merge resolution") : null;
                                for(int literal : trueLiterals) addInternalTrueLiteralTask(literal,true,step);
                                trueLiterals.clear();
                                if(status == 1) {clauseM.exists = false; return true;}
                                clauseM.inferenceStep = step;
                                if(status == -1) throw new UnsatClause(clauseM,problemId,solverId);
                                clauseM.inferenceStep = step;
                                if(monitoring) monitor.println(monitorId,step.info());
                                return true;}
                            clauseN.increaseTimestamp(timestampLevel);
                            return true;} ))) break;}

            if(clauseM.size() == 2) {timestamp += size + 2; return simplifyBinaryClause(clauseM,true);}
            if(clauseM.size() < size) {--i; size = clauseM.size();}}
        increaseTimestamp(size + 2);
        return true;}

    // EQUIVALENCE REPLACEMENTS

    /** replaces all occurrences of the equivalent literal by  its representative.
     * <br>
     * This step eliminates the literal from the search space.<br>
     *
     * @param equivalence  an equivalence p == q
     * @throws Unsatisfiable if replacing a literal causes an inconsistency.
     */
    void processEquivalence(Equivalence equivalence) throws Result {

        int representative = equivalence.representative;
        int literal = equivalence.literal;
        InferenceStep step = equivalence.inferenceStep;
        for(int sign = 1; sign >= -1; sign -= 2) {
            int newLiteral = sign*representative;
            literalIndexTwo.forAllLiterals(sign*literal,null,
                    (literalObject -> {
                        replaceLiteral(literalObject,newLiteral,literalIndexTwo,step);
                        ++statistics.equivalenceReplacements;
                        return false;}));
            literalIndexMore.forAllLiterals(sign*literal,null,
                    (literalObject -> {replaceLiteral(literalObject,newLiteral,literalIndexMore,step);
                        ++statistics.equivalenceReplacements;
                        return false;}));}}

    /** replaces the literal (in literalObject) by another literal (representative), typically equivalence replacement.
     * <br>
     * If the clause becomes a tautology or is subsumed, it is removed.<br>
     * If the clause is not a disjunction, it is simplified after replacement. <br>
     * True literals may be derived, which are put into the global model,<br>
     * and generate a trueLiteral task.<br>
     * If the clause became a two-literal clause, it is put into the literalIndexTwo index.<br>
     * The modified clause generates a clause task.<br>
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
        if(status == 0 && isSubsumed(clause)) {
            ++statistics.subsumedClauses;
            removeClause(clause,false,false);
            return;}
        if(size > 2 && clause.size() == 2) moveToIndexTwo(clause);
        clauses.updateClauseNumbers(clause,+1);
        addSimplificationTask(clause);}

    /** removes literals from a longer disjunction by binary merge resolution.
     * <br>
     * p,q,phi <br>
     * -p,q<br>
     * -------<br>
     * q,phi<br>
     * If the resulting clause is a unit clause, it is added to the model and a trueLiteral task is added to the queue. <br>
     * Examples: <br>
     * p,q,r,s                p,q,r<br>
     * -p,r                   -p,q<br>
     * -s,r                   -r,q<br>
     * -----                  ------<br>
     * q,r                    true(q)
     *
     * @param clause1 a longer disjunction.
     * @return true if the clause survived.
     * @throws Unsatisfiable if a unit clause causes a contradiction in the model.
     */
    boolean simplifyLongerDisjunctionByBinaryClauses(Clause clause1) throws Result {
        ArrayList<Literal> literals = clause1.literals;
        for(int i = 0; i < literals.size(); ++i) {
            Literal literalObjectP = literals.get(i);
            if(literalIndexTwo.forAllLiterals(-literalObjectP.literal,
                    (literalObjectPNeg -> clause1.findLiteral(literalObjectPNeg.clause.otherLiteral(literalObjectPNeg).literal) != null),
                    (literalObjectPNeg -> {
                        String clauseBefore = (trackReasoning || monitoring) ? clause1.toString(symboltable,0) : null;
                        literals.remove(literalObjectP); --clause1.expandedSize;
                        InfResolution step = (trackReasoning || monitoring) ?
                                new InfResolution(clause1,clauseBefore,literalObjectPNeg.clause,
                                        literalObjectPNeg.clause.toString(symboltable,0),clause1,
                                        symboltable, "merge resolution") : null;
                        clause1.inferenceStep = step;
                        if(monitoring) monitor.println(monitorId,step.info());
                        return true;}))) --i;}
        if(literals.size() == 1) {
            addInternalTrueLiteralTask(literals.get(0).literal,true, clause1.inferenceStep);
            return false;}
        return true;}

    /** performs all simplifications between the given clause and all the other clauses.
     * <br>
     * - subsumed clauses are removed.<br>
     * - mergeResolution with all candidates is done.
     *
     * @param clause   a clause
     * @throws Result usually Unsatisfiable
     */
    void processSimplifications(Clause clause) throws Result {
        if(clause.size() == 2) {simplifyLongerClausesByBinaryClause(clause);}
        else {simplifyLongerClausesByLongerClause(clause);}}

    // SUBSUMPTION CHECKS


    /** checks if the clause is subsumed by another clause.
     * <br>
     * Two-literal clauses are already checked when they are simplified.
     * Therefor this method needs to be called only for longer clauses.
     *
     * @param clause a clause
     * @return null or the subsumer clause.
     */
    boolean isSubsumed(Clause clause) {
        if(clause.size() > 2)
            if(longerClauseIsSubsumedByBinaryClauses(clause) || longerClauseIsSubsumedByLongerClauses(clause)) {
                ++statistics.subsumedClauses;
                return true;}
        return false;}


    /** checks if the longer subsumee is subsumed by a binary subsumer.
     * <br>
     * A longer disjunction is subsumed by a binary clause of its two literals are contained in the longer clause.<br>
     * For atleast-clauses the condition is: p,q subsumes atleast n p^n,q^n, phi.
     * <br>
     * This method is called when a new resolvent is created.
     *
     * @param subsumee a longer clause
     * @return true if the clause is subsumed.
     */
    boolean longerClauseIsSubsumedByBinaryClauses(Clause subsumee) {
        assert(subsumee.size() > 2);
        int limit = subsumee.limit;
        for(Literal literalObjectLonger1 : subsumee.literals) {
            if(literalObjectLonger1.multiplicity != limit) continue;
            Literal subsumerLiteral = literalIndexTwo.findLiteral(literalObjectLonger1.literal,
                    literalObjectBinary1 -> {
                        int literalBinary2 = literalObjectBinary1.clause.otherLiteral(literalObjectBinary1).literal;
                        Literal literalObjectLonger2 = subsumee.findLiteral(literalBinary2);
                        return literalObjectLonger2 != null && literalObjectLonger2.multiplicity == limit;});
            if (subsumerLiteral != null) return true;}
        return false;}


    /** checks if the longer clause is subsumed by another longer clause.
     * <br>
     *  atleast n p^a,... subsumes atleast n- p^a+,...<br>
     *  where n- means &lt;= n and a+ means &gt;=a.<br>
     *  p_1,...,p_k subsumes atleast n p_1^n,...,p_k^n,phi<br>
     *  Notice that the timestampSubsumption is changed!
     *
     * @param subsumee a longer clause
     * @return true if the clause is subsumed.
     */
    boolean longerClauseIsSubsumedByLongerClauses(Clause subsumee) {
        int size = subsumee.size();
        assert size > 2;
        int limit = subsumee.limit;
        for(final Literal literalObject1 : subsumee.literals) {
            int multiplicity1 = literalObject1.multiplicity;
            Literal subsumerLiteral = literalIndexMore.findLiteral(literalObject1.literal,
                    literalObject2->{
                        Clause subsumer = literalObject2.clause;
                        if(subsumer == subsumee) return false;
                        int multiplicity2 = literalObject2.multiplicity;
                        if(subsumer.timestampSubsumption < timestampSubsumption) { // first candidate literal
                            if(subsumer.size() <= size &&
                                    (subsumer.limit >= limit && multiplicity2 <= multiplicity1) ||
                                    (subsumer.isDisjunction && multiplicity2 == subsumer.limit))
                                subsumer.timestampSubsumption = timestampSubsumption;
                            return false;}
                        else {
                            if(subsumer.timestampSubsumption - timestampSubsumption == subsumer.size()-2 &&
                                    multiplicity2 <= multiplicity1 ||
                                    (subsumer.isDisjunction && multiplicity2 == subsumer.limit))
                                return true;
                            if(multiplicity2 <= multiplicity1 || (subsumer.isDisjunction && multiplicity2 == subsumer.limit))
                                ++subsumer.timestampSubsumption;}
                        return false;});
            if(subsumerLiteral != null) {timestampSubsumption += size+2; return true;}}
        timestampSubsumption += size+2;
        return false;}


    /** removes all longer disjunctions which are subsumed by a binary subsumer.
     * <br>
     * For disjunctions, subsumption is just the subset relationship.<br>
     * For atleast clauses subsumption is: <br>
     * p,q subsumes atleast n p^n,q^n,phi
     * <br>
     * timestampSubsumption is used.
     *
     * @param subsumer a binary clause.
     * @throws Unsatisfiable technically necessary, but should not happen.
     */
    protected void removeLongerClausesSubsumedByBinaryClause(final Clause subsumer) throws Result {
        assert(subsumer.size() == 2);
        literalIndexMore.timestampClauses(subsumer.literals.get(0).literal,
                (literalObjectP -> literalObjectP.multiplicity == literalObjectP.clause.limit), -1,
                timestampSubsumption);

        literalIndexMore.forAllLiterals(subsumer.literals.get(1).literal,
                (literalObjectQ -> {
                    Clause subsumee = literalObjectQ.clause;
                    return subsumee.timestampSubsumption == timestampSubsumption && literalObjectQ.multiplicity == subsumee.limit;}),
                (literalObjectQ -> {
                    removeClause(literalObjectQ.clause,true,true);
                    ++statistics.subsumedClauses;
                    return false;}));
        ++timestampSubsumption;}

    /** removes all clauses subsumed by the given clause.
     * <br>
     *  A clause atleast n    p_1^k1 ... p_n^kn subsumes <br>
     *  a clause atleast n-.. p_1^(k1+..) ... p_n^(kn+..) phi <br>
     *  where - means smaller or equal and + means larger or equal.<br>
     *  A disjunction p_1,...,p_k subsumes atleast n p_1^n,...,p_k^n,phi
     *
     * @param subsumer       a clause,
     * @throws Unsatisfiable should not happen.
     */
    protected void removeClausesSubsumedByLongerClause(final Clause subsumer) throws Result {
        int subsumerSize = subsumer.literals.size();
        int sumsumerLimit = subsumer.limit;
        boolean isDisjunction = subsumer.isDisjunction;
        final Literal firstLiteral = subsumer.literals.get(0);
        if(!literalIndexMore.timestampClauses(firstLiteral.literal,
                (subsumeeLiteral -> {
                    Clause subsumee = subsumeeLiteral.clause;
                    return (subsumee != subsumer && subsumee.literals.size() >= subsumerSize &&
                            (subsumee.limit <= sumsumerLimit && subsumeeLiteral.multiplicity >= firstLiteral.multiplicity) ||
                            (isDisjunction && subsumeeLiteral.multiplicity == subsumee.limit));}),-1,
                timestampSubsumption)) return;

        for(int i = 1; i < subsumer.literals.size(); ++i) { // find candidates.
            final int im = i-1;
            Literal subsumerLiteral = subsumer.literals.get(i);
            literalIndexMore.forAllLiterals(subsumerLiteral.literal,
                    (subsumeeLiteral -> {
                        Clause subsumee = subsumeeLiteral.clause;
                        if((subsumee.timestampSubsumption - timestampSubsumption) == im &&
                                subsumeeLiteral.multiplicity >= subsumerLiteral.multiplicity ||
                                (isDisjunction && subsumeeLiteral.multiplicity == subsumee.limit)) {
                            ++subsumee.timestampSubsumption; return true;}
                        return false;}),
                    (subsumeeLiteral -> {
                        Clause subsumee = subsumeeLiteral.clause;
                        if(subsumee.timestampSubsumption - timestampSubsumption == subsumerSize-1){
                            removeClause(subsumee,true,true);}
                        return false;}));}
        timestampSubsumption += subsumerSize + 1;}


    // MERGE RESOLUTION


    /** performs merge resolution between a binary clause and the longer clauses.
     * <br>
     * Binary MergeResolution with disjunctions:  p,q and -p,q,phi -&gt; q,phi. (destructively) <br>
     * Binary MergeResolution with atleast:       p,q and &gt;= m -p, q^m,phi &gt;= m q^m,phi (destructively)<br>
     * Derived true literals are inserted into the model.<br>
     * The simplified clauses generate a simplification task, which is immediately processed.
     *
     * @param clause1   the binary clause.
     * @param literal1  either the first or the second literal.
     * @param literal2  the other literal.
     * @throws Unsatisfiable if inserting a derived unit clause into the model causes a contradiction.
     */
    void mergeResolutionBinaryClauseWithLongerClauses(final Clause clause1, int literal1, int literal2) throws Result {
        assert(clause1.size() == 2);
        int timestamp = increaseTimestamp(1); // just to be sure.
        try{literalIndexMore.timestampClauses(-literal1,(literalObject -> literalObject.multiplicity == 1), timestampLevel, timestamp);
            // literal1,literal2 and -literal1,literal2,rest -> literal2,rest
            literalIndexMore.forAllLiterals(literal2,
                    (literalObject2-> literalObject2.clause.eqTimestamp(timestampLevel,timestamp) &&
                            literalObject2.multiplicity == literalObject2.clause.limit),
                    (literalObject2-> {// clauses with literal2 can merge with clause1
                        Clause clause2 = literalObject2.clause;
                        Literal negLiteralObject1 = clause2.findLiteral(-literal1); // find -p
                        String clause2Before = (monitoring || trackReasoning) ? clause2.toString(symboltable,0) : null;
                        ++statistics.mergeResolutionTwoMore;
                        if(removeLiteralFromClause(negLiteralObject1)) {
                            InfMergeResolutionMore step = (monitoring || trackReasoning) ? new InfMergeResolutionMore(clause1,clause2Before,clause2,symboltable) : null;
                            if(trackReasoning) clause2.inferenceStep = step;
                            if(monitoring) monitor.println(monitorId,step.info());
                            if(simplifyClause(clause2,true)) {
                                addSimplificationTask(clause2);
                                processSimplifyingTasks();}}
                        return false;}));}
        finally {increaseTimestamp(1);}}

    /** returns the timestamp at the current recursion level.
     *
     * @return the timestamp at the current recursion level.
     */
    int getTimestamp() {
        if(timestamps.size() <= timestampLevel) {timestamps.add(1); return 1;}
        else {return timestamps.getInt(timestampLevel);}}


    /** increases the timestamp at the current recursion level.
     *
     * @return the increased timestamp.*/
    int increaseTimestamp(int amount) {
        if(timestamps.size() <= timestampLevel) {timestamps.add(amount); return amount;}
        else {int timestamp = timestamps.getInt(timestampLevel)+amount;
            timestamps.set(timestampLevel,timestamp);
            return timestamp;}}

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
    protected boolean mergeResolutionBetweenLongerClauses(final Clause clauseP) throws Result {
        int clausePSize = clauseP.literals.size();
        int limitP = clauseP.limit;
        int timestamp = getTimestamp();
        try{
            for(final Literal literalObjectP : clauseP.literals) { // mark potential candidates with timestamp
                timestamp = increaseTimestamp (clausePSize + 1);
                int literalPNeg = -literalObjectP.literal;
                literalIndexMore.timestampClauses(literalPNeg,
                        (literalObjectS -> literalObjectS.clause.size() >= clausePSize),timestampLevel, timestamp);
                int i = 0;
                for(final Literal literalObjectQ : clauseP.literals) {
                    if(literalObjectQ == literalObjectP) continue;
                    int im = i++; int tStamp = timestamp;
                    if(literalIndexMore.forAllLiterals(literalObjectQ.literal,
                            literalObjectQQ -> literalObjectQQ.clause.getTimestamp(timestampLevel) - tStamp == im,
                            literalObjectQQ -> {
                                Clause clausQQ = literalObjectQQ.clause;  // this is the potential merge partner.
                                Literal literalObjectQQNeg = clausQQ.findLiteral(literalPNeg);
                                int newLimit = clauseP.limit + clausQQ.limit -
                                        Math.max(literalObjectP.multiplicity, literalObjectQQNeg.multiplicity);
                                if(literalObjectQQ.multiplicity == newLimit) {
                                    int timestamp1 =  clausQQ.increaseTimestamp(timestampLevel);
                                    if(timestamp1 - tStamp == clausePSize-1) { // mergepartner found
                                        ++statistics.mergeResolutionMoreMore;
                                        boolean destructive = clauseP.isDisjunction && clausQQ.isDisjunction;
                                        if(!destructive) {
                                            destructive = true;
                                            for(final Literal litObjectSi : clausQQ.literals) {
                                                int litSi = litObjectSi.literal;
                                                if(litSi != literalPNeg && clauseP.findLiteral(litSi) != null) {
                                                    destructive &= litObjectSi.multiplicity == newLimit;}}}
                                        String resolventBefore = trackReasoning ? clausQQ.toString(symboltable,0) : null;
                                        boolean removeP = limitP == 1 && clausQQ.size() == clausePSize;
                                        if(destructive) {
                                            if(removeLiteralFromClause(literalObjectQQNeg)) {
                                                InfMergeResolutionMore step = (trackReasoning || monitoring) ?
                                                        new InfMergeResolutionMore(clauseP,resolventBefore,clausQQ,symboltable) : null;
                                                if(trackReasoning) clausQQ.inferenceStep = step;
                                                if(monitoring) monitor.println(monitorId,step.info());
                                                addSimplificationTask(clausQQ);
                                                processSimplifyingTasks();}
                                            if(removeP) { // only a disjunction is definitely subsumed and can be removed.
                                                removeClause(clauseP,true,true); return true;}}
                                        else {if(resolve(literalObjectP,literalObjectQQNeg,true, "Merge Resolution") != null);
                                                    processSimplifyingTasks();}
                                        return false;}}
                                return false;})) return clauseP.exists;}}
            return clauseP.exists;}
        finally{increaseTimestamp(clausePSize + 1);}}



    /** creates all resolvents with the given clause which are not longer than the clause itself.
     * <br>
     * This may generate a lot of clauses, but their length is bounded by the length of the existing clauses.<br>
     * For each new clause all simplification tasks are immediately processed.
     *
     * @param clause        a 3-literal clause.
     * @throws Unsatisfiable if a contradiction is discovered.
     */
    void mergeResolutionPartial(final Clause clause) throws Result {
        int size = clause.size();
        try{
            for(final Literal resolutionLiteralObject : clause.literals) {
                if(!clause.exists) return;
                literalIndexTwo.forAllLiterals(-resolutionLiteralObject.literal,null, // resolution with binary clauses.
                        negLiteralObject -> {resolve(resolutionLiteralObject,negLiteralObject,true, "Merge Resolution Partial"); return false;});
                int timestamp = increaseTimestamp(size+2);
                int posLiteral = resolutionLiteralObject.literal;
                literalIndexMore.timestampClauses(-posLiteral,  // timestamp all potential resolution partners.
                        (litObject -> litObject.clause.size() <= size),timestampLevel, timestamp);

                for(final Literal literalObject1 : clause.literals) {
                    if(!clause.exists) return;
                    if(literalObject1 == resolutionLiteralObject) continue;
                    literalIndexMore.forAllLiterals(literalObject1.literal,null,
                            (litObject -> { // all literals except two of them must merge.
                                Clause otherClause = litObject.clause;
                                if(otherClause.getTimestamp(timestampLevel) - timestamp == otherClause.size()-3) {
                                    if(resolve(resolutionLiteralObject,otherClause.findLiteral(-posLiteral),true,"Merge Resolution Partial") != null)
                                        processSimplifyingTasks();
                                        if(!clause.exists) return true;}
                                else otherClause.increaseTimestamp(timestampLevel);
                                return false;}));}}}
        finally {increaseTimestamp(size+2);}}


    // BINARY CLAUSES

    /** checks if the binary clause is locally true
     *
     * @param clause a binary clause
     * @return true if it is locally true.
     */
    boolean binaryClauseIsLocallyTrue(Clause clause) {
        return localStatus(clause.literals.get(0).literal) == 1 ||  localStatus(clause.literals.get(1).literal) == 1;}





    /** generates all binary resolvents with the given binary clause and simplifies the entire clause set with the resolvent
     *
     * @param clause1       a binary clause
     * @throws Unsatisfiable if the resolvent cause a contradictio.
     */
    void expandBinaryClauseWithBinaryClauses(final Clause clause1) throws Result {
        assert(clause1.size() == 2);
        int literal1 = clause1.literals.get(0).literal;
        int literal2 = clause1.literals.get(1).literal;
        literalIndexTwo.forAllLiterals(-literal1, // now we look for a clause -literal1,literal
                null,
                (literalObject -> {
                    Clause clause2 = literalObject.clause;
                    Clause resolvent = new Clause(nextId.getAsInt(),literal2,clause2.otherLiteral(literalObject).literal);
                    ++statistics.binaryResolvents;
                    if(simplifyBinaryClause(resolvent,false)) { // subsumption, mergeResolution, equivalence detection
                        insertClause(resolvent);
                        InfResolution step = (trackReasoning || monitoring) ?
                                new InfResolution(clause1,clause2,resolvent,symboltable,"Binary Expansion") : null;
                        resolvent.inferenceStep = step;
                        if(monitoring) monitor.println(monitorId,step.info());
                        simplifyLongerClausesByBinaryClause(resolvent);} // subsumption, mergeResolution
                    return !clause1.exists;}));}

    /** generates all resolvents between the given binary clause and the longer clauses.
     * <br>
     * Because one parent clause is a binary clause, the resolvent is not longer than the longer parent clause.<br>
     * The resolvents are immediately simplified and used to simplify the other clauses.
     *
     * @param clause1  a binary clause.
     * @throws Result  if the clause set could be decided.
     */
    void expandBinaryClauseWithLongerClauses(final Clause clause1) throws Result {
        assert(clause1.size() == 2);
        for(Literal literalObject : clause1.literals) {
            literalIndexMore.forAllLiterals(-literalObject.literal, null,
                    (literalObject2 -> {
                        if(resolve(literalObject,literalObject2,true,"resolution expansion") != null) {
                            processSimplifyingTasks();} // simplify all clauses with the new resolvent.
                        return true;}));}}



    // PROCESSING LONGER CLAUSES


    /** simplifies the longer clause by other longer clauses.
     * - if the clause is subsumed, it is removed.<br>
     * - other clauses subsumed by the clause are removed. <br>
     * - merge resolution with the other longer clause is performed.
     * <br>
     * Notice that simplifications by binary clauses have already been done in the methods for binary clauses.
     *
     * @param clause a longer clause.
     * @throws Unsatisfiable if a contradiction is discovered.
     */
    void simplifyLongerClausesByLongerClause(final Clause clause) throws Result {
        if(longerClauseIsSubsumedByLongerClauses(clause)) {
            removeClause(clause,true,true);
            return;}
        removeClausesSubsumedByLongerClause(clause);
        mergeResolutionBetweenLongerClauses(clause);
    }

    void expandLongerClause(Clause clause) throws Result {
        saturateBinaryClausesWithLongerClause(clause);
        if(clause.exists) tripleResolution(clause);

    }


    /** computes all resolvents between the given longer parent clause and the binary clauses.
     * <br>
     * If merge resolution with the longer clause is possible, then merge resolution is done and the iteration stops.
     *
     * @param longerClause   a longer clause
     * @throws Unsatisfiable if a contradiction is discovered.
     */
    void saturateBinaryClausesWithLongerClause(final Clause longerClause) throws Result {
        if(monitoring) {
            int candidates = 0;
            for(Literal literalObject : longerClause.literals) candidates += literalIndexTwo.size(-literalObject.literal);
            if(candidates == 0) return;
            monitor.print(monitorId, "Saturation between longer and binary clauses. Candidates: " + candidates);}
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
                                if(removeLiteralFromClause(literalObject)){
                                    InfResolution step = (trackReasoning || monitoring) ?
                                            new InfResolution(binaryClause, binaryClause.toString(symboltable,0),
                                                    longerClause, longerClauseString, longerClause, symboltable, "Binary Saturation") : null;
                                    if(trackReasoning) longerClause.inferenceStep = step;
                                    if(monitoring) monitor.println(monitorId,step.info());
                                    addSimplificationTask(longerClause);
                                    ++statistics.mergeResolutionTwoMore;
                                    processSimplifyingTasks();}
                                return true;}}
                        else {
                            resolve(literalObject,negLiteralObject,true, "Binary Saturation");
                            processSimplifyingTasks();}
                        return false;}))
                return;}}


    /**performs triple resolution with the given clause.
     * <br>
     * p,q,r<br>
     * p,q,a<br>
     * -r,-a,s<br>
     * -------<br>
     * p,q,s
     *
     * @param clause    a three-literal clause
     * @throws Unsatisfiable if the resolvent yields a contradiction.
     */
    void tripleResolution(Clause clause) throws Result {
        assert clause.size() == 3;
        for(Literal literalObjectP : clause.literals) {
            int literalP = literalObjectP.literal;
            for(Literal literalObjectQ :clause.literals) {
                if(literalObjectQ == literalObjectP) continue;
                int timestamp = increaseTimestamp(1) ;
                int literalQ = literalObjectQ.literal;
                Literal literalObjectR = clause.findThirdLiteral(literalP,literalQ);
                int literalR = literalObjectR.literal;
                literalIndexMore.timestampClauses(literalP,(litObject->litObject.clause != clause && litObject.clause.size()==3),
                        timestampLevel, timestamp);
                literalIndexMore.timestampClauses(-literalR,(litObject->litObject.clause.size()==3),timestampLevel, timestamp);
                literalIndexMore.forAllLiterals(literalQ,(litObject -> litObject.clause.eqTimestamp(timestampLevel,timestamp)),
                        (litObjectQQ -> {
                            Clause clauseQ = litObjectQQ.clause;
                            Literal literalObjectA = clauseQ.findThirdLiteral(literalP,literalQ);
                            int literalA = literalObjectA.literal;
                            literalIndexMore.forAllLiterals(-literalA,(litObject1 -> litObject1.clause.eqTimestamp(timestampLevel,timestamp)),
                                    (litObjectANeg ->{
                                        Clause clauseANeg = litObjectANeg.clause;
                                        Literal literalRNeg = clauseANeg.findLiteral(-literalR);
                                        if(literalRNeg != null) {
                                            Clause resolvent = resolve(literalObjectR,literalRNeg,false,"Triple Resolution");
                                            if(resolvent != null) {
                                                if(resolvent.size() == 4) {
                                                    resolve(literalObjectA,resolvent.findLiteral(-literalA),true, "Triple Resolution");
                                                    ++statistics.tripleResolutions;}
                                                else{insertClause(resolvent); addSimplificationTask(resolvent);}}}
                                        return false;}));
                            return false;}));}
            increaseTimestamp(1);}}


    private final IntArrayList trueLiterals = new IntArrayList();
    /** creates a resolvent between the clauses with the two literals.
     * <br>
     * The resolvent is checked for subsumption, simplified and inserted into the internal data structures.
     * A new simplification task is inserted into the task queue.
     *
     * @param posLiteral     a parent literal
     * @param negLiteral     a parent literal
     * @param insert         true if the clause is to be inserted.
     * @param comment        for the InfercenceStep and the monitor.
     * @return               null or the resolvent.
     * @throws Unsatisfiable if a contradiction is discovered.
     */
    Clause resolve(final Literal posLiteral,final Literal negLiteral, boolean insert, String comment) throws Result {
        trueLiterals.clear();
        Clause resolvent = Clause.resolve(posLiteral,negLiteral,nextId,(trueLiterals::add));
        for(int literal : trueLiterals) {
            ++statistics.derivedTrueLiterals;
            InfResolutionTrueLiteral step = (trackReasoning || monitoring) ?
                    new InfResolutionTrueLiteral(posLiteral.clause,negLiteral.clause,literal,symboltable) : null;
            if(monitoring) monitor.println(monitorId, step.info(symboltable));
            addInternalTrueLiteralTask(literal,true,step);}
        if(resolvent == null || isSubsumed(resolvent)) return null;
        InfResolution step = (trackReasoning || monitoring) ?
                new InfResolution(posLiteral.clause,negLiteral.clause, resolvent, symboltable, comment) : null;
        if(trackReasoning) resolvent.inferenceStep = step;
        if(insert) insertClause(resolvent);
        if(resolvent.size() == 2) ++statistics.binaryResolvents; else ++statistics.longerResolvents;
        if(monitoring) monitor.println(monitorId, step.info());
        if(!simplifyClause(resolvent,insert)) return null;
        if(insert) addSimplificationTask(resolvent);
        return resolvent;}

    /** simplifies the clause itself.
     *
     * @param clause   a clause
     * @param inserted true if the clause is already inserted into the datastructures.
     * @return true if the clause survived
     * @throws Unsatisfiable if a contradiction is encountered.
     */
    boolean simplifyClause(Clause clause, boolean inserted) throws Result {
        if(clause.size() == 2) return simplifyBinaryClause(clause,inserted);
        return simplifyLongerClause(clause,inserted);}

    boolean simplifyLongerClause(Clause clause, boolean inserted) throws Result {
        try {
            if(clause.isDisjunction) {
                if(simplifyLongerDisjunctionByBinaryClauses(clause))
                    simplifyLongerDisjunctionByLongerClauses(clause);}
            else {if(simplifyAtleastClauseByBinaryClauses(clause))
                simplifyAtleastClauseByLongerClauses(clause);; }
            return true;}
        finally{clause.determineClauseType();}}



    /** removes literals from a longer atleast-clause by binary merge resolution and clause simplification.
     * <br>
     * atleast m p,q^m,phi <br>
     * -p,q<br>
     * -------<br>
     * atleast m q^m,phi<br>
     * If the resulting clause is a unit clause, it is added to the model and a trueLiteral task is added to the queue. <br>
     * Examples: <br>
     * atleast 2 p,q^2,r,s      atleast 2 p,q^2,r<br>
     * -p,r                     -p,r <br>
     * ------------------       -----------------<br>
     * atleast 2 q^2,r,s        atleast 2 q^2,r -&gt; true(q)
     *
     * @param clause1 a longer disjunction.
     * @return true if the clause survived.
     * @throws Unsatisfiable if a unit clause causes a contradiction in the model.
     */
    boolean simplifyAtleastClauseByBinaryClauses(Clause clause1) throws Result {
        ArrayList<Literal> literals = clause1.literals;
        int limit = clause1.limit;
        trueLiterals.clear();
        for(int i = 0; i < literals.size(); ++i) {
            Literal literalObjectP = literals.get(i);
            int literalP = literalObjectP.literal;
            if(literalIndexTwo.forAllLiterals(-literalObjectP.literal,
                    (literalObjectPNeg -> { // -p in binary clause
                        if(literalObjectPNeg.multiplicity > 1) return false;
                        Clause clause2 = literalObjectPNeg.clause; // binary clause with -p
                        Literal literalObjectQ = clause1.findLiteral(clause2.otherLiteral(literalObjectPNeg).literal);
                        return literalObjectQ != null && literalObjectQ.multiplicity == limit;}),
                    (literalObjectPNeg -> {
                        String clauseBefore = (trackReasoning || monitoring) ? clause1.toString(symboltable,0) : null;
                        int status = clause1.removeLiteral(literalP, (literal -> trueLiterals.add(literal)));
                        InfResolution step = (trackReasoning || monitoring) ?
                                new InfResolution(clause1,clauseBefore,literalObjectPNeg.clause,
                                        literalObjectPNeg.clause.toString(symboltable,0),clause1,
                                        symboltable, "merge resolution") : null;
                        for(int literal : trueLiterals) addInternalTrueLiteralTask(literal,true,step);
                        trueLiterals.clear();
                        if(status == 1) {clause1.exists = false; return true;}
                        clause1.inferenceStep = step;
                        if(status == -1) throw new UnsatClause(clause1,problemId,solverId);
                        if(monitoring) monitor.println(monitorId,step.info());
                        return true;}))) {
                if(!clause1.exists) return false;
                --i;}}
        if(literals.size() == 1) {
            addInternalTrueLiteralTask(literals.get(0).literal,true, clause1.inferenceStep);
            return false;}
        if(literals.size() == 2) return simplifyBinaryClause(clause1,true);
        return true;}



    /** removes literals from a longer disjunction by merge resolution with longer clauses.
     * <br>
     * p,q,r,phi <br>
     * -p,q,r<br>
     * -------<br>
     * q,r,phi<br>
      * Examples: <br>
     * p,q,r,s <br>
     * -p,r,s <br>
     * -s,q,r <br>
     * ----- <br>
     * q,r
     * <br>
     * If the simplified clause is binary then it is further simplified by binary clauses.
     *
     * @param clause1 a longer disjunction.
     * @return true if the clause survived.
     * @throws Unsatisfiable should not happen.
     */
    boolean simplifyLongerDisjunctionByLongerClauses(Clause clause1) throws Result {
        int size = clause1.size();
        for(int i = 0; i < clause1.size(); ++i) {
            int timestamp =  increaseTimestamp(size + 2);
            Literal literalObjectP = clause1.literals.get(i);
            if(!literalIndexMore.timestampClauses(-literalObjectP.literal,
                    (literalObjectPNeg -> literalObjectPNeg.clause.isDisjunction && literalObjectPNeg.clause.size() <= clause1.size()),
                    timestampLevel, timestamp)) continue;
            int im = 0;
            for(Literal literalObjectQ : clause1.literals) {
                if(literalObjectQ == literalObjectP) continue;
                int imp = im++;
                if(!literalIndexMore.forAllLiterals(literalObjectQ.literal,
                    (literalObjectQC-> literalObjectQC.clause.eqTimestamp(timestampLevel, getTimestamp() + imp)),
                        (literalObjectQC-> {
                            Clause clause2 = literalObjectQC.clause;
                            if(clause2.getTimestamp(timestampLevel) - getTimestamp() == clause2.size()-2) {
                                String clauseBefore = (trackReasoning || monitoring) ? clause1.toString(symboltable,0) : null;
                                clause1.literals.remove(literalObjectP); --clause1.expandedSize;
                                InfResolution step = (trackReasoning || monitoring) ?
                                        new InfResolution(clause1,clauseBefore,literalObjectQ.clause,
                                                literalObjectQ.clause.toString(symboltable,0),clause1, symboltable, "merge resolution") : null;
                                clause1.inferenceStep = step;
                                if(monitoring) monitor.println(monitorId,step.info());
                                return true;}
                            clause2.increaseTimestamp(timestampLevel);
                            return true;} ))) break;}

            if(clause1.size() == 2) {timestamp += size + 2; return simplifyBinaryClause(clause1,true);}
            if(clause1.size() < size) {--i; size = clause1.size();}}
        increaseTimestamp(size + 2);
        return true;}




    /** adds a ClauseTask (ProcessBinaryClause or ProcessLongerClause) depending on the clause's size.
     *
     * @param clause a clause.
     */
    void addSimplificationTask(final Clause clause) {
        if(clause.task != null) clause.task.clause = null;
        synchronized (this) {queue.add(Task.popTask(0,null,clause,TaskType.SIMPLIFYSELF));}}



    /** for collecting literals.*/
    private final IntArrayList removedLiterals = new IntArrayList();



        /** inserts a clause into the internal lists.
         *
         * @param clause a clause.
         */
    protected void insertClause(final Clause clause) {
        Literals literalIndex = (clause.size() == 2) ? literalIndexTwo : literalIndexMore;
        for(Literal literalObject : clause.literals) literalIndex.addLiteral(literalObject);
        clauses.addClause(clause);
    }


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
        if(updateNumbers) clauses.updateClauseNumbers(clause,-1);}

    /** removes the clause from the internal lists and sets the task's clause to null;
     *
     * @param clause  a clause to be removed.
     * @param checkPurity if true then the clause's literals are checked for purity.
     * @param updateNumbers if true then the clause numbers are updated.
     * @throws Unsatisfiable should not happen.
     */
    protected void removeClause(final Clause clause, final boolean checkPurity, final boolean updateNumbers) throws Unsatisfiable {
        removeClause(clause,checkPurity,(clause.size() == 2) ? literalIndexTwo :literalIndexMore, updateNumbers);
        Task task = clause.task;
        if(task != null) task.clause = null;}


    /** removes the literal from the clause and from the corresponding index.
     * <br>
     * If the literal becomes pure, it is inserted into the model.<br>
     * If the limit is reduced to 0, the entire clause is removed.<br>
     * If the clause becomes a two-literal clause, it is moved to the two-literal index<br>
     * Clauses.updateClauseNumbers is called.
     *
     * @param literalObject the literal object to be removed.
     * @return true if the clause still exists.
     * @throws Unsatisfiable if inserting a pure literal into the model causes a contradiction.
     */
    protected boolean removeLiteralFromClause(final Literal literalObject) throws Unsatisfiable {
        Clause clause = literalObject.clause;
        trueLiterals.clear();
        clauses.updateClauseNumbers(clause,-1);
        Literals literalIndex = (clause.size() == 2) ? literalIndexTwo : literalIndexMore;
        byte status = clause.removeLiteral(literalObject.literal,(trueLiterals::add));
        if(status == -1) throw new UnsatEmptyClause(problemId,solverId,clause.identifier(),null);
        for(int literal :trueLiterals) addInternalTrueLiteralTask(literal,true,null);
        if(clause.size() == 2) moveToIndexTwo(clause);
        if(status == 1 || isSubsumed(clause)) {
            removeClause(clause,true,false); return false;}
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
        if(trackReasoning) steps.clear();
        for(Literal literalObject : clause.literals) {
            int literal = literalObject.literal;
            switch(model.status(literal)) {
                case 1:  trueLiterals.add(literal); if(trackReasoning) steps.add(model.getInferenceStep(literal));  break;
                case -1: trueLiterals.add(-literal); if(trackReasoning) steps.add(model.getInferenceStep(literal));  break;
                case 0:
                switch(localStatus(literal)) {
                    case  1: trueLiterals.add(literal);
                             if(trackReasoning) steps.add(inferenceSteps[Math.abs(literal)]); break;
                    case -1: trueLiterals.add(-literal);
                             if(trackReasoning) steps.add(inferenceSteps[Math.abs(literal)]); break;}}}}

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
     * - If the number of all resolvents with a literal does not exceed the number of clauses with this literal,
     *   then the resolvents are generated and the parent clauses are removed.<br>
     * If there are still predicates with non-empty literals in the index, the task is added to the queue again.
     *
     * @param task         the processElimination task.
     * @throws Unsatisfiable if the resolvent is unsatisfiable.
     */
    void processElimination(final Task task) throws Result {
        if(monitoring) {
            monitor.println(monitorId, "Starting ProcessElimination. Local Model: " + localModelString());
            if(printClauses) printSeparated();
        }
        boolean atEnd = false;
        try{
            for(int predicate = 1; predicate <= predicates; ++predicate) {
                if(localModel[predicate] != 0) continue;
                int sizeP = literalIndexMore.size(predicate,3);
                int sizeN = literalIndexMore.size(-predicate,3);
                if(sizeP == 0 && sizeN == 0) continue;
                if(sizeP == 0) {
                    ++statistics.partiallyPureLiterals;
                    InfPureLiteral step = (trackReasoning || monitoring) ? new InfPureLiteral(-predicate,true): null;
                    if(monitoring) monitor.println(monitorId,step.info(symboltable));
                    addInternalTrueLiteralTask(-predicate,false,step);
                    return;}
                if(sizeN == 0) {
                    ++statistics.partiallyPureLiterals;
                    InfPureLiteral step = (trackReasoning || monitoring) ? new InfPureLiteral(predicate,true): null;
                    if(monitoring) monitor.println(monitorId,step.info(symboltable));
                    addInternalTrueLiteralTask(predicate,false,step);
                    return;}
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
                        resolve(literalP,literalN,true, "Elimination of " + predicate);}}
                for(Literal literalP : literalsP) {Clause clause = literalP.clause; removeClause(clause,false,true); literalP.clause = clause;}
                for(Literal literalN : literalsN) {Clause clause = literalN.clause; removeClause(literalN.clause,false,true);literalN.clause = clause;}
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
        if(monitoring) monitor.println(monitorId,"Completing Model:  " + localModelString());
        if(checkConsistency) checkConsistency();
        for(int i = eliminatedPredicates.size()-1; i >= 0; --i) {
            for(Literal literalObject : eliminatedPredicates.get(i)) {
                Clause clause = literalObject.clause;
                int trueLiterals = 0;
                for(Literal litObject : clause.literals) {
                    if(localStatus(litObject.literal) == 1) ++trueLiterals;}
                if(trueLiterals < clause.limit)
                    for(Literal  litObject : clause.literals) {
                        if(localStatus(litObject.literal) == 0) {
                            makeLocallyTrue(litObject.literal,null);
                            if(++trueLiterals == clause.limit) break;}}}}
        Unsatisfiable unsatisfiable = equivalences.completeModel(this::localStatus,
                (literal -> makeLocallyTrue(literal,null)));
        if(unsatisfiable != null) {
            System.out.println("Contradiction in completed model for Equivalences. Should not happen!");
            System.out.println(localModelString());
            System.out.println(unsatisfiable.description(symboltable));
            if(printClauses) printSeparated();
            System.out.println(equivalences.toString(symboltable));
            System.out.println(statistics.toString());
            new Exception().printStackTrace();
            System.exit(1);}
    }


    /** generates a local model from either the positive and mixedPositive literals or
     * from the negative and mixedNegative literals in the clauses.
     *
     * @throws Satisfiable after the model has been generated.*/
    void generatePositiveOrNegativeModel() throws Result {
        byte status = clauses.status;
        if(status != 0) {
            if (status == +1) generatePositiveModel();
            if (status == -1) generateNegativeModel();
            completeModel();
            model.exchangeModel(localModel);
            throw new Satisfiable(problemId,solverId,model);}}

    /** generates a local model from the positive and mixed positive literals in the clauses.*/
    void generatePositiveModel() throws Result {
        if(monitoring) monitor.println(monitorId, "Generating Positive Model from model " + localModelString());
        if(printClauses) printSeparated();
        clauses.forAll(clause-> {
            if(clause.clauseType == ClauseType.POSITIVE || clause.clauseType == ClauseType.MIXEDPOSITIVE) {
                int trueLiterals = 0;
                for(Literal literalObject : clause.literals) {
                    if(localStatus(literalObject.literal) == 1) trueLiterals += literalObject.multiplicity;}
                if(trueLiterals < clause.limit) {
                    for(Literal literalObject : clause.literals) {
                        int literal = literalObject.literal;
                        if(literal < 0 || localStatus(literal) != 0) continue;
                        makeLocallyTrue(literal,null);
                        trueLiterals += literalObject.multiplicity;
                        if(trueLiterals >= clause.limit) break;}
                    assert trueLiterals >= clause.limit;}}
            return false;});}


    /** generates a local model from the negative and mixed negative literals in the clauses.*/
    void generateNegativeModel() throws Result {
        if(monitoring) monitor.println(monitorId,"Generating Negative Model from model \" + localModelString()");
        if(printClauses) printSeparated();
        clauses.forAll(clause-> {
            if(clause.clauseType == ClauseType.NEGATIVE || clause.clauseType == ClauseType.MIXEDNEGATIVE) {
                int trueLiterals = 0;
                for(Literal literalObject : clause.literals) {
                    if(localStatus(literalObject.literal) == 1) trueLiterals += literalObject.multiplicity;}
                if(trueLiterals < clause.limit) {
                    for(Literal literalObject : clause.literals) {
                        int literal = literalObject.literal;
                        if(literal > 0 || localStatus(literal) != 0) continue;
                        makeLocallyTrue(literal,null);
                        trueLiterals += literalObject.multiplicity;
                        if(trueLiterals >= clause.limit) break;}
                    assert trueLiterals >= clause.limit;}}
            return false;});}


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
        checkClauses();
        String error = clauses.checkClauseNumbers();
        if(error != null) {
            System.out.println(error);
            new Exception().printStackTrace();
            System.exit(1);
        }}

    /** tests all clauses if they are false in the local model.
     *
     */
    void checkClauses() {
        StringBuilder st = new StringBuilder();
        try{
            if(problemSupervisor != null) {
                ArrayList<int[]> falseInputClauses = problemSupervisor.inputClauses.falseClausesInModel(this::localStatus);
                if(!falseInputClauses.isEmpty()) {
                    st.append("False Input Clauses:\n");
                    for(int[] clause : falseInputClauses) {
                        st.append(InputClauses.toString(clause)).append("\n");}}}
            clauses.forAll(clause -> {
                if(isFalse(clause)) st.append(clause.toString(symboltable,0)).append("\n");
                return false;});
            if(st.length() > 0) {
                System.out.println("Clauses which are false in the local model\n" +
                        localModelString() + ":\n"+ st.toString());
                new Exception().printStackTrace();
                System.exit(1);}}
        catch(Result uns) {}}

    /** checks if the clause is definitively false in an even incomplete local model.
     * <br>
     * A clause must be false if the number of true a+ undefined literals is not enough to satisfy the limit.
     *
     * @param clause a clause to be tested
     * @return true if the clause is false in the local model, even if it is incomplete.
     */
    boolean isFalse(Clause clause) {
        int potentiallyTrueLiterals = 0;
        for(Literal literalObject : clause.literals) {
            if(localStatus(literalObject.literal) >= 0) ++potentiallyTrueLiterals;}
        return potentiallyTrueLiterals < clause.limit;}


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
