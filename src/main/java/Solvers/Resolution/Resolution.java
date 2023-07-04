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
    private int timestamp = 1;

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
        if(trackReasoning) {
            if(inferenceSteps == null || inferenceSteps.length < predicates+1) inferenceSteps = new InferenceStep[predicates+1];
            else for(int predicate = 1; predicate <= predicates; ++predicate) inferenceSteps[predicate] = null;}
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
        Clause clause; int subIdentifier;
        for(IntArrayList normalizedClause : problemSupervisor.normalizer.clauses) {
            switch(Normalizer.getQuantifier(normalizedClause)) {
                case OR:
                case ATLEAST: insertClause(clause = new Clause(normalizedClause,0));
                    addClauseTask(clause); break;
                case ATMOST:  insertClause(clause = new Clause(Normalizer.atmost2Atleast(normalizedClause),0));
                    addClauseTask(clause); break;
                case EXACTLY:
                    subIdentifier = 0;
                    for(IntArrayList atleastClause : Normalizer.exactlyToAtleast(normalizedClause)) {
                    insertClause(clause = new Clause(atleastClause,++subIdentifier));
                    addClauseTask(clause);} break;
                case INTERVAL:
                    subIdentifier = 0;
                    for(IntArrayList atleastClause : Normalizer.intervalToAtleast(normalizedClause)) {
                        insertClause(clause = new Clause(atleastClause,++subIdentifier));
                        addClauseTask(clause);}}}
        statistics.initialClauses = clauses.size;
        if(clauses.isEmpty()) throw new Satisfiable(problemId,solverId, model);
        if(printClauses) {
            System.out.println("INPUT CLAUSES: " + clauses.size());
            printSeparated();}}

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
        synchronized (this) {queue.add(Task.popTask(literal, inferenceStep,null,false));}
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
        synchronized (this) {queue.add(Task.popTask(literal, inferenceStep,null,false));}}



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
                if(task.literal != 0) {
                    int literal = task.literal; InferenceStep step = task.inferenceStep;
                    Task.pushTask(task); // to be reused.
                    processTrueLiteral(literal, step);}
                else {
                    Clause clause = task.clause;
                    if(task.expand) {
                        Task.pushTask(task); // to be reused.
                        processClauseExpand(clause);}
                    else {if(clause.size() == 2) simplifyBinaryClause(clause,task);
                         else simplifyLongerClause(clause,task);}}
                if(checkConsistency) checkConsistency();
                boolean changed = false;
                if(clauses.isEmpty()) {
                    if(monitoring) monitor.print(monitorId,"Clause set is empty");
                    completeModel();
                    throw new Satisfiable(problemId,solverId,model);}
                if(trueLiteralsInQueue == 0 && clauses.status != 0) generatePositiveOrNegativeModel();
                if(queue.isEmpty()) {
                    System.out.println("Empty Queue Clauses: " + clauses.size + ", Local Model: " + localModelString());
                    if(printClauses) printSeparated();
                    System.out.println(statistics.toString());
                }
                if(monitoring  && printClauses && changed) {
                    //System.out.println("Local Model: " + localModelString());
                    //printSeparated();
                }
                if(trueLiteralsInQueue == 0 && binaryClausesInQueue == 0 && literalIndexMore.size() == 0) {
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


    /** processes all simplifying tasks in the queue.
     *
     * @throws Result if a contradiction is encountered or the clause set became empty.
     */
    public void processTasksSimplifying() throws Result {
        Task task;
        while(!myThread.isInterrupted() && ((task = queue.peek()) != null)) {
            if (task.literal == 0 && task.clause == null)  {queue.poll(); continue;}
            if(task.expand == true) return;
            queue.poll();
            if (monitoring) monitor.print(monitorId, task.toString(symboltable));
            if (task.literal != 0) {
                int literal = task.literal;
                InferenceStep step = task.inferenceStep;
                Task.pushTask(task); // to be reused.
                processTrueLiteral(literal, step);}
            else {
                Clause clause = task.clause;
                if (clause.size() == 2) simplifyBinaryClause(clause, task);
                else simplifyLongerClause(clause, task);
                }
        }}


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
    protected void processLongerClause(final Clause clause, boolean saturateBinaryClauses) throws Unsatisfiable {
         if(clause.size() < 3) {
            processBinaryClause(clause);
            return;}
        if((longerClauseIsSubsumedByBinaryClause(clause) != null) ||
           (longerClauseIsSubsumedByLongerClause(clause) != null)) {
            removeClause(clause,true,true);
            return;}
        removeClausesSubsumedByLongerClause(clause);
        if(!mergeResolutionMoreMore(clause)) return;
        if(saturateBinaryClauses) saturateBinaryClausesWithLongerClause(clause);
        if(clause.exists) mergeResolutionPartial(clause);
        if(clause.exists && clause.size() == 3) tripleResolution(clause);
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
     *  For an equivalence p = q where one of the literals is true, the other literal is also made true.<br>
     *  Triggered equivalences whose trigger became true are processed immediately.
     *
     * @param oldTrueLiteral a true literal.
     * @throws Unsatisfiable if a contradiction is found.
     *  */
    void processTrueLiteral(final int oldTrueLiteral, final InferenceStep inferenceStep) throws Unsatisfiable {
        processTrueLiteralTwo(oldTrueLiteral,inferenceStep);
        processTrueLiteralMore(oldTrueLiteral);
        equivalences.applyTrueLiteral(this::localStatus,inferenceStep,
                (trueLiteral,step) -> {
                    if(monitoring) monitor.println(monitorId, "Equivalent literal " +
                            Symboltable.toString(trueLiteral,symboltable) + " added to model.");
                    addInternalTrueLiteralTask(trueLiteral,true,
                            trackReasoning ? new InfEquivalentTruth(trueLiteral,oldTrueLiteral,step) : null);
                            ++statistics.derivedTrueLiterals;});}

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
        for(int sign = 1; sign >= -1; sign -= 2) {
            literalIndexMore.forAllLiterals(sign*oldTrueLiteral,null,
                    literalObject -> {
                        Clause clause = literalObject.clause;
                        clauses.updateClauseNumbers(clause,-1);
                        String clauseBefore =  (monitoring || trackReasoning) ? clause.toString(symboltable,0) : null;
                        boolean globallyTrue = true;
                        if(trackReasoning || monitoring) {
                            collectTrueLiterals(clause);
                            for(int literal : trueLiterals) if(model.status(literal) == 0) {globallyTrue = false; break;}}
                        boolean globTrue = globallyTrue;
                        switch(clause.removeLiterals(
                                litObject -> localStatus(litObject.literal),
                                litObject -> {literalIndexMore.removeLiteral(litObject);
                                            removedLiterals.add(localStatus(litObject.literal)*litObject.literal);},
                                trueLiteral -> {
                                    ++statistics.derivedTrueLiterals;
                                    InfTrueLiteral step  = (trackReasoning || monitoring) ?
                                        new InfTrueLiteral(clauseBefore, clause, trueLiterals, steps, trueLiteral,symboltable ) : null;
                                    if(monitoring) monitor.println(monitorId,step.info(symboltable));
                                    trueLiterals.add(trueLiteral);
                                    addInternalTrueLiteralTask(trueLiteral,globTrue,step);})) {
                            case  1: removeClause(clause,false,false); return false;
                            case -1: {if(trackReasoning) {
                                        clause.inferenceStep = new InfTrueLiteralRemoval(clauseBefore,trueLiterals,steps, clause,symboltable);}
                                throw new UnsatClause(clause,problemId,solverId);}}
                        InfTrueLiteralRemoval step = null;
                        if(trackReasoning || monitoring) step = new InfTrueLiteralRemoval(clauseBefore,trueLiterals,steps,clause,symboltable);
                        if(monitoring) monitor.println(monitorId,step.info(symboltable));
                        if(trackReasoning) clause.inferenceStep = step;
                        clauses.updateClauseNumbers(clause,1);
                        if(clause.size() == 2) moveToIndexTwo(clause);
                        addClauseTask(clause);
                        return false;});}
        literalIndexMore.removePredicate(oldTrueLiteral);
        for(int removedLiteral :removedLiterals) checkPurity(removedLiteral);
    }

    // Methods for Binary Clauses


    /** simplifies the binary clause and all other clauses, which can be simplified by the clause.
     * <br>
     * - if the clause is subsumed, it is removed.<br>
     * - clauses subsumed by the binary clause are removed. <br>
     * - merge resolution with the binary clause is performed.<br>
     * - equivalences are detected. <br>
     * - saturation with the binary clause and the other binary clauses is performed.<br>
     * - The clause gets reinserted into the queue for resolution with longer clauses.
     *
     * @param clause         a two-literal clause
     * @param task           the task to be reused.
     * @throws Unsatisfiable if a contradiction is discovered.
     */
    void simplifyBinaryClause(final Clause clause, Task task) throws Unsatisfiable {
        assert clause.size() == 2;
        if(binaryClauseIsLocallyTrue(clause)){
            removeClause(clause,true,true);
            return;}
        if(binaryClauseIsSubsumed(clause) != null) {
            removeClause(clause,true,literalIndexTwo,true);
            ++statistics.subsumedClauses;
            return;}
        removeBinaryClausesSubsumedByBinaryClause(clause);
        removeLongerClausesSubsumedByBinaryClause(clause);
        int literal1 = clause.literals.get(0).literal;
        int literal2 = clause.literals.get(1).literal;
        mergeResolutionAndEquivalenceTwoTwo(clause,literal1,literal2,true);
        if(!clause.exists) return;
        mergeResolutionAndEquivalenceTwoTwo(clause,literal2,literal1,false);
        if(!clause.exists) return;
        mergeResolutionTwoMore(clause,literal1,literal2);
        mergeResolutionTwoMore(clause,literal2,literal1);
        if(checkPurity(literal1) || checkPurity(literal2)) {
            removeClause(clause,false,literalIndexTwo,true); return;}
        saturateBinaryClausesWithBinaryClause(clause);
        if(clause.exists) {
            task.makeExpand();
            synchronized (this) {queue.add(task);}}}

    /** performs merge resolution and equivalence detection between the given clause and the other binary clauses.
     * <br>
     * Merge Resolution:  p,q and -p,q -&gt; true(q). <br>
     * Equivalence detection: p,q and -p,-q -&gt p == -q. <br>
     * If this succeeds then the other parent clause is removed and the given clause need not be inserted into the datastructures.
     *
     * @param clause1 a binary clause which is not yet inserted into the datastructures.
     * @return true if the clause survived.
     * @throws Unsatisfiable if the new true literal contradicts the model.
     */
    boolean simplifyBinaryClause(Clause clause1) throws Unsatisfiable {
        ArrayList<Literal> literals = clause1.literals;
        for(Literal literalObject : literals) {
            int literal1 = literalObject.literal;
            int literal2 = clause1.otherLiteral(literalObject).literal;
            if(literalIndexTwo.forAllLiterals(-literal1, // now we look for a clause -literal1,literal2
                (literalObject1Neg -> literalObject1Neg.clause.otherLiteral(literalObject1Neg).literal == literal2),
                (literalObject1Neg -> {              // merge resolution yields literal2
                    Clause clause2 = literalObject1Neg.clause;
                    InfMergeResolutionTwo step = (trackReasoning || monitoring) ?
                            new InfMergeResolutionTwo(clause1,clause2,literal2) : null;
                    if(monitoring) monitor.println(monitorId,step.info(symboltable));
                    removeClause(clause2,true,true);
                    ++statistics.mergeResolutionTwoTwo;
                    addInternalTrueLiteralTask(literal2,true, step);
                    return true;}))) return false;}

        // Now we look for equivalences.
        int literal1 = literals.get(0).literal;
        int literal2 = literals.get(1).literal;
        return !literalIndexTwo.forAllLiterals(-literal1, // now we look for a clause -literal1,-literal2
           (literalObject1Neg -> literalObject1Neg.clause.otherLiteral(literalObject1Neg).literal == -literal2),
           (literalObject1Neg -> {              // an equivalence is found.
               Clause clause2 = literalObject1Neg.clause;
               InfEquivalence step = (trackReasoning || monitoring) ?
                       new InfEquivalence(clause1,clause2,literal1,-literal2,symboltable) : null;
               if(monitoring) monitor.println(monitorId,step.info(symboltable));
               removeClause(clause2,true,true);
               Equivalence equivalence = new Equivalence(literal1,-literal2,step);
               ++statistics.equivalences;
               equivalences.add(equivalence);
               processEquivalence(equivalence);
               return true;}));}

    /** checks if the binary clause is locally true
     *
     * @param clause a binary clause
     * @return true if it is locally true.
     */
    boolean binaryClauseIsLocallyTrue(Clause clause) {
        return localStatus(clause.literals.get(0).literal) == 1 ||  localStatus(clause.literals.get(1).literal) == 1;}


    /** checks if the binary clause is subsumed by another binary clause.
     * <br>
     * timestamp is not used.
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

    /** removes all binary clauses which are subsumed by a binary subsumer.
     * <br>
     * timestamp is not used.
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
        assert(clause1.size() == 2);                // parent clause: literal1,literal2
        ++timestamp; // just to be sure.
        try{
            literalIndexTwo.timestampClauses(-literal1,null,timestamp,true);

            literalIndexTwo.forAllLiterals(literal2, // now we look for a clause -literal1,literal2
                    (literalObject -> literalObject.clause.timestamp1 == timestamp),
                    (literalObject -> {              // merge resolution yields literal2
                        Clause clause2 = literalObject.clause;
                        InfMergeResolutionTwo step = (trackReasoning || monitoring) ?
                                new InfMergeResolutionTwo(clause1,clause2,literal2) : null;
                        if(monitoring) monitor.println(monitorId,step.info(symboltable));
                        addInternalTrueLiteralTask(literal2,true, step);
                        removeClause(clause1,true,true);
                        removeClause(clause2,true,true);
                        ++statistics.mergeResolutionTwoTwo;
                        return true;}));

            if(checkEquivalence) { // now we look for a clause -literal1,-literal2
                literalIndexTwo.forAllLiterals(-literal2,
                        (literalObject -> literalObject.clause.timestamp1 == timestamp),
                        (literalObject -> {  // this yields an equivalence literal1 == -literal2
                            Clause clause2 = literalObject.clause;
                            removeClause(clause1,false,true);
                            removeClause(clause2,false,true);
                            int sign = literal1 < 0 ? -1:1;
                            InfEquivalence step = (trackReasoning || monitoring) ?
                                    new InfEquivalence(clause1,clause2,sign*literal1,-sign*literal2,symboltable) : null;
                            if(monitoring) monitor.println(monitorId,step.info(symboltable));
                            Equivalence equivalence = new Equivalence(literal1,-literal2,step);
                            equivalences.add(equivalence);
                            ++statistics.equivalences;
                            processEquivalence(equivalence);
                            return true;}));}}
        finally {++timestamp;}}

    /** replaces all occurrences of the equivalent literal by  its representative.
     * <br>
     * This step eliminates the literal from the search space.<br>
     *
     * @param equivalence  an equivalence p == q
     * @throws Unsatisfiable if replacing a literal causes an inconsistency.
     */
    void processEquivalence(Equivalence equivalence) throws Unsatisfiable {
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
                    (literalObject -> {
                        replaceLiteral(literalObject,newLiteral,literalIndexMore,step);
                        ++statistics.equivalenceReplacements;
                        return false;}));}}


    /** performs merge resolution between a binary clause and a longer clause.
     * <br>
     * Binary MergeResolution with disjunctions:  p,q and -p,q,phi -&gt; q,phi. (destructively) <br>
     * Binary MergeResolution with atleast:       p,q and &gt;= m -p^n, q^k,phi and k = m+1-n -&gt; &gt;= m q^k,phi (destructively)<br>
     * If the condition k = m+1-n does not hold then the merge resolvent is added to the clauses.<br>
     * Triggered Equivalence Recognition: p,q and -p,-q,r -&gt; r -&gt; p == q. (only with 3-literal disjunctions)  <br>
     * Derived true literals are inserted into the model.<br>
     *
     * @param clause1   the binary clause.
     * @param literal1  either the first or the second literal.
     * @param literal2  the other literal.
     * @throws Unsatisfiable if inserting a derived unit clause into the model causes a contradiction.
     */
    void mergeResolutionTwoMore(final Clause clause1, int literal1, int literal2) throws Unsatisfiable {
        assert(clause1.size() == 2);
        ++timestamp; // just to be sure.
        try{literalIndexMore.timestampClauses(-literal1,null,timestamp,true);
            // literal1,literal2 and -literal1,literal2,rest -> literal2,rest
            literalIndexMore.forAllLiterals(literal2,
                    (literalObject2-> literalObject2.clause.timestamp1 == timestamp),
                    (literalObject2-> {// clauses with literal2 can merge with clause1
                        Clause clause2 = literalObject2.clause;
                        Literal negLiteralObject1 = clause2.findLiteral(-literal1); // find -p
                        if(literalObject2.multiplicity == clause2.limit+1-negLiteralObject1.multiplicity) { // condition for destructive merge
                            String clause2Before = null;
                            if(monitoring || trackReasoning) {clause2Before = clause2.toString(symboltable,0);}
                            if(removeLiteralFromClause(negLiteralObject1,false)) {
                                InfMergeResolutionMore step = (monitoring || trackReasoning) ? new InfMergeResolutionMore(clause1,clause2Before,clause2,symboltable) : null;
                                if(trackReasoning) clause2.inferenceStep = step;
                                if(monitoring) monitor.println(monitorId,step.info());}}
                        else {resolve(negLiteralObject1,clause1.findLiteral(literal1),true, "Merge Resolution"); } // non-destructive merge
                        ++statistics.mergeResolutionTwoMore;
                        return false;}));}
        finally {++timestamp;}}

    /** performs resolution between the given clause and all other binary clauses.
     * <br>
     * Subsumed resolvents are ignored. <br>
     * If the resolvent's literals are identical it is inserted into the model.<br>
     * Other resolvents generate a new simplifying task.
     *
     * @param parentClause1   a binary clause,
     * @throws Unsatisfiable  if the model detects a contradiction.
     */
    protected void saturateBinaryClausesWithBinaryClause(final Clause parentClause1) throws Unsatisfiable {
        assert(parentClause1.size() == 2);
        if(monitoring) {
            int candidates = literalIndexTwo.size(-parentClause1.literals.get(0).literal) +
                    literalIndexTwo.size(-parentClause1.literals.get(1).literal);
            if(candidates == 0) return;
            monitor.print(monitorId, "Saturation between binary clauses. Candidates: " + candidates);}
        for(final Literal parentLiteralObject1 : parentClause1.literals) {
            Literal parentLiteralObject2 = literalIndexTwo.getFirstLiteralObject(-parentLiteralObject1.literal);
            while(parentLiteralObject2 != null) {
                Clause parentClause2 = parentLiteralObject2.clause;
                if(parentClause2 == null) {parentLiteralObject2 = parentLiteralObject2.nextLiteral; continue;}
                Clause resolvent = resolveBetweenBinaryClauses(parentClause1,parentClause2);
                if(resolvent != null) {
                    InfResolution step = (trackReasoning || monitoring) ?
                            new InfResolution(parentClause1, parentClause2, resolvent, symboltable, "Binary Saturation") : null;
                    if(trackReasoning) resolvent.inferenceStep = step;
                    if(monitoring) monitor.println(monitorId,step.info());
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
            InfMergeResolutionTwo step = (trackReasoning || monitoring) ? new InfMergeResolutionTwo(clause1,clause2,literal1) : null;
            if(monitoring) {monitor.println(monitorId,step.info(symboltable));}
            addInternalTrueLiteralTask(literal1,true,step);
            removeClause(clause1,true,true);
            removeClause(clause2,true,true);
            return null;}
        final Clause resolvent = new Clause(nextId.getAsInt(), literal1, literal2);
        if(binaryClauseIsSubsumed(resolvent) != null) return null;
        ++statistics.binaryResolvents;
        InfResolution step = (trackReasoning || monitoring) ?
                new InfResolution(clause1,clause2,resolvent,symboltable, "Binary Resolution") : null;
        if(trackReasoning) resolvent.inferenceStep = step;
        if(monitoring) monitor.println(monitorId,step.info());
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
        if(monitoring) {
            int candidates = literalIndexMore.size(-binaryClause.literals.get(0).literal) +
                    literalIndexMore.size(-binaryClause.literals.get(1).literal);
            if(candidates == 0) return;
            monitor.print(monitorId, "Saturation between binary and longer clauses. Candidates: " + candidates);}
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
                                if(removeLiteralFromClause(longerLiteralObject,false)) {
                                    InfResolution step = (trackReasoning || monitoring) ?
                                        new InfResolution(binaryClause, binaryClauseString, longerClause, longerClauseString, longerClause, symboltable, "Binary Saturation") : null;
                                    if(trackReasoning) longerClause.inferenceStep = step;
                                    if(monitoring) monitor.println(monitorId,step.info());
                                    ++statistics.mergeResolutionTwoMore;
                                    processLongerClause(longerClause,false);
                                    //addClauseTask(longerClause);
                                }
                                    return false;}}
                        Clause resolvent = resolve(binaryLiteralObject,longerLiteralObject,true, "Binary Saturation");
                        if(resolvent != null) processLongerClause(resolvent,false);
                        return false;});}}


    /** computes all resolvents between the given longer parent clause and the binary clauses.
     * <br>
     * If merge resolution with the longer clause is possible, then merge resolution is done and the iteration stops.
     *
     * @param longerClause   a longer clause
     * @throws Unsatisfiable if a contradiction is discovered.
     */
    void saturateBinaryClausesWithLongerClause(final Clause longerClause) throws Unsatisfiable {
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
                                if(removeLiteralFromClause(literalObject,false)){
                                    InfResolution step = (trackReasoning || monitoring) ?
                                            new InfResolution(binaryClause, binaryClause.toString(symboltable,0),
                                                    longerClause, longerClauseString, longerClause, symboltable, "Binary Saturation") : null;
                                    if(trackReasoning) longerClause.inferenceStep = step;
                                    if(monitoring) monitor.println(monitorId,step.info());
                                    addClauseTask(longerClause);
                                    ++statistics.mergeResolutionTwoMore;}
                                return true;}}
                        resolve(literalObject,negLiteralObject,true, "Binary Saturation");
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
                        (literalObjectS -> literalObjectS.clause.size() >= clausePSize),timestamp,true);
                int i = 0;
                for(final Literal literalObjectQ : clauseP.literals) {
                    if(literalObjectQ == literalObjectP) continue;
                    int im = i++;
                    if(literalIndexMore.forAllLiterals(literalObjectQ.literal,
                        literalObjectQQ -> literalObjectQQ.clause.timestamp1 - timestamp == im,
                        literalObjectQQ -> {
                                Clause clausQQ = literalObjectQQ.clause;  // this is the potential merge partner.
                           Literal literalObjectQQNeg = clausQQ.findLiteral(literalPNeg);
                                int newLimit = clauseP.limit + clausQQ.limit -
                                    Math.max(literalObjectP.multiplicity, literalObjectQQNeg.multiplicity);
                                if(literalObjectQQ.multiplicity == newLimit) {
                                    ++clausQQ.timestamp1;
                                     if(clausQQ.timestamp1 - timestamp == clausePSize-1) { // mergepartner found
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
                                            if(removeLiteralFromClause(literalObjectQQNeg,false)) {
                                                addClauseTask(clausQQ);
                                                InfMergeResolutionMore step = (trackReasoning || monitoring) ?
                                                        new InfMergeResolutionMore(clauseP,resolventBefore,clausQQ,symboltable) : null;
                                                if(trackReasoning) clausQQ.inferenceStep = step;
                                                if(monitoring) monitor.println(monitorId,step.info());}
                                            if(removeP) { // only a disjunction is definitely subsumed and can be removed.
                                                removeClause(clauseP,true,true); return true;}}
                                        else resolve(literalObjectP,literalObjectQQNeg,true, "Merge Resolution");
                                        return false;}}
                                return false;})) return clauseP.exists;}}
                return clauseP.exists;}
        finally{timestamp += clausePSize + 1;}}


   
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
                    negLiteralObject -> {resolve(resolutionLiteralObject,negLiteralObject,true, "Merge Resolution Partial"); return false;});
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
                                resolve(resolutionLiteralObject,otherClause.findLiteral(-posLiteral),true,"Merge Resolution Partial");
                            else ++otherClause.timestamp1;
                            return false;}));}}
            timestamp += size+2;}

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
    void tripleResolution(Clause clause) throws Unsatisfiable {
        assert clause.size() == 3;
        for(Literal literalObjectP : clause.literals) {
            int literalP = literalObjectP.literal;
            for(Literal literalObjectQ :clause.literals) {
                if(literalObjectQ == literalObjectP) continue;
                ++timestamp;
                int literalQ = literalObjectQ.literal;
                Literal literalObjectR = clause.findThirdLiteral(literalP,literalQ);
                int literalR = literalObjectR.literal;
                literalIndexMore.timestampClauses(literalP,(litObject->litObject.clause != clause && litObject.clause.size()==3),
                        timestamp,true);
                literalIndexMore.timestampClauses(-literalR,(litObject->litObject.clause.size()==3),timestamp,true);
                literalIndexMore.forAllLiterals(literalQ,(litObject -> litObject.clause.timestamp1 == timestamp),
                        (litObjectQQ -> {
                            Clause clauseQ = litObjectQQ.clause;
                            Literal literalObjectA = clauseQ.findThirdLiteral(literalP,literalQ);
                            int literalA = literalObjectA.literal;
                            literalIndexMore.forAllLiterals(-literalA,(litObject1 -> litObject1.clause.timestamp1 == timestamp),
                                    (litObjectANeg ->{
                                        Clause clauseANeg = litObjectANeg.clause;
                                        Literal literalRNeg = clauseANeg.findLiteral(-literalR);
                                        if(literalRNeg != null) {
                                            Clause resolvent = resolve(literalObjectR,literalRNeg,false,"Triple Resolution");
                                            if(resolvent != null) {
                                                if(resolvent.size() == 4) {
                                                    resolve(literalObjectA,resolvent.findLiteral(-literalA),true, "Triple Resolution");
                                                    ++statistics.tripleResolutions;}
                                                else{insertClause(resolvent); addClauseTask(resolvent);}}}
                                        return false;}));
                            return false;}));}
            ++timestamp;}}


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
    Clause resolve(final Literal posLiteral,final Literal negLiteral, boolean insert, String comment) throws Unsatisfiable {
        trueLiterals.clear();
        Clause resolvent = Clause.resolve(posLiteral,negLiteral,nextId,(trueLiterals::add));
        for(int literal : trueLiterals) {
            ++statistics.derivedTrueLiterals;
            InfResolutionTrueLiteral step = (trackReasoning || monitoring) ?
                    new InfResolutionTrueLiteral(posLiteral.clause,negLiteral.clause,literal,symboltable) : null;
            if(monitoring) monitor.println(monitorId, step.info(symboltable));
            addInternalTrueLiteralTask(literal,true,step);}
        if(resolvent == null || isSubsumed(resolvent) != null) return null;
        InfResolution step = (trackReasoning || monitoring) ? new InfResolution(posLiteral.clause,negLiteral.clause, resolvent, symboltable, comment) : null;
        if(trackReasoning) resolvent.inferenceStep = step;
        if(insert) insertClause(resolvent);
        if(resolvent.size() == 2) ++statistics.binaryResolvents; else ++statistics.longerResolvents;
        if(monitoring) monitor.println(monitorId, step.info());
        resolvent = simplifyNewClause(resolvent);
        if(insert && resolvent != null) addClauseTask(resolvent);
        return resolvent;}

    Clause simplifyNewClause(Clause clause) throws Unsatisfiable {
        if(clause.size() == 2) return simplifyBinaryClause(clause) ? null : clause;
        return simplifyLongerClause(clause,null);}

    Clause simplifyLongerClause(Clause clause, Task task) throws Unsatisfiable {
        try {
            if(clause.isDisjunction) {
                if(simplifyLongerDisjunctionByBinaryClauses(clause))
                    simplifyLongerDisjunctionByLongerClauses(clause);}
            else {if(simplifyAtleastClauseByBinaryClauses(clause))
                simplifyAtleastClauseByLongerClauses(clause);; }
            return clause;}
        finally{clause.determineClauseType();}}

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
    boolean simplifyLongerDisjunctionByBinaryClauses(Clause clause1) throws Unsatisfiable {
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
    boolean simplifyAtleastClauseByBinaryClauses(Clause clause1) throws Unsatisfiable {
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
        if(literals.size() == 2) return simplifyBinaryClause(clause1);
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
    boolean simplifyLongerDisjunctionByLongerClauses(Clause clause1) throws Unsatisfiable {
        int size = clause1.size();
        for(int i = 0; i < clause1.size(); ++i) {
            timestamp += size + 2;
            Literal literalObjectP = clause1.literals.get(i);
            if(!literalIndexMore.timestampClauses(-literalObjectP.literal,
                    (literalObjectPNeg -> literalObjectPNeg.clause.isDisjunction && literalObjectPNeg.clause.size() <= clause1.size()),
                    timestamp,true)) continue;
            int im = 0;
            for(Literal literalObjectQ : clause1.literals) {
                if(literalObjectQ == literalObjectP) continue;
                int imp = im++;
                if(!literalIndexMore.forAllLiterals(literalObjectQ.literal,
                    (literalObjectQC-> literalObjectQC.clause.timestamp1 == timestamp + imp),
                        (literalObjectQC-> {
                            Clause clause2 = literalObjectQC.clause;
                            if(clause2.timestamp1 - timestamp == clause2.size()-2) {
                                String clauseBefore = (trackReasoning || monitoring) ? clause1.toString(symboltable,0) : null;
                                clause1.literals.remove(literalObjectP); --clause1.expandedSize;
                                InfResolution step = (trackReasoning || monitoring) ?
                                        new InfResolution(clause1,clauseBefore,literalObjectQ.clause,
                                                literalObjectQ.clause.toString(symboltable,0),clause1, symboltable, "merge resolution") : null;
                                clause1.inferenceStep = step;
                                if(monitoring) monitor.println(monitorId,step.info());
                                return true;}
                            ++clause2.timestamp1;
                            return true;} ))) break;}

            if(clause1.size() == 2) {timestamp += size + 2; return simplifyBinaryClause(clause1);}
            if(clause1.size() < size) {--i; size = clause1.size();}}
        timestamp += size + 2;
        return true;}

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
    boolean simplifyAtleastClauseByLongerClauses(Clause clauseM) throws Unsatisfiable {
        int limitM = clauseM.limit;
        ArrayList<Literal> literalsM = clauseM.literals;
        int size = clauseM.size();
        trueLiterals.clear();
        for(int i = 0; i < literalsM.size(); ++i) {
            timestamp += size + 2;
            Literal literalObjectP = literalsM.get(i);
            int literalPNeg = -literalObjectP.literal;
            int multL = literalObjectP.multiplicity;
            if(!literalIndexMore.timestampClauses(literalPNeg,
                    (literalObjectPNeg -> {
                        Clause clauseN = literalObjectPNeg.clause;
                        int multK = literalObjectPNeg.multiplicity;
                        return (literalObjectPNeg.clause.size() <= clauseM.size() && clauseN.limit == Math.max(multL,multK));}),
                    timestamp,true)) continue;
            int im = 0;
            for(Literal literalObjectQ : clauseM.literals) {
                if(literalObjectQ == literalObjectP) continue;
                int imp = im++;
                if(!literalIndexMore.forAllLiterals(literalObjectQ.literal,
                        (literalObjectQC-> literalObjectQC.clause.timestamp1 == timestamp + imp),
                        (literalObjectQC-> {
                            Clause clauseN = literalObjectQC.clause;
                            if(clauseN.timestamp1 - timestamp == clauseN.size()-2) {
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
                            ++clauseN.timestamp1;
                            return true;} ))) break;}

            if(clauseM.size() == 2) {timestamp += size + 2; return simplifyBinaryClause(clauseM);}
            if(clauseM.size() < size) {--i; size = clauseM.size();}}
        timestamp += size + 2;
        return true;}


    /** adds a ClauseTask (ProcessBinaryClause or ProcessLongerClause) depending on the clause's size.
     *
     * @param clause a clause.
     */
    void addClauseTask(final Clause clause) {
        synchronized (this) {queue.add(Task.popTask(0,null,clause,false));}}



    /** for collecting literals.*/
    private final IntArrayList removedLiterals = new IntArrayList();


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
        byte status = clause.removeLiterals((l-> (l == literalObject) ? (byte)(reduceLimit ? +1:-1) : 0),
                                           (l -> literalIndex.removeLiteral(literalObject)),
                                           (trueLiterals::add));
        if(status == -1) throw new UnsatEmptyClause(problemId,solverId,clause.identifier(),null);
        for(int literal :trueLiterals) addInternalTrueLiteralTask(literal,true,null);
        if(clause.size() == 2) moveToIndexTwo(clause);
        if(status == 1 || isSubsumed(clause) != null) {
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
    void processElimination(final Task task) throws Unsatisfiable {
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
    void generatePositiveModel() throws Unsatisfiable {
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
    void generateNegativeModel() throws Unsatisfiable {
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
        catch(Unsatisfiable uns) {}}

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
