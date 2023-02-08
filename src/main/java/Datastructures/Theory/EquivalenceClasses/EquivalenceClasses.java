package Datastructures.Theory.EquivalenceClasses;

import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Datastructures.Task;
import Datastructures.Theory.Model;
import InferenceSteps.InferenceStep;
import Management.Monitor.Monitor;
import Management.ProblemSupervisor;
import Utilities.TriConsumer;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.concurrent.PriorityBlockingQueue;


/** This class manages lists of equivalence classes of literals.
 * It operates in two modes: <br>
 * Initial phase: analysis of the input clauses. <br>
 * Search phase: parallel to the solvers, as thread.<br>
 *
 * Initial phase: <br>
 * It is called after the unit clauses are put into the model.
 * The input ist:
 *      1. the model with the initial unit clauses.<br>
 *      2. the basic equivalence clauses one by one. <br>
 * The literals in the equivalence clauses are analysed and new equivalence classes are generated.
 * New unit clauses are put into the model.
 * <br>
 * Search phase as thread: <br>
 * It gets its input from the model and from the TwoLiteral module.
 * <br>
 * Equivalences are stored as clauses with sorted literals
 * The first literal (with the smallest predicate) is the representative of the equivalence class. <br>
 * The representative is always positive.
 */
public class EquivalenceClasses  {
    public final ProblemSupervisor problemSupervisor;

    /** for collecting statistics */
    public EquivalenceStatistics statistics;

    /** controls the computation of the clause's origins */
    public final boolean trackReasoning;

    /** The id of the current problem to be solved */
    private final String problemId;

    /** the global model of true literals */
    public final Model model;

    /** null or the global symboltable */
    public Symboltable symboltable;

    /** for logging the actions of this class */
    private final Monitor monitor;

    /** indicates monitoring is on */
    private boolean monitoring = false;

    /** for distinguishing the monitoring areas */
    private String monitorId = null;

    /** The list of equivalence classes */
    public ArrayList<EquivalenceClass> equivalenceClasses;


    /** These two types can occur in the task queue */
    private enum TaskType {
        TRUELITERAL, // a new true literal is obtained from the model
        EQUIVALENCE} // a new binary equivalence is found in the TwoLiteral module.

    /** gets the priority for the objects in the queue.
     *
     * @param task the task in the queue
     * @return the priority of the objects in the queue.
     */
    private int getPriority(Task<EquivalenceClasses.TaskType> task) {
        switch(task.taskType) {
            case TRUELITERAL: return Integer.MIN_VALUE + Math.abs((Integer)task.a);
            case EQUIVALENCE: return (Math.abs((Integer)task.a));} // this guarantees a deterministic sequence of the tasks
        return 4;}

    /** A queue of newly derived unit literals and binary equivalences.
     * The unit literals are automatically put at the beginning of the queue.
     */
    private final PriorityBlockingQueue<Task<EquivalenceClasses.TaskType>> queue =
            new PriorityBlockingQueue<>(10, Comparator.comparingInt(this::getPriority));

    /** These observers are called for new equivalences: (representative,literal,inferenceStep)
     * They are not called for the EQUIV inputClauses.*/
    private final ArrayList<TriConsumer<Integer,Integer,InferenceStep>> observers = new ArrayList<>();

    /** creates the EquivalenceClasses instance
     *
     * @param problemSupervisor  the problem supervisor.
     * @param monitor            null or a monitor.
     */
    public EquivalenceClasses(ProblemSupervisor problemSupervisor, Monitor monitor) {
        this.problemSupervisor = problemSupervisor;
        this.monitor = monitor;
        problemId = problemSupervisor.problemId;
        model = problemSupervisor.model;
        statistics   = new EquivalenceStatistics(problemId);
        trackReasoning = problemSupervisor.globalParameters.trackReasoning;
        if(monitor != null) {
            monitoring = true;
            monitorId = problemId+"-EQV";}}

    /** turns all equivalences from the input clauses into EquivalenceClass objects.
     *
     * @param equivalences the equivalence classes from the input clauses
     * @throws Unsatisfiable if a contradiction is discovered.
     */
    public void integrateEQUIVClauses(ArrayList<int[]> equivalences) throws Unsatisfiable {
        try{
            for(int[] equivalence : equivalences) {
                EquivalenceClass equivalenceClass = EquivalenceClass.makeEquivalenceClass(equivalence,model,trackReasoning);
                if(equivalenceClass != null) equivalenceClasses.add(equivalenceClass);}

            // Now we must join overlapping equivalence classes.
            // This should actually not be necessary, but it is logically possible and allowed.
            int length = equivalenceClasses.size();
            for(int i = 0; i < length; ++i) {
                EquivalenceClass ec1 = equivalenceClasses.get(i);
                for(int j = i+1; j < length; ++j) {
                    EquivalenceClass ec2 = equivalenceClasses.get(j);
                    int sign = ec1.overlaps(ec2);
                    if(sign != 0) {
                        EquivalenceClass ec = ec1.joinEquivalenceClass(ec2,sign,observers);
                        --length;
                        if(ec == ec1) {equivalenceClasses.remove(i--); break;}
                        else{equivalenceClasses.remove(j--);}}}}}
        catch(Unsatisfiable unsatifiable) {
            unsatifiable.solverClass = EquivalenceClasses.class;
            unsatifiable.solverId    = "EquivalenceClasses";
            unsatifiable.problemId   = problemId;
            throw unsatifiable;}}


    /** Any solver which is interested to know about newly derived equivalences can add an observer.
     * The observer is called with (representative, literal, inferenceStep) as soon as new equivalences
     * representative == literal are derived.
     *
     * @param observer a TriConsumer for transferring newly derived equivalences.
     */
    public void addObserver(TriConsumer<Integer,Integer,InferenceStep> observer) {
        observers.add(observer);}


    /** Starts the instance in a thread.
     * The thread waits for newly derived unit clauses (via the model) and newly derived
     * equivalences and integrates them into the equivalence classes.
     * During this process, new unit clauses and new equivalences may be derived.
     * The unit clauses are transferred to the model, and the equivalences are made available
     * by the equivalenceObservers.
     * <br>
     * The loop can only be stopped by an interrupt or a contradiction.
     * <br>
     * The result is stored into the variable result.
     */
    public void run() {
        model.addObserver(this::addTrueLiteral);
        Task<TaskType> task;
        while(!Thread.interrupted()) {
            try {
                if(monitoring) {monitor.print(monitorId,"Queue is waiting\n" + Task.queueToString(queue));}
                task = queue.take(); // waits if the queue is empty
                switch(task.taskType){
                    case TRUELITERAL: applyTrueLiteral((Integer)task.a,(InferenceStep)task.b); break;
                    case EQUIVALENCE: addEquivalence((Integer)task.a,(Integer)task.b,(InferenceStep) task.c); break;}
                if(monitoring) {
                    monitor.print(monitorId,"Current equivalences:\n" + toString(symboltable));}}
            catch(InterruptedException ex) {return;}
            catch(Unsatisfiable unsatisfiable) {
                problemSupervisor.finished("EquivalenceClasses", unsatisfiable,"Contradiction like p = -p found.");
                return;}}}

    /** adds a true literal to the queue
     *
     * @param literal a true literal
     * @param inferenceStep which caused the truth
     */
    public void addTrueLiteral(int literal,InferenceStep inferenceStep) {
        if(monitoring) {
            monitor.print(monitorId,"In:   True literal " +
                    Symboltable.toString(literal,symboltable));}
        synchronized (this) {queue.add(new Task<>(TaskType.TRUELITERAL, literal, inferenceStep));}}

    /** This method is to be called to announce a newly derived equivalence
     * literal1 = literal2.
     * The equivalence is put into the queue.
     *
     * @param literal1      a literal.
     * @param literal2      an equivalent literal.
     * @param inferenceStep which caused the equivalence.
     */
    public void integrateEquivalence(int literal1, int literal2, InferenceStep inferenceStep) {
        ++statistics.derivedClasses;
        if(monitoring) {
            monitor.print(monitorId,"In:   Equivalence " +
                    Symboltable.toString(literal1, symboltable) + " = " +
                    Symboltable.toString(literal2, symboltable));}
        Task<TaskType> task = new Task<>(TaskType.EQUIVALENCE,literal1,literal2,inferenceStep);
        synchronized (this) {queue.add(task);}}


    /** integrates a clause into the equivalence classes.
     * Literals are replaced by representatives of other equivalence classes (if necessary) <br>
     * True and false literals cause all equivalent literals to become true/false <br>
     * Double literals are removed. <br>
     * Complementary literals cause Unsatisfiable to be thrown. <br>
     * Overlapping equivalences are joined to the new clause (the  clauses are removed).<br>
     * All observers are applied to derived clauses.
     *
     * @param literal1 a literal
     * @param literal2 a literal with literal1 = literal2
     * @param inferenceStep which caused the equivalence
     */
    protected void addEquivalence(int literal1, int literal2, InferenceStep inferenceStep) throws Unsatisfiable {
        if(monitoring) {
            monitor.print(monitorId,"Exec: equivalence: " + Symboltable.toString(literal1,symboltable) +
                    " = " + Symboltable.toString(literal2,symboltable));}
        EquivalenceClass equivalenceClass1 = null;
        EquivalenceClass equivalenceClass2 = null;
        int sign1 = 0;
        int sign2 = 0;
        int sign = 0;
        for(EquivalenceClass equivalenceClass : equivalenceClasses) {
            sign = equivalenceClass.containsLiteral(literal1);
            if(sign != 0) {
                equivalenceClass.addNewEquivalence(sign*literal1,sign*literal2,inferenceStep,observers);}
            else {
                sign = equivalenceClass.containsLiteral(literal2);
                if(sign != 0) {
                    equivalenceClass.addNewEquivalence(sign*literal2,sign*literal1,inferenceStep,observers);}}
            if(sign != 0) {
                if(sign1 == 0) {
                    sign1 = sign; equivalenceClass1 = equivalenceClass;}
                else {sign2 = sign; equivalenceClass2 = equivalenceClass;}
                continue;}}

        if(sign1 != 0 && sign2 != 0) {
            EquivalenceClass equivalenceClass = equivalenceClass1.joinEquivalenceClass(equivalenceClass2,sign1,observers);
            equivalenceClasses.remove(equivalenceClass);}
        }

    /** A true literal causes all other equivalent literals to become true.
     *
     * @param literal a true literal
     * @throws Unsatisfiable if a contradiction occurs.
     */
    protected void applyTrueLiteral(int literal, InferenceStep inferenceStep) throws Unsatisfiable {
        for(EquivalenceClass equivalenceClass : equivalenceClasses) {
            int sign = equivalenceClass.containsLiteral(literal);
            if(sign != 0) {
                equivalenceClass.applyTrueLiteral(literal, sign,inferenceStep,model);
                equivalenceClasses.remove(equivalenceClass);
                if(monitoring) {
                    monitor.print(monitorId,"EquivalenceClasses: True literal " +
                            Symboltable.toString(literal, symboltable) + " applied to equivalence class\n" +
                            equivalenceClass.toString(symboltable));} // the classes are disjoint.
                return;}}
        }



    /** If a literal in an equivalence class is true, then all other literals are also made true
     * This method is called when a partial model is found, which is supposed to be extendable to a complete model.
     *
     * @throws Unsatisfiable if the resulting model becomes inconsistent (should never happen)
     */
    public void completeModel() throws Unsatisfiable {
        for(EquivalenceClass equivalenceClass : equivalenceClasses) {
            IntArrayList literals = equivalenceClass.literals;
            ArrayList<InferenceStep> inferenceSteps = equivalenceClass.inferenceSteps;
            int status = model.status(equivalenceClass.representative);
            if(status != 0) {
                for(int i = 0; i < literals.size(); ++i) {
                    model.add(status*literals.getInt(i),
                            (inferenceSteps == null) ? null : inferenceSteps.get(i));}
                return;}
            for(int i = 0; i < literals.size(); ++i) {
                int literal = literals.getInt(i);
                status = model.status(literal);
                if(status != 0) {
                    model.add(status* equivalenceClass.representative,null);
                    for(int j = 0; j < literals.size(); ++j) {
                        if(j == 0) continue;
                        model.add(literals.getInt(j),(inferenceSteps == null) ? null : inferenceSteps.get(j));}
                    return;}}}}


    /** checks if there is no equivalence class
     *
     * @return true if there is no equivalence class
     */
    public synchronized boolean isEmpty() {
        return equivalenceClasses.isEmpty();}

    /** turns the equivalence classes into a string "representative =  literal1 = literal2 = ... [origins]".
     * No symboltable is used.
     *
     * @return a string representation of the equivalence classes.
     */
    public String toString() {return toString(null);}


    /** turns the equivalence classes into a string "representative =  literal1 = literal2 = ... [origins]"
     *
     * @param symboltable or null
     * @return a string representation of the equivalence classes.
     */
    public String toString(Symboltable symboltable) {
        if(equivalenceClasses.isEmpty()) return "No equivalence classes in problem " + problemId + "\n";
        StringBuilder string = new StringBuilder();
        string.append("Equivalence Classes of Problem ").append(problemId).append(":\n");
        for(EquivalenceClass equivalenceClass: equivalenceClasses) {
            string.append(equivalenceClass.toString(symboltable)).append("\n");}
        return string.toString();}

}
