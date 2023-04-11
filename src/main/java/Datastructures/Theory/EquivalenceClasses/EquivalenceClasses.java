package Datastructures.Theory.EquivalenceClasses;

import Datastructures.Results.Result;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Statistics.Statistic;
import Datastructures.Symboltable;
import Datastructures.Task;
import Datastructures.Theory.Model;
import InferenceSteps.InferenceStep;
import Management.Monitor.Monitor;
import Management.ProblemSupervisor;
import Solvers.Solver;
import Utilities.TriConsumer;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.concurrent.PriorityBlockingQueue;


/** This class manages lists of equivalence classes of literals.
 * It operates in two modes: <br>
 * Initial phase: analysis of the input clauses. <br>
 * Search phase: parallel to the solvers, as thread.<br>
 *
 * Initial phase: <br>
 * It is called after the unit clauses are put into the model.
 * The input ist:<br>
 *      1. the model with the initial unit clauses,<br>
 *      2. the input equivalence clauses. <br>
 * The literals in the equivalence clauses are analysed and new equivalence classes are generated.<br>
 * New unit clauses are put into the model.
 * <br>
 * In the search phase the class works as a parallel thread.
 * It gets input from other threads and can derive new true literals and new equivalences.
 */
public class EquivalenceClasses extends Solver {
    static {
        //QUSat.addHelper("equivalences",() -> help());
    }

    /** the problem supervisor */
    public ProblemSupervisor problemSupervisor;

    /** for collecting statistics */
    public EquivalenceStatistics statistics;

    /** controls the computation of the clause's origins */
    public boolean trackReasoning;

    /** The id of the current problem to be solved */
    private final String problemId;

    /** the global model of true literals */
    public Model model;

    /** null or the global symboltable */
    public Symboltable symboltable;

    /** for logging the actions of this class */
    private final Monitor monitor;

    /** indicates monitoring is on */
    private boolean monitoring;

    /** for distinguishing the monitoring areas */
    private String monitorId;

    /** The list of equivalence classes */
    public ArrayList<EquivalenceClass> equivalenceClasses = new ArrayList<>();

    private boolean isInterrupted = false;

    /** These two types can occur in the task queue */
    private enum TaskType {
        /** a new true literal is obtained from the model */
        TRUELITERAL, 
        /** a new binary equivalence is found in the TwoLiteral module. */
        EQUIVALENCE}

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
        solverParameters = new HashMap<>();
        solverParameters.put("name","EquivalenceClasses");
        solverNumber = 1;
        this.problemSupervisor = problemSupervisor;
        this.monitor = monitor;
        if(problemSupervisor != null) {
            problemId      = problemSupervisor.problemId;
            model          = problemSupervisor.model;
            trackReasoning = problemSupervisor.globalParameters.trackReasoning;}
        else {    // for test purposes.
            problemId      = "TestProblem";
            model          = new Model(20);
            trackReasoning = true;}
        statistics   = new EquivalenceStatistics(problemId);

        if(monitor != null) {
            monitoring = true;
            monitorId = "EQV";}
    }

    public void interrupt() {
        isInterrupted = true;
        Thread.currentThread().interrupt();}

    /** turns all equivalences from the input clauses into EquivalenceClass objects.
     *
     * @param equivalences the equivalence classes from the input clauses
     * @throws Unsatisfiable if a contradiction is discovered.
     */
    public void readEquivalences(ArrayList<int[]> equivalences) throws Unsatisfiable {
        try{
            for(int[] equivalence : equivalences) {
                EquivalenceClass equivalenceClass = EquivalenceClass.makeEquivalenceClass(equivalence,trackReasoning);
                if(equivalenceClass != null) equivalenceClasses.add(equivalenceClass);}

            // Now we must join overlapping equivalence classes.
            // This should actually not be necessary, but it is logically possible and allowed.
            for(int i = 0; i < equivalenceClasses.size(); ++i) {
                EquivalenceClass ec1 = equivalenceClasses.get(i);
                for(int j = i+1; j < equivalenceClasses.size(); ++j) {
                    EquivalenceClass ec2 = equivalenceClasses.get(j);
                    int sign = ec1.overlaps(ec2);
                    if(sign != 0) {
                        EquivalenceClass ec = ec1.joinOverlappingClasses(ec2,sign);
                        equivalenceClasses.remove(j);
                        equivalenceClasses.remove(i--);
                        equivalenceClasses.add(ec);
                        break;}}}
            statistics.inputClasses = equivalenceClasses.size();
            initialize(problemSupervisor);}
        catch(Unsatisfiable unsatifiable) {
            unsatifiable.solverId = "EquivalenceClasses";
            unsatifiable.problemId   = problemId;
            throw unsatifiable;}}


    /** Any solver which is interested to know about newly derived equivalences can add an observer.
     * The observer is called with (representative, literal, inferenceStep) as soon as new equivalences
     * representative == literal are derived.
     *
     * @param observer a TriConsumer for transferring newly derived equivalences.
     */
    public synchronized void addObserver(TriConsumer<Integer,Integer,InferenceStep> observer) {
        observers.add(observer);}

    /** Installs the observer in the model.
     */
    @Override
    public void installCommunication(ProblemSupervisor problemSupervisor) {
        problemSupervisor.model.addObserver(this::addTrueLiteralTask);}


    @Override
    public Result solveProblem(ProblemSupervisor problemSupervisor) {
        try{
        processTasks(false);}
        catch(Result result) {
            result.problemId = problemId;
            result.statistic = statistics;
            result.solverId = "EquivalenceClasses";
            return result;}
        return null;}


    @Override
    public Statistic getStatistics() {
        return statistics;
    }


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
    public void processTasks(boolean once) throws Unsatisfiable {
        Task<TaskType> task;
        while(!isInterrupted) {
            try {
                if(monitoring) {monitor.print(monitorId,"Queue is waiting\n" + Task.queueToString(queue));}
                task = queue.take(); // waits if the queue is empty
                switch(task.taskType){
                    case TRUELITERAL: processTrueLiteral((Integer)task.a,(InferenceStep)task.b); break;
                    case EQUIVALENCE: processEquivalence((Integer)task.a,(Integer)task.b,(InferenceStep) task.c); break;}
                if(monitoring) {
                    monitor.print(monitorId,"Current equivalences:\n" + toString(symboltable));}}
            catch(InterruptedException ex) {return;}
            if(once) return;}}

    /** finds the representative for the class containing the literal.
     *
     * @param literal a literal
     * @return 0 or the representative for the literal
     */
    public int getRepresentative(int literal) {
        for(EquivalenceClass equivalenceClass : equivalenceClasses) {
            int sign = equivalenceClass.containsLiteral(literal);
            if(sign != 0) return sign*equivalenceClass.representative;}
        return  0;}



    /** adds a true literal to the queue
     *
     * @param literal a true literal
     * @param inferenceStep which caused the truth
     */
    public void addTrueLiteralTask(int literal, InferenceStep inferenceStep) {
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
    public void addEquivalenceTask(int literal1, int literal2, InferenceStep inferenceStep) {
        ++statistics.derivedClasses;
        if(monitoring) {
            monitor.print(monitorId,"In:   Equivalence " +
                    Symboltable.toString(literal1, symboltable) + " = " +
                    Symboltable.toString(literal2, symboltable));}
        Task<TaskType> task = new Task<>(TaskType.EQUIVALENCE,literal1,literal2,inferenceStep);
        synchronized (this) {queue.add(task);}}


    /** A true literal causes all other equivalent literals to become true.
     *
     * @param literal a true literal
     * @param inferenceStep the inference that caused the truth of the literal.
     * @throws Unsatisfiable if a contradiction occurs.
     */
    protected void processTrueLiteral(int literal, InferenceStep inferenceStep) throws Unsatisfiable {
        ++statistics.importedTrueLiterals;
        for(EquivalenceClass equivalenceClass : equivalenceClasses) {
            int sign = equivalenceClass.containsLiteral(literal);
            if(sign != 0) {
                equivalenceClass.applyTrueLiteral(literal, sign,inferenceStep,model,statistics);
                equivalenceClasses.remove(equivalenceClass);
                if(monitoring) {
                    monitor.print(monitorId,"EquivalenceClasses: True literal " +
                            Symboltable.toString(literal, symboltable) + " applied to equivalence class\n" +
                            equivalenceClass.toString(symboltable));} // the classes are disjoint.
                return;}}
    }


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
     * @throws Unsatisfiable when a contradiction is found.
     */
    protected void processEquivalence(int literal1, int literal2, InferenceStep inferenceStep) throws Unsatisfiable {
        if(monitoring) {
            monitor.print(monitorId,"Exec: equivalence: " + Symboltable.toString(literal1,symboltable) +
                    " = " + Symboltable.toString(literal2,symboltable));}
        EquivalenceClass equivalenceClass1 = null;
        EquivalenceClass equivalenceClass2 = null;
        int sign1 = 0;
        int sign2 = 0;
        for(EquivalenceClass equivalenceClass : equivalenceClasses) {
            sign1 = equivalenceClass.containsLiteral(literal1);
            if(sign1 != 0) {equivalenceClass1 = equivalenceClass; break;}}
        for(EquivalenceClass equivalenceClass : equivalenceClasses) {
            sign2 = equivalenceClass.containsLiteral(literal2);
            if(sign2 != 0) {equivalenceClass2 = equivalenceClass; break;}}

        if(sign1 != 0 && sign2 != 0) {
            EquivalenceClass equivalenceClass =
                    equivalenceClass1.joinEquivalenceClass(equivalenceClass2,
                            sign1*literal1, sign2*literal2,sign1*sign2, inferenceStep, observers);
            equivalenceClasses.remove(equivalenceClass1);equivalenceClasses.remove(equivalenceClass2);
            equivalenceClasses.add(equivalenceClass);
            ++statistics.joinedClasses;
            return;}
        if(sign1 != 0) {
            equivalenceClass1.addNewEquivalence(sign1*literal1,sign1*literal2,inferenceStep,observers);
            return;}
        if(sign2 != 0) {
            equivalenceClass2.addNewEquivalence(sign2*literal2,sign2*literal1,inferenceStep,observers);
            return;}

        ArrayList<InferenceStep> inferenceSteps = new ArrayList<>();
        inferenceSteps.add(inferenceStep);
        int representative = literal1; int literal = literal2;
        if(Math.abs(literal) < Math.abs(representative)) {representative = literal2; literal = literal1;}
        if(representative < 0) {representative *=-1; literal *= -1;}
        equivalenceClasses.add(new EquivalenceClass(representative,IntArrayList.wrap(new int[]{literal}),inferenceSteps));
        for(TriConsumer<Integer,Integer,InferenceStep> observer : observers)
            observer.accept(representative,literal,inferenceStep);
        ++statistics.derivedClasses;
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

    /** returns the number of equivalence classes
     *
     * @return the number of equivalence classes.
     */
    public synchronized int size() {
        return equivalenceClasses.size();
    }

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
        if(equivalenceClasses.isEmpty()) return "";
        StringBuilder string = new StringBuilder();
        for(EquivalenceClass equivalenceClass: equivalenceClasses) {
            string.append(equivalenceClass.toString(symboltable)).append("\n");}
        return string.toString();}

}
