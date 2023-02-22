package Solvers.Simplifier;

import Datastructures.Clauses.InputClauses;
import Datastructures.Results.Result;
import Datastructures.Statistics.Statistic;
import Datastructures.Symboltable;
import Datastructures.Task;
import InferenceSteps.InferenceStep;
import Management.Monitor.Monitor;
import Management.ProblemSupervisor;
import Solvers.Solver;

import java.util.Comparator;
import java.util.concurrent.PriorityBlockingQueue;

import static java.lang.Thread.interrupted;

public class Simplifier extends Solver {

    private InputClauses inputClauses;
    private ProblemSupervisor problemSupervisor;

    /** controls the computation of the clause's origins */
    public boolean trackReasoning;

    /** The id of the current problem to be solved */
    private String problemId;

    /** for logging the actions of this class */
    private Monitor monitor;

    /** indicates monitoring is on */
    private boolean monitoring;

    /** for distinguishing the monitoring areas */
    private String monitorId;

    private Literals literalIndex;
    private Clauses clauses;

    public Simplifier(ProblemSupervisor problemSupervisor) {
        this.problemSupervisor = problemSupervisor;
        this.inputClauses = problemSupervisor.inputClauses;
    }

    public Simplifier(InputClauses inputClauses) {
        this.inputClauses = inputClauses;}

    private enum TaskType {
        /** a new true literal is obtained from the model */
        TrueLiteral,
        /** a new binary equivalence is found in the TwoLiteral module. */
        Equivalence,
        TwoLiteralClause,
        SimplifyTwoLiteralClauses,
        SimplifyLongerClauses
    }

    /** gets the priority for the objects in the queue.
     *
     * @param task the task in the queue
     * @return the priority of the objects in the queue.
     */
    private int getPriority(Task<Simplifier.TaskType> task) {
        switch(task.taskType) {
            case TrueLiteral: return Integer.MIN_VALUE + Math.abs((Integer)task.a);
            case Equivalence: return (Math.abs((Integer)task.a)); // this guarantees a deterministic sequence of the tasks
            case TwoLiteralClause:          return 100;
            case SimplifyTwoLiteralClauses: return 101;
            case SimplifyLongerClauses:     return 102;}
        return 0;}

    /** A queue of newly derived unit literals and binary equivalences.
     * The unit literals are automatically put at the beginning of the queue.
     */
    private final PriorityBlockingQueue<Task<Simplifier.TaskType>> queue =
            new PriorityBlockingQueue<>(10, Comparator.comparingInt(this::getPriority));

    /** adds a true literal to the queue
     *
     * @param literal a true literal
     * @param inferenceStep which caused the truth
     */
    public void addTrueLiteral(int literal, InferenceStep inferenceStep) {
        if(monitoring) {
            monitor.print(monitorId,"In:   True literal " +
                    Symboltable.toString(literal,symboltable));}
        synchronized (this) {queue.add(new Task<>(TaskType.TrueLiteral, literal, inferenceStep));}}


    public void run() {
        Task<Simplifier.TaskType> task;
        while(!interrupted()) {
            try {
                if(monitoring) {monitor.print(monitorId,"Queue is waiting\n" + Task.queueToString(queue));}
                task = queue.take(); // waits if the queue is empty
                switch(task.taskType){
                    case TrueLiteral: applyTrueLiteral((Integer)task.a,(InferenceStep)task.b); break;
                    case Equivalence: applyEquivalence((Integer)task.a,(Integer)task.b,(InferenceStep) task.c); break;
                    case TwoLiteralClause: break;
                    case SimplifyTwoLiteralClauses: break;
                    case SimplifyLongerClauses: break;}}
            catch(InterruptedException ex) {return;}
            catch(Result result) {
                problemSupervisor.finished("Simplifier", result,"");
                return;}}}

    protected void applyTrueLiteral(int literal, InferenceStep inferenceStep) throws Result {
        Literal literalData = literalIndex.getFirstLiteral(literal);
        while(literalData != null) {
            Clause clause = literalData.clause;
            if(clause.applyTrueLiteral(literalData, literalIndex,model)) clauses.removeClause(clause);
            Literal nextLiteral = literalData.nextLiteral;
            literalIndex.removeLiteral(literalData);
            literalData = nextLiteral;}

        literalData = literalIndex.getFirstLiteral(-literal);
        while(literalData != null) {
            Clause clause = literalData.clause;
            if(clause.applyFalseLiteral(literalData, literalIndex,model)) clauses.removeClause(clause);
            Literal nextLiteral = literalData.nextLiteral;
            literalIndex.removeLiteral(literalData);
            literalData = nextLiteral;}


    }

    protected void applyEquivalence(int representative, int literal, InferenceStep inferenceStep) {

    }

    @Override
    public Result solveProblem(InputClauses inputClauses) {
        return null;
    }

    @Override
    public void prepare() {

    }

    @Override
    public Statistic getStatistics() {
        return null;
    }
}
