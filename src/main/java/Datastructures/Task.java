package Datastructures;

import InferenceSteps.InferenceStep;

import java.util.Arrays;
import java.util.concurrent.PriorityBlockingQueue;

/** A Task class for priority queues
 */
public class Task {
    enum TaskType {TRUELITERAL,PURITY, SHORTENED_CLAUSE};

    static int purityShift = 1000000;
    static int subsumptionShift = 2000000;

    TaskType taskType;

    int literal = 0;

    Clause clause = null;

    InferenceStep inferenceStep;

    /** the priority of the task (the smaller the higher).
     * True literal tasks get higher priority.*/
    public int priority;

    /** creates a new task
     *
     * @param taskType       the type of the task
     */
    public Task(TaskType taskType, int literal, InferenceStep inferenceStep) {
        this.taskType = taskType;
        this.literal = literal;
        this.inferenceStep = inferenceStep;
        switch(taskType) {
            case TRUELITERAL: priority = Math.abs(literal);
            case PURITY: priority = Math.abs(literal) + purityShift;
        }
    }

    public Task(TaskType taskType, Clause clause) {
        this.taskType = taskType;
        this.clause = clause ;
        priority = clause.id + subsumptionShift;
    }



    /** turns the task into a string.
     *
     * @return a string
     */
    public String toString(Symboltable symbolTable) {
        StringBuilder st = new StringBuilder();
        st.append(taskType.toString());
        if (clause != null) st.append("Clause: ").append(clause.toString(symbolTable,0));
        if(literal != 0) {
            st.append(": Literal ").append(Symboltable.toString(literal,symbolTable));
            if(inferenceStep != null) st.append("\n  ").append(inferenceStep.toString(symbolTable));}
        return st.toString();}

    /** turns the queue into a string of tasks, sorted according to the queue's comparator.
     *
     * @param queue a PriorityBlockingQueue
     * @return the queue as string.
     */
    public static String queueToString(PriorityBlockingQueue queue) {
        Task[] tasks = new Task[queue.size()];
        queue.toArray(tasks);
        Arrays.sort(tasks, queue.comparator());
        return Arrays.toString(tasks);}
}
