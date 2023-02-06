package Datastructures;

import java.util.Arrays;
import java.util.concurrent.PriorityBlockingQueue;

/** A Task class for priority queues
 *
 * @param <TaskType> Type of task
 */
public class Task<TaskType> {
    public TaskType taskType;
    public int priority;
    public Object a;
    public Object b;
    public Object c;

    /** creates a new task
     *
     * @param taskType       the type of the task
     * @param a              customary
     */
    public Task(TaskType taskType, Object a) {
        this.taskType = taskType;
        this.a = a;
        this.b = null;
        this.c = null;
    }

    /** creates a new task
     *
     * @param taskType       the type of the task
     * @param a              customary
     * @param b              customary
     */
    public Task(TaskType taskType, Object a, Object b) {
        this.taskType = taskType;
        this.a = a;
        this.b = b;
        this.c = null;
    }

    /** creates a new task
     *
     * @param taskType       the type of the task
     * @param a              customary
     * @param b              customary
     * @param c              customary
     */
    public Task(TaskType taskType, Object a, Object b, Object c) {
        this.taskType = taskType;
        this.a = a;
        this.b = b;
        this.c = c;
    }
    /** turns the task into a string.
     *
     * @return a string
     */
    public String toString() {
        StringBuilder st = new StringBuilder();
        try{
            Class inferenceStep = Class.forName("InferenceSteps.InferenceStep");
            st.append("\n  "+taskType.toString());
            if(a != null && a.getClass().getSuperclass() != inferenceStep)
                st.append(" a: " + (a.getClass() == int[].class ? Arrays.toString((int[])a) : a.toString()));
            if(b != null && b.getClass().getSuperclass() != inferenceStep)
                st.append(", b: " + (b.getClass() == int[].class ? Arrays.toString((int[])b) : b.toString()));}
        catch(Exception ex) {System.out.println( "Unknown class name: InferenceSteps.InferenceStep");}
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
