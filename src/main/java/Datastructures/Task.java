package Datastructures;

import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.concurrent.PriorityBlockingQueue;

/** A Task class for priority queues
 *
 * @param <TaskType>
 */
public class Task<TaskType> {
    public TaskType taskType;
    public int priority;
    public Object a;
    public Object b;

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
    }

    /** turns the task into a string.
     *
     * @return a string
     */
    public String toString() {
        StringBuilder st = new StringBuilder();
        st.append("\n  "+taskType.toString());
        if(a != null) st.append(" a: " + (a.getClass() == int[].class ? Arrays.toString((int[])a) : a.toString()));
        if(b != null) st.append(", b: " + (b.getClass() == int[].class ? Arrays.toString((int[])b) : b.toString()));
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
