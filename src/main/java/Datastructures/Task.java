package Datastructures;

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
    public IntArrayList origins;
    public Object a;
    public Object b;
    public Task(TaskType taskType, IntArrayList origins, Object a, Object b) {
        this.taskType = taskType;
        this.origins = origins;
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
        if(origins != null) st.append(" @ ").append(origins.toString());
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
