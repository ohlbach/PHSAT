package Datastructures;

import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.Arrays;

/** A Task class for priority queues
 *
 * @param <TaskType>
 */
public class Task<TaskType> {
    public TaskType taskType;
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
        if(origins != null) st.append(" ").append(origins.toString());
        if(a != null) st.append(" a: " + (a.getClass() == int[].class ? Arrays.toString((int[])a) : a.toString()));
        if(b != null) st.append(", b: " + (b.getClass() == int[].class ? Arrays.toString((int[])b) : b.toString()));
        return st.toString();}
}
