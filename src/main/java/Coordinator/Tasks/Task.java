package Coordinator.Tasks;

import Datastructures.Results.Result;

import java.util.function.Supplier;

/** The purpose of this class and its subclasses is to fill a Priority Chain with prioritizes tasks to be executed.
 * The smaller the listPosition the earlier the tasks are executed.
 * <br>
 * Created by ohlbach on 10.10.2018.
 */
public class Task {
    /** the task's listPosition */
    public int priority;

    public Supplier<Result> handler;
    Supplier<String> toString;

    /** constructs a task
     *
     * @param priority   of the task
     */
    public Task(int priority, Supplier<Result> handler, Supplier<String> toString) {
        this.priority = priority;
        this.handler = handler;
        this.toString = toString;}

    public String toString() {
        return "P" + priority + ": " + toString.get();}



}
