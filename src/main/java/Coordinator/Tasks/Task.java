package Coordinator.Tasks;

import Datastructures.Results.Result;
import Management.Monitor;

import java.util.ArrayList;

/** The purpose of this class and its subclasses is to fill a Priority Chain with prioritizes tasks to be executed.
 * The smaller the listPosition the earlier the tasks are executed.
 * <br>
 * Created by ohlbach on 10.10.2018.
 */
public abstract class Task {
    /** the task's listPosition */
    public int priority;

    /** a task may become obsolete. Then ignore should become true */
    public boolean ignore = false;

    private Monitor monitor;

    private String sourceId;

    /** constructs a task
     *
     * @param priority   of the task
     */
    public Task(int priority, Monitor monitor, String sourceId) {
        this.priority = priority;
        this.monitor = monitor;
        this.sourceId = sourceId;}

    /** takes care of a true literal which is still in the task queue
     *
     * @param literal a true literal
     * @param tasks   for adding new tasks
     * @return true if a contradiction was detected
     */
    public boolean makeTrue(int literal, ArrayList<Task> tasks) {return false;}

    /** The method executes the task. It must be implemented in the subclasses
     *
     * @return the result of execution
     */
    public Result execute() {
        if(monitor != null) {monitor.print(sourceId,toString());}
        return null;}

    public int trueLiteral() {return 0;}




}
