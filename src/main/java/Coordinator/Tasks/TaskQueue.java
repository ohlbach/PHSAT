package Coordinator.Tasks;

import Datastructures.Results.Result;
import Management.Monitor.MonitorLife;

import java.util.Arrays;
import java.util.Comparator;
import java.util.concurrent.PriorityBlockingQueue;

/** This class maintains a priority queue of tasks.
 * The processTasks-method executes the tasks in the queue according to their priority.
 * If the queue is empty then taskSupplier.nextTask() is called.
 * New tasks can be added by calling addTask, either when tasks are executed, or by other threads.
 *
 * The processTasks-method stops when the queue is empty and the nextTask() method returns null.
 *
 * Created by ohlbach on 25.06.2019.
 */
public class TaskQueue {

    private String id;

    /** This is the task queue */
    private PriorityBlockingQueue<Task> taskQueue =
            new PriorityBlockingQueue<Task>(10, Comparator.comparingInt(task->task.priority));

    /** controls printing new tasks and tasks to be executed */
    private MonitorLife monitor = null;

    /** constructs a new TaskQueue threads
     *
     * @param monitor controls printing added tasks and tasks to be executed
     */
    public TaskQueue(String id, MonitorLife monitor) {
        this.id = id;
        this.monitor = monitor;}

    /** processes all task until a result is obtained or the queue becomes empty
     */
    public Result run() throws InterruptedException {
        Task task;
        while((task = taskQueue.poll()) != null) {
            if(monitor != null) {monitor.print(id,"Executing "+ task.toString());}
            if(Thread.interrupted()) {throw new InterruptedException();}
            Result result = task.handler.get();
            if(result != null) {return result;}}
        return null;}


    /** adds a task to the task queue.
     *
     * @param task the task to be added
     */
    public void add(Task task) {
        if(monitor != null) {monitor.print(id,"Inserted  "+task.toString());}
        taskQueue.add(task);}

    /** returns true if the task queue is empty
     *
     * @return true if the task queue is empty
     */
    public boolean isEmpty() {
        return taskQueue.isEmpty();}

    /** turns the task queue into a string, a line for each task
     *
     * @return the task queue as a string.
     */
    public String toString() {
        StringBuilder st = new StringBuilder();
        Task[] array = new Task[taskQueue.size()];
        taskQueue.toArray(array);
        Arrays.sort(array,Comparator.comparingInt(task->task.priority));
        int i = 0;
        for(Task task : array) {st.append(""+ ++i+". ").append(task.toString()).append("\n");}
        return st.toString();
    }


}
