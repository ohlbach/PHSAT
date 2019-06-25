package Coordinator;

import Datastructures.Results.Result;
import Datastructures.Results.Unsatisfiable;
import Solvers.Solver;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.PriorityQueue;

/** This class maintains a priority queue of tasks.
 * The run-method executes the tasks in the queue according to their priority.
 * If the queue is empty then solver.nextTask() is called.
 * New tasks can be added by calling addTask, either when tasks are executed, or by other threads.
 *
 * The run-method stops when the queue is empty and the nextTask() method returns null.
 *
 * Created by ohlbach on 25.06.2019.
 */
public class TaskQueueThread  extends Thread {

    /** This is the task queue */
    private PriorityQueue<Task> taskQueue =
            new PriorityQueue<Task>(10, Comparator.comparingInt(task->task.priority));

    /** controls printing new tasks and tasks to be executed */
    private boolean debug = false;

    /** The solver which provides nextTask() */
    private Solver solver;

    /** The final result when the run-method has stopped. */
    public Result result = null;

    /** constructs a new TaskQueue threads
     *
     * @param solver which provides the nextTask method
     * @param debug controls printing added tasks and tasks to be executed
     */
    public TaskQueueThread(Solver solver, boolean debug) {
        this.solver = solver;
        this.debug = debug;}

    /** processes all task until a result is obtained or the queue becomes empty
     */
    public void run() {
        Task task;
        ArrayList<Task> tasks = new ArrayList<>();
        while(result == null && (task = getTask()) != null) {
            if(task.ignore) {continue;}
            if(debug) {
                System.out.println("TASK to be executed:");
                System.out.println(task.toString());}

            int trueLiteral = task.trueLiteral();
            if(trueLiteral != 0) { // a true literal may change the previously inserted tasks.
                tasks.clear();
                for(Task othertask : taskQueue) {
                    if(othertask.makeTrue(trueLiteral,tasks)) {
                        result = new Unsatisfiable(solver.model,trueLiteral);
                        return;}}
                for(Task newTask : tasks) {addTask(newTask);}}
            result = task.execute();}}

    /** returns the first task in the queue, or the the nextTask()
     *
     * @return the first task in the queue, or the the nextTask()
     */
    private synchronized Task getTask() {
        if(taskQueue.isEmpty()) {
            Task task = solver.nextTask();
            return (task == null) ? null : task;}
        return taskQueue.poll();}

    /** adds a task to the task queue.
     * It notifies a getTask-method in the central processor, which might be waiting for new tasks
     *
     * @param task the task to be added
     */
    public synchronized void addTask(Task task) {
        if(debug) {
            System.out.println("TASK Added");
            System.out.println(task.toString());}
        taskQueue.add(task);}
}
