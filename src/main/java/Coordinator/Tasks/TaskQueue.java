package Coordinator.Tasks;

import Datastructures.Results.Result;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Theory.Model;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.PriorityQueue;
import java.util.function.Supplier;

/** This class maintains a priority queue of tasks.
 * The run-method executes the tasks in the queue according to their priority.
 * If the queue is empty then taskSupplier.nextTask() is called.
 * New tasks can be added by calling addTask, either when tasks are executed, or by other threads.
 *
 * The run-method stops when the queue is empty and the nextTask() method returns null.
 *
 * Created by ohlbach on 25.06.2019.
 */
public class TaskQueue extends Thread {

    /** This is the task queue */
    private PriorityQueue<Task> taskQueue =
            new PriorityQueue<Task>(10, Comparator.comparingInt(task->task.priority));

    /** controls printing new tasks and tasks to be executed */
    private boolean monitor = false;

    /**  a model for the clauses */
    private Model model = null;

    /** The taskSupplier which provides nextTask() */
    private Supplier<Task> taskSupplier;

    /** The final result when the run-method has stopped. */
    public Result result = null;

    private boolean stopped = false;

    /** constructs a new TaskQueue threads
     *
     * @param taskSupplier which provides the nextTask method
     * @param model a model for the clauses
     * @param monitor controls printing added tasks and tasks to be executed
     */
    public TaskQueue(Supplier<Task> taskSupplier, Model model, boolean monitor) {
        this.taskSupplier = taskSupplier;
        this.model = model;
        this.monitor = monitor;}

    /** processes all task until a result is obtained or the queue becomes empty
     */
    public void run() {
        Task task;
        ArrayList<Task> tasks = new ArrayList<>();
        while(result == null && (task = getTask()) != null) {
            if(task.ignore) {continue;}
            if(monitor) {
                System.out.println("TASK to be executed:");
                System.out.println(task.toString());}
            if(task == stopTask) {return;}

            int trueLiteral = task.trueLiteral();
            if(trueLiteral != 0) { // a true literal may change the previously inserted tasks.
                tasks.clear();
                for(Task othertask : taskQueue) {
                    if(othertask.makeTrue(trueLiteral,tasks)) {
                        result = new Unsatisfiable(model,trueLiteral);
                        return;}}
                for(Task newTask : tasks) {addTask(newTask);}}
            result = task.execute();}}

    /** returns the first task in the queue, or the the nextTask()
     *
     * @return the first task in the queue, or the the nextTask()
     */
    private synchronized Task getTask() {
        if(taskQueue.isEmpty()) {
            if(taskSupplier == null) {
                while(taskQueue.isEmpty()) {
                    try {wait();}
                    catch (InterruptedException e) {return null;}}}
        else {return taskSupplier.get();}}
        return taskQueue.poll();}

    /** adds a task to the task queue.
     *
     * @param task the task to be added
     */
    public synchronized void addTask(Task task) {
        if(stopped) {return;}
        if(monitor) {
            System.out.println("TASK Added");
            System.out.println(task.toString());}
        taskQueue.add(task);
        if(taskSupplier == null) {notify();}}

    private static Task stopTask = new Stop();

    /** This method adds a Stop-task to the end of the queue.
     * It causes all other task to be finished, and then to stop.
     * The method waits until the task queue is worked off.
     * An interrupt causes all tasks to be deleted and to stop the execution.
     */
    public synchronized void finish() {
        taskQueue.add(stopTask);
        stopped = true;
        if(monitor) {System.out.println("Task Queue stopped");}
        try {join();}
        catch (InterruptedException e) {
            taskQueue.clear();
            taskQueue.add(stopTask);}}


    private static class Stop extends Task {
        public Stop() {
            super(Integer.MAX_VALUE,null,"stop");}
    }
}
