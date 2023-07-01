package Solvers.Resolution;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;

import java.util.Stack;

/** A task is either a true literal which has to be applied to all clauses, or a clause to be simplified or expanded.
 * <br>
 * If the literal is != 0, then it must be a true literal which has to be applied to all clauses.<br>
 * If the clause is not null and expand == false then the clause has to be simplified.<br>
 * If expand == true then the clause is to be used for generating new resolvents
 * <br>
 * The priority is always positive, and smaller priority value actually means higher priority. <br>
 * If expand == false then the priority is the clause's size<br>
 * Otherwise the priority value is priorityShift + the clause's size. <br>
 * Therefore expansion tasks have always lower priority.
 */
public class Task {
    /** for distinguishing expansion tasks and simplification tasks. */
    private static final int priorityShift = 1000000;

    /** 0 or a true literal */
    int literal;
    /** null or a clause to be simplified or expanded */
    Clause clause;
    /** false: simplify the clause, true: expand the clause */
    boolean expand;

    /** the inference step for true literals */
    InferenceStep inferenceStep;
    /** the priority value */
    int priority;

    /** constructs a new task.
     *
     * @param literal 0 or a true literal.
     * @param clause  null or a clause.
     * @param expand  true: expand, false: simplify.
     */
    public Task(int literal, InferenceStep inferenceStep, Clause clause, boolean expand) {
        this.literal = literal;
        this.inferenceStep = inferenceStep;
        this.clause  = clause;
        this.expand  = expand;
        if(expand) priority = priorityShift + clause.size();
        else {priority = (clause != null) ? clause.size() : 1;}}

    /** turns the task into a string.
     *
     * @return the task as a string.
     */
    public String toString() {
        return toString(null);}

    /** turns the task into a string.
     *
     * @param symboltable null or a symboltable
     * @return the task as a string.
     */
    public String toString(Symboltable symboltable) {
        if(expand) return "Task Expand " + clause.toString(symboltable,0);
        return "Task Simplify " + ((literal == 0) ? clause.toString(symboltable,0): Symboltable.toString(literal,symboltable));
    }

    /** a stack of tasks to be reused.
     */
    private static final Stack<Task> taskStack = new Stack<>();

    /** either reuses an unused task or generates a new one.
     *
     * @param literal 0 or a true literal.
     * @param clause  null or a clause.
     * @param expand  true: expand, false: simplify.
     * @return a task.
     */
    static Task popTask(int literal, InferenceStep inferenceStep, Clause clause, boolean expand) {
        if(taskStack.isEmpty()) return new Task(literal,inferenceStep,clause,expand);
        Task task = taskStack.pop();
        task.literal = literal;
        task.inferenceStep = inferenceStep;
        task.clause = clause;
        task.expand = expand;
        if(expand) task.priority = priorityShift + clause.size();
        else {task.priority = (clause != null) ? clause.size() : 1;}
        return task;}

    /** puts a used task back into the stack (for later reusing it).
     *
     * @param task a now unused task.
     */
    static void pushTask(Task task) {
        task.clause = null;
        taskStack.push(task);}
}
