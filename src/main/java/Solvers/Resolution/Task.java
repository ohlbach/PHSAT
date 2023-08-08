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
    TaskType taskType;

    /** the inference step for true literals */
    InferenceStep inferenceStep;
    /** the priority value */
    int priority;

    /** constructs a new task.
     *
     * @param literal 0 or a true literal.
     * @param clause  null or a clause.
     * @param taskType  the type of task.
     */
    public Task(int literal, InferenceStep inferenceStep, Clause clause, TaskType taskType) {
        this.literal = literal;
        this.inferenceStep = inferenceStep;
        this.clause  = clause;
        this.taskType = taskType;
        setPriority();}

    void setPriority() {
        switch (taskType) {
            case TRUELITERAL:      priority = Math.abs(literal); break;
            case SIMPLIFYSELF:     priority = priorityShift   + clause.size(); break;
            case SIMPLIFYOTHERS:   priority = 2*priorityShift + clause.size(); break;
            case EXPANDBINARY:     priority = 3*priorityShift + clause.size(); break;
            case PARTIALPURITY:    priority = 4*priorityShift; break;
            case TRIPLERESOLUTION: priority = 5*priorityShift + clause.size();}}

    /** changes the task type of the task and adapts the priority.
     *
     * @param taskType a new task type for the task.
     */
    void setTaskType(TaskType taskType) {
        this.taskType = taskType;
        setPriority();}

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
        String cl = clause != null ? clause.toString(symboltable,0) : null;
        String pr = " Priority: " + priority;
        switch (taskType) {
            case TRUELITERAL:    return "True Literal Task: " + Symboltable.toString(literal,symboltable) + pr;
            case SIMPLIFYSELF:   return "Simplify-Self Task: "    + cl+ pr ;
            case SIMPLIFYOTHERS: return "Simplify-Others Task: "  + cl+ pr ;
            case EXPANDBINARY:   return "Binary Expansion Task: " + cl+ pr ;
            case PARTIALPURITY:  return "Partial Purity Task "    + pr;}
        return "";}

    /** a stack of tasks to be reused.
     */
    private static final Stack<Task> taskStack = new Stack<>();

    /** either reuses an unused task or generates a new one.
     *
     * @param literal 0 or a true literal.
     * @param clause  null or a clause.
     * @param taskType  true: taskType, false: simplify.
     * @return a task.
     */
    static Task popTask(int literal, InferenceStep inferenceStep, Clause clause, TaskType taskType) {
        if(taskStack.isEmpty()) return new Task(literal,inferenceStep,clause,taskType);
        Task task = taskStack.pop();
        task.literal = literal;
        task.inferenceStep = inferenceStep;
        task.clause = clause;
        task.taskType = taskType;
        task.setPriority();
        return task;}

    /** puts a used task back into the stack (for later reusing it).
     *
     * @param task a now unused task.
     */
    static void pushTask(Task task) {
        task.clause = null;
        taskStack.push(task);}
}
