package Solvers.Normalizer;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;

/**
 * The Task class represents a task in the Normalizer. It can have two types:
 * 1. True Literal Task: Represents a task that indicates a predicate with a true value.
 * 2. Equality Literal Task: Represents a task that indicates an equality between two literals.
 */
public class Task {
    enum TaskType {TRUELITERAL,EQUIVALENCE,PURITY};

    TaskType taskType;

    /** The inference step that caused the truth of the literal or the equalitiy of the literals respectively.
     * It may be null.*/
    InferenceStep inferenceStep;

    /** a true literal */
    int trueLiteral = 0;

    /** the first literal of the equality */
    int eqLiteral1;

    /** the second literal of the equality */
    int eqLiteral2;

    /** the priority of the task (the smaller the higher).
     * True literal tasks get higher priority.*/
    int priority;

    /** for distinguishing true literal tasks from equalitiy tasks */
    static private int priorityShift = 1000000;

    /** Constructs a Task object with a true literal and an InferenceStep.
     * The priority is priorityShift+|trueLiteral|.
     * This guarantees that the tasks are chosen deterministically.
     *
     * @param trueLiteral   the true literal value
     * @param inferenceStep the InferenceStep object that caused the truth of the literal
     */
    public Task(int trueLiteral, InferenceStep inferenceStep) {
        taskType = TaskType.TRUELITERAL;
        this.trueLiteral = trueLiteral;
        this.inferenceStep = inferenceStep;
        priority = priorityShift + Math.abs(trueLiteral);}

    /** Constructs a Task object for an equality and an InferenceStep, and calculates its priority.
     * The priority is |eqLiteral1|+ |eqLiteral2|.
     * This guarantees that the tasks are chosen deterministically.
     *
     * @param eqLiteral1 the first literal of the equality
     * @param eqLiteral2 the second literal of the equality
     * @param inferenceStep the InferenceStep object that caused the equality of the literals
     */
    public Task(int eqLiteral1, int eqLiteral2, InferenceStep inferenceStep) {
        taskType = TaskType.EQUIVALENCE;
        this.eqLiteral1 = eqLiteral1;
        this.eqLiteral2 = eqLiteral2;
        this.inferenceStep = inferenceStep;
        priority = Math.abs(eqLiteral1) + Math.abs(eqLiteral2);}

    /** generates a PURITY task.
     */
    public Task() {
        taskType = TaskType.PURITY;
        priority = 2*priorityShift;}

    /** Returns a string representation of the Task object.
     *
     * @param symboltable null or the Symboltable object used to obtain the string representation of the literals.
     * @return The string representation of the Task object.
     */
    public String toString(Symboltable symboltable) {
        switch (taskType) {
            case TRUELITERAL: return "true(" + Symboltable.toString(trueLiteral,symboltable) + ")";
            case EQUIVALENCE: return symboltable.toString(eqLiteral1,symboltable) + " = " + Symboltable.toString(eqLiteral2,symboltable);
            default: return "Purity";}}
}
