package Coordinator;

import Datastructures.Clauses.Clause;
import Datastructures.Results.Result;
import Datastructures.Results.Satisfiable;
import Datastructures.Results.Unsatisfiable;

import java.util.Arrays;

/** The purpose of this class and its subclasses is to fill a Priority Chain with prioritizes task to be executed.
 * The smaller the priority the earlier the tasks are executed.
 * Created by ohlbach on 10.10.2018.
 */
public abstract class Task {
    /** the task's propority */
    public int priority;
    /** the processor which executes the task */
    public Processor processor;

    /** constructs a task
     *
     * @param priority   of the task
     * @param processor  the processor which executes the task
     */
    Task(int priority, Processor processor) {
        this.priority = priority;
        this.processor = processor;}

    /** The method executes the task. It must be implemented in the subclasses
     *
     * @return the result of execution
     */
    public abstract Result execute();

    /** It contains an Unsatisfiable object.
     * Because of priority 0 it is moved to the front of the task queue
     */
    public static class Unsatisfiability extends Task {
        Unsatisfiable unsatisfiable;

        Unsatisfiability(Unsatisfiable unsatisfiable, Processor processor) {
            super(0, processor);
            this.unsatisfiable = unsatisfiable;}

        public Result execute(){return unsatisfiable;}

        public String toString() {
            return "Task: Unsatisfiable " + unsatisfiable.toString();}}

    /** It contains a Satisfiable object.
     * Because of priority 0 it is moved to the front of the task queue
     */
    public static class Satisfiability extends Task {
        Satisfiable satisfiable;

        Satisfiability(Satisfiable satisfiable, Processor processor) {
            super(0, processor);
            this.satisfiable = satisfiable;}

        public Result execute(){return satisfiable;}

        public String toString() {
            return "Task: Satisfiable " + satisfiable.toString();}
    }


    /** It contains a newly derived unit literal.
     */
    public static class OneLiteral extends Task {
        int literal;
        OneLiteral(int literal, Processor processor) {
            super(1, processor);
            this.literal = literal;}

        public Result  execute() {return processor.processOneLiteralClause(literal);}

        public String toString() {
            return "Task: One Literal " + literal;}}

    /** It contains a newly derived equivalence class
     */
    public static class Equivalence extends Task {
        int[] equivalences;

        Equivalence(int[] equivalences, Processor processor) {
            super(2, processor);
            this.equivalences = equivalences;}

        public Result execute() {return processor.processEquivalence(equivalences);}

        public String toString() {
            return "Task: Equivalences " + Arrays.toString(equivalences);}
    }

    /** It contains a newly derived two-literal clause
     */
    public static class TwoLiteral extends Task {
        int literal1,literal2;
        TwoLiteral(int literal1, int literal2, Processor processor) {
            super(3, processor);
            this.literal1 = literal1;
            this.literal2 = literal2;}

        public Result execute() {return processor.processTwoLiteralClause(literal1,literal2);}

        public String toString() {
            return "Task: Binary Clause " + literal1 + "," + literal2;}
    }

    /** It contains a newly shortened clause with at least 3 literals.
     */
    public static class ShortenedClause extends Task {
        Clause clause;

        ShortenedClause(Clause clause, Processor processor) {
            super(4, processor);
            this.clause = clause;}

        public Result execute() {return processor.processLongerClause(clause);}

        public String toString() {
            return "Task: Longer Clause " + clause.toString();}
}

}
