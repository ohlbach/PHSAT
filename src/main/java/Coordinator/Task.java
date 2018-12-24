package Coordinator;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseStructure;
import Datastructures.Results.Result;
import Datastructures.Results.Satisfiable;
import Datastructures.Results.Unsatisfiable;
import Utilities.Utilities;

import java.util.ArrayList;
import java.util.Arrays;

/** The purpose of this class and its subclasses is to fill a Priority Chain with prioritizes tasks to be executed.
 * The smaller the priority the earlier the tasks are executed.
 * <br>
 * Created by Ohlbach on 10.10.2018.
 */
public abstract class Task {
    /** the task's priority */
    public int priority;
    /** the processor which executes the task */
    public Processor processor;
    /** a task may become obsolete. Then ignore should become true */
    public boolean ignore = false;

    /** constructs a task
     *
     * @param priority   of the task
     * @param processor  the processor which executes the task
     */
    public Task(int priority, Processor processor) {
        this.priority = priority;
        this.processor = processor;}

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
        if(processor.monitoring) {
            processor.monitor.print(processor.id,toString());}
        return null;}

    /** It contains an Unsatisfiable object.
     * Because of priority 0 it is moved to the front of the task queue
     */
    public static class Unsatisfiability extends Task {
        private Unsatisfiable unsatisfiable;

        /** creates a task signalling an unsatisfiability.
         * It gets priority 0 and is therefore moved to the front of the priority queue.
         *
         * @param unsatisfiable an Unsatisfiability object
         * @param processor      which generated the unsatisfiability
         */
        public Unsatisfiability(Unsatisfiable unsatisfiable, Processor processor) {
            super(0, processor);
            this.unsatisfiable = unsatisfiable;}

        /**
         * @return the Unsatisfiable object
         */
        public Result execute(){super.execute();  return unsatisfiable;}

        public String toString() {
            return "Task: " + unsatisfiable.toString();}}

    /** It contains a Satisfiable object.
     * Because of priority 0 it is moved to the front of the task queue
     */
    public static class Satisfiability extends Task {
        private Satisfiable satisfiable;

        /** creates a task signalling a satisfiability.
         * It gets priority 0 and is therefore moved to the front of the priority queue.
         *
         * @param satisfiable a Satisfiability object
         * @param processor   which generated the unsatisfiability
         */
        public Satisfiability(Satisfiable satisfiable, Processor processor) {
            super(0, processor);
            this.satisfiable = satisfiable;}

        /**
         * @return the Satisfiable object
         */
        public Result execute(){super.execute();  return satisfiable;}

        public String toString() {
            return "Task: Satisfiable " + satisfiable.toString();}
    }


    /** It contains a newly derived unit literal.
     */
    public static class TrueLiteral extends Task {
        /** the unit literal */
        private int literal;

        /** constructs a unit-literal task.
         * It gets priority 1
         *
         * @param literal   the unit literal
         * @param processor which generated the unit literal
         */
        public TrueLiteral(int literal, Processor processor) {
            super(1, processor);
            this.literal = literal;}

        /** the unit literal may have become true or false in the meantime
         *
         * @param trueLiteral a true literal
         * @param tasks   for adding new tasks
         * @return true if a contradiction was detected
         */
        public boolean makeTrue(int trueLiteral, ArrayList<Task> tasks) {
            if(literal == trueLiteral) {ignore = true; return false;}
            if(literal == -trueLiteral) {return true;}
            return false;}

        /** calls the processor's processOneLiteralClause method
         *
         * @return Un/Satisfiable if this has ben detected, otherwise null
         */
        public Result  execute() {super.execute();  return processor.processOneLiteralClause(literal);}

        public String toString() {
            return "Task: One Literal " + literal;}}

    /** It contains a newly derived equivalence class
     */
    public static class Equivalence extends Task {
        /** the equivalence class. The first literal servs as representative */
        private int[] equivalences;

        /** constructs an Equivalence Task
         *
         * @param equivalences  an equivalence class
         * @param processor which execute the equivalence processing
         */
        public Equivalence(int[] equivalences, Processor processor) {
            super(3, processor);
            this.equivalences = equivalences;}

        /** Some of the literals in the equivalence class may have become true or false in the meantime.
         * In this case the other literals in the class become also true/false.
         *
         * @param trueLiteral    a true literal
         * @param tasks   for adding new tasks
         * @return false
         */
            public boolean makeTrue(int trueLiteral, ArrayList<Task> tasks) {
                    int position = Utilities.contains(equivalences,trueLiteral);
                if(position >= 0) {
                    for(int i = 0; i < equivalences.length; ++i) {
                        if(i != position) {tasks.add(new TrueLiteral(equivalences[i],processor));}}
                    ignore = true;
                    return false;}
                position = Utilities.contains(equivalences,-trueLiteral);
                if(position >= 0) {
                    for(int i = 0; i < equivalences.length; ++i) {
                        if(i != position) {tasks.add(new TrueLiteral(-equivalences[i],processor));}}
                    ignore = true;
                    return false;}
                return false;}

        /** asks the processor to deal with the equivalence
         *
         * @return Un/Satisfiable if this has been detected, otherwise null
         */
        public Result execute() {super.execute();  return processor.processEquivalence(equivalences);}

        public String toString() {
            return "Task: Equivalences " + Arrays.toString(equivalences);}
    }

    /** It contains a newly derived two-literal clause
     */
    public static class BinaryClause extends Task {
        /** the two literals of the clause */
        private  int literal1,literal2;

        /** constructs a two-literal task
         *
         * @param literal1 a literal
         * @param literal2 a literal
         * @param processor which has to process the task
         */
        public BinaryClause(int literal1, int literal2, Processor processor) {
            super(4, processor);
            this.literal1 = literal1;
            this.literal2 = literal2;}

        /** one or both literals may have become true/false in the meantime.
         * In this case one-literal task are generated.
         *
         * @param trueLiteral a true literal
         * @param tasks   for adding new tasks
         * @return false
         */
        public boolean makeTrue(int trueLiteral, ArrayList<Task> tasks) {
            if(trueLiteral == literal1 || trueLiteral == literal2) {ignore = true; return false;}
            if(trueLiteral == -literal1) {
                tasks.add(new TrueLiteral(literal2,processor));
                ignore = true;
                return false;}
            if(trueLiteral == -literal2) {
                tasks.add(new TrueLiteral(literal1,processor));
                ignore = true;
                return false;}
            return false;}

        /** calls the processor to process the two-literal clause
         *
         * @return  Un/Satisfiable if this has been detected, otherwise null
         */
        public Result execute() {
            super.execute();
            return processor.processTwoLiteralClause(literal1,literal2);}

        public String toString() {
            return "Task: Binary Clause " + literal1 + "," + literal2;}
    }

    /** It contains a newly shortened clause with at least 3 literals.
     */
    public static class ShortenedClause extends Task {
        /** a clause which may trigger further simplifications */
        private  Clause clause;

        /** constructs a task for using the clause to simplify other clauses
         *
         * @param clause    a clause with at least 3 literals
         * @param processor which must process the clause
         */
        public ShortenedClause(Clause clause, Processor processor) {
            super(5, processor);
            this.clause = clause;}

        /** If a literal in the clause became true or false, it has already been changed.
         *  If the clause has been removed, the task becomes obsolete.
         *
         * @param trueLiteral  ignored
         * @param tasks   for adding new tasks
         * @return false
         */
        public boolean makeTrue(int trueLiteral, ArrayList<Task> tasks) {
            if(clause.removed) {ignore = true;}
            return false;}

        /** calls the processor to use the clause for further simplifications
         *
         * @return  Un/Satisfiable if this has been detected, otherwise null
         */
        public Result execute() {super.execute();  return processor.processLongerClause(clause);}

        public String toString() {
            return "Task: Longer Clause " + clause.toString();}
}

    /** It removes pure clauses
     */
    public static class Purity extends Task {
        /** a pure literal */
        private int literal;

        /** constructs a task for purity removal
         *
         * @param literal a pure literal
         * @param processor which must process the purity removal
         */
        public Purity(int literal, Processor processor) {
            super(2, processor);
            this.literal = literal;}

        public Result execute() {super.execute();  return processor.processPurity(literal);}

        public String toString() {
            return "Task Purity: " + literal;}

        /** The pure literal may already have become true or false.
         * A pure literal may become true, but there may also be models where the literal is false.
         * Therefore a pure literal which has become false is not a contradiction.
         * Nevertheless if the literal has become true/false, the task becomes obsolete.
         *
         * @param trueLiteral  a true literal
         * @param tasks   for adding new tasks
         * @return false
         */
        public boolean makeTrue(int trueLiteral, ArrayList<Task> tasks) {
            if(literal == trueLiteral || literal == -trueLiteral ) {ignore = true;}
            return false;}}



    /** It removes a literal from a clause
     *  This task can be generated by an external processor which has different data structures
     */
    public static class LiteralRemoval extends Task {
        private String clauseId;
        private int literal;

        /** constructs a LiteralRemoval task
         *
         * @param clauseId  of the clause from which the literal is to be removed
         * @param literal   the literal to be removed
         * @param processor which has to do the removal
         */
        public LiteralRemoval(String clauseId, int literal, Processor processor) {
            super(6,processor);
            this.clauseId = clauseId;
            this.literal  = literal;}

        /** calls the processor's processLiteralRemoval method
         *
         * @return the result of the processLiteralRemoval method
         */
        public Result execute() {
            super.execute();
            return processor.processLiteralRemoval(clauseId,literal);}

        /** If the literal has become true/false already, nothing has to be done
         *
         * @param trueLiteral a true literal
         * @param tasks   for adding new tasks
         * @return false
         */
        public boolean makeTrue(int trueLiteral, ArrayList<Task> tasks) {
            if(Math.abs(literal) == Math.abs(trueLiteral)) {ignore = true;}
            return false;}

        public String toString() {
            return "Task: Remove literal " + literal + " from clause " + clauseId;}
    }
}
