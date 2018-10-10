package Coordinator;

import Algorithms.Algorithms;
import Datastructures.Clauses.Clause;
import Datastructures.Results.Result;
import Datastructures.Results.Satisfiable;
import Datastructures.Results.Unsatisfiable;

/**
 * Created by ohlbach on 10.10.2018.
 */
public abstract class Task {
    public int priority;
    public Processor processor;

    Task(int priority, Processor processor) {
        this.priority = priority;
        this.processor = processor;}

    public abstract Result execute();

    public static class Unsatisfiability extends Task {
        Unsatisfiable unsatisfiable;

        Unsatisfiability(Unsatisfiable unsatisfiable, Processor processor) {
            super(0, processor);
            this.unsatisfiable = unsatisfiable;}

        public Result execute(){return unsatisfiable;}
    }

    public static class Satisfiability extends Task {
        Satisfiable satisfiable;

        Satisfiability(Satisfiable satisfiable, Processor processor) {
            super(0, processor);
            this.satisfiable = satisfiable;}

        public Result execute(){return satisfiable;}
    }


    public static class OneLiteral extends Task {
        int literal;
        OneLiteral(int literal, Processor processor) {
            super(1, processor);
            this.literal = literal;}

        public Result  execute() {return processor.processOneLiteralClause(literal);}}

    public static class Equivalence extends Task {
        int[] equivalences;

        Equivalence(int[] equivalences, Processor processor) {
            super(2, processor);
            this.equivalences = equivalences;}

        public Result execute() {return processor.processEquivalence(equivalences);}
}

    public static class TwoLiteral extends Task {
        int literal1,literal2;
        TwoLiteral(int literal1, int literal2, Processor processor) {
            super(3, processor);
            this.literal1 = literal1;
            this.literal2 = literal2;}

        public Result execute() {return processor.processTwoLiteralClause(literal1,literal2);}
    }


    public static class ShortenedClause extends Task {
        Clause clause;

        ShortenedClause(Clause clause, Processor processor) {
            super(4, processor);
            this.clause = clause;}

        public Result execute() {return processor.processLongerClause(clause);}
}

}
