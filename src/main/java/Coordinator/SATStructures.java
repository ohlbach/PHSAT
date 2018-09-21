package Coordinator;

import Algorithms.Algorithms;
import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseList;
import Datastructures.Clauses.ClauseType;
import Datastructures.Theory.*;
import Datastructures.Literals.CLiteral;
import Datastructures.Results.Result;
import Datastructures.Results.Unsatisfiable;

import java.util.*;

/**
 * Created by ohlbach on 14.09.2018.
 */
public class SATStructures {
    public int predicates;
    private Model model;
    private ImplicationGraph implicationGraph;
    private EquivalenceClasses equivalences = null;
    private DisjointnessClasses disjointnesses = null;
    private Disjunctions disjunctions = null;

    private PriorityQueue<Task> taskQueue = new PriorityQueue<Task>((task1,task2) -> Integer.compare(task1.priority, task2.priority));


     class Task {
        public int priority;

        Task(int priority) {this.priority = priority;}

        void execute() {};}

    class UnsatisfiabilityTask extends Task {
        Unsatisfiable unsatisfiable;

        UnsatisfiabilityTask(Unsatisfiable unsatisfiable) {
            super(0);
            this.unsatisfiable = unsatisfiable;}}

    class OneLiteralTask extends Task {
         int literal;
         OneLiteralTask(int literal) {
             super(1);
             this.literal = literal;}

         void execute() {makeTrue(literal);}}

    class TwoLiteralTask extends Task {
        int literal1,literal2;
        TwoLiteralTask(int literal1, int literal2) {
            super(2);
            this.literal1 = literal1;
            this.literal2 = literal2;}

        void execute() {processTwoLiteralClause(literal1,literal2);}}




    class ShortenedClauseTask extends Task{
        Clause clause;

        ShortenedClauseTask(Clause clause) {
            super(3);
            this.clause = clause;}

        void execute() {Algorithms.subsumesAndResolves(clause,disjunctions.disjunctions,implicationGraph);;}
    }





    public SATStructures(int size, int predicates) {
        this.predicates = predicates;
        Model model       = new Model(predicates);
        implicationGraph  = new ImplicationGraph(predicates);
        equivalences      = new EquivalenceClasses(model,implicationGraph);
        disjointnesses    = new DisjointnessClasses(model,implicationGraph,equivalences);
        disjunctions      = new Disjunctions(size,model,implicationGraph,equivalences);
        disjunctions.disjunctions.literalRemovalObservers.add(clause -> taskQueue.add(makeShortenedClauseTask(clause)));
        implicationGraph.trueLiteralObservers.add(literal->taskQueue.add(new OneLiteralTask(literal)));
        implicationGraph.implicationObservers.add((from,to) -> taskQueue.add(new TwoLiteralTask(-from,to)));
        equivalences.trueLiteralObservers.add(literal->taskQueue.add(new OneLiteralTask(literal)));
        equivalences.unsatisfiabilityObservers.add(unsat -> taskQueue.add(new UnsatisfiabilityTask(unsat)));
        disjointnesses.unsatisfiabilityObservers.add(unsat -> taskQueue.add(new UnsatisfiabilityTask(unsat)));
        disjointnesses.trueLiteralObservers.add(literal->taskQueue.add(new OneLiteralTask(literal)));

    }

    Task makeShortenedClauseTask(Clause clause) {
        switch(clause.size()) {
            case 0: return new UnsatisfiabilityTask(new Unsatisfiable("Clause " + clause.id + " became empty"));
            case 1: return new OneLiteralTask(clause.getLiteral(0));
            case 2: return new TwoLiteralTask(clause.getLiteral(0),clause.getLiteral(1));
            default: return new ShortenedClauseTask(clause);
        }
    }

    private boolean makeTrue(int literal) {
        int status = model.add(literal);
        if(status == -1) {taskQueue.add(new UnsatisfiabilityTask(new Unsatisfiable(model,literal))); return true;}
        if(status == 1) {return false;}
        disjunctions.makeTrue(literal);
        implicationGraph.makeTrue(literal);
        return false;}

    private Unsatisfiable processTasks() {
        while(!taskQueue.isEmpty()) {
            Task task = taskQueue.poll();
            if(task instanceof UnsatisfiabilityTask) {return ((UnsatisfiabilityTask)task).unsatisfiable;}
            task.execute();}
        return null;}


    public Result addBasicClause(int[] basicClause) {
        Unsatisfiable result = null;
        switch(ClauseType.getType(basicClause[1])) {
            case OR:
                result = disjunctions.addBasicDisjunction(basicClause);
                break;
            case AND:
                for(int i = 1; i < basicClause.length; ++i) {
                    if(makeTrue(basicClause[i])) {break;}}
                break;
            case XOR:
                result = disjunctions.addBasicDisjunction(basicClause);
                if(result != null) {return result;}
            case DISJOINT:
                addBasicDISJOINTClause(basicClause);
                break;
            case EQUIV:
                 equivalences.addEquivalenceClass(basicClause);
                break;}
        if(result != null) {return processTasks();}
        return null;
    }


    private void processTwoLiteralClause(int literal1, int literal2){

    }

        private void addBasicDISJOINTClause(int[] basicClause) {
            Clause clause = disjointnesses.addDisjointnessClass(basicClause);
            if(clause != null) {
                int size = clause.size();
                for(int i = 0; i < size; ++i) {
                    int literal = clause.getLiteral(i);
                    for (int j = i+1; j < size; ++j) {
                        implicationGraph.addImplication(literal,clause.getLiteral(j));}}}
        }






}
