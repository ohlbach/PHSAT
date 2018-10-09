package Coordinator;

import Algorithms.Algorithms;
import Datastructures.Clauses.BasicClauseList;
import Datastructures.Clauses.Clause;
import Datastructures.Results.Satisfiable;
import Datastructures.Theory.*;
import Datastructures.Results.Result;
import Datastructures.Results.Unsatisfiable;

import java.util.*;

/**
 * Created by ohlbach on 14.09.2018.
 */
public class Preprocessor {
    public int predicates;
    BasicClauseList basicClauseList;
    Incoming incoming;
    Outgoing outgoing;
    HashMap<String,Object> problemControl;
    ArrayList<Integer> pureLiterals = null;
    public Model model;
    public Disjunctions disjunctions = null;
    public ImplicationDAG implicationDAG;
    public EquivalenceClasses equivalences = null;
    public DisjointnessClasses disjointnesses = null;

    private PriorityQueue<Task> taskQueue = new PriorityQueue<Task>((task1,task2) -> Integer.compare(task1.priority, task2.priority));

     class Task {
        public int priority;

        Task(int priority) {this.priority = priority;}

        void execute(ChangeBlock changeBlock) {};}

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

         void execute(ChangeBlock changeBlock) {
             makeTrue(literal);
             if(changeBlock != null) {changeBlock.addOneLiteralClause(literal);}}}

    class EquivalenceTask extends Task{
        int representative;
        int literal;

        EquivalenceTask(int representative, int literal) {
            super(2);
            this.representative = representative;
            this.literal = literal;}

        void execute(ChangeBlock changeBlock) {
            replaceByRepresentative(representative,literal);
            if(changeBlock != null) {changeBlock.addEquivalences(representative,literal);}}
    }

    class TwoLiteralTask extends Task {
        int literal1,literal2;
        TwoLiteralTask(int literal1, int literal2) {
            super(3);
            this.literal1 = literal1;
            this.literal2 = literal2;}

        void execute(ChangeBlock changeBlock) {
            processTwoLiteralClause(literal1,literal2);
            if(changeBlock != null) {changeBlock.addTwoLiteralClause(-literal1,literal2);}
        }}


    class ShortenedClauseTask extends Task{
        Clause clause;

        ShortenedClauseTask(Clause clause) {
            super(4);
            this.clause = clause;}

        void execute(ChangeBlock changeBlock) {Algorithms.subsumeAndResolve(clause,disjunctions.disjunctions, implicationDAG);}
    }


    public Preprocessor(HashMap<String,Object> globalParameters, HashMap<String,Object> problemControl) {
        basicClauseList = (BasicClauseList)problemControl.get("clauses");
        this.predicates = basicClauseList.predicates;
        this.incoming   = incoming;
        this.outgoing   = outgoing;
        Model model       = new Model(predicates);
        implicationDAG = new ImplicationDAG();
        equivalences      = new EquivalenceClasses(model, implicationDAG);
        disjointnesses    = new DisjointnessClasses(model, implicationDAG,equivalences);
        disjunctions      = new Disjunctions(basicClauseList.disjunctions.size(),model, implicationDAG,equivalences);
        disjunctions.disjunctions.addLiteralRemovalObserver(cLiteral -> taskQueue.add(makeShortenedClauseTask(cLiteral.clause)));
        implicationDAG.addTrueLiteralObserver(    literal -> taskQueue.add(new OneLiteralTask(literal)));
        implicationDAG.addImplicationObserver( (from, to) -> taskQueue.add(new TwoLiteralTask(-from,to)));
        equivalences.trueLiteralObservers.add(    literal -> taskQueue.add(new OneLiteralTask(literal)));
        equivalences.unsatisfiabilityObservers.add( unsat -> taskQueue.add(new UnsatisfiabilityTask(unsat)));
        disjointnesses.unsatisfiabilityObservers.add(unsat -> taskQueue.add(new UnsatisfiabilityTask(unsat)));
        disjointnesses.trueLiteralObservers.add(   literal -> taskQueue.add(new OneLiteralTask(literal)));
    }

    Task makeShortenedClauseTask(Clause clause) {
        switch(clause.size()) {
            case 0:  return new UnsatisfiabilityTask(new Unsatisfiable("Clause " + clause.id + " became empty"));
            case 1:  return new OneLiteralTask(clause.getLiteral(0));
            case 2:  return new TwoLiteralTask(clause.getLiteral(0),clause.getLiteral(1));
            default: return new ShortenedClauseTask(clause);
        }
    }

    private Unsatisfiable processTasks(ChangeBlock changeBlock) {
        while(!taskQueue.isEmpty()) {
            Task task = taskQueue.poll();
            if(task instanceof UnsatisfiabilityTask) {return ((UnsatisfiabilityTask)task).unsatisfiable;}
            task.execute(changeBlock);}
        return null;}

    public Result addDisjunction(int[] basicClause) {
        disjunctions.addBasicDisjunction(basicClause);
        return processTasks(null);}

    public Result addConjunction(int[] basicClause) {
        for(int i = 1; i < basicClause.length; ++i) {
            if(makeTrue(basicClause[i])) {return processTasks(null);}}
        return null;}

    public Result addXor(int[] basicClause) {
         addDisjunction(basicClause);
         addDisjoint(basicClause);
        return processTasks(null);}


    public Result addDisjoint(int[] basicClause) {
        Clause clause = disjointnesses.addDisjointnessClass(basicClause);
        if(clause != null) {
            int size = clause.size();
            for(int i = 0; i < size; ++i) {
                int literal = clause.getLiteral(i);
                for (int j = i+1; j < size; ++j) {
                    implicationDAG.addImplication(literal,clause.getLiteral(j));}}}
        return processTasks(null);}

    public Result addEquivalence(int[] basicClause) {
        equivalences.addEquivalenceClass(basicClause);
        return processTasks(null);}


    public Result transferBasicClauses() {
        Result result;
        for(int[] basicClause: basicClauseList.disjunctions) {
            result = addDisjunction(basicClause);
            if(result != null) {return result;}}
        for(int[] basicClause: basicClauseList.conjunctions) {
            result = addConjunction(basicClause);
            if(result != null) {return result;}}
        for(int[] basicClause: basicClauseList.xor) {
            result = addXor(basicClause);
            if(result != null) {return result;}}
        for(int[] basicClause: basicClauseList.disjoints) {
            result = addDisjoint(basicClause);
            if(result != null) {return result;}}
        for(int[] basicClause: basicClauseList.equivalences) {
            result = addEquivalence(basicClause);
            if(result != null) {return result;}}
        return purityCheck();}

    private boolean makeTrue(int literal) {
        int status = model.add(literal);
        if(status == -1) {taskQueue.add(new UnsatisfiabilityTask(new Unsatisfiable(model,literal))); return true;}
        if(status == 1) {return false;}
        disjunctions.makeTrue(literal);
        implicationDAG.newTrueLiteral(literal);
        return false;}

    private void processTwoLiteralClause(int literal1, int literal2){
         disjunctions.simplifyClauses(literal1,literal2);}

    private void replaceByRepresentative(int representative, int literal) {
        disjunctions.replaceByRepresentative(representative,literal);
    }


    public Result purityCheck() {
        pureLiterals = disjunctions.pureLiterals();
        disjunctions.addPurityObserver(literal -> pureLiterals.add(literal));
        for(int i = 0; i < pureLiterals.size(); ++i) {
            Integer literal = pureLiterals.get(i);
            if(implicationDAG.isEmpty(literal)) {
                model.add(literal);
                disjunctions.removeLiteral(literal);
                implicationDAG.removeFalseLiteral(-literal);}}
        pureLiterals.clear();
        if(disjunctions.isEmpty()) {
            //implicationDAG.completeModel(model);  ????
            return new Satisfiable();}
        return null;}

    public Result processIncomingData() {
        Object[] changes = null;
        while((changes = incoming.getIncoming()) != null) {
            int[] ones = (int[])changes[0];
            int[][]twos = (int[][])changes[1];
            if(ones != null) {
                for(int literal : ones) { taskQueue.add(new OneLiteralTask(literal));}}
            if(twos != null) {
                for(int[] literals : twos) {taskQueue.add(new TwoLiteralTask(literals[0],literals[1]));}}
            ChangeBlock changeBlock = new ChangeBlock();
            Unsatisfiable result = processTasks(changeBlock);
            if(!changeBlock.isEmpty()) {outgoing.send(changeBlock);}
            if(result != null) {return result;}}
        return null;
    }

}
