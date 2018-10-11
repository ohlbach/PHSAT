package Coordinator;

import Algorithms.Algorithms;
import Datastructures.Clauses.BasicClauseList;
import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseList;
import Datastructures.Results.Result;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Theory.DisjointnessClasses;
import Datastructures.Theory.EquivalenceClasses;
import Datastructures.Theory.ImplicationDAG;
import Datastructures.Theory.Model;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.PriorityQueue;

/**
 * Created by ohlbach on 10.10.2018.
 */
public class Processor {
    public int predicates;
    protected HashMap<String,Object> globalParameters;
    protected HashMap<String,Object> problemParameters;
    protected BasicClauseList basicClauseList = null;
    public ClauseList clauses = null;
    public Model      model   = null;
    public ImplicationDAG      implicationDAG = null;
    public EquivalenceClasses  equivalences   = null;
    public DisjointnessClasses disjointnesses = null;


    public Processor(){}

    public Processor(HashMap<String,Object> globalParameters, HashMap<String,Object> problemParameters, BasicClauseList basicClauseList) {
        this.predicates = basicClauseList.predicates;
        this.globalParameters = globalParameters;
        this.problemParameters = problemParameters;
        this.basicClauseList = basicClauseList;
        clauses.addLiteralRemovalObserver(       cLiteral -> addTask(makeShortenedClauseTask(cLiteral.clause)));
        implicationDAG.addTrueLiteralObserver(    literal -> addTask(new Task.OneLiteral(literal,this)));
        implicationDAG.addImplicationObserver( (from, to) -> addTask(new Task.TwoLiteral(-from,to,this)));
        implicationDAG.addEquivalenceObserver(equivalence -> addTask(new Task.Equivalence(equivalence,this)));
        equivalences.addTrueLiteralObserver(      literal -> addTask(new Task.OneLiteral(literal,this)));
        equivalences.addUnsatisfiabilityObserver(   unsat -> addTask(new Task.Unsatisfiability(unsat,this)));
        disjointnesses.addUnsatisfiabilityObserver( unsat -> addTask(new Task.Unsatisfiability(unsat,this)));
        disjointnesses.addTrueLiteralObserver(    literal -> addTask(new Task.OneLiteral(literal,this)));
    }

    protected PriorityQueue<Task> taskQueue = new PriorityQueue<Task>(Comparator.comparingInt(task->task.priority));


    protected void addTask(Task task) {taskQueue.add(task);}



    public Result processOneLiteralClause(int literal) {
        int status = model.add(literal);
        if(status == -1) {
            Unsatisfiable result = new Unsatisfiable(model,literal);
            taskQueue.add(new Task.Unsatisfiability(result,this)); return result;}
        if(status == 1) {return null;}
        clauses.makeTrue(literal);
        implicationDAG.newTrueLiteral(literal);
        return null;}


    public Result processTwoLiteralClause(int literal1, int literal2){
        Algorithms.simplifyWithImplication(-literal1,literal2,clauses,implicationDAG);
        return null;}


    public Result processLongerClause(Clause clause){
        Algorithms.subsumedAndResolved(clause,clauses,implicationDAG);
        Algorithms.resolve(clause,clauses,implicationDAG);
        return null;}

    public Result processEquivalence(int[] equivalents) {
        Clause eqClass = equivalences.addEquivalence(equivalents);
        if(eqClass == null) {return null;}
        int representative = eqClass.getLiteral(0);
        int start = representative == equivalents[0] ? 1: 0;
        for(int i = start; i < equivalents.length; ++i) {
            clauses.replaceByRepresentative(representative,equivalents[i]);}
        return null;}


    Task makeShortenedClauseTask(Clause clause) {
        switch(clause.size()) {
            case 0:  return new Task.Unsatisfiability(new Unsatisfiable("Clause " + clause.id + " became empty"),this);
            case 1: clauses.removeClause(clause);
                return new Task.OneLiteral(clause.getLiteral(0),this);
            case 2: clauses.removeClause(clause);
                return new Task.TwoLiteral(clause.getLiteral(0),clause.getLiteral(1),this);
            default: return new Task.ShortenedClause(clause,this);}
    }


    private ArrayList<Integer> pureLiterals = null;
    public Result purityCheck() {
        pureLiterals = clauses.pureLiterals();
        clauses.addPurityObserver(literal -> pureLiterals.add(literal));
        for(int i = 0; i < pureLiterals.size(); ++i) {
            Integer literal = pureLiterals.get(i);
            if(implicationDAG.isEmpty(literal)) {
                model.add(literal);
                clauses.removeLiteral(literal);
                implicationDAG.removeFalseLiteral(-literal);}}
        pureLiterals.clear();
        if(clauses.isEmpty()) {
            implicationDAG.completeModel(model);
            return Result.makeResult(model,basicClauseList);}
        return null;}
}
