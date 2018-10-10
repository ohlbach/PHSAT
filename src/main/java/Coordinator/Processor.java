package Coordinator;

import Algorithms.Algorithms;
import Datastructures.Clauses.BasicClauseList;
import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseList;
import Datastructures.Results.Result;
import Datastructures.Results.Satisfiable;
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
    protected BasicClauseList basicClauseList;
    public ClauseList clauses;
    public Model model;
    public ImplicationDAG implicationDAG;
    public EquivalenceClasses equivalences = null;
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
        equivalences.trueLiteralObservers.add(    literal -> addTask(new Task.OneLiteral(literal,this)));
        equivalences.unsatisfiabilityObservers.add( unsat -> addTask(new Task.Unsatisfiability(unsat,this)));
        disjointnesses.unsatisfiabilityObservers.add(unsat -> addTask(new Task.Unsatisfiability(unsat,this)));
        disjointnesses.trueLiteralObservers.add(   literal -> addTask(new Task.OneLiteral(literal,this)));
    }

    protected PriorityQueue<Task> taskQueue = new PriorityQueue<Task>(Comparator.comparingInt(task->task.priority));


    protected synchronized void addTask(Task task) {taskQueue.add(task); notify();}

    private synchronized Task getTask() {
        while(taskQueue.isEmpty()) {
            try {wait();} catch (InterruptedException e) {continue;}
            return taskQueue.poll();}
        return null;}

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

    public Result processEquivalence(int[] equivalence) {
        return null;
    }


    protected void replaceByRepresentative(int representative, int literal) {
        clauses.replaceByRepresentative(representative,literal);
    }

    Task makeShortenedClauseTask(Clause clause) {
        switch(clause.size()) {
            case 0:  return new Task.Unsatisfiability(new Unsatisfiable("Clause " + clause.id + " became empty"),this);
            case 1: clauses.removeClause(clause);
                return new Task.OneLiteral(clause.getLiteral(0),this);
            case 2: clauses.removeClause(clause);
                return new Task.TwoLiteral(clause.getLiteral(0),clause.getLiteral(1),this);
            default: return new Task.ShortenedClause(clause,this);}
    }

    /** This method waits for new tasks and executes them until the execution returns a result (unsatisfiable or satisfiable)
     *
     * @return the result (unsatisfiable or satisfiable)
     */
    public Result processTasks() {
        Result result;
        while((result = getTask().execute()) != null) {}
        return result;}

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
            return new Satisfiable();}
        return null;}
}
