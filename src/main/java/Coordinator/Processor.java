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
import Management.GlobalParameters;
import Management.Monitor;
import Management.ProblemSupervisor;

import java.util.*;

/**
 * Created by ohlbach on 10.10.2018.
 */
public class Processor {
    public int predicates;
    protected ProblemSupervisor supervisor;
    protected GlobalParameters globalParameters;
    protected HashMap<String,Object> problemParameters;
    protected BasicClauseList basicClauseList = null;
    public ClauseList clauses = null;
    public Model      model   = null;
    public ImplicationDAG      implicationDAG = null;
    public EquivalenceClasses  equivalences   = null;
    public DisjointnessClasses disjointnesses = null;
    public Monitor monitor;
    protected boolean monitoring;
    protected String monitorId = null;


    public Processor(){}

    public Processor(ProblemSupervisor supervisor, GlobalParameters globalParameters, HashMap<String,Object> problemParameters,
                     BasicClauseList basicClauseList, String monitorId) {
        this.predicates = basicClauseList.predicates;
        this.supervisor = supervisor;
        this.globalParameters = globalParameters;
        this.problemParameters = problemParameters;
        this.basicClauseList = basicClauseList;
        monitor = globalParameters.monitor;
        monitoring = monitor.monitoring();
        this.monitorId = monitorId;
        clauses.addLiteralRemovalObserver(       cLiteral -> addTask(makeShortenedClauseTask(cLiteral.clause)));
        implicationDAG.addTrueLiteralObserver(    literal -> addTask(new Task.OneLiteral(literal,this)));
        implicationDAG.addImplicationObserver( (from, to) -> addTask(new Task.TwoLiteral(-from,to,this)));
        implicationDAG.addEquivalenceObserver(equivalence -> addTask(new Task.Equivalence(equivalence,this)));
        equivalences.addTrueLiteralObserver(      literal -> addTask(new Task.OneLiteral(literal,this)));
        equivalences.addUnsatisfiabilityObserver(   unsat -> addTask(new Task.Unsatisfiability(unsat,this)));
        disjointnesses.addUnsatisfiabilityObserver( unsat -> addTask(new Task.Unsatisfiability(unsat,this)));
        disjointnesses.addTrueLiteralObserver(    literal -> addTask(new Task.OneLiteral(literal,this)));
        if(monitoring) {addMonitors();}
    }

    private void addMonitors() {
        clauses.addLiteralRemovalObserver(       cLiteral ->
                monitor.print(monitorId,"Literal " + cLiteral.literal + " removed from clause " + cLiteral.clause.id));
        implicationDAG.addTrueLiteralObserver(    literal ->
                monitor.print(monitorId,"Literal " + literal + " became true."));
        implicationDAG.addImplicationObserver( (from, to) ->
                monitor.print(monitorId,"New implication " + from + " -> " + to + " derived."));
        implicationDAG.addEquivalenceObserver(equivalence ->
                monitor.print(monitorId,"Equivalent literals " + Arrays.toString(equivalence) + " derived."));
        equivalences.addTrueLiteralObserver(      literal ->
                monitor.print(monitorId,"True literal " + literal + " in equivalences derived."));
        equivalences.addUnsatisfiabilityObserver(   unsat ->
                monitor.print(monitorId,"Unsatisfiability in equivalences detected."));
        disjointnesses.addUnsatisfiabilityObserver( unsat ->
                monitor.print(monitorId,"Unsatisfiability in disjointnesses detected."));
        disjointnesses.addTrueLiteralObserver(    literal ->
                monitor.print(monitorId,"True literal " + literal + " in disjointnesses derived."));
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


    protected ArrayList<Integer> pureLiterals = null;
}
