package Coordinator;

import Algorithms.Algorithms;
import Datastructures.Clauses.BasicClauseList;
import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseList;
import Datastructures.Literals.CLiteral;
import Datastructures.Results.Result;
import Datastructures.Results.Satisfiable;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Statistics.Statistic;
import Datastructures.Theory.DisjointnessClasses;
import Datastructures.Theory.EquivalenceClasses;
import Datastructures.Theory.ImplicationDAG;
import Datastructures.Theory.Model;
import Management.GlobalParameters;
import Management.Monitor;
import Management.ProblemSupervisor;

import java.util.*;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

/**
 * Created by ohlbach on 10.10.2018.
 */
public abstract class Processor {
    public String id;
    public int predicates;
    public ProblemSupervisor supervisor;
    public GlobalParameters globalParameters;
    public HashMap<String,Object> applicationParameters;
    public BasicClauseList basicClauseList = null;
    public ClauseList clauses = null;
    public Model model = null;
    public ImplicationDAG      implicationDAG = null;
    public EquivalenceClasses  equivalences   = null;
    public DisjointnessClasses disjointnesses = null;
    public Monitor monitor = null;
    public boolean monitoring = false;
    public Statistic statistics = null;


    public Processor(){}

    public Processor(ProblemSupervisor supervisor, GlobalParameters globalParameters, HashMap<String,Object> applicationParameters,
                     BasicClauseList basicClauseList) {
        id = (String)applicationParameters.get("name");
        this.predicates            = basicClauseList.predicates;
        this.supervisor            = supervisor;
        this.globalParameters      = globalParameters;
        this.applicationParameters = applicationParameters;
        this.basicClauseList       = basicClauseList;
        monitor                    = globalParameters.monitor;
        monitoring                 = monitor.monitoring();
        if(monitoring) {monitor.addThread(id,null);}}


    protected Consumer<CLiteral>            longClauseObserver = cLiteral    -> addTask(makeShortenedClauseTask(cLiteral.clause));
    protected Consumer<Integer>             oneLiteralObserver = literal     -> addTask(new Task.OneLiteral(literal,this));
    protected BiConsumer<Integer,Integer>  implicationObserver = (from,to)   -> addTask(new Task.TwoLiteral(-from,to,this));
    protected Consumer<int[]>              equivalenceObserver = equivalence -> addTask(new Task.Equivalence(equivalence,this));
    protected Consumer<Unsatisfiable> unsatisfiabilityObserver = unsat       -> addTask(new Task.Unsatisfiability(unsat,this));
    protected Consumer<Satisfiable>     satisfiabilityObserver = sat         -> addTask(new Task.Satisfiability(sat,this));


    private void addMonitors() {
        clauses.addLiteralRemovalObserver(       cLiteral ->
                monitor.print(id,"Literal " + cLiteral.literal + " removed from clause " + cLiteral.clause.id));
        implicationDAG.addTrueLiteralObserver(    literal ->
                monitor.print(id,"Literal " + literal + " became true."));
        implicationDAG.addImplicationObserver( (from, to) ->
                monitor.print(id,"New implication " + from + " -> " + to + " derived."));
        implicationDAG.addEquivalenceObserver(equivalence ->
                monitor.print(id,"Equivalent literals " + Arrays.toString(equivalence) + " derived."));
        equivalences.addTrueLiteralObserver(      literal ->
                monitor.print(id,"True literal " + literal + " in equivalences derived."));
        equivalences.addUnsatisfiabilityObserver(   unsat ->
                monitor.print(id,"Unsatisfiability in equivalences detected."));
        disjointnesses.addUnsatisfiabilityObserver( unsat ->
                monitor.print(id,"Unsatisfiability in disjointnesses detected."));
        disjointnesses.addTrueLiteralObserver(    literal ->
                monitor.print(id,"True literal " + literal + " in disjointnesses derived."));
    }

    protected PriorityQueue<Task> taskQueue = new PriorityQueue<Task>(Comparator.comparingInt(task->task.priority));


    public void addTask(Task task) {taskQueue.add(task);}

    /** This method waits for new tasks and executes them until the execution returns a result (unsatisfiable or satisfiable)
     *
     * @return the result (unsatisfiable or satisfiable)
     */
    public Result processTasks() {
        Result result;
        while((result = getTask().execute()) != null) {}
        return result;}

    private synchronized Task getTask() {
        while(taskQueue.isEmpty()) {
            try {wait();} catch (InterruptedException e) {continue;}
            return taskQueue.poll();}
        return null;}

    protected Consumer<Integer> purityObserver = literal -> {
        if(implicationDAG.isEmpty(literal)) {taskQueue.add(new Task.Purity(literal,this));}};

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
        implicationDAG.addClause(literal1,literal2);
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

    public Result processPurity(int literal) {
        model.add(literal);
        clauses.removeLiteral(literal);
        implicationDAG.newTrueLiteral(literal);
        if(clauses.isEmpty()) {return Result.makeResult(model,basicClauseList);}
        return null;}


    protected Task makeShortenedClauseTask(Clause clause) {
        return makeShortenedClauseTask(clause,this);}

    protected Task makeShortenedClauseTask(Clause clause, Processor processor) {
        switch(clause.size()) {
            case 0:  return new Task.Unsatisfiability(new Unsatisfiable("Clause " + clause.id + " became empty"),processor);
            case 1: clauses.removeClause(clause);
                return new Task.OneLiteral(clause.getLiteral(0),processor);
            case 2: clauses.removeClause(clause);
                return new Task.TwoLiteral(clause.getLiteral(0),clause.getLiteral(1),processor);
            default: return new Task.ShortenedClause(clause,processor);}
    }

}
