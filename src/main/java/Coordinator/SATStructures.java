package Coordinator;

import Algorithms.Algorithms;
import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseList;
import Datastructures.Clauses.ClauseType;
import Datastructures.Theory.DisjointnessClasses;
import Datastructures.Theory.EquivalenceClasses;
import Datastructures.Literals.CLiteral;
import Datastructures.Results.Result;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Theory.ImplicationGraph;
import Datastructures.Theory.Model;

import java.util.*;
import java.util.function.BiConsumer;
import java.util.function.Function;

/**
 * Created by ohlbach on 14.09.2018.
 */
public class SATStructures {
    public int predicates;
    private ClauseList orClauses = null;
    private EquivalenceClasses equivalences = null;
    private DisjointnessClasses disjointnesses = null;
    private HashMap<Integer,Integer> replacements = new HashMap<>();

    private Model model;
    private ImplicationGraph implicationGraph;
    private PriorityQueue<Task> taskQueue = new PriorityQueue<Task>((task1,task2) -> Integer.compare(task1.priority, task2.priority));


    abstract class Task {
        public int priority;

        Task(int priority) {this.priority = priority;}

        abstract Unsatisfiable execute();}


    class ClauseTask extends Task{
        Clause clause;
        Function<Clause,Unsatisfiable> function;

        ClauseTask(Clause clause, int priority, Function<Clause,Unsatisfiable> function) {
            super(priority);
            this.clause = clause; this.function = function;}

        Unsatisfiable execute() {return function.apply(clause);}
    }

    class ImplicationTask extends Task{
        int from,to;
        BiConsumer<Integer,Integer> consumer;

        ImplicationTask(int from, int to, int priority, BiConsumer<Integer,Integer> consumer) {
            super(priority);
            this.from = from; this.to = to; this.consumer = consumer;}

        Unsatisfiable execute() {consumer.accept(from,to); return null;}
    }

    class TrueLiteralTask extends Task{
        int literal;

        TrueLiteralTask(int literal, int priority) {
            super(priority);
            this.literal = literal;}

        Unsatisfiable execute() {return makeTrue(literal);}
    }



    public SATStructures(int size, int predicates) {
        this.predicates = predicates;
        Model model = new Model(predicates);
        implicationGraph = new ImplicationGraph(predicates);
        orClauses = new ClauseList(size,predicates);
        orClauses.literalRemovalObservers.add(clause -> {taskQueue.add(makeTask(clause));});
        implicationGraph.trueLiteralObservers.add(literal->{taskQueue.add(new TrueLiteralTask(literal,1));});
    }

    Task makeTask(Clause clause) {
        switch(clause.size()) {
            case 0: return new ClauseTask(clause,0,(claus -> new Unsatisfiable("Clause " + claus.id + " became empty")));
            case 1: return new TrueLiteralTask(clause.cliterals.get(0).literal, 1);
            case 2: return new ClauseTask(clause,2,
                    (claus -> {implicationGraph.addClause(clause.getLiteral(0),clause.getLiteral(1)); return null;}));
            default: return new ClauseTask(clause,3,
                    (claus -> {Algorithms.subsumesAndResolves(claus,orClauses,implicationGraph); return null;}));
        }
    }

    private Unsatisfiable makeTrue(int literal) {
        int status = model.add(literal);
        if(status == -1) {return new Unsatisfiable(model,literal);}
        if(status == 1) {return null;}
        orClauses.makeTrue(literal);
        implicationGraph.makeTrue(literal);
        return null;}

    private Unsatisfiable processTasks() {
        while(!taskQueue.isEmpty()) {
            Unsatisfiable result = taskQueue.poll().execute();
            if(result != null) {return result;}}
        return null;}


    public Result addBasicClause(int[] basicClause) {
        Unsatisfiable result = null;
        switch(ClauseType.getType(basicClause[1])) {
            case OR:
                result = addBasicORClause(basicClause);
                break;
            case AND:
                for(int i = 1; i < basicClause.length; ++i) {
                    result = makeTrue(basicClause[i]);
                    if(result != null) {return result;}}
                break;
            case XOR:
                result = addBasicORClause(basicClause);
                if(result != null) {return result;}
                Clause clause = disjointnesses.addDisjointnessClass(basicClause);
                break;
            case DISJOINT:
                clause = disjointnesses.addDisjointnessClass(basicClause);
                break;
            case EQUIV:
                 equivalences.addEquivalenceClass(basicClause);
                break;}
        if(result != null) {return processTasks();}
        return null;
    }

    /** simplifies a basicORClause and turns them into either units, parts of the implication graph,
     * or a Clause, added to ofClauses.
     *
     * @param basicClause a basic clause
     * @return null or an Unsatisfiable object.
     */
    public Unsatisfiable addBasicORClause(int[] basicClause) {
        Clause clause = makeORClause(basicClause);
        for(int i = 0; i < 2; ++i) {
            if(clause == null) {return null;}
            switch(clause.size()) {
                case 0: return new Unsatisfiable(model,basicClause);
                case 1: return makeTrue(clause.cliterals.get(0).literal);
                case 2: implicationGraph.addClause(clause.cliterals.get(0).literal,clause.cliterals.get(1).literal);
                        return null;}
                if(i == 0) {clause = Algorithms.subsumedAndResolved(clause,orClauses,implicationGraph);}}
        orClauses.addClause(clause);
        return null;}

    /** turns a basicORClause into a clause. <br/>
     * False literals and double literals are ignored. <br/>~
     * True literals and complementary literals indicate tautologies. <br/>
     * Literals are replaced by their representatives in an equivalence class.
     * Implied literals are removed, i.e.  p,q,r and p -&gt; r causes remove(p)
     *
     * @param basicClause
     * @return the new simplified clause, or null if the clause is just to be ignored.
     */
    Clause makeORClause(int[] basicClause) {
        Clause clause = new Clause(""+basicClause[0],basicClause.length);
        for(int i = 2; i < basicClause.length;++i) {
            int literal = equivalences.mapToRepresentative(basicClause[i]);
            if(model.isTrue(literal)  || clause.cliterals.contains(-literal)) {return null;}
            if(model.isFalse(literal) || clause.cliterals.contains(literal)) {continue;}
            CLiteral cLiteral = new CLiteral(literal);
            clause.addCLiteralDirectly(cLiteral);}

        for(int i = 0; i < clause.size(); ++i) {   // p,q,r  and p -> r: remove p
            CLiteral cLiteral1 = clause.cliterals.get(i);
            TreeSet implied = implicationGraph.getImplicants(cLiteral1.literal);
            if(!implied.isEmpty()) {
                for(CLiteral cLiteral2 : clause.cliterals) {
                    if(cLiteral1 != cLiteral2 && implied.contains(cLiteral2.literal)) {
                        clause.removeLiteral(cLiteral1);
                        --i;
                        break;}}}}
        return clause;}






}
