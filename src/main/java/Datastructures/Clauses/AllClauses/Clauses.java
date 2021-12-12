
package Datastructures.Clauses.AllClauses;

import Datastructures.Clauses.BasicClauseList;
import Datastructures.Clauses.Clause;
import Datastructures.Clauses.Connective;
import Datastructures.Clauses.Simplifiers.ClauseSimplifier;
import Datastructures.Literals.CLiteral;
import Datastructures.Literals.LitAlgorithms;
import Datastructures.Results.Result;
import Datastructures.Results.Satisfiable;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Results.UnsatisfiableInterval;
import Datastructures.Symboltable;
import Datastructures.Task;
import Datastructures.Theory.EquivalenceClasses;
import Datastructures.Theory.Model;
import Datastructures.TwoLiteral.TwoLitClauses;
import InferenceSteps.InferenceStep;
import InferenceSteps.Input;
import Management.Monitor;
import Management.ProblemSupervisor;
import Utilities.BucketSortedIndex;
import Utilities.BucketSortedList;
import Utilities.Interval;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.concurrent.PriorityBlockingQueue;

public class Clauses extends Thread {
    /** supervises the problem solution */
    public final ProblemSupervisor problemSupervisor;
    private final String problemId;
    private final Thread thread;

    public final Model model;
    public EquivalenceClasses equivalenceClasses;
    public final boolean monitoring;
    public final Monitor monitor;
    public final String monitorId;
    public final boolean trackReasoning;
    public final Symboltable symboltable;
    public TwoLitClauses twoLitClauses;
    public ClauseSimplifier clauseSimplifier;

    private int maxClauseLength = 0;
    private int timestamp = 1;
    public ClausesStatistics statistics;

    private final HashMap<Integer,Clause> clauses = new HashMap<>();
    public final BucketSortedIndex<CLiteral> literalIndex;


    private enum TaskType {
        TRUELITERAL, EQUIVALENCE, DISJOINTNESS, INSERTCLAUSE, TWOLITCLAUSE, SIMPLIFYALL, SIMPLIFYOTHERS,
        MRESOLUTION
    }

    /** gets the priority for the objects in the queue.
     *
     * @param task the objects in the queue
     * @return the priority of the objects in the queue.
     */
    private int getPriority(Task<Clauses.TaskType> task) {
        switch(task.taskType) {
            case TRUELITERAL:  return 0;
            case INSERTCLAUSE: return ((Clause)task.a).size();
            case EQUIVALENCE:  return 100;
            case DISJOINTNESS: return 101;
            case SIMPLIFYALL:  return 102;
            case SIMPLIFYOTHERS: return 103 + ((Clause)task.a).size();
            case MRESOLUTION: return 200;
        }
        return -1;}

    /** A queue of newly derived unit literals, newly derived binary equivalences and disjointness clauses
     * The unit literals are automatically put at the beginning of the queue.
     */
    private final PriorityBlockingQueue<Task<Clauses.TaskType>> queue =
            new PriorityBlockingQueue<>(10, Comparator.comparingInt(this::getPriority));

    /** constructs a new AllClauses instance
     *
     * @param problemSupervisor    coordinates several solvers.
     */
    public Clauses(ProblemSupervisor problemSupervisor) {
        this.problemSupervisor = problemSupervisor;
        problemSupervisor.clauses = this;
        problemId = problemSupervisor.problemId;
        thread = Thread.currentThread();
        model = problemSupervisor.model;
        symboltable = model.symboltable;
        equivalenceClasses = problemSupervisor.equivalenceClasses;
        twoLitClauses = problemSupervisor.twoLitClauses;
        monitor = problemSupervisor.globalParameters.monitor;
        monitoring = monitor != null;
        monitorId = problemId + "Clauses";
        statistics = new ClausesStatistics("Clauses");
        trackReasoning = problemSupervisor.globalParameters.trackReasoning;
        literalIndex = new BucketSortedIndex<CLiteral>(model.predicates+1,
                (cLiteral->cLiteral.literal),
                (cLiteral->cLiteral.clause.size()));
        clauseSimplifier = new ClauseSimplifier(this,thread);
    }

    public void integrateBasicClauses() throws Unsatisfiable {
        try{
            BasicClauseList basicClauses = problemSupervisor.basicClauseList;
            for(int[] basicClause : basicClauses.conjunctions) {
                integrateAnd(basicClause);}
            for(int[] basicClause : basicClauses.equivalences) {
                equivalenceClasses.addBasicEquivalenceClause(basicClause);}
            model.addObserver(thread,this::addTrueLiteral);
            equivalenceClasses.addObserver(this::addEquivalence);
            for(int[] basicClause : basicClauses.disjunctions) {
                integrateBasicClause(basicClause);}
            for(int[] basicClause : basicClauses.quantifieds) {
                integrateBasicClause(basicClause);}
            for(int[] basicClause : basicClauses.intervals) {
                integrateBasicClause(basicClause);}}
        catch(Unsatisfiable unsatisfiable) {
             unsatisfiable.problemId   = problemId;
             unsatisfiable.solverClass = Clauses.class;
             unsatisfiable.solverId    = "Clauses";
             unsatisfiable.statistic   = statistics;
             unsatisfiable.symboltable = symboltable;
             throw unsatisfiable;}}

    /** puts the literals of an AND-clause into the model
     *
     * @param basicClause a basic AND-clause [id,type,literal1,...]
     * @throws Unsatisfiable if there are contradictory literals
     */
    private void integrateAnd(int[] basicClause) throws Unsatisfiable {
        assert basicClause[1] == Connective.AND.ordinal();
        InferenceStep step = new Input(basicClause[0]);
        for(int i = 2; i < basicClause.length; ++i) {
            model.add(basicClause[i],step,thread);}}


    /** simplifies and integrates a basic clause into the local data structures
     *
     * @param basicClause a basic or-clause
     * @throws Unsatisfiable if a contradiction is discovered.
     */
    private void integrateBasicClause(int[] basicClause) throws Unsatisfiable {
        Clause clause = new Clause(basicClause);
        if(clause.connective == Connective.AND) {clauseSimplifier.andToModel(clause); return;}
        clause = clauseSimplifier.simplify(clause);
        if(clause == null) return;
        clause = forwardSubsumption(clause);
        if(clause == null) return;
        insertClause(clause);
        if(clause.connective == Connective.OR && clause.size() == 2) {twoLitClauses.addDerivedClause(clause);}
    }

    /** puts a true literal into the queue.
     *
     * @param literal a true literal
     * @param inference for making the literal true
     */
    public void addTrueLiteral(int literal, InferenceStep inference){
        if(monitoring) {
            monitor.print(monitorId,"In:   Unit literal " +
                    Symboltable.toString(literal,model.symboltable));}
        queue.add(new Task<>(Clauses.TaskType.TRUELITERAL, literal, inference));}

    /** puts an equivalence into the queue
     *
     * @param eClause an equivalence clause
     */
    public void addEquivalence(Clause eClause) {
        if(monitoring) {
            monitor.print(monitorId,"In:   equivalence " + eClause.toString(0,model.symboltable));}
        queue.add(new Task<>(Clauses.TaskType.EQUIVALENCE,eClause,null));}


    /** inserts the clause into the local data structures.
     *
     * @param clause  the clause to be inserted.
     */
    protected void insertClause(Clause clause) {
        maxClauseLength = Math.max(maxClauseLength,clause.size());
        clauses.put(clause.id,clause);
        for(CLiteral cliteral : clause) {literalIndex.add(cliteral);}
        switch(clause.structure) {
            case NEGATIVE: ++statistics.negativeClauses; break;
            case POSITIVE: ++statistics.positiveClauses; break;}
        ++statistics.clauses[clause.connective.ordinal()];}

    /** removes a clause from the internal lists
     *
     * @param clause a clause to be removed
     */
    private void removeClause(Clause clause) {
        switch(clause.structure) {
            case NEGATIVE: --statistics.negativeClauses; break;
            case POSITIVE: --statistics.positiveClauses; break;}
        --statistics.clauses[clause.connective.ordinal()];
        for(CLiteral cliteral : clause) literalIndex.remove(cliteral);
        clauses.remove(clause.id);}

    /** removes the iterator's next clause from the index and the clauses
     *
     * @param cliteral iterator's next cliteral;
     * @param iterator an iterator over the literal index.
     * @return cliteral;
     */
    private CLiteral removeClause(CLiteral cliteral, BucketSortedList<CLiteral>.BucketIterator iterator)  {
        Clause clause = cliteral.clause;
        switch(clause.structure) {
            case NEGATIVE: --statistics.negativeClauses; break;
            case POSITIVE: --statistics.positiveClauses; break;}
        --statistics.clauses[clause.connective.ordinal()];
        iterator.remove();
        for(CLiteral clit : clause) if(clit != cliteral) literalIndex.remove(cliteral);
        clauses.remove(clause.id);
        return cliteral;}





    /** works off the queue
     * new true literals <br>
     * new equivalences <br>
     * new disjointnesses <br>
     */
    /*
    public void run() {
        while(!Thread.interrupted()) {
            try {
                if(monitoring) {monitor.print(monitorId,"Queue is waiting:\n"+Task.queueToString(queue));}
                Task<Clauses.TaskType> task = queue.take(); // waits if the queue is empty
                if(monitoring) System.out.println("Next Task: " + task.toString());
                switch (task.taskType) {
                    case TRUELITERAL:
                        integrateTrueLiteral((Integer)task.a);
                        break;
                    case EQUIVALENCE:
                        integrateEquivalence((Clause)task.a);
                        break;
                }
                if(clausesFinished && queue.isEmpty()) {
                    if(clauses.isEmpty()) throw new Satisfiable(model);
                    checkSatisfiabiliy();
                    checkPurity();}}
            catch(InterruptedException ex) {return;}
            catch(Result result) {problemSupervisor.setResult(result,"AllClauses"); return;}}}
*/
    private static int[] quantifierTypes = new int[]{
            Connective.ATLEAST.ordinal(),Connective.ATMOST.ordinal(),Connective.EXACTLY.ordinal()};

    /** applies a true literal to all clauses.
     * Clauses containing the literal are removed.<br>
     * In clauses containing the negated literal, this literal is removed.<br>
     * For the resulting clause a INSERTCLAUSE Task is generated.
     *
     * @param literal a true literal
     * @throws Unsatisfiable if a contradiction is found.
     */
    private void integrateTrueLiteral(int literal) throws Unsatisfiable {
        // remove all orClauses with the literal
        int predicate = Math.abs(literal);
        BucketSortedIndex<CLiteral> index = literalIndex;
        BucketSortedList<CLiteral>.BucketIterator iterator = index.popIterator(literal);
        while (iterator.hasNext()) {removeClause(iterator.next(), iterator);}
        index.pushIterator(literal, iterator);

        // remove from all orClauses -literal
        iterator = index.popIterator(-literal);
        while (iterator.hasNext()) {
            CLiteral cLiteral = iterator.next();
            Clause clause = cLiteral.clause;
            removeClause(cLiteral, iterator);
            clause = clauseSimplifier.simplify(clause);
            if(clause != null) insertClause(clause);}
        index.pushIterator(-literal, iterator);
        index.clearBoth(predicate);

        for(int quantifierType : quantifierTypes) {
            index = literalIndex;
            for(int sign = -1; sign <= +1; sign += 2) {
                iterator = index.popIterator(sign*literal);
                while (iterator.hasNext()) {
                    CLiteral cLiteral = iterator.next();
                    Clause clause = cLiteral.clause;
                    removeClause(cLiteral, iterator);
                    clause = clauseSimplifier.simplify(clause);
                    if(clause != null) insertClause(clause);}
                index.pushIterator(sign*literal, iterator);}
            index.clearBoth(predicate);}}

    /** checks subsumption and intersection between an old clause and a new clause
     * The old clause subsumes the new one: ignore the new one <br>
     * The new clause subsumes the old one: remove the old one <br>
     * The literals are equal, but the intervals don't intersect: throw Unsatisfiable <br>
     * The literals are equal and the interval of the new clause is a subset of the interval of the old clause:<br>
     *   Remove the old clause </br>
     * The literals are equal, and the intervals intersect:<br>
     *   Remove the old clause, and either construct a new clause, or change the interval of the new clause.
     *
     * @param clause a new clause (not yet integrated)
     * @return null (clause subsumed) or, the possibly cloned or modified, clause
     */

    protected Clause forwardSubsumption(Clause clause) throws Unsatisfiable {
        Clause subsumer = LitAlgorithms.isSubsumed(clause,literalIndex,timestamp);
        timestamp += clause.size() + 2;

        if(subsumer == null) return clause;
        if(subsumer.limit >= clause.limit) {
            ++statistics.forwardSubsumptions;
            if(monitoring)
                monitor.print(monitorId, "Clause " + clause.toString(0,symboltable) +
                        " is subsumed by clause " + subsumer.toString(0,symboltable));
            return null;}

        if(clause.size() != subsumer.size()) return clause;

        // Now the two clauses have the same literals

        /*
        Interval interval = subsumer.interval.intersect(clause.interval);
        if(interval == null) {throw new UnsatisfiableInterval(subsumer,clause);}

        removeClause(subsumer);
        if(clause.interval.isSubset(subsumer.interval)) {
            ++statistics.backwardSubsumptions;
            if(monitoring)
                monitor.print(monitorId, "Clause " + subsumer.toString(0,symboltable) +
                            " is subsumed by clause " + clause.toString(0,symboltable));
            return clause;}
        if(trackReasoning) {
            Clause newClause = clause.clone(problemSupervisor.nextClauseId());
            newClause.interval = interval;
            newClause.setConnective();
            InferenceStep step = new InfIntervalIntersection(subsumer,clause,newClause);
            newClause.inferenceStep = step;
            if(monitoring) {monitor.print(monitorId,step.toString(symboltable));
            return newClause;}}
        else {clause.interval = interval; clause.setConnective();}
        */

        return clause;}


}

