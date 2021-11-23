package Datastructures.Clauses.AllClauses;

import Datastructures.Clauses.BasicClauseList;
import Datastructures.Clauses.ClauseOld;
import Datastructures.Clauses.Connective;
import Datastructures.Clauses.Simplifiers.ClauseSimplifierOld;
import Datastructures.Literals.CLiteralOld;
import Datastructures.Literals.LitAlgorithms;
import Datastructures.Results.Result;
import Datastructures.Results.Satisfiable;
import Datastructures.Results.Unsatisfiable;
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

import java.util.ArrayList;
import java.util.Comparator;
import java.util.concurrent.PriorityBlockingQueue;

public class Clauses extends Thread {
    /** supervises the problem solution */
    public final ProblemSupervisor problemSupervisor;
    private final String problemId;
    private final Thread thread;

    protected final Model model;
    public EquivalenceClasses equivalenceClasses;
    private final boolean monitoring;
    protected final Monitor monitor;
    protected final String monitorId;
    protected final boolean trackReasoning;
    private final Symboltable symboltable;
    public TwoLitClauses twoLitClauses;
    public ClauseSimplifierOld clauseSimplifier;

    private int maxClauseLength = 0;
    private int timestamp = 0;
    public ClausesStatistics statistics;

    private final ArrayList<ClauseOld>[] clauses = new ArrayList[Connective.size()];
    private final BucketSortedIndex<CLiteralOld>[] literalIndex = new BucketSortedIndex[Connective.size()];


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
            case INSERTCLAUSE: return ((ClauseOld)task.a).size();
            case EQUIVALENCE:  return 100;
            case DISJOINTNESS: return 101;
            case SIMPLIFYALL:  return 102;
            case SIMPLIFYOTHERS: return 103 + ((ClauseOld)task.a).size();
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
        clauseSimplifier = new ClauseSimplifierOld(problemSupervisor,monitor,"ClSimp",thread);

        for(int i = 0; i < clauses.length; ++i) {
            clauses[i] = new ArrayList<>();
            literalIndex[i] = new BucketSortedIndex<CLiteralOld>(model.predicates+1,
                (cLiteral->cLiteral.literal),
                (cLiteral->cLiteral.clause.size()));}
    }

    public void integrateBasicClauses() throws Unsatisfiable {
        BasicClauseList basicClauses = problemSupervisor.basicClauseList;
        for(int[] basicClause : basicClauses.conjunctions) {
            integrateAnd(basicClause);}
        for(int[] basicClause : basicClauses.equivalences) {
            equivalenceClasses.addBasicEquivalenceClause(basicClause);}
        model.addObserver(thread,this::addTrueLiteral);
        equivalenceClasses.addObserver(this::addEquivalence);
        for(int[] basicClause : basicClauses.disjunctions) {
            integrateBasicClause(basicClause);}
        for(int[] basicClause : basicClauses.atleasts) {
            integrateBasicClause(basicClause);}
        for(int[] basicClause : basicClauses.atmosts) {
            integrateBasicClause(basicClause);}
        for(int[] basicClause : basicClauses.exactlys) {
            integrateBasicClause(basicClause);}}

    /** puts the literals of an AND-clause into the model
     *
     * @param basicClause a basic AND-clause [id,type,literal1,...]
     * @throws Unsatisfiable if there are contradictory literals
     */
   private void integrateAnd(int[] basicClause) throws Unsatisfiable {
       assert basicClause[1] == Connective.AND.ordinal();
       for(int i = 2; i < basicClause.length; ++i) {
           model.add(basicClause[i],new Input(basicClause[0]),thread);}}


    /** simplifies and integrates a basic Or-clause into the local data structures
     *
     * @param basicClause a basic or-clause
     * @throws Unsatisfiable if a contradiction is discovered.
     */
    private void integrateBasicClause(int[] basicClause) throws Unsatisfiable {
        ClauseOld clause = new ClauseOld(basicClause);
        clause = clauseSimplifier.simplify(clause);
        if(clause == null) return;
        if(isSubsumed(clause)) return;
        insertClause(clause);
        if(clause.clauseIsOr() && clause.size() == 2) {twoLitClauses.addDerivedClause(clause);}
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
    public void addEquivalence(ClauseOld eClause) {
        if(monitoring) {
            monitor.print(monitorId,"In:   equivalence " + eClause.toString(0,model.symboltable));}
        queue.add(new Task<>(Clauses.TaskType.EQUIVALENCE,eClause,null));}


    /** inserts the clause into the local data structures.
     *
     * @param clause  the clause to be inserted.
     */
    protected void insertClause(ClauseOld clause) {
        maxClauseLength = Math.max(maxClauseLength,clause.size());
        int type = clause.connective.ordinal();
        ++statistics.clauses[type];
        clauses[type].add(clause);
        for(CLiteralOld cliteral : clause) {literalIndex[type].add(cliteral);}
        switch(clause.structure) {
            case NEGATIVE: ++statistics.negativeClauses; break;
            case POSITIVE: ++statistics.positiveClauses; break;}}


    /** removes the iterator's next clause from the index and the clauses
     *
     * @param cliteral iterator's next cliteral;
     * @param iterator an iterator over the literal index.
     * @return cliteral;
     */
    private CLiteralOld removeClause(CLiteralOld cliteral, BucketSortedList<CLiteralOld>.BucketIterator iterator)  {
        ClauseOld clause = cliteral.clause;
        int type = clause.connective.ordinal();
        switch(clause.structure) {
            case NEGATIVE: --statistics.negativeClauses; break;
            case POSITIVE: --statistics.positiveClauses; break;}
        --statistics.clauses[type];
        iterator.remove();
        for(CLiteralOld clit : clause) if(clit != cliteral) literalIndex[type].remove(cliteral);
        clauses[type].remove(clause);
        return cliteral;}




    /** works off the queue
     * new true literals <br>
     * new equivalences <br>
     * new disjointnesses <br>
     */
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
                        integrateEquivalence((ClauseOld)task.a);
                        break;
                }
                if(clausesFinished && queue.isEmpty()) {
                    if(clauses.isEmpty()) throw new Satisfiable(model);
                    checkSatisfiabiliy();
                    checkPurity();}}
            catch(InterruptedException ex) {return;}
            catch(Result result) {problemSupervisor.setResult(result,"AllClauses"); return;}}}

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
        BucketSortedIndex<CLiteralOld> index = literalIndex[Connective.OR.ordinal()];
        BucketSortedList<CLiteralOld>.BucketIterator iterator = index.popIterator(literal);
        while (iterator.hasNext()) {removeClause(iterator.next(), iterator);}
        index.pushIterator(literal, iterator);

        // remove from all orClauses -literal
        iterator = index.popIterator(-literal);
        while (iterator.hasNext()) {
            CLiteralOld cLiteral = iterator.next();
            ClauseOld clause = cLiteral.clause;
            removeClause(cLiteral, iterator);
            clause = clauseSimplifier.simplify(clause);
            if(clause != null) insertClause(clause);}
        index.pushIterator(-literal, iterator);
        index.clearBoth(predicate);

        for(int quantifierType : quantifierTypes) {
            index = literalIndex[quantifierType];
            for(int sign = -1; sign <= +1; sign += 2) {
                iterator = index.popIterator(sign*literal);
                while (iterator.hasNext()) {
                    CLiteralOld cLiteral = iterator.next();
                    ClauseOld clause = cLiteral.clause;
                    removeClause(cLiteral, iterator);
                    clause = clauseSimplifier.simplify(clause);
                    if(clause != null) insertClause(clause);}
                index.pushIterator(sign*literal, iterator);}
            index.clearBoth(predicate);}}

        /** checks if the or-clause is subsumed by another clause
         *
         * @param clause an or-clause
         * @return true if the clause is subsumed
         */
    protected boolean isSubsumed(ClauseOld clause) {
        timestamp += maxClauseLength + 2;
        ClauseOld subsumer = LitAlgorithms.isSubsumed(clause,orIndex,timestamp);
        timestamp += clause.size() + 2;
        if(subsumer != null) {
            ++statistics.forwardSubsumptions;
            if(monitoring)
                monitor.print(monitorId, "Clause " + clause.toString(0,symboltable) +
                        " is subsumed by clause " + subsumer.toString(0,symboltable));
            return true;}
        return false;}

    }
