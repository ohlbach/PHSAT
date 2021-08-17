package Datastructures.Clauses;

import Datastructures.Literals.CLiteral;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Datastructures.Theory.DisjointnessClasses;
import Datastructures.Theory.EquivalenceClasses;
import Datastructures.Theory.Model;
import Datastructures.TwoLiteral.TwoLitClauses;
import Management.GlobalParameters;
import Management.Monitor;
import Management.ProblemSupervisor;
import Utilities.BucketSortedIndex;
import Utilities.BucketSortedList;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import javafx.util.Pair;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.concurrent.PriorityBlockingQueue;

import static Utilities.Utilities.joinIntArraysSorted;
import Utilities.Triple;

public class AllClauses {

    /** supervises the problem solution */
    private ProblemSupervisor problemSupervisor;
    private Thread thread;

    private Model model;
    private BasicClauseList     basicClauseList;
    private EquivalenceClasses  equivalenceClasses;
    private DisjointnessClasses disjointnessClasses;
    private TwoLitClauses       twoLitClauses;

    private boolean monitoring = false;
    private Monitor monitor;
    private String monitorId;
    private boolean trackReasoning;
    private GlobalParameters globalParameters;
    private String problemId;
    private Symboltable symboltable;

    private int predicates;
    private int counter = 0;
    private int maxClauseLength = 0;
    private BucketSortedList<Clause> clauses;
    BucketSortedIndex<CLiteral> literalIndex = null;
    AllClausesStatistics statistics;

    /** A queue of newly derived unit literals, newly derived binary disjointnesses and basic disjointness clauses
     * The unit literals are automatically put at the beginning of the queue.
     */
    private final PriorityBlockingQueue<Triple<Object,Object,IntArrayList>> queue =
            new PriorityBlockingQueue<Triple<Object,Object,IntArrayList>>(10,
                    (o1,o2) ->
                    Integer.compare(getPriority(o1),getPriority(o2)));

    /** gets the priority for the objects in the queue.
     *
     * @param triple the class of the objects in the queue.
     * @return the priority of the objects in the queue.
     */
    private int getPriority(Triple triple) {
        if(triple.a.getClass() == Integer.class &&
                triple.b == null)                     {
            return 0;}    // true literal
        if(triple.a.getClass() == Integer.class &&
                triple.b.getClass() == Integer.class) {
            return 1;}    // equivalence
        return 2;};       // disjointness class

    /** constructs a new
     *
     * @param problemSupervisor    coordinates several solvers.
     */
    public AllClauses(ProblemSupervisor problemSupervisor) throws Unsatisfiable {
        this.problemSupervisor = problemSupervisor;
        globalParameters = problemSupervisor.globalParameters;
        thread = Thread.currentThread();
        model = problemSupervisor.model;
        basicClauseList = problemSupervisor.basicClauseList;
        equivalenceClasses = problemSupervisor.equivalenceClasses;
        disjointnessClasses = problemSupervisor.disjointnessClasses;
        twoLitClauses       = problemSupervisor.twoLitClauses;
        problemId = problemSupervisor.problemId;
        monitor = problemSupervisor.globalParameters.monitor;
        monitoring = monitor != null;
        monitorId = problemId+"AC";
        trackReasoning = globalParameters.trackReasoning;
        symboltable = model.symboltable;
        predicates = symboltable.predicates;
        clauses      = new BucketSortedList<Clause>(clause->clause.size());
        literalIndex = new BucketSortedIndex<CLiteral>(predicates+1,
                (cLiteral->cLiteral.literal),
                (cLiteral->cLiteral.clause.size()));
        statistics = new AllClausesStatistics(problemId);

        model.addObserver(Thread.currentThread(),(literal, origins) ->
                queue.add(new Triple(literal,null,origins)));

        equivalenceClasses.addObserver((representative,literal,origins) ->
                queue.add(new Triple(representative,literal,origins)));

        disjointnessClasses.addObserver((disjoints) ->
                queue.add(new Triple(disjoints,null,null)));

        initializeAnd();
        initializeEqv();
        initializeDisjoints();
        initializeXors();
        initializeDisjunctions();
    }
    /** This method initially fills up the model with the conjunctions and the disjunctions with one literal.
     * At this stage there is no further interaction with other parts.
     *
     * @throws Unsatisfiable if a contradiction occurs.
     */
    private void initializeAnd() throws Unsatisfiable {
        for(int[] basicClause : basicClauseList.conjunctions) {
            for(int i = 2; i < basicClause.length; ++i) {
                IntArrayList origin = new IntArrayList(); origin.add(basicClause[0]);
                model.add(basicClause[i],origin,thread);}}}


    /** This method puts the equivalence clauses into the equivalence classes
     *
     * @throws Unsatisfiable if a contradiction occurs.
     */
    private void initializeEqv() throws Unsatisfiable {
        for(int[] clause : basicClauseList.equivalences) {
            equivalenceClasses.addBasicEquivalenceClause(clause);}}

    /** This method puts the disjointness clauses into the disjointness classes
     *
     * @throws Unsatisfiable if a contradiction occurs.
     */
    private void initializeDisjoints() throws Unsatisfiable {
        for(int[] clause : basicClauseList.disjoints) {
            disjointnessClasses.addDisjointnessClause(clause);}}

    /** This method puts the xor clauses into the disjointness classes and the clauses
     *
     * @throws Unsatisfiable if a contradiction occurs.
     */
    private void initializeXors() throws Unsatisfiable {
        for(int[] clause : basicClauseList.xors) {
            disjointnessClasses.addDisjointnessClause(clause);
            addClause(clause);}}

    /** This method puts the disjunctions clauses into the clauses
     *
     * @throws Unsatisfiable if a contradiction occurs.
     */
    private void initializeDisjunctions() throws Unsatisfiable {
        for(int[] clause : basicClauseList.disjunctions) {
            addClause(clause);}}


    /** turns a single basic clause into a Clause datastructure. <br>
     * Literals are replaced by their representative in the equivalence classes.<br>
     * True or false literlas are eliminated<br>
     * Double literals are removed.<br>
     * Tautologies are ignored.<br>
     * Empty clauses cause thrown of Unsatisfiable <br>
     * Two-literal clauses are put into twoLitClauses.
     *
     * @param basicClause [id,type,lit1,...]
     * @throws Unsatisfiable if the clause is empty, otherwise null
     */
    private void addClause(int[] basicClause) throws Unsatisfiable {
        Clause clause = new Clause(++counter,basicClause.length-2);
        IntArrayList origins = trackReasoning ? IntArrayList.wrap(new int[]{basicClause[0]}) : null;
        for(int i = 2; i < basicClause.length; ++i) {
            int originalLiteral =  basicClause[i];
            int literal = originalLiteral;
            literal = equivalenceClasses.getRepresentative(originalLiteral);
            if(trackReasoning && literal != originalLiteral) {
                origins = joinIntArraysSorted(origins,equivalenceClasses.getOrigins(originalLiteral));}
            switch(model.status(literal)) {
                case +1: return; // true clause
                case -1:
                    if(trackReasoning) {origins = joinIntArraysSorted(origins,model.getOrigin(literal));}
                    continue;}
            clause.add(new CLiteral(literal,clause,i-2));}

        if(clause.isEmpty()) {
            throw new Unsatisfiable("Clause " + BasicClauseList.clauseToString(0,basicClause,symboltable) +
                    " became empty",origins);}

        if(clause.hasComplementaries()) {
            if(monitoring) {
                monitor.print(monitorId, "Clause " +
                        BasicClauseList.clauseToString(0,basicClause,symboltable) + " became a tautology");}
            return;}
        else {
            clause.removeDoubles();
            switch(clause.size()) {
                case 1:
                    model.add(clause.getLiteral(0),origins,null); // back to this process
                    return;
                case 2: twoLitClauses.addDerivedClause(clause.getLiteral(0),
                        clause.getLiteral(1),origins);
                        return;}
            clause.setStructure();
            clause.origins = origins;
            insertClause(clause);}}


    public Unsatisfiable runQueue() {
        while(!Thread.interrupted()) {
            try {
                if(monitoring) {monitor.print(monitorId,"Queue is waiting");}
                Triple<Object, Object, IntArrayList> triple = queue.take(); // waits if the queue is empty
                if(triple.a.getClass() == Integer.class && triple.b == null) {
                    integrateTrueLiteral((Integer)triple.a,triple.c);
                    continue;}
                if(triple.a.getClass() == Integer.class && triple.b.getClass() == Integer.class) {
                    //integrateEquivalence(triple.a,triple.b,triple.c);
                    continue;}
                //integrateDisjointness(triple.a);
            }
            catch(InterruptedException ex) {return null;}
            catch(Unsatisfiable unsatisfiable) {
                return unsatisfiable;}}
            return null;}

    /** applies a true literal to all clauses.
     * Clauses containing the literal are removed.<br>
     * In clauses containing the negated literal, this literal is removed.<br>
     * If the resulting clause is a unit clause, it is added to the model and removed from the clauses.<br>
     * If the resulting clause is a two-literal clause, it is added to the twoLitClauses and  not removed
     * from the clauses.
     *
     * @param literal a true literal
     * @param origins the basic clause ids for the truth of the literal
     * @throws Unsatisfiable if a contradiction is found.
     */
    private void integrateTrueLiteral(int literal, IntArrayList origins) throws Unsatisfiable {
        Iterator<CLiteral> iterator = literalIndex.popIterator(literal);
        while(iterator.hasNext()) {
            CLiteral cliteral = iterator.next();
            removeFromIndex(cliteral.clause);}
        literalIndex.pushIterator(literal,iterator);

        iterator = literalIndex.popIterator(-literal);
        while(iterator.hasNext()) {
            CLiteral cliteral = iterator.next();
            Clause clause = cliteral.clause;
            clause.remove(cliteral);
            removeFromIndex(clause);
            iterator.remove();
            switch(clause.size()) {
                case 1:
                    model.add(clause.getLiteral(0),
                            joinIntArraysSorted(clause.origins,origins),null);
                    removeFromIndex(clause);
                case 2: twoLitClauses.addDerivedClause(clause.getLiteral(0),clause.getLiteral(2),
                        joinIntArraysSorted(clause.origins,origins));}}
        literalIndex.pushIterator(-literal,iterator);}

    /** inserts the clause into the local data structures.
     *
     * @param clause  the clause to be inserted.
     */
    void insertClause(Clause clause) {
        ++statistics.clauses;
        maxClauseLength = Math.max(maxClauseLength,clause.size());
        clauses.add(clause);
        insertIntoIndex(clause);
        if(clause.isPositive()) {++statistics.positiveClauses;}
        else {if(clause.isNegative()) {++statistics.negativeClauses;}}}

    /** inserts a clause into the literal index
     *
     * @param clause  the clause to be inserted
     */
    void insertIntoIndex(Clause clause) {
        for(CLiteral cliteral : clause) {literalIndex.add(cliteral);}}

    /** removes a clause from the literal index
     *
     * @param clause  the clause to be inserted
     */
    void removeFromIndex(Clause clause) {
        for(CLiteral cliteral : clause) {literalIndex.remove(cliteral);}}
}