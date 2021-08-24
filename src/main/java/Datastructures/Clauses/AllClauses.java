package Datastructures.Clauses;

import Datastructures.Literals.CLiteral;
import Datastructures.Literals.LitAlgorithms;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Datastructures.Theory.DisjointnessClass;
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
    private int timestamp = 1;

    private static enum TaskType {
        TRUELITERAL, EQUIVALENCE, DISJOINTNESS,REPLACEMENTRESOLUTION, SIMPLIFY,
    }

    private static class Task {
        TaskType taskType;
        IntArrayList origins;
        Object a;
        Object b;
        Task(TaskType taskType, IntArrayList origins, Object a, Object b) {
            this.taskType = taskType;
            this.origins = origins;
            this.a = a;
            this.b = b;
        }
    }


    /** A queue of newly derived unit literals, newly derived binary equivalences and disjointness clauses
     * The unit literals are automatically put at the beginning of the queue.
     */
    private final PriorityBlockingQueue<Task> queue =
            new PriorityBlockingQueue<Task>(10,
                    (task1,task2) ->Integer.compare(getPriority(task1),getPriority(task2)));

    /** gets the priority for the objects in the queue.
     *
     * @param task the objects in the queue
     * @return the priority of the objects in the queue.
     */
    private int getPriority(Task task) {
        switch(task.taskType) {
            case TRUELITERAL:  return 0;
            case EQUIVALENCE:  return 1;
            case DISJOINTNESS: return 2;
            case REPLACEMENTRESOLUTION: return 3;
            case SIMPLIFY: return 1 + (((CLiteral)task.b).clause).size();
            }
        return -1;}


    /** constructs a new AllClauses instance
     *
     * @param problemSupervisor    coordinates several solvers.
     */
    public AllClauses(ProblemSupervisor problemSupervisor) throws Unsatisfiable {
        this.problemSupervisor = problemSupervisor;
        globalParameters = problemSupervisor.globalParameters;
        thread = Thread.currentThread();
        model = problemSupervisor.model;
        basicClauseList = problemSupervisor.basicClauseList;
        equivalenceClasses  = problemSupervisor.equivalenceClasses;
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
                queue.add(new Task(TaskType.TRUELITERAL,origins, literal,null)));

        equivalenceClasses.addObserver((representative,literal,origins) ->
                queue.add(new Task(TaskType.EQUIVALENCE,origins,representative,literal)));

        disjointnessClasses.addObserver((disjoints) ->
                queue.add(new Task(TaskType.DISJOINTNESS,null, disjoints, null)));

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
                model.add(basicClause[i],IntArrayList.wrap(new int[]{basicClause[0]}),thread);}}}


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


    /** works off the queue
     * new true literals <br>
     * new equivalences <br>
     * new disjointnesses <br>
     *
     * @return null or Unsatisfiable if a contradiction is found.
     */
    public Unsatisfiable runQueue() {
        while(!Thread.interrupted()) {
            try {
                if(monitoring) {monitor.print(monitorId,"Queue is waiting");}
                Task task = queue.take(); // waits if the queue is empty
                switch (task.taskType) {
                    case TRUELITERAL: integrateTrueLiteral((Integer)task.a,task.origins);
                    break;
                    case EQUIVALENCE:
                        integrateEquivalence((Integer)task.a,(Integer)task.b,task.origins);
                        break;
                    case DISJOINTNESS:
                        integrateDisjointness((DisjointnessClass)task.a);
                        break;
                    case REPLACEMENTRESOLUTION:
                        replacementResolutionBackwards();
                    case SIMPLIFY:
                        simplify((Clause)task.a);
                }}
            catch(InterruptedException ex) {return null;}
            catch(Unsatisfiable unsatisfiable) {return unsatisfiable;}}
        return null;}



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
        int position = -1;
        for(int i = 2; i < basicClause.length; ++i) {
            int originalLiteral =  basicClause[i];
            int literal = equivalenceClasses.getRepresentative(originalLiteral);
            if(trackReasoning && literal != originalLiteral) {
                origins = joinIntArraysSorted(origins,equivalenceClasses.getOrigins(originalLiteral));}

            switch(model.status(literal)) {
                case +1: return; // true literal:  ignore clause
                case -1:         // false literal: ignore literal
                    if(trackReasoning) {origins = joinIntArraysSorted(origins,model.getOrigin(literal));}
                    continue;}

            switch(clause.contains(literal)) {
                case +1: continue; // double literal
                case -1:
                    ++statistics.tautologies;
                    if(monitoring) {
                        monitor.print(monitorId, "Clause " +
                                BasicClauseList.clauseToString(0,basicClause,symboltable) + " became a tautology");}
                    return;}   // tautology

            clause.add(new CLiteral(literal,clause,++position));}

        switch(clause.size()) {
            case 0:
                throw new Unsatisfiable("Clause " + BasicClauseList.clauseToString(0,basicClause,symboltable) +
                    " became empty",origins);
                case 1:
                    ++statistics.derivedUnitClauses;
                    model.add(clause.getLiteral(0),origins,null); // back to this process
                    return;}

        if(isSubsumed(clause)) return;
        if(clause.size() == 2) twoLitClauses.addDerivedClause(clause.getLiteral(0),
                                                              clause.getLiteral(1),origins);
        clause.setStructure();
        clause.origins = origins;
        insertClause(clause);}


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
        BucketSortedList<CLiteral>.BucketIterator iterator = literalIndex.popIterator(literal);
        while(iterator.hasNext()) {   // remove all clauses with the literal
            removeClause(iterator);}
        literalIndex.pushIterator(literal,iterator);

        iterator = literalIndex.popIterator(-literal);
        while(iterator.hasNext()) {               // remove from all clauses the negated literal
            removeLiteral(iterator,origins);}
        literalIndex.pushIterator(-literal,iterator);}

    /** replaces all occurrences of literal by representative
     * resulting tautologies are deleted <br>
     * unit literals are put into the model <br>
     * two-literal clauses are kept and put into the twoLiteral module.
     *
     * @param representative
     * @param literal
     * @param origins      the basic clause ids for the equivalence.
     * @throws Unsatisfiable if a contradiction is found
     */
    private void integrateEquivalence(int representative, int literal, IntArrayList origins) throws Unsatisfiable {
        BucketSortedList<CLiteral>.BucketIterator iterator;
        for(int i = 1; i <= 2; ++i) {
            iterator = literalIndex.popIterator(literal);
            while(iterator.hasNext()) {
                Clause clause = replaceLiteral(iterator,representative,literal,origins);
                if(clause != null) subsumes(clause);}
            literalIndex.pushIterator(literal,iterator);
            literal = -literal;
            representative = -representative;}
    }

    /** turns a disjointness class into the corresponding list of two-literal clauses.
     *
     * @param disjoints a disjointness class.
     */
    private void integrateDisjointness(DisjointnessClass disjoints) {
        IntArrayList origins  = disjoints.origins;
        IntArrayList literals = disjoints.literals;
        int size = literals.size();
        for(int i = 0; i < size; ++i) {
            int literal1 = -literals.getInt(i);
            for(int j = i+1; j < size; ++j) {
                Clause clause = new Clause(++counter,literal1,-literals.getInt(j),origins);
                if(!isSubsumed(clause)) {
                    insertClause(clause);
                    subsumes(clause);}}}}

    /** checks if the clause is subsumed by another clause
     *
     * @param clause a clause
     * @return true if the clause is subsumed
     */
    private boolean isSubsumed(Clause clause) {
        Clause subsumer = LitAlgorithms.isSubsumed(clause,literalIndex,timestamp);
        timestamp += clause.size() + 2;
        if(subsumer != null) {
            ++statistics.forwardSubsumptions;
            if(monitoring)
                monitor.print(monitorId, "Clause\n" + clause.toString(4,symboltable) +
                        " is subsumed by clause\n" + subsumer.toString(4,symboltable));
            return true;}
        return false;}

    private ArrayList<Clause> subsumedClauses = new ArrayList<>();

    /** removes all clauses subsumed by the given clause
     *
     * @param clause a clause
     */
    private void subsumes(Clause clause) {
        subsumedClauses.clear();
        LitAlgorithms.subsumes(clause,literalIndex,timestamp,subsumedClauses);
        for(Clause subsumed : subsumedClauses) {
            ++statistics.backwardSubsumptions;
            if(monitoring)
                monitor.print(monitorId, "Clause\n" + subsumed.toString(4,symboltable) +
                        " is subsumed by clause\n" + clause.toString(4,symboltable));
            removeClause(clause);}
        timestamp += 2;
    }

    /** performs all replacement resolutions
     *
     * @throws Unsatisfiable if a contradiction is found.
     */
    private void replacementResolutionBackwards() throws Unsatisfiable{
        ArrayList<Object[]> results = new ArrayList<>();
        for(Clause clause : clauses) {
            Object[] result = LitAlgorithms.replacementResolutionBackwards(clause,literalIndex,timestamp);
            timestamp += 2;
            if(result != null) results.add(result);}

        for(Object[] result : results) {
            CLiteral cliteral = (CLiteral)result[0];
            Clause otherClause = (Clause)result[1];
            ++statistics.backwardReplacementResolutions;
            if(monitoring) {
                monitor.print(monitorId, "Literal " + cliteral.toString(symboltable) +
                        " in  clause \n" + cliteral.clause.toString(3,symboltable) +
                        " will be removed by replacement resolution with clause\n" +
                        otherClause.toString(3,symboltable));}
            if(removeLiteral(cliteral,trackReasoning ? joinIntArraysSorted(cliteral.clause.origins,otherClause.origins) : null))
                queue.add(new Task(TaskType.SIMPLIFY,null,cliteral.clause,null));}
    }

    private void simplify(Clause clause) {
        subsumes(clause);


    }

    /** inserts the clause into the local data structures.
     *
     * @param clause  the clause to be inserted.
     */
    void insertClause(Clause clause) {
        ++statistics.clauses;
        maxClauseLength = Math.max(maxClauseLength,clause.size());
        clauses.add(clause);
        for(CLiteral cliteral : clause) {literalIndex.add(cliteral);}
        switch(clause.structure) {
            case NEGATIVE: ++statistics.negativeClauses; break;
            case POSITIVE: ++statistics.positiveClauses; break;}}

    /** removes the clause
     *
     * @param clause a clause to be removed.
     */
    private void removeClause(Clause clause) {
        switch(clause.structure) {
            case NEGATIVE: --statistics.negativeClauses; break;
            case POSITIVE: --statistics.positiveClauses; break;}
        --statistics.clauses;
        for(CLiteral cliteral : clause) {literalIndex.remove(cliteral);}
        clauses.remove(clause);}


    /** removes the iterator's next clause
     *
     * @param iterator an iterator over the literal index.
     */
    private void removeClause(BucketSortedList<CLiteral>.BucketIterator iterator) {
        CLiteral cliteral = iterator.next();
        Clause clause = cliteral.clause;
        switch(clause.structure) {
            case NEGATIVE: --statistics.negativeClauses; break;
            case POSITIVE: --statistics.positiveClauses; break;}
        --statistics.clauses;
        iterator.remove();
        for(CLiteral clit : clause) {
            if(clit != cliteral) literalIndex.remove(clit);}
        clauses.remove(cliteral.clause);}

    /** removes the cliteral
     *
     * @param cliteral the literal to be removed
     * @param origins the basic clause ids for removing the literal.
     * @return true if the clause survived
     */
    private boolean removeLiteral(CLiteral cliteral, IntArrayList origins) throws Unsatisfiable {
        Clause clause = cliteral.clause;
        switch(clause.structure) {
            case NEGATIVE: --statistics.negativeClauses; break;
            case POSITIVE: --statistics.positiveClauses; break;}
        --statistics.clauses;
        clause.remove(cliteral);
        clause.setStructure();
        literalIndex.remove(cliteral);
        return updateClause(clause,origins);}


    /** removes the iterator's next literal
     *
     * @param iterator an iterator over the literal index.
     * @param origins the basic clause ids for removing the literal.
     */
    private void removeLiteral(BucketSortedList<CLiteral>.BucketIterator iterator, IntArrayList origins) throws Unsatisfiable {
        CLiteral cliteral = iterator.next();
        Clause clause = cliteral.clause;
        switch(clause.structure) {
            case NEGATIVE: --statistics.negativeClauses; break;
            case POSITIVE: --statistics.positiveClauses; break;}
        --statistics.clauses;
        clause.remove(cliteral);
        clause.setStructure();
        iterator.remove();
        updateClause(clause,origins);}

    private boolean updateClause(Clause clause, IntArrayList origins) throws Unsatisfiable{
        switch(clause.size()) {
            case 1:
                model.add(clause.getLiteral(0),
                        trackReasoning ? joinIntArraysSorted(clause.origins,origins) : null,null);
                literalIndex.remove(clause.getCLiteral(0));
                clauses.remove(clause);
                return false;
            case 2: twoLitClauses.addDerivedClause(clause.getLiteral(0),clause.getLiteral(2),
                    trackReasoning ? joinIntArraysSorted(clause.origins,origins) : null);} // keep the two-literal clause

        switch(clause.structure) {
            case NEGATIVE: ++statistics.negativeClauses; break;
            case POSITIVE: ++statistics.positiveClauses; break;}
        ++statistics.clauses;
        return true;}

    /** replaces the oldLiteral by the newLiteral
     *
     * @param iterator    the next() yields the oldLiteral
     * @param newLiteral  a new literal
     * @param oldLiteral  an old literal in the clause
     * @param origins     null or the clause ids for the replacement
     * @return            null (tautology) or the changed clause
     * @throws Unsatisfiable if a contradiction has been found.
     */
    private Clause replaceLiteral(BucketSortedList<CLiteral>.BucketIterator iterator,
                                int newLiteral, int oldLiteral, IntArrayList origins) throws Unsatisfiable {
        CLiteral cliteral = iterator.next();
        Clause clause = cliteral.clause;
        IntArrayList orig = trackReasoning ? joinIntArraysSorted(clause.origins,origins) : null;
        switch(clause.contains(newLiteral)) {
            case +1 : removeLiteral(iterator,orig); return clause;  // double oldLiteral
            case -1 : removeClause(iterator);       return null;}   // tautology

        switch(clause.structure) {
            case NEGATIVE: --statistics.negativeClauses; break;
            case POSITIVE: --statistics.positiveClauses; break;}
        iterator.remove();
        cliteral.literal = newLiteral;
        literalIndex.add(cliteral);
        clause.origins = orig;
        clause.setStructure();
        switch(clause.structure) {
            case NEGATIVE: ++statistics.negativeClauses; break;
            case POSITIVE: ++statistics.positiveClauses; break;}
        return clause;
    }


}