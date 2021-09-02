package Datastructures.Theory;

import Datastructures.Clauses.BasicClauseList;
import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseType;
import Datastructures.Literals.CLiteral;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Datastructures.Task;
import Management.Monitor;
import Management.ProblemSupervisor;
import Utilities.BucketSortedIndex;
import Utilities.BucketSortedList;
import com.sun.istack.internal.Nullable;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import java.util.*;
import java.util.concurrent.PriorityBlockingQueue;
import java.util.function.Consumer;

import static Utilities.Utilities.*;

/** A disjointness clause is a set of literals which are pairwise contradictory.
 * Such a clause may come from the input data, or be derived from binary clauses.
 * The class works as Thread in parallel to the other solvers.
 *
 * Created by Ohlbach on 21.09.2018.
 */
public class DisjointnessClasses {
    /** supervises the problem solution */
    private final ProblemSupervisor problemSupervisor;

    /** the current problem's id */
    private final String problemId;

    /** for enumerating the clauses */
    private int counter = 0;

    /** the statistics of this thread */
    public DisjointnessStatistics statistics;

    /** controls the computation of the clause's origins */
    private boolean trackReasoning;

    /** activates monitoring */
    private final boolean monitoring;

    /** for logging the actions of this class */
    private final Monitor monitor;

    /** for distinguishing the monitoring areas */
    private final String monitorId;

    /** The global (usually partial) model */
    private final Model model;

    /** The equivalence classes thread */
    private final EquivalenceClasses equivalenceClasses;

    /** The list of Disjointnes clauses */
    private final BucketSortedList<Clause> clauses;

    /** maps literals to the literal occurrences in clauses */
    private final BucketSortedIndex<CLiteral> literalIndex;

    /** supports time consuming algorithms */
    private int timestamp = 0;

    /** maps literals to disjoint literals */
    private final HashMap<Integer, ArrayList<CLiteral>> disjointnesses = new HashMap<>();

    /** called in integrateDisjointnessClause for a new clause */
    private final ArrayList<Consumer<Clause>> observers = new ArrayList<>();

    /** types of tasks in the queue */
    private enum TaskType {
        TRUELITERAL, INSERTCLAUSE, EQUIVALENCE}


    /** A queue of tasks */
    private final PriorityBlockingQueue<Task<TaskType>> queue =
            new PriorityBlockingQueue<>(10, Comparator.comparingInt(this::getPriority));

    /** gets the priority for the objects in the queue.
     *
     * @param task the task in the queue
     * @return the priority of the objects in the queue.
     */
    private int getPriority(Task<TaskType> task) {
        switch(task.taskType) {
            case TRUELITERAL:  return Integer.MIN_VALUE;
            case EQUIVALENCE:  return Integer.MIN_VALUE + 1;
            case INSERTCLAUSE: return ((Clause)task.a).id;} // makes the sequence deterministic
        return 4;}




    /** creates a new instance
     *
     * @param problemSupervisor the problem supervisor
     */
    public DisjointnessClasses(ProblemSupervisor problemSupervisor) {
        this.problemSupervisor = problemSupervisor;
        problemSupervisor.disjointnessClasses = this;
        problemId = problemSupervisor.problemId;
        model = problemSupervisor.model;
        equivalenceClasses = problemSupervisor.equivalenceClasses;
        clauses      = new BucketSortedList<Clause>(Clause::size);
        literalIndex = new BucketSortedIndex<CLiteral>(model.predicates+1,
                (cLiteral->cLiteral.literal),
                (cLiteral->cLiteral.clause.size()));
        statistics = new DisjointnessStatistics(problemId);
        trackReasoning = problemSupervisor.globalParameters.trackReasoning;
        monitor = problemSupervisor.globalParameters.monitor;
        monitoring = monitor != null;
        monitorId = problemId + "DISJ";}

    /** Any solver which is interested to know about newly derived disjointnesses can add an observer.
     * The observer is called with (literals, origins) as soon as new disjointnesses
     * literal1 != literal2 != ... are derived.
     *
     * @param observer a TriConsumer for transferring newly derived equivalences.
     */
    public void addObserver(Consumer<Clause> observer) {
        observers.add(observer);}



    /** This method is started as thread.
     * It reads and executes tasks from the queue
     * It can only be stopped by an interrupt or when a contradiction is found.
     *
     */
    public void run() {
        model.addObserver(Thread.currentThread(),this::addTrueLiteral);
        equivalenceClasses.addObserver(this::addEquivalence);

        while(!Thread.interrupted()) {
            try {
                if(monitoring) {monitor.print(monitorId,"Queue is waiting\n" + Task.queueToString(queue));}
                Task<TaskType> task = queue.take();
                IntArrayList origins = task.origins;
                switch(task.taskType) {
                    case TRUELITERAL:      integrateTrueLiteral((Integer)task.a,origins); break;
                    case INSERTCLAUSE:     integrateDisjointnessClause((Clause)task.a);   break;
                    case EQUIVALENCE:      integrateEquivalence((Integer)task.a, (Integer)task.b, origins);}
                if(monitoring) {monitor.print(monitorId,toString(model.symboltable));}}
            catch(InterruptedException ex) {return;}
            catch(Unsatisfiable unsatisfiable) {
                problemSupervisor.setResult(unsatisfiable,"Disjointness");
                return;}}}

    /** Adds a basic disjointness clause to the queue.
     *
     * @param basicClause a basic disjointness clause with at least 3 disjoint literals
     */
    public synchronized void addDisjointnessClause(int[] basicClause) {
        assert basicClause.length > 4;
        assert basicClause[1] == ClauseType.DISJOINT.ordinal() || basicClause[1] == ClauseType.XOR.ordinal();
        ++statistics.basicClauses;
        if(monitoring) {
            monitor.print(monitorId,"In:   disjointness clause: " +
                    BasicClauseList.clauseToString(0,basicClause,model.symboltable));}
        queue.add(new Task<>(TaskType.INSERTCLAUSE,null,new Clause(++counter,basicClause),null));}

    /** adds a true literal to the queue
     *
     * @param literal a true literal
     * @param origins the basic clause ids for the true literal
     */
    public synchronized void addTrueLiteral(int literal,IntArrayList origins) {
        ++statistics.trueLiterals;
        monitor.print(monitorId,"In:   True literal " +
                Symboltable.toString(literal,model.symboltable) +
                (origins == null ? "" : " " + origins));
        queue.add(new Task<>(TaskType.TRUELITERAL, origins, literal, null));}

    /** adds a new derived disjointness p,q,r to the queue
     *
     * @param literals some literals
     * @param origins the basic clause ids for the disjointness
     */
    public synchronized void addDerivedDisjoints(IntArrayList literals, IntArrayList origins) {
        ++statistics.derivedDisjointesses;
        if(monitoring) {
            monitor.print(monitorId,"In:   Disjointness " +
                    Symboltable.toString(literals,model.symboltable) +
                    (origins == null ? "" : " " + origins));}
        Clause clause = new Clause(++counter,ClauseType.DISJOINT,literals,origins);
        queue.add(new Task<>(TaskType.INSERTCLAUSE, null, clause, null));}

      /** adds a new equivalence class representative == literal to the queue
     *
     * @param representative the representative of the class
     * @param literal        the literal which equals the representative
     * @param origins        the ids of the basic clauses causing the equivalence
     */

    public synchronized void addEquivalence(int representative, int literal, IntArrayList origins) {
        ++statistics.equivalences;
        if(monitoring) {
            monitor.print(monitorId,"In:   Equivalence " +
                    Symboltable.toString(representative, model.symboltable) + " = " +
                    Symboltable.toString(literal, model.symboltable)  +
                    (origins == null ? "" : " " + origins));}
        queue.add(new Task<>(TaskType.EQUIVALENCE, origins, representative, literal));}



    /** turns a basicClause into a disjointness class. <br>
     *  Before treating the literals, they are mapped to their representatives in the equivalence classes (if necessary) <br>
      *
     * @param clause a new disjointness clause
     */
    protected void integrateDisjointnessClause(Clause clause) throws Unsatisfiable {
        if(monitoring) {
            monitor.print(monitorId,"Exec: disjointness clause: " + clause.toString(0,model.symboltable));}
        clause = normalizeClause(clause);
        if(clause == null) return;
        Clause subsumer = isSubsumed(clause);
        if(subsumer != null) {
            if(monitoring) {
                monitor.print(monitorId,"New clause is subsumed by " +
                        subsumer.toString(0,model.symboltable));}
            return;}
        if(!clauses.isEmpty()) {
            resolveClause(clause);
            if(clause.size() <= 1) {return;}}

        extendNormalizedClause(clause);
        removeSubsumedClauses(clause);
        insertClause(clause);
        for(Consumer<Clause> observer : observers) observer.accept(clause);
    }


    /** performs a number of transformations and simplifications.
     * replaces literals by representatives in some equivalence class<br>
     * checks for true/false literals<br>
     * checks for double literals p,p (p becomes false)<br>
     * checks for complementary literals p,-p (all other literals become false)<br>
     *
     * @param clause  a disjointness clause
     * @return null or the normalized clause
     * @throws Unsatisfiable if a contradiction is discovered.
     */
    private Clause normalizeClause(Clause clause) throws Unsatisfiable {
        ArrayList<CLiteral> cliterals = clause.cliterals;

        if(!equivalenceClasses.isEmpty()) {  // replacement of equivalent literals
            for(CLiteral cliteral : cliterals) {
                int oldLiteral = cliteral.literal;
                int literal = equivalenceClasses.getRepresentative(oldLiteral);
                if(literal != oldLiteral) {
                    cliteral.literal = literal;
                    if(trackReasoning) clause.origins = joinIntArraysSorted(clause.origins,
                                        equivalenceClasses.getOrigins(oldLiteral));}}}

        for(int i = 0; i < cliterals.size(); ++i) {
            CLiteral cliteral = cliterals.get(i);
            int literal = cliteral.literal;
            switch(model.status(literal)) { // checking truth values of the literals
                case -1: clause.remove(cliteral); --i; continue; // false literals can be ignored.
                case +1: // true literals cause all other literals to become false.
                    if(monitoring) {
                        monitor.print(monitorId,"True literal " + Symboltable.toString(literal,model.symboltable) +
                                " causes all other literals in disjointness clause\n" +
                                clause.toString(0, model.symboltable) + " to become false.");}
                    for(CLiteral clit : cliterals) {
                        if(clit != cliteral) {
                            model.add(-clit.literal,
                                trackReasoning ? joinIntArraysSorted(clause.origins,model.getOrigin(literal)) : null,
                                null);}} // back to this
                    return null;} // clause no longer needed

            switch(clause.contains(literal,cliteral)) {
                case +1: // double literal is false
                    if(monitoring) {
                        monitor.print(monitorId,"Disjointenss clause " + clause.toString(0,model.symboltable)+
                            " contains double literal " + Symboltable.toString(literal, model.symboltable) +
                                " which becomes false.");}
                     model.add(-literal,
                            trackReasoning ? joinIntArraysSorted(clause.origins,model.getOrigin(literal)) : null,
                            null);  // back to this
                    for(int j = 0; j < cliterals.size(); ++j) {
                        CLiteral clit = cliterals.get(j);
                        if(clit.literal == literal) {cliterals.remove(clit); --j;}}
                    return clause.size() > 1 ?  normalizeClause(clause) : null;
                case -1: // p != -p is true. All other literals become false.
                    if(monitoring) {
                        monitor.print(monitorId,"Complementary literals +-" +
                                Symboltable.toString(literal,model.symboltable) +
                                " causes all other literals in disjointness clause " +
                                clause.toString(0, model.symboltable) + " to become false.");}
                    for(CLiteral clit : cliterals) {
                        if(Math.abs(clit.literal) != Math.abs(literal)) {
                            model.add(-clit.literal,clause.origins,null);}}
                    return null;}}
        return clause.size() > 1 ? clause : null;}


    /** A new Disjointness: p,q may interact with an old disjointness p,-q, which causes -p to become true.
     * This method looks for these kind of resolution possibilities to generate unit clauses.
     * In this case p is removed from the literals
     *
     * @param clause the clause to be tested
     * @throws Unsatisfiable if an contradiction is found.
     */
    private void resolveClause(Clause clause) throws Unsatisfiable {
        for(CLiteral cliteral1 : clause.cliterals) {
            timestamp += 2;
            int literal1 = cliteral1.literal;
            literalIndex.withIterator(literal1,(iterator -> iterator.next().clause.timestamp = timestamp));
            // all clauses with literal1 are timestamped

            for(CLiteral cliteral2 : clause.cliterals) {
                int literal2 = cliteral1.literal;
                if(cliteral1 != cliteral2) { // now we look for clauses with -literal2
                    CLiteral clit2 = literalIndex.findWithIterator(-literal2,(iterator -> {
                        CLiteral clit = iterator.next();
                        return (clit.clause.timestamp == timestamp) ? clit : null;}));

                    if(clit2 != null) { // clit2.clause has literal1, -literal2
                        statistics.resolutions++;
                        if(monitoring) {
                            monitor.print(monitorId, "Resolution of clause\n" +
                                    clause.toString(4, model.symboltable) + " with clause\n" +
                                    clit2.clause.toString(4, model.symboltable) + "\n  yields unit literal " +
                                    Symboltable.toString(-literal1, model.symboltable));}
                        IntArrayList origins = trackReasoning ? joinIntArraysSorted(clause.origins, clit2.clause.origins) : null;
                        model.add(-literal1,origins,null);
                        clause.remove(cliteral1);
                        clause.origins = origins;
                        if(clause.size() > 1) resolveClause(clause); // try again
                        return;}}}}
        timestamp += 2;}

    /** tries to extend a list of disjoint literals.
     * Example: literals = p,q,r
     * Find a literal s such that p != s, q != s and r !=s holds.
     * This literal is added to p,q,r.
     *
     * @param clause a list of disjointness clause
     */
    private void extendNormalizedClause(Clause clause) {
        boolean extended = true;
        while(extended) {  // several extensions are possible
            extended = false;
            for(CLiteral cliteral1 : clause.cliterals) {
                int literal1 = cliteral1.literal;
                ArrayList<CLiteral> disjoints = disjointnesses.get(literal1);
                if(disjoints == null) break; // no extension possible
                for(CLiteral cliteral2 : disjoints) {
                    int literal2 = cliteral2.literal; // literal2 is a candidate for extension
                    boolean found = true;
                    for(CLiteral cliteral11 : clause.cliterals) { // all other literals must also be disjoint to literal2
                        if(cliteral11 != cliteral1) continue;
                        if(!areDisjoint(cliteral11.literal,literal2)) {found = false; break;}}
                    if(found) {
                        extended = true;
                        if(monitoring) {
                            monitor.print(monitorId,"Disjointenss Clause " +  clause.toString(0, model.symboltable) +
                                    "\nwill be extended with literal " + Symboltable.toString(literal2,model.symboltable));}
                        clause.add(literal2);
                        statistics.extendedClasses++;
                        if(trackReasoning) {
                            IntArrayList origins = clause.origins;
                            for(CLiteral cliteral11 : clause.cliterals)
                                origins = joinIntArraysSorted(origins,getOrigins(cliteral11.literal,literal2));
                            clause.origins = origins;}}}}}}

    /** checks of a new disjointness class is a subset of an already existing one.
     * In this case the new class is superfluous
     *
     * @param clause a disjointness clause
     * @return null or the subsumer class
     */
    private Clause isSubsumed(Clause clause) {
        for(Clause dClause : clauses) {
            if(clause.isSubset(dClause)) {statistics.forwardSubsumed++; return dClause;}}
        return null;}

    private final ArrayList<Clause> subsumed = new ArrayList<>();

    /** removes all disjointness classes which are subsets of the new class
     *
     * @param clause a new disjointness clause
     */
    private void removeSubsumedClauses(Clause clause) {
        subsumed.clear();
        for(Clause clause2 : clauses) {
            if(clause2.isSubset(clause)) subsumed.add(clause2);}
        for(Clause clause2 : subsumed) removeClause(clause2);}


    /** A true literal p in a disjointness clause p,q,r causes q,r to become false.
     * A false literal p in a disjointness clause p,q,r causes p to be removed <br>
     * All literals which get a truth value are removed form the disjointness classes.
     *
     * @param literal a true literal
     * @param origins the basic clause ids for the truth of the literal
     * @throws Unsatisfiable if a contradiction is found.
     */
    protected void integrateTrueLiteral(int literal, IntArrayList origins) throws Unsatisfiable {
        if (monitoring) {
            monitor.print(monitorId, "Exec: true literal: " +
                    Symboltable.toString(literal, model.symboltable));}
        if(!literalIndex.isEmpty(literal)) {
            BucketSortedList<CLiteral>.BucketIterator iterator = literalIndex.popIterator(literal);
            while(iterator.hasNext()) { // all other literals in the clauses become false.
                CLiteral cliteral1 = iterator.next();
                Clause clause = cliteral1.clause;
                if(monitoring) monitor.print(monitorId, "All other literals in clause " +
                        clause.toString(0,model.symboltable) + " become false.");
                for(CLiteral cliteral2 : clause.cliterals) {
                    if(cliteral1 != cliteral2) {
                        model.add(-cliteral2.literal,
                                trackReasoning ? joinIntArraysSorted(origins,clause.origins) : null, null);}}
                removeClause(cliteral1,iterator);}
            literalIndex.pushIterator(literal,iterator);}

        if(!literalIndex.isEmpty(-literal)) {
            literalIndex.withIterator(-literal, (iterator -> {
                CLiteral cliteral1 = iterator.next();
                Clause clause = cliteral1.clause;
                removeClause(cliteral1, iterator);
                clause.remove(cliteral1);
                if(clause.size() > 1) {
                    if(trackReasoning) clause.origins = joinIntArraysSorted(origins,clause.origins);
                    queue.add(new Task<>(TaskType.INSERTCLAUSE, null, clause, null));}}));}
        }



    /** removes all clauses with literal and creates a new Task which does the replacements
     *
     * @param representative representative == literal
     * @param literal        representative == literal
     * @param origins        null or the basic clause ids for this equivalence
     */
    protected void integrateEquivalence(int representative, int literal, IntArrayList origins) {
        if(monitoring) {
            monitor.print(monitorId,"Exec: equivalence " +
                    Symboltable.toString(representative,model.symboltable) + " = " +
                    Symboltable.toString(literal,model.symboltable));}
        for(int i = 1; i <= 2; ++i) {
            if(!literalIndex.isEmpty(literal)) {
                literalIndex.withIterator(literal,(iterator -> {
                CLiteral cLiteral = iterator.next();
                Clause clause = iterator.next().clause;
                removeClause(cLiteral,iterator);
                if(trackReasoning) clause.origins = joinIntArraysSorted(origins,clause.origins);
                queue.add(new Task<>(TaskType.INSERTCLAUSE, null, clause, null));}));}
            literal = -literal;}}


    /** checks if two literals are disjoint
     *
     * @param literal1 a literal
     * @param literal2 a literal
     * @return true if the two literals are disjoint
     */
    public boolean areDisjoint(int literal1, int literal2) {
        if(literal1 == -literal2) {return true;}
        ArrayList<CLiteral> result = disjointnesses.get(literal1);
        if (result == null) {return false;}
        for(CLiteral cliteral : result) {
            if(cliteral.literal == literal2) return true;}
        return false;}


    /** returns the origins (basicClause ids) which caused literal1 disjoint to literal2.
     * The origins may not be unique. <br>
     * Example: p,q,r  and p,q,s<br>
     * Both have origins for p,q.<br>
     * In this case the first ones found is returned.<br>
     *
     * @param literal1 a literal
     * @param literal2 a literal
     * @return null or the origins which caused the literals to be disjoint.
     */
    public IntArrayList getOrigins(int literal1, int literal2) {
        if(literal1 == literal2) {return null;}
        if(literal1 == -literal2) {return null;}
        ArrayList<CLiteral> result = disjointnesses.get(literal1);
        if (result == null) {return null;}
        for(CLiteral cliteral : result) {
            if(cliteral.literal == literal2) return cliteral.clause.origins;}
        return null;}

    /** inserts the clause into the local data structures.
     *
     * @param clause  the clause to be inserted.
     */
    private void insertClause(Clause clause) {
        ++statistics.clauses;
        clauses.add(clause);
        for(CLiteral cliteral1 : clause) {
            literalIndex.add(cliteral1);
            for(CLiteral cliteral2 : clause)
                if(cliteral1 != cliteral2)
                    disjointnesses.computeIfAbsent(cliteral1.literal,
                            k-> new ArrayList<>()).add(cliteral2);}}

    /** removes a clause from the internal lists.
     *
     * @param clause a clause to be removed.
     */
    private void removeClause(Clause clause) {
        --statistics.clauses;
        clauses.remove(clause);
        for(CLiteral cliteral1 : clause) {
            literalIndex.remove(cliteral1);
            for(CLiteral cliteral2 : clause)
                if(cliteral1 != cliteral2) disjointnesses.get(cliteral1.literal).remove(cliteral2);}}

    /** removes the iterator's next clause from the index and the clauses
     *
     * @param cliteral iterator's next cliteral;
     * @param iterator an iterator over the literal index.
     * @return cliteral;
     */
    private CLiteral removeClause(CLiteral cliteral,BucketSortedList<CLiteral>.BucketIterator iterator)  {
        Clause clause = cliteral.clause;
        --statistics.clauses;
        clauses.remove(clause);
        iterator.remove(); // removes cliteral and keeps the iterator okay
        for(CLiteral cliteral1 : clause) {
            if(cliteral1 != cliteral) literalIndex.remove(cliteral1);
            for(CLiteral cliteral2 : clause)
                if(cliteral1 != cliteral2) disjointnesses.get(cliteral1.literal).remove(cliteral2);}
        return cliteral;}

    /** checks if there are disjointness classes
     *
     * @return true if there are no disjointness classes
     */
    public boolean isEmpty() {
        return clauses.isEmpty();}

    /** turns the disjoinentesses into a string.
     *
     * @return the disjointnesses as a string
     */
    public String toString() {
        return toString(null);}

    /** lists all disjointness classes
     *
     * @return all disjointness classes as string
     */
    public String toString(@Nullable Symboltable symboltable) {
        if(clauses.isEmpty()) {return "";}
        StringBuilder string = new StringBuilder();
        string.append("Disjointness Clauses of Problem " + problemId + ":");
        int width = Integer.toString(counter).length();
        for(Clause clause : clauses) {
            string.append("\n").append(clause.toString(width,symboltable));}
        return string.toString();}

    /** turns the internal data structures into a string
     *
     * @param symboltable null or a symboltable
     * @return the datastructures as string.
     */
    public String infoString(@Nullable Symboltable symboltable) {
        StringBuilder string = new StringBuilder();
        if(!clauses.isEmpty()) {
            int width = Integer.toString(counter).length();
            string.append("Disjointness Classes of Problem "+problemId+":\n");
            for(Clause clause : clauses) {
                string.append(clause.infoString(width,symboltable)).append("\n");}}
        if(!disjointnesses.isEmpty()) {
            string.append("\nDisjoint Literals of Problem "+problemId+":\n");
            disjointnesses.forEach((literal, disjoints) -> {
                string.append(Symboltable.toString(literal,symboltable)).append(": ");
                for(CLiteral clit : disjoints){
                    string.append(Symboltable.toString(clit.literal  ,symboltable)).append(",");}
                string.append("\n");});}
        if(!queue.isEmpty()) {
            string.append("Disjointnesses Queue of Problem "+problemId+":").append(Task.queueToString(queue));}
        return string.toString();}
    }