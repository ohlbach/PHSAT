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

import javafx.util.Pair;

import static Utilities.Utilities.*;

/** A disjointness class is a set of literals which are pairwise contradictory.
 * Such a class may come from the input data, or be derived from binary clauses.
 * The class works as Thread in parallel to the other solvers.
 *
 * Created by Ohlbach on 21.09.2018.
 */
public class DisjointnessClasses {
    /** supervises the problem solution */
    private final ProblemSupervisor problemSupervisor;

    private final String problemId;

    /** for enumerating the classes */
    private int counter = 0;

    /** the statistics of this thread */
    public DisjointnessStatistics statistics;

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


    private final BucketSortedList<Clause> clauses;
    private final BucketSortedIndex<CLiteral> literalIndex;

    private int maxClauseLength;

    private int timestamp = 0;

    /** maps literals to disjoint literals */
    //private final HashMap<Integer, IntArrayList> disjointnesses = new HashMap<>();

    /** The list of disjointness classes */
    //private final ArrayList<DisjointnessClass> disjointnessClasses = new ArrayList<>();

    private final ArrayList<Consumer<DisjointnessClass>> disjointnessObservers = new ArrayList<>();

    private final ClauseType clauseType = ClauseType.DISJOINT;

    /** The current thread */
    public Thread thread;

    private enum TaskType {
        TRUELITERAL, DERIVEDDISJOINTS, DISJOINTNESSCLAUSE ,EQUIVALENCE}


    /** A queue of newly derived unit literals, newly derived binary disjointnesses and basic disjointness clauses
     * The unit literals are automatically put at the beginning of the queue.
     */
    private final PriorityBlockingQueue<Task<TaskType>> queue =
            new PriorityBlockingQueue<>(10, Comparator.comparingInt(this::getPriority));

    /** gets the priority for the objects in the queue.
     *
     * @param task the task in the queue
     * @return the priority of the objects in the queue.
     */
    private int getPriority(Task<TaskType> task) {
        switch(task.taskType) {
            case TRUELITERAL:        return Integer.MIN_VALUE;
            case EQUIVALENCE:        return Integer.MIN_VALUE + 1;
            case DISJOINTNESSCLAUSE: return Integer.MIN_VALUE + 2 + ((int[])task.a)[0];
            case DERIVEDDISJOINTS:   return task.priority;}
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
        statistics = new DisjointnessStatistics(problemSupervisor.problemId);
        trackReasoning = problemSupervisor.globalParameters.trackReasoning;
        monitor = problemSupervisor.globalParameters.monitor;
        monitoring = monitor != null;
        monitorId = problemId + "DISJ";
        thread = Thread.currentThread();}

    /** Any solver which is interested to know about newly derived disjointnesses can add an observer.
     * The observer is called with (literals, origins) as soon as new disjointnesses
     * literal1 != literal2 != ... are derived.
     *
     * @param observer a TriConsumer for transferring newly derived equivalences.
     */
    public void addObserver(Consumer<DisjointnessClass> observer) {
        disjointnessObservers.add(observer);}



    /** This method is started as thread.
     * It reads and executes tasks from the queue
     * It can only be stopped by an interrupt or when a contradiction is found.
     *
     */
    public void run() {
        thread = Thread.currentThread();
        model.addObserver(thread,this::addTrueLiteral);
        equivalenceClasses.addObserver(this::addEquivalence);

        while(!Thread.interrupted()) {
            try {
                if(monitoring) {monitor.print(monitorId,"Queue is waiting\n" + Task.queueToString(queue));}
                Task<TaskType> task = queue.take();
                IntArrayList origins = task.origins;
                switch(task.taskType) {
                    case TRUELITERAL:        integrateTrueLiteral((Integer)task.a,origins);           break;
                    case DISJOINTNESSCLAUSE: integrateDisjointnessClause((Clause)task.a);   break;
                    case DERIVEDDISJOINTS:   integrateDerivedDisjoints((IntArrayList)task.a,origins); break;
                    case EQUIVALENCE:        integrateEquivalence((Integer)task.a, (Integer)task.b, origins);}
                if(monitoring) {
                    monitor.print(monitorId,"Current disjointnesses:\n" +
                            toString("            ",model.symboltable));}}
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
        if(monitoring) {
            monitor.print(monitorId,"In:   disjointness clause: " +
                    BasicClauseList.clauseToString(0,basicClause,model.symboltable));}
        statistics.clauses++;
        queue.add(new Task<>(TaskType.DISJOINTNESSCLAUSE,null,new Clause(++counter,basicClause),null));}

    /** adds a true literal to the queue
     *
     * @param literal a true literal
     * @param origins the basic clause ids for the true literal
     */
    public synchronized void addTrueLiteral(int literal,IntArrayList origins) {
        statistics.trueLiterals++;
        monitor.print(monitorId,"In:   Unit literal " +
                Symboltable.toString(literal,model.symboltable) +
                (origins == null ? "" : " " + origins));
        queue.add(new Task<>(TaskType.TRUELITERAL, origins, literal, null));}

    private int priority = 0;
    /** adds a new derived disjointness p,q,r to the queue
     *
     * @param literals some literals
     * @param origins the basic clause ids for the disjointness
     */
    public synchronized void addDerivedDisjoints(IntArrayList literals, IntArrayList origins) {
        statistics.derivedDisjointesses++;
        if(monitoring) {
            monitor.print(monitorId,"In:   Disjointness " +
                    Symboltable.toString(literals,model.symboltable) +
                    (origins == null ? "" : " " + origins));}
        Task<TaskType> task = new Task<>(TaskType.DERIVEDDISJOINTS, origins, literals, null);
        task.priority = ++priority;
        queue.add(task);}

      /** adds a new equivalence class representative == literal to the queue
     *
     * @param representative the representative of the class
     * @param literal        the literal which equals the representative
     * @param origins        the ids of the basic clauses causing the equivalence
     */

    public synchronized void addEquivalence(int representative, int literal, IntArrayList origins) {
        statistics.equivalences++;
        if(monitoring) {
            monitor.print(monitorId,"In:   Equivalence " +
                    Symboltable.toString(representative, model.symboltable) + " = " +
                    Symboltable.toString(literal, model.symboltable)  +
                    (origins == null ? "" : " " + origins));}
        queue.add(new Task<>(TaskType.EQUIVALENCE, origins, representative, literal));}

    /** integrates a new disjointness submitted from some other part of the program.
     * The method simulates a basic clause.
     *
     * @param literals a list of disjoint literals
     * @param origins   the basic clause ids causing the disjointness
     * @throws Unsatisfiable if a contradiction is found.
     */
    public void integrateDerivedDisjoints(IntArrayList literals, IntArrayList origins) throws Unsatisfiable {
        if(monitoring) {
            monitor.print(monitorId,"Exec: Disjointness " +
                    Symboltable.toString(literals, model.symboltable)  +
                    (origins == null ? "" : " " + origins));}
        int[] clause = new int[literals.size()+2];
        clause[0] = -1;
        clause[1] = ClauseType.DISJOINT.ordinal();
        for(int i = 0; i < literals.size(); ++i) {clause[i+2] = literals.getInt(i);}
        DisjointnessClass dClass = integrateDisjointnessClause(clause,origins);
        if(dClass != null) {
            for(Consumer<DisjointnessClass> observer : disjointnessObservers) {
                observer.accept(dClass);}}
    }

    /** turns a basicClause into a disjointness class. <br>
     *  Before treating the literals, they are mapped to their representatives in the equivalence classes (if necessary) <br>
     *  The resulting clause may contain literals with mixed sign. <br>
     *  These are mapped back to the solver by calling the binaryClauseHandler. <br>
     *  The resulting clause may also contain literals p,p which imply -p. <br>
     *  These are mapped back to the solver by calling the unaryClauseHandler.
     *
     * @param clause        a disjointness clause
     * @return null or the new disjointnes class
     */
    protected Clause integrateDisjointnessClause(Clause clause) throws Unsatisfiable {
        if(monitoring) {
            monitor.print(monitorId,"Exec: disjointness clause: " + clause.toString(0,model.symboltable));}
        clause = normalizeClause(clause);
        if(clause == null) return null;
        Clause subsumer = isSubsumed(clause);
        if(subsumer != null) {
            if(monitoring) {
                monitor.print(monitorId,"new class is subsumed by " +
                        subsumer.toString(0,model.symboltable));}
            return null;}
        if(!clauses.isEmpty()) {
            resolveNormalizedClause(clause);
            if(clause.size() == 1) {return null;}}

        result = extendNormalizedClause(literals,origins);
        if(result != null) {literals = result.getKey(); origins = result.getValue();}
        removeSubsumedClasses(literals,null);
        DisjointnessClass dClass = createDisjointnessClass(literals,origins);
        if(monitoring) {
            monitor.print(monitorId,"Exec: disjointness class: " +
                    dClass.toString("",model.symboltable));}
        return clause;}


    /**
     * @param clause  a disjointness clause
     * @return null or the normalized clause
     * @throws Unsatisfiable if a contradiction is discovered.
     */
    private Clause normalizeClause(Clause clause) throws Unsatisfiable {
        // replace literals by representatives in some equivalence class
        // check for true/false literals
        // check for double literals p,p (unsatisfiable)
        // check for complementary literals p,-p (superfluous)
        ArrayList<CLiteral> cliterals = clause.cliterals;
        if(!equivalenceClasses.isEmpty()) {
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
            switch(model.status(literal)) {
                case -1: clause.remove(cliteral); --i; continue;
                case +1:
                    if(monitoring) {
                        monitor.print(monitorId,"True literal " + Symboltable.toString(literal,model.symboltable) +
                                " causes all other literals in disjointness clause " +
                                clause.toString(0, model.symboltable) + " to become false.");}
                    for(CLiteral clit : cliterals) {
                        if(clit != cliteral) {
                            model.add(-clit.literal,
                                trackReasoning ? joinIntArraysSorted(clause.origins,model.getOrigin(literal)) : null,
                                null);}}
                    return null;}

            switch(clause.contains(literal,cliteral)) {
                case +1: throw new Unsatisfiable("Disjointenss clause " + clause.toString(0,model.symboltable)+
                        " contains double literal " + Symboltable.toString(literal, model.symboltable),
                        clause.origins);
                case -1: clause.remove(cliteral); --i; continue;}}
        return (clause.size() == 1)  ? null : clause;}

    /** inserts the clause into the local data structures.
     *
     * @param clause  the clause to be inserted.
     */
    private void insertClause(Clause clause) {
        ++statistics.clauses;
        maxClauseLength = Math.max(maxClauseLength,clause.size());
        clauses.add(clause);
        for(CLiteral cliteral : clause) {literalIndex.add(cliteral);}}

    /** A new Disjointness: p,q may interact with an old disjointness p,-q, which causes -p to become true.
     * This method looks for these kind of resolution possibilities to generate unit clauses.
     * In this case p is removed from the literals
     *
     * @param clause the clause to be tested
     * @throws Unsatisfiable if an contradiction is found.
     */
    private void resolveNormalizedClause(Clause clause) throws Unsatisfiable {
        for(CLiteral cliteral1 : clause.cliterals) {
            timestamp += 2;
            int literal1 = cliteral1.literal;
            literalIndex.withIterator(literal1,(iterator -> iterator.next().clause.timestamp = timestamp));
            for(CLiteral cliteral2 : clause.cliterals) {
                int literal2 = cliteral1.literal;
                if(cliteral1 != cliteral2) {
                    CLiteral clit2 = literalIndex.findWithIterator(-literal2,(iterator -> {
                        CLiteral clit = iterator.next();
                        if(clit.clause.timestamp == timestamp) return clit;
                        return null;}));
                    if(clit2 != null) {
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
                        if(clause.size() > 1) resolveNormalizedClause(clause);
                        return;}}}}
        timestamp += 2;}

    /** tries to extend a list of disjoint literals.
     * Example: literals = p,q,r
     * Find a literal s such that p != s, q != s and r !=s holds.
     * This literal is added to p,q,r.
     *
     * @param literals a list of disjoint literals
     * @param origins the basic clause ids for the disjointness of the literals
     * @return  null (not extended) or a pair [extended literals, extended origins]
     */
    private Pair<IntArrayList,IntArrayList> extendNormalizedClause(IntArrayList literals, IntArrayList origins) {
        boolean lengthened = false;
        boolean extended = true;
        while(extended) {  // several extensions are possible
            extended = false;
            for(int literal1 : literals) {
                IntArrayList disjoints = disjointnesses.get(literal1);
                if(disjoints == null) break; // no extension possible
                for(int literal2 : disjoints) { // literal2 is a candidate for extension
                    boolean found = true;
                    for(int literal11 : literals) { // all other literals must also be disjoint to literal2
                        if(literal11 == literal1) continue;
                        if(!areDisjoint(literal11,literal2)) {found = false; break;}}
                    if(found) {
                        extended = true; lengthened = true;
                        if(monitoring) {
                            monitor.print(monitorId,Symboltable.toString(literals,model.symboltable) +
                                    " will be extended with literal " + Symboltable.toString(literal2,model.symboltable));}
                        literals.add(literal2);
                        statistics.extendedClasses++;
                        for(int literal11 : literals) {
                            if(literal11 != literal1)
                                origins = joinIntArraysSorted(getOrigins(literal11,literal2),origins);}}}}}
        return lengthened ? new Pair<>(literals,origins) : null;}

    /** checks of a new disjointness class is a subset of an already existing one.
     * In this case the new class is superfluous
     *
     * @param clause a disjointness clause
     * @return null or the subsumer class
     */
    private Clause isSubsumed(Clause clause) {
        for(Clause dClause : clauses) {
            if(clause.isSubset(dClause)) {
                statistics.forwardSubsumed++;
                return dClause;}}
        return null;}

    /** removes all disjointness classes which are subsets of the new class
     *
     * @param literals a new disjointness class
     * @param ignore a new disjointness class
     * @return true if some class has been removed.
     */
    private boolean removeSubsumedClasses(IntArrayList literals, DisjointnessClass ignore) {
        boolean[] removed = new boolean[]{false};
        disjointnessClasses.removeIf((DisjointnessClass dClass) -> {
                if(dClass != ignore && isSubset(dClass.literals,literals)) {
                    removed[0] = true;
                    statistics.backwardSubsumed++;
                    return true;}
                return false;});
        return removed[0];}


    /** A true literal p in a disjointness class p,q,r causes q,r to become false.
     * All literals which get a truth value are removed form the disjointness classes.
     *
     * @param literal a true literal
     * @param origins the basic clause ids for the truth of the literal
     * @throws Unsatisfiable if a contradiction is found.
     */
    protected void integrateTrueLiteral(int literal, IntArrayList origins) throws Unsatisfiable {
        if(monitoring) {
            monitor.print(monitorId,"Exec: unit iteral: " +
                Symboltable.toString(literal, model.symboltable));}
        for(int i = 0; i < disjointnessClasses.size(); ++i) {
            DisjointnessClass dClass = disjointnessClasses.get(i);
            if(dClass.literals.contains(literal)) { // the class
                for(int literal1 : dClass.literals) {
                    disjointnesses.remove(literal1); disjointnesses.remove(-literal1);
                    disjointnesses.forEach((lit,lits) -> {lits.rem(literal1); lits.rem(-literal1);});
                    if(literal != literal1) {
                        statistics.derivedLiterals++;
                        model.add(-literal1,joinIntArraysSorted(origins,dClass.origins),thread);}}
                disjointnessClasses.remove(i--);
                continue;}
            if(dClass.literals.contains(-literal)) { // -literal is false and can be removed.
                dClass.literals.rem(-literal);
                if(dClass.literals.size() <= 2) {    // we keep only disjointness classes with > 2 element
                    disjointnessClasses.remove(i--);}}}}

    /** creates a new DisjointnessClass and integrates it into the data structures.
     *
     * @param literals a new disjointness class
     * @param origins the list of basic clause ids causing the disjointness.
     * @return the new disjointness class
     */
    private DisjointnessClass createDisjointnessClass(IntArrayList literals, IntArrayList origins) {
        DisjointnessClass dClass = new DisjointnessClass(++counter,literals,origins);
        disjointnessClasses.add(dClass);
        for(int literal1 : literals) {
            IntArrayList disjoints = disjointnesses.get(literal1);
            if(disjoints == null) {
                disjoints = new IntArrayList();
                disjointnesses.put(literal1,disjoints);}
            for(int literal2 : literals) {
                if(literal1 != literal2) addInt(disjoints,literal2);}}
        return dClass;}

    /** replaces literal by representative in each disjointness class.
     * Double literal in the resulting class clauses Unsatisfiable exception.
     *
     * @param representative representative == literal
     * @param literal        representative == literal
     * @param origins        null or the basic clause ids for this equivalence
     * @throws Unsatisfiable if a double literal is in a disjointness class
     */
    protected void integrateEquivalence(int representative, int literal, IntArrayList origins) throws Unsatisfiable{
        if(monitoring) {
            monitor.print(monitorId,"Exec: equivalence " +
                    Symboltable.toString(representative,model.symboltable) + " = " +
                    Symboltable.toString(literal,model.symboltable));}
        IntArrayList litpos = disjointnesses.get(literal);
        IntArrayList litneg = disjointnesses.get(-literal);
        if(litpos == null && litneg == null) return; // literal is unknown
        for(int i = 0; i < disjointnessClasses.size(); ++i) {
            DisjointnessClass dClass = disjointnessClasses.get(i);
            boolean changed = dClass.replaceEquivalence(representative,literal,origins);
            if(!changed) continue;
            if(dClass.literals.size() <= 2 || (isSubsumed(dClass.literals, dClass) != null)) {
                disjointnessClasses.remove(i--); continue;}
            for(Consumer<DisjointnessClass> observer : disjointnessObservers) {
                observer.accept(dClass);
            if(removeSubsumedClasses(dClass.literals, dClass)) i = 0;}

        if(litpos != null) {
            IntArrayList reppos = disjointnesses.get(representative);
            if(reppos == null) {disjointnesses.put(representative,litpos.clone());}
            else addIntArray(reppos,litpos);}
        if(litneg != null) {
            IntArrayList repneg = disjointnesses.get(-representative);
            if(repneg == null) {disjointnesses.put(-representative,litneg.clone());}
            else addIntArray(repneg,litneg);}
        disjointnesses.remove(literal);
        disjointnesses.remove(-literal);
        disjointnesses.forEach((lit,literals) -> {
            replaceBy(literals,literal,representative);
            replaceBy(literals,-literal,-representative);});} }


    /** checks if two literals are disjoint
     *
     * @param literal1 a literal
     * @param literal2 a literal
     * @return true if the two literals are disjoint
     */
    public boolean areDisjoint(int literal1, int literal2) {
        if(literal1 == -literal2) {return true;}
        IntArrayList result = disjointnesses.get(literal1);
        if (result == null) {return false;}
        return result.contains(literal2);}


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
        for(DisjointnessClass dClass : disjointnessClasses) {
            if(dClass.literals.contains(literal1) && dClass.literals.contains(literal2)) {
                return dClass.origins;}}
        return null;}


    /** checks if there are disjointness classes
     *
     * @return true if there are no disjointness classes
     */
    public boolean isEmpty() {
        return disjointnesses.isEmpty();}

    /** turns the disjoinentesses into a string.
     *
     * @return the disjointnesses as a string
     */
    public String toString() {
        return toString("",null);}

    /** lists all disjointness classes
     *
     * @param prefix for the lines
     * @return all disjointness classes as string
     */
    public String toString(String prefix,@Nullable Symboltable symboltable) {
        if(disjointnessClasses.isEmpty()) {return "";}
        StringBuilder string = new StringBuilder();
        string.append("Disjointness Classes of Problem " + problemId + ":\n");
        int size = disjointnessClasses.size();
        for(int i = 0; i < size; ++i) {
            string.append(disjointnessClasses.get(i).toString(prefix,symboltable));
            if(i < size-1) string.append("\n");}
        return string.toString();}

    /** turns the internal data structures into a string
     *
     * @param symboltable null or a symboltable
     * @return the datastructures as string.
     */
    public String infoString(@Nullable Symboltable symboltable) {
        StringBuilder string = new StringBuilder();
        if(!disjointnessClasses.isEmpty()) {
            string.append("Disjointness Classes of Problem "+problemId+":\n");
            for(DisjointnessClass dClass : disjointnessClasses) {
                string.append(dClass.infoString(symboltable)).append("\n");}}
        if(!disjointnesses.isEmpty()) {
            string.append("\nDisjoint Literals of Problem "+problemId+":\n");
            disjointnesses.forEach((literal, disjoints) -> {
                string.append(Symboltable.toString(literal,symboltable)).append(": ");
                string.append(Symboltable.toString(disjoints,symboltable)).append("\n");});}
        if(!queue.isEmpty()) {
            string.append("Disjointnesses Queue of Problem "+problemId+":").append(Task.queueToString(queue));}
        return string.toString();}
    }