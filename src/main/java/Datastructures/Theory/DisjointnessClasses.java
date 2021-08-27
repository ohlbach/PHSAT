package Datastructures.Theory;

import Datastructures.Clauses.BasicClauseList;
import Datastructures.Clauses.ClauseType;
import Datastructures.Results.Result;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Management.Monitor;
import Management.ProblemSupervisor;
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
    private ProblemSupervisor problemSupervisor;

    /** for enumerating the classes */
    private int counter = 0;

    /** the final result of this thread */
    public Result result = null;

    /** the statistics of this thread */
    public DisjointnessStatistics statistics;

    /** activates monitoring */
    private boolean monitoring = false;

    /** for logging the actions of this class */
    private final Monitor monitor;

    /** for distinguishing the monitoring areas */
    private final String monitorId;

    /** The global (usually partial) model */
    private final Model model;

    /** The equivalence classes thread */
    private final EquivalenceClasses equivalenceClasses;

    /** maps literals to disjoint literals */
    private final HashMap<Integer, IntArrayList> disjointnesses = new HashMap<>();

    /** The list of disjointness classes */
    private final ArrayList<DisjointnessClass> disjointnessClasses = new ArrayList<>();

    private final ArrayList<Consumer<DisjointnessClass>> disjointnessObservers = new ArrayList<>();


    /** The cuurent thread */
    public Thread thread;

    public Thread supervisorThread;


    /** A queue of newly derived unit literals, newly derived binary disjointnesses and basic disjointness clauses
     * The unit literals are automatically put at the beginning of the queue.
     */
    private final PriorityBlockingQueue<Pair<Object,IntArrayList>> queue =
            new PriorityBlockingQueue<>(10, Comparator.comparingInt((Pair<Object, IntArrayList> o) -> getPriority(o.getKey().getClass())));


    /** gets the priority for the objects in the queue.
     *
     * @param clazz the class of the objects in the queue.
     * @return the priority of the objects in the queue.
     */
    private int getPriority(Class<?> clazz) {
        if(clazz == Integer.class)   {return 0;}    // true literal
        if(clazz == int[].class)     {return 1;}    // basic clause
        if(clazz == ArrayList.class) {return 2;}    // Triple p,q,r
        return 3;}                                  // Equivalence class


    /** creates a new instance
     *
     * @param problemSupervisor the problem supervisor
     */
    public DisjointnessClasses(ProblemSupervisor problemSupervisor) {
        this.problemSupervisor = problemSupervisor;
        model = problemSupervisor.model;
        equivalenceClasses = problemSupervisor.equivalenceClasses;
        supervisorThread = problemSupervisor.supervisorThread;
        statistics = new DisjointnessStatistics(problemSupervisor.problemId);
        monitor = problemSupervisor.globalParameters.monitor;
        monitoring = monitor != null;
        monitorId = problemSupervisor.problemId + "DISJ";
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
                if(monitoring) {monitor.print(monitorId,"Queue is waiting");}
                Pair<Object, IntArrayList> object = queue.take();
                Object key = object.getKey();
                Class<?> cl = key.getClass();
                IntArrayList origins = object.getValue();
                if(cl == Integer.class) {integrateTrueLiteral((Integer)key,origins);
                    continue;}
                if(cl == int[].class) {integrateDisjointnessClause((int[])key,null); continue;}
                if(cl == IntArrayList.class) {integrateDerivedDisjoints((IntArrayList)key,origins); continue;}
                if(cl == Pair.class) {
                    integrateEquivalence((Integer)((Pair<?, ?>) key).getKey(), (Integer)((Pair<?, ?>) key).getValue(), origins);
                    continue;}
                if(monitoring) {
                    monitor.print(monitorId,"Current disjoinentesses:\n" +
                            toString("            ",model.symboltable));}}
            catch(InterruptedException ex) {return;}
            catch(Unsatisfiable unsatisfiable) {
                problemSupervisor.setResult(result,"Disjointnesses");}}}

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
        statistics.basicClauses++;
        queue.add(new Pair<>(basicClause,IntArrayList.wrap(new int[]{basicClause[0]})));}

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
        queue.add(new Pair<>(literal,origins));}

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
        queue.add(new Pair<>(literals,origins));}

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
        queue.add(new Pair<>(new Pair<>(representative,literal),origins));}

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
     * @param basicClause        [clause-problemId,typenumber,literal1,...]
     * @return null or the new disjointnes class
     */
    protected DisjointnessClass integrateDisjointnessClause(int[] basicClause,IntArrayList origin) throws Unsatisfiable {
        if(monitoring && basicClause[0] >= 0) {
            monitor.print(monitorId,"Exec: disjointness clause: " +
                    BasicClauseList.clauseToString(0,basicClause,model.symboltable));}
        Pair<IntArrayList,IntArrayList>  result = normalizeClause(basicClause,origin); // [IntArrayList of literals, ArrayList[IntArrayList] of origins]
        if(result == null) return null;
        IntArrayList literals = result.getKey();
        DisjointnessClass subsumer = isSubsumed(literals,null);
        if(subsumer != null) {
            if(monitoring) {
                monitor.print(monitorId,"new class is subsumed by " +
                        subsumer.toString("",model.symboltable));}
            return null;}
        IntArrayList origins = result.getValue();
        if(!resolveNormalizedClause(literals,origins)) return null;
        result = extendNormalizedClause(literals,origins);
        if(result != null) {literals = result.getKey(); origins = result.getValue();}
        removeSubsumedClasses(literals,null);
        DisjointnessClass dClass = createDisjointnessClass(literals,origins);
        if(monitoring) {
            monitor.print(monitorId,"Exec: disjointness class: " +
                    dClass.toString("",model.symboltable));}
        return dClass;}


    /**
     * @param basicClause  a disjointness clause
     * @return             null or a pair [literals,origins]
     * @throws             Unsatisfiable if a contradiction is discovered.
     */
    private Pair<IntArrayList,IntArrayList> normalizeClause(int[] basicClause, IntArrayList origin) throws Unsatisfiable {
        if(origin == null && basicClause[0] >= 0) {
            origin = new IntArrayList();
            origin.add(basicClause[0]);}
        IntArrayList literals = new IntArrayList();
        int trueLiteral = 0;
        // replace literals by representatives in some equivalence class
        // check for true/false literals
        // check for double literals p,p (unsatisfiable)
        // check for complementary literals p,-p (superfluous)
        for(int i = 2; i < basicClause.length; ++i) {
            int literal = basicClause[i];
            int representative = equivalenceClasses.getRepresentative(literal);
            if(model.isFalse(representative)) continue;
            if(literals.contains(-representative)) {statistics.tautologies++; continue;}

            if(literals.contains(representative)) {
                literals.add(representative);
                throw new Unsatisfiable("Disjointness clause " +
                        BasicClauseList.clauseToString(0,basicClause,model.symboltable) +
                        " normalized to " + Symboltable.toString(literals, model.symboltable) +
                        " contains double literal " + Symboltable.toString(representative,model.symboltable),
                        origin);}
            if(model.isTrue(representative)) {trueLiteral = representative; continue;}
            literals.add(representative);
            if(literal != representative)
                origin = joinIntArraysSorted(equivalenceClasses.getOrigins(literal), origin);}

        if(literals.size() < 2) {return null;}
        if(trueLiteral == 0) return new Pair<>(literals,origin);
        for(int i = 0; i < literals.size(); ++i) {
            int literal = literals.getInt(i);
            if(literal == trueLiteral) continue;
            model.add(-literal,origin,thread);}
        return null;}

    /** A new Disjointness: p,q may interact with an old disjointness p,-q, which causes -p to become true.
     * This method looks for these kind of resolution possibilities to generate unit clauses.
     * In this case p is removed from the literals and from its origins.
     *
     * @param literals the disjoint literals
     * @param origins  the corresponding origin
     * @return true if there are still disjointnesses left.
     * @throws Unsatisfiable if an contradiction is found.
     */
    private boolean resolveNormalizedClause(IntArrayList literals, IntArrayList origins) throws Unsatisfiable {
        for(int i = 0; i < literals.size();++i) { // we check for  p,q and p,-q which means -p
            int literal1 = literals.getInt(i);
            IntArrayList disjoints = disjointnesses.get(literal1);
            if(disjoints == null) continue;
            for(int j = 0; j < literals.size();++j) {
                int literal2 = literals.getInt(j);
                if(literal1 != literal2 && disjoints.contains(-literal2)) {
                    IntArrayList origin = joinIntArraysSorted(getOrigins(literal1,-literal2),origins);
                    if(monitoring) {
                        monitor.print(monitorId,"Found " +
                                Symboltable.toString(literal1, model.symboltable) + " != " +
                                Symboltable.toString(literal2, model.symboltable) + " and " +
                                Symboltable.toString(literal1, model.symboltable) + " != " +
                                Symboltable.toString(-literal2, model.symboltable) + ":" +
                                (origins == null ? " " : " " + origins) +
                                " which yields " + Symboltable.toString(-literal1, model.symboltable));}
                    model.add(-literal1,origin,null); // put it into the own queue as well
                    statistics.resolutions++;
                    literals.removeInt(i);
                    --i;
                    break;}}}
        return literals.size() > 1;}

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
     * @param literals a list of disjoint literals
     * @param ignore a class to be ignored
     * @return null or the subsumer class
     */
    private DisjointnessClass isSubsumed(IntArrayList literals, DisjointnessClass ignore) {
        for(DisjointnessClass dClass : disjointnessClasses) {
            if(dClass != ignore &&  isSubset(literals,dClass.literals)) {
                statistics.forwardSubsumed++;
                return dClass;}}
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
        string.append("Disjointness Classes:\n");
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
        if(disjointnessClasses.isEmpty()) {return "";}
        StringBuilder string = new StringBuilder();
        string.append("Disjointness Classes:\n");
        for(DisjointnessClass dClass : disjointnessClasses) {
            string.append(dClass.infoString(symboltable)).append("\n");}
        string.append("\nDisjoint Literals;\n");
        disjointnesses.forEach((literal, disjoints) -> {
            string.append(Symboltable.toString(literal,symboltable)).append(": ");
            string.append(Symboltable.toString(disjoints,symboltable)).append("\n");});
            return string.toString();}
    }