package Datastructures.Theory;

import Datastructures.Clauses.ClauseType;
import Datastructures.Results.Result;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import com.sun.istack.internal.Nullable;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import java.util.*;
import java.util.concurrent.PriorityBlockingQueue;

import javafx.util.Pair;

import static Utilities.Utilities.*;

/** A disjointness class is a set of literals which are pairwise contradictory.
 * Such a class may come from the input data, or be derived from binary clauses.
 * The class works as Thread in parallel to the other solvers.
 *
 * Created by Ohlbach on 21.09.2018.
 */
public class DisjointnessClasses {

    /** The global (usually partial) model */
    private Model model;

    /** The equivalence classes thread */
    private EquivalenceClasses equivalenceClasses;

    /** maps literals to disjoint literals */
    private HashMap<Integer, IntArrayList> disjointnesses = new HashMap<>();

    /** The list of disjointness classes */
    private ArrayList<DisjointnessClass> disjointnessClasses = new ArrayList<>();

    public DisjointnessStatistics statistics = null;

    /** A queue of newly derived unit literals, newly derived binary disjointnesses and basic disjointness clauses
     * The unit literals are automatically put at the beginning of the queue.
     */
    private final PriorityBlockingQueue<Pair<Object,IntArrayList>> queue =
            new PriorityBlockingQueue<>(10,(Pair<Object,IntArrayList> o1, Pair<Object,IntArrayList> o2) -> {
                Class cl = o1.getKey().getClass();
                if(cl == Integer.class) return 0;        // true literal
                if(cl == IntArrayList.class) {return 1;} // basic clause
                if(cl == ArrayList.class) {return 2;}    // Triple p,q,r
                return 3;});                             // Equivalence class


    /** The cuurent thread */
    public Thread thread;

    /** fetches true literals from the model */
    public OneLiteralTransfer transferer;

    /** creates a new instance
     *
     * @param model               the global (partial) model
     * @param equivalenceClasses  the equivalence classes (thread)
     */
    public DisjointnessClasses(Model model, EquivalenceClasses equivalenceClasses, String problemId) {
        this.model = model;
        this.equivalenceClasses = equivalenceClasses;
        statistics = new DisjointnessStatistics(problemId);}

    /** This method is started as thread.
     * It reads and executes tasks from the queue
     * It can only be stopped by an interrupt or when a contradiction is found.
     *
     * @return null or Unsatisfiable
     */
    public Result run() {
        thread = Thread.currentThread();
        transferer = new OneLiteralTransfer(thread,
                (Integer literal, IntArrayList origins) -> addTrueLiteral(literal,origins));
        model.addTransferer(transferer);

        equivalenceClasses.addEquivalenceObserver((representative, literal, origins) ->
                addEquivalenceClass (representative,literal,origins));

        while(!Thread.interrupted()) {
            try {
                Pair<Object, IntArrayList> object = queue.take();
                Object key = object.getKey();
                Class cl = key.getClass();
                IntArrayList origins = object.getValue();
                if(cl == Integer.class) {integrateTrueLiteral((Integer)key,origins);
                    continue;}
                if(cl == IntArrayList.class) {integrateDisjointnessClause((int[])key,null); continue;}
                if(cl == ArrayList.class) {integrateDisjointnessClass((ArrayList)key,origins); continue;}
                if(cl == Pair.class) {
                    integrateEquivalence((int)((Pair<?, ?>) key).getKey(), (int)((Pair<?, ?>) key).getValue(), origins);
                    continue;}}
            catch(InterruptedException ex) {return null;}
            catch(Unsatisfiable unsatisfiable) {return unsatisfiable;}}
        return null;}

    /** Adds a basic disjointness clause to the queue.
     *
     * @param basicClause a basic disjointness clause with at least 3 disjoint literals
     */
    public synchronized void addDisjointnessClause(int[] basicClause) {
        assert basicClause.length > 4;
        assert basicClause[1] == ClauseType.DISJOINT.ordinal() || basicClause[1] == ClauseType.XOR.ordinal();
        statistics.basicClauses++;
        queue.add(new Pair(basicClause,null)); }

    /** adds a true literal to the queue
     *
     * @param literal a true literal
     * @param origins the basic clause ids for the true literal
     */
    public synchronized void addTrueLiteral(int literal,IntArrayList origins) {
        statistics.trueLiterals++;
        queue.add(new Pair(literal,origins));}

    /** adds a new derived disjointness p,q,r to the queue
     *
     * @param literal1
     * @param literal2
     * @param literal3
     * @param origins the basic clause ids for the disjointness
     */
    public synchronized void addNewDisjoints(int literal1, int literal2, int literal3, IntArrayList origins) {
        statistics.derivedDisjointesses++;
        ArrayList<Integer> list = new ArrayList<>(3);
        list.add(literal1); list.add(literal2); list.add(literal3);
        queue.add(new Pair(list,origins));}

      /** adds a new equivalence class representative == literal to the queue
     *
     * @param representative the representative of the class
     * @param literal        the literal which equals the representative
     * @param origins        the ids of the basic clauses causing the equivalence
     */

    public synchronized void addEquivalenceClass(int representative, int literal, IntArrayList origins) {
        statistics.equivalences++;
        queue.add(new Pair(new Pair(representative,literal),origins));}

    /** integrates a new disjointness submitted from some other part of the program.
     * The method simulates a basic clause.
     *
     * @param literals a list of disjoint literals
     * @param origin   the basic clause ids causing the disjointness
     * @throws Unsatisfiable if a contradiction is found.
     */
    public void integrateDisjointnessClass(ArrayList<Integer> literals, IntArrayList origin) throws Unsatisfiable {
        int[] clause = new int[literals.size()+2];
        clause[0] = 0;
        clause[1] = 0;
        for(int i = 0; i < literals.size(); ++i) {clause[i+2] = literals.get(i);}
        integrateDisjointnessClause(clause,origin);}

    /** turns a basicClause into a disjointness class. <br>
     *  Before treating the literals, they are mapped to their representatives in the equivalence classes (if necessary) <br>
     *  The resulting clause may contain literals with mixed sign. <br>
     *  These are mapped back to the solver by calling the binaryClauseHandler. <br>
     *  The resulting clause may also contain literals p,p which imply -p. <br>
     *  These are mapped back to the solver by calling the unaryClauseHandler.
     *
     * @param basicClause        [clause-problemId,typenumber,literal1,...]
     * @return true if the clause caused a change, otherwise false.
     */
    public void integrateDisjointnessClause(int[] basicClause, IntArrayList origin) throws Unsatisfiable {
        Pair<IntArrayList,IntArrayList>  result = normalizeClause(basicClause,origin); // [IntArrayList of literals, ArrayList[IntArrayList] of origins]
        if(result == null) return;
        IntArrayList literals = result.getKey();
        if(isSubsumed(literals)) return;
        IntArrayList origins = result.getValue();
        if(!resolveNormalizedClause(literals,origins)) return;
        result = extendNormalizedClause(literals,origins);
        if(result != null) {literals = result.getKey(); origins = result.getValue();}
        removeSubsumedClasses(literals);
        createDisjointnessClass(literals,origins);
    }


    /**
     * @param basicClause  a disjointness clause
     * @return             null or a pair [literals,origins]
     * @throws             Unsatisfiable if a contradiction is discovered.
     */
    protected Pair<IntArrayList,IntArrayList> normalizeClause(int[] basicClause, IntArrayList origin) throws Unsatisfiable {
        if(origin == null) {
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
                throw new Unsatisfiable("Basic clause " + basicClause.toString() +
                        " normalized to " + literals.toString() +
                        " contains double literal" + representative,origin);}
            if(model.isTrue(representative)) {trueLiteral = representative; continue;}
            literals.add(representative);
            if(literal != representative)
                origin = joinIntArrays(equivalenceClasses.getOrigins(literal), origin);}

        if(literals.size() < 2) {return null;}
        if(trueLiteral == 0) return new Pair(literals,origin);
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
    protected boolean resolveNormalizedClause(IntArrayList literals, IntArrayList origins) throws Unsatisfiable {
        for(int i = 0; i < literals.size();++i) { // we check for  p,q and p,-q which means -p
            int literal1 = literals.getInt(i);
            IntArrayList disjoints = disjointnesses.get(literal1);
            if(disjoints == null) continue;
            for(int j = 0; j < literals.size();++j) {
                int literal2 = literals.getInt(j);
                if(literal1 != literal2 && disjoints.contains(-literal2)) {
                    IntArrayList origin = joinIntArrays(getOrigins(literal1,-literal2),origins);
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
    protected Pair<IntArrayList,IntArrayList> extendNormalizedClause(IntArrayList literals, IntArrayList origins) {
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
                        literals.add(literal2);
                        statistics.joinedClasses++;
                        for(int literal11 : literals) {
                            if(literal11 != literal1)
                                origins = joinIntArrays(getOrigins(literal11,literal2),origins);}}}}}
        return lengthened ? new Pair(literals,origins) : null;}

    /** checks of a new disjointness class is a subset of an already existing one.
     * In this case the new class is superfluous
     *
     * @param literals a list of disjoint literals
     * @return true if literals is a subset of disjoint literals
     */
    protected boolean isSubsumed(IntArrayList literals) {
        for(DisjointnessClass disjoints : disjointnessClasses) {
            if(isSubset(literals,disjoints.literals)) {
                statistics.forwardSubsumed++;
                return true;}}
        return false;}

    /** removes all disjointness classes which are subsets of the new class
     *
     * @param literals a new disjointness class
     * @return true if some class has been removed.
     */
    protected boolean removeSubsumedClasses(IntArrayList literals) {
        boolean[] removed = new boolean[]{false};
        disjointnessClasses.removeIf((DisjointnessClass disjoint) -> {
                if(isSubset(disjoint.literals,literals)) {
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
        for(int i = 0; i < disjointnessClasses.size(); ++i) {
            DisjointnessClass dClass = disjointnessClasses.get(i);
            if(dClass.literals.contains(literal)) { // the class
                for(int literal1 : dClass.literals) {
                    disjointnesses.remove(literal1); disjointnesses.remove(-literal1);
                    disjointnesses.forEach((lit,lits) -> {lits.rem(literal1); lits.rem(-literal1);});
                    if(literal != literal1) {
                        statistics.derivedLiterals++;
                        model.add(-literal1,origins,thread);}}
                disjointnessClasses.remove(i--);
                continue;}
            if(dClass.literals.contains(-literal)) { // -literal is false and can be removed.
                dClass.literals.rem(-literal);
                if(dClass.literals.size() <= 2) {    // we keep only disjointness classes with > 2 element
                    disjointnessClasses.remove(i--);
                    continue;}}}}

    /** creates a new DisjointnessClass and integrates it into the data structures.
     *
     * @param literals a new disjointness class
     * @param origins the list of basic clause ids causing the disjointness.
     */
    protected void createDisjointnessClass(IntArrayList literals, IntArrayList origins) {
        DisjointnessClass dClass = new DisjointnessClass(literals,origins);
        disjointnessClasses.add(dClass);
        for(int literal1 : literals) {
            IntArrayList disjoints = disjointnesses.get(literal1);
            if(disjoints == null) {
                disjoints = new IntArrayList();
                disjointnesses.put(literal1,disjoints);}
            for(int literal2 : literals) {
                if(literal1 != literal2) addInt(literals,literal2);}}}

    /** replaces literal by representative in each disjointness class.
     * Double literal in the resulting class clauses Unsatisfiable exception.
     *
     * @param representative
     * @param literal
     * @param origins
     * @throws Unsatisfiable if a double literal is in a disjointness class
     */
    protected void integrateEquivalence(int representative, int literal, IntArrayList origins) throws Unsatisfiable{
        IntArrayList litpos = disjointnesses.get(literal);
        IntArrayList litneg = disjointnesses.get(-literal);
        if(litpos == null && litneg == null) return; // literal is unknown
        for(int i = 0; i < disjointnessClasses.size(); ++i) {
            DisjointnessClass dClass = disjointnessClasses.get(i);
            boolean changed = dClass.replaceEquivalence(representative,literal,origins);
            if(!changed) continue;
            if(dClass.literals.size() <= 2 || (isSubsumed(dClass.literals))) {
                disjointnessClasses.remove(i--); continue;}
            if(removeSubsumedClasses(dClass.literals)) {i = 0; continue;}}

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
            replaceBy(literals,-literal,-representative);});
    }
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
        return disjointnesses == null || disjointnesses.isEmpty();}

    /** lists all disjointness classes
     *
     * @return all disjointness classes as string
     */
    public String toString(@Nullable Symboltable symboltable) {
        if(disjointnessClasses.isEmpty()) {return "";}
        StringBuilder string = new StringBuilder();
        string.append("Disjointness Classes:\n");
        int size = disjointnessClasses.size();
        for(int i = 0; i < size; ++i) {
            string.append(disjointnessClasses.get(i).toString(symboltable));
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
            string.append(Symboltable.getLiteralName(literal,symboltable)).append(": ");
            string.append(Symboltable.getLiteralNames(disjoints,symboltable)).append("\n");});
            return string.toString();}
    }