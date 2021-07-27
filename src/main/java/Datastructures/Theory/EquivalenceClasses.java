package Datastructures.Theory;

import Datastructures.Clauses.ClauseType;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Utilities.TriConsumer;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import javafx.util.Pair;

import java.util.ArrayList;
import java.util.concurrent.PriorityBlockingQueue;

import static Utilities.Utilities.joinIntArrays;

/** This class manages lists of equivalence classes of literals.
 * It operates in two modes: <br>
 * Initial phase: analysis of the input clauses. <br>
 * Search phase: parallel to the solvers, as thread.<br>
 *
 * Initial phase: <br>
 * It is called after the unit clauses are put into the model.
 * The input ist:
 *      1. the model with the initial unit clauses.<br>
 *      2. the basic equivalence clauses one by one. <br>
 * The literals in the equivalence clauses are analysed and new equivalence classes are generated.
 * New unit clauses are put into the model.
 * <br>
 * Search phase as thread: <br>
 * It gets its input from the model and from the TwoLiteral module.
 *
 *
 */

public class EquivalenceClasses extends Thread {

    /** stores the equivalence classes */
    private ArrayList<EquivalenceClass> equivalenceClasses = new ArrayList<>();

    /** the global model of true literals */
    private Model model = null;

    /** The threadId of currentThread() */
    private long threadId = 0;

    /** transfers new unit literals from the model to this class */
    private OneLiteralTransfer transferer = null;


    /** A queue of newly derived unit literals and binary equivalences.
     * The unit literals are automatically put at the beginning of the queue.
     */
    private final PriorityBlockingQueue<Pair<Object,IntArrayList>> queue =
            new PriorityBlockingQueue<>(10,(Pair<Object,IntArrayList> o1, Pair<Object,IntArrayList> o2) ->
                    (o1.getKey().getClass() == Integer.class) ? -1 : +1);

    private ArrayList<TriConsumer<Integer,Integer,IntArrayList>> equivalenceObservers = new ArrayList<>();

    /** created a new instance with the global model.
     * The model may already contain true literals.
     * The unit-literal transferer is added to the model.
     *
     * @param model
     */
    public EquivalenceClasses(Model model) {
        this.model = model;}

    /** Any solver which is interested to know about newly derived equivalences can add an observer.
     * The observer is called with (literal1, literal2, origins) as soon as new equivalences
     * literal1 == literal2 are derived.
     *
     * @param observer a TriConsumer for transferring newly derived equivalences.
     */
    public void addEquivalenceObserver(TriConsumer<Integer,Integer,IntArrayList> observer) {
        equivalenceObservers.add(observer);}

    /** Starts the instance as thread.
     * The thread waits for newly derived unit clauses (via the model) and newly derived
     * equivalences (via the TwoLiteral module) and integrates them into the equivalence classes.
     * During this process, new unit clauses and new equivalences may be derived.
     * The unit clauses are transferred to the model, and the equivalences are made available
     * by the equivalenceObservers.
     */
    public void run() {
        transferer = new OneLiteralTransfer(currentThread().getId(),
                (Integer literal, IntArrayList origins) -> {
                    synchronized(this) {queue.add(new Pair(literal,origins));}});
        model.addTransferer(transferer);
        try {
            Pair<Object, IntArrayList> object = queue.take();
            Object key = object.getKey();
            if(key.getClass() == Integer.class) {
                integrateTrueLiteral((Integer)key,object.getValue());}
            else {
                Pair<Integer,Integer> equivalence = (Pair<Integer,Integer>)key;
                addEquivalence(equivalence.getKey(),equivalence.getValue(),object.getValue());}}
        catch(InterruptedException ex) {}
        catch(Unsatisfiable uns) {}
    }

    /** This method is to be called by the TwoLiteral module to announce a newly derived equivalence
     * literal1 = literal2.
     * The equivalences is put into the queue.
     *
     * @param literal1 a literal
     * @param literal2 an equivalent literal
     * @param origins  the list of basic clause Ids used to derive the equivalence.
     */
    public void addDerivedEquivalence(int literal1, int literal2, IntArrayList origins) {
        queue.add(new Pair(new Pair(literal1,literal2),origins));}

    /** adds a basic equivalence clause to the equivalence classes.
     * All literals are replaced by the representatives of already existing equivalence classes.
     * Double occurrences are ignored.<br>
     * A contradiction may occur, if for example p,-q is to be added to p,q.<br>
     * In this case an Unsatisfiable exception is thrown.<br>
     * If the new equivalence class overlaps with the old one, the two are joined.
     *
     * @param clause a basic equivalence clause
     * @throws Unsatisfiable if a contradictory truth value has been discovered.
     */
    public void addBasicEquivalenceClause(int[] clause) throws Unsatisfiable {
        assert clause.length > 3;
        assert ClauseType.getType(clause[1]) == ClauseType.EQUIV;
        expandTruthValue(clause); // may throw Unsatisfiable

        IntArrayList origins = new IntArrayList();
        origins.add(clause[0]);
        ArrayList<Pair<Integer,IntArrayList>> literals = new ArrayList<>();
        for(int i = 2; i < clause.length; ++i) {
            int literal = clause[i];
            int representative = getRepresentative(literal);
            boolean found = false;
            for(Pair<Integer,IntArrayList> pair : literals) {
                if(representative ==  pair.getKey()) {found = true; break;} // double occurrence
                if(representative == -pair.getKey()) {     // contradiction p = -p
                    throw new Unsatisfiable(
                            "Wenn adding new equivalence class " + clause[0] +
                            ": Found " + representative + " = " + -representative + " Origin: " +
                                    joinIntArrays(origins,pair.getValue()).toString());}}
            if(found) {continue;} // double occurrence p == p is always true
            if(representative != literal) {
                literals.add(new Pair(representative,joinIntArrays(origins,getOrigins(literal))));}
            else {literals.add(new Pair(literal,origins));}}
        if(literals.size() == 1) return;
        joinEquivalenceClass(new EquivalenceClass(literals));}

    /** checks the truth value of the literals in the basic clause.
     * If a literal is true then all literals in the clause are made true.
     * If a literal is false then all literals in the clause are made false.<br>
     *
     * @param clause a basic clause
     * @return true, if all literals got a truth value.
     * @throws Unsatisfiable if a contradictory truth value has been discovered.
     */
    protected boolean expandTruthValue(int[] clause) throws Unsatisfiable {
        for(int i = 2; i < clause.length; ++i) {
            int literal = clause[i];
            IntArrayList origins = new IntArrayList(); origins.add(clause[0]);
            switch(model.status(literal)) {
                case +1:
                    for(int j = 2; j < clause.length; ++j) {
                        if(i != j) model.add(clause[j],origins,threadId);}
                    return true;
                case -1:
                    for(int j = 2; j < clause.length; ++j) {
                        if(i != j) model.add(-clause[j],origins,threadId);}
                    return true;}}
        return false;}

    /** joins the new equivalence class to the old ones.
     *  Two special cases may occur: <br>
     *  1. old class: p = q,r <br>
     *     new class: p = s,t<br>
     *     Then s,t are added to old class<br>
     *  2. old class: 5 = 7,8
     *     new class: 3 = +-5,9,10 <br>
     *     The +-7,+-8 is added to new class and old class is removed.
     *
     * @param newClass A new equivalence class. Only the representatives may occur in old classes.
     */
    protected void joinEquivalenceClass(EquivalenceClass newClass) {
        int newRepresentative = newClass.representative;
        for(EquivalenceClass oldClass : equivalenceClasses) {
            if(newRepresentative == oldClass.representative) {
                oldClass.literals.addAll(newClass.literals);
                oldClass.origins.addAll( newClass.origins);
                return;}
            int sign = newClass.contains(oldClass.representative);
            if(sign != 0) {
                newClass.literals.addAll(oldClass.literals);
                newClass.origins.addAll( oldClass.origins);
                equivalenceClasses.remove(oldClass);
                break;}}
        equivalenceClasses.add(newClass);}

    /** A true literal causes all other equivalent literals to become true.
     *
     * @param literal a true literal
     * @param origins the baisc clause ids causing the literal to become true.
     * @throws Unsatisfiable if a contradiction occurs.
     */
    protected void integrateTrueLiteral(int literal, IntArrayList origins) throws Unsatisfiable {
        for(EquivalenceClass eqClass : equivalenceClasses) {
            int sign = eqClass.contains(literal);
            if(sign != 0) {
                model.add(sign* eqClass.representative,origins,threadId);
                for(int i = 0; i < eqClass.literals.size(); ++i) {
                        model.add(sign*eqClass.literals.getInt(i),
                                joinIntArrays(origins,eqClass.origins.get(i)),threadId);}}
                equivalenceClasses.remove(eqClass);
                return;}}


    /** add the equivalence literal1 = literal2 to the equivalence classes, either to an existing one or a new one is created.
     * Adding an equivalence p = -q to a class p = q causes a contradiction to be reported.
     *
     * @param literal1
     * @param literal2
     * @param origins  the indices of the basic clauses causes this equivalence.
     * @throws Unsatisfiable, if a contradiction occurs
     */
    public synchronized void addEquivalence(int literal1, int literal2, IntArrayList origins) throws Unsatisfiable{
        int status = model.status(literal1);
        if(status != 0) {model.add(status*literal2,origins,threadId); return;}
        status = model.status(literal2);
        if(status != 0) {model.add(status*literal1,origins,threadId); return;}
        if(equivalenceClasses.isEmpty()) {
            equivalenceClasses.add(new EquivalenceClass(literal1,literal2,origins));
            return;}
        int lit1 = getRepresentative(literal1);
        int lit2 = getRepresentative(literal2);
        if(lit1 == lit2) return;
        if(lit1 == -lit2) {
            origins = joinIntArrays(joinIntArrays(origins,getOrigins(literal1)),getOrigins((literal2)));
            throw new Unsatisfiable(
                    "Wenn adding new equivalence class " + literal1 + " = " + literal2 +
                            ": Found " + lit1 + " = " + lit2 + ", Origin: " + origins.toString());}

        for(EquivalenceClass eqClass : equivalenceClasses) {
            switch(eqClass.contains(literal1)) {
                case 1:  eqClass.addEquivalence(literal2,origins);  return;
                case -1: eqClass.addEquivalence(-literal2,origins); return;}
            switch(eqClass.contains(literal2)) {
                case 1:  eqClass.addEquivalence(literal1,origins);  return;
                case -1: eqClass.addEquivalence(-literal1,origins); return;}}
        equivalenceClasses.add(new EquivalenceClass(literal1,literal2,origins));}

    /** maps literals to their representative in the equivalence class.
     *
     * @param literal a literal
     * @return the literal or the representative of the literal's equivalence class.
     */
    public synchronized int getRepresentative(int literal) {
        if(equivalenceClasses == null) {return literal;}
        for(EquivalenceClass eqClass : equivalenceClasses) {
            int representative = eqClass.getRepresentative(literal);
            if(representative != literal) {return representative;}}
        return literal;}

    /** maps a literal to the origins of the equivalence with the literal with its representative
     *
     * @param literal
     * @return null or the indices of the basic clauses causing this equivalence.
     */
    public synchronized IntArrayList getOrigins(int literal) {
        if(equivalenceClasses == null) {return null;}
        for(EquivalenceClass eqClass : equivalenceClasses) {
            IntArrayList origins = eqClass.getOrigins(literal);
            if(origins != null) {return origins;}}
        return null;}

    /** turns the equivalence classes into a string "literal1 = literal2 = ... = representative\n..."
     *
     * @return a string representation of the equivalence classes.
     */
    public String toString() {return toString(null);}

    /** turns the equivalence classes into a string "literal1 = literal2 = ... = representative\n..."
     *
     * @param symboltable or null
     * @return a string representation of the equivalence classes.
     */
    public String toString(Symboltable symboltable) {
        StringBuilder string = new StringBuilder();
        for(EquivalenceClass eqClass : equivalenceClasses) {
            string.append(eqClass.toString(symboltable)).append("\n");}
        return string.toString();}

    /** turns the equivalence classes into a string "literal1[origins] = literal2[origins] = ... = representative"
     *
     * @return a string representation of the equivalence class with the origins.
     */
    public String infoString() {return infoString(null);}


    /** turns the equivalence classes into a string "literal1[origins] = literal2[origins] = ... = representative"
     *
     * @param symboltable or null
     * @return a string representation of the equivalence class with the origins.
     */
    public String infoString(Symboltable symboltable) {
        StringBuilder string = new StringBuilder();
        for(EquivalenceClass eqClass : equivalenceClasses) {
            string.append(eqClass.infoString(symboltable)).append("\n");}
        return string.toString();}

    public ArrayList<TriConsumer<Integer, Integer, IntArrayList>> getEquivalenceObservers() {
        return equivalenceObservers;
    }
}
