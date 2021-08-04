package Datastructures.Theory;

import Datastructures.Clauses.ClauseType;
import Datastructures.Results.Inconsistency;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Management.Monitor;
import Utilities.TriConsumer;
import com.sun.istack.internal.Nullable;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import javafx.util.Pair;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.concurrent.PriorityBlockingQueue;

import static Utilities.Utilities.*;

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
 */

public class EquivalenceClasses  {

    /** stores the equivalence classes */
    private ArrayList<EquivalenceClass> equivalenceClasses = new ArrayList<>();

    /** the global model of true literals */
    private Model model = null;

    /** The threadId of currentThread() */
    private Thread thread = null;

    /** for collecting statistics */
    public EquivalenceStatistics statistics;

    /** for logging the actions of this class */
    public Monitor monitor;

    /** indicates monitoring is on */
    private boolean monitoring = false;

    /** for distinguishing the monitoring areas */
    private String monitorId = null;

    private String problemId;

    /** A queue of newly derived unit literals and binary equivalences.
     * The unit literals are automatically put at the beginning of the queue.
     */
    private final PriorityBlockingQueue<Pair<Object,IntArrayList>> queue =
            new PriorityBlockingQueue<>(10,(Pair<Object,IntArrayList> o1, Pair<Object,IntArrayList> o2) ->
                    (o1.getKey().getClass() == Integer.class) ? -1 : +1);

    private ArrayList<TriConsumer<Integer,Integer,IntArrayList>> equivalenceObservers = new ArrayList<>();

    /** created a new instance with the global model.
     * The model may already contain true literals.
     *
     * @param model
     */
    public EquivalenceClasses(Model model, String problemId, Monitor monitor) {
        this.problemId = problemId;
        thread = Thread.currentThread();
        this.model = model;
        statistics = new EquivalenceStatistics(problemId);
        if(monitor != null) {
            monitoring = true;
            monitorId = problemId+"-EQ";
            monitor.addThread(monitorId,"" +"EquivalenceClasses");}}

    /** Any solver which is interested to know about newly derived equivalences can add an observer.
     * The observer is called with (literal1, literal2, origins) as soon as new equivalences
     * literal1 == literal2 are derived.
     *
     * @param observer a TriConsumer for transferring newly derived equivalences.
     */
    public void addEquivalenceObserver(TriConsumer<Integer,Integer,IntArrayList> observer) {
        equivalenceObservers.add(observer);}

    /** Starts the instance in a thread.
     * The thread waits for newly derived unit clauses (via the model) and newly derived
     * equivalences (via the TwoLiteral module) and integrates them into the equivalence classes.
     * During this process, new unit clauses and new equivalences may be derived.
     * The unit clauses are transferred to the model, and the equivalences are made available
     * by the equivalenceObservers.
     * <br>
     * The loop can only be stopped by an interrupt or a contradiction.
     *
     * @returns Unsatisfiable as soon as a contradiction is discovered.
     */
    public Unsatisfiable run() {
        thread = Thread.currentThread();
        model.addObserver(thread,
                (Integer literal, IntArrayList origins) ->
                    queue.add(new Pair(literal,origins)));
        while(!Thread.interrupted()) {
            try {
                Pair<Object, IntArrayList> object = queue.take(); // waits if the queue is empty
                Object key = object.getKey();
                if(key.getClass() == Integer.class) {
                    integrateTrueLiteral((Integer)key,object.getValue());}
                else {
                    Pair<Integer,Integer> equivalence = (Pair<Integer,Integer>)key;
                    addEquivalence(equivalence.getKey(),equivalence.getValue(),object.getValue());}}
            catch(InterruptedException ex) {return null;}
            catch(Unsatisfiable unsatisfiable) {return unsatisfiable;}}
        return null;}

    /** This method is to be called by the TwoLiteral module to announce a newly derived equivalence
     * literal1 = literal2.
     * The equivalences is put into the queue.
     *
     * @param literal1 a literal
     * @param literal2 an equivalent literal
     * @param origins  the list of basic clause Ids used to derive the equivalence.
     */
    public void addDerivedEquivalence(int literal1, int literal2, IntArrayList origins) {
        statistics.derivedClasses++;
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
        statistics.basicClauses++;
        if(expandTruthValue(clause)) return; // may throw Unsatisfiable

        IntArrayList origins = new IntArrayList(1);
        origins.add(clause[0]);
        IntArrayList literals = new IntArrayList(clause.length-2);
        for(int i = 2; i < clause.length; ++i) {
            int literal = clause[i];
            int representative = getRepresentative(literal);
            if(literals.contains(representative)) continue; // double occurrence
            if(literals.contains(-representative)) // contradiction p = -p
                throw new Unsatisfiable(
                        "Wenn adding new equivalence class " + clause[0] +
                                ": Found " + representative + " = " + -representative,origins);
            if(representative != literal) addIntArray(origins,getOrigins(literal));
            literals.add(literal);}
        if(literals.size() == 1) return;
        joinEquivalenceClass(new EquivalenceClass(literals,origins));}

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
            IntArrayList origins = new IntArrayList(1); origins.add(clause[0]);
            switch(model.status(literal)) {
                case +1:
                    for(int j = 2; j < clause.length; ++j) {
                        if(i != j) model.add(clause[j],origins,thread);}
                    return true;
                case -1:
                    for(int j = 2; j < clause.length; ++j) {
                        if(i != j) model.add(-clause[j],origins,thread);}
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
                oldClass.literals = addIntArray(oldClass.literals,newClass.literals);
                oldClass.origins  = addIntArray(oldClass.origins,newClass.origins);
                return;}
            int sign = newClass.contains(oldClass.representative);
            if(sign != 0) {
                for(int literal : oldClass.literals) {
                    newClass.literals = addInt(newClass.literals,sign*literal);}
                newClass.origins = addIntArray(newClass.origins,oldClass.origins);
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
                if(monitoring) {
                    monitor.print(monitorId,"Applying true literal " +
                            Symboltable.toString(literal, model.symboltable) + "@" +
                            Symboltable.toString(origins,null) +
                            " to EQ class " + eqClass.toString(model.symboltable));}
                origins = addIntArray(eqClass.origins,origins);
                model.add(sign* eqClass.representative,origins,thread);
                for(int i = 0; i < eqClass.literals.size(); ++i) {
                        model.add(sign*eqClass.literals.getInt(i),origins,thread);}
                equivalenceClasses.remove(eqClass);
                return;}}}


    /** add the equivalence literal1 = literal2 to the equivalence classes, either to an existing one or a new one is created.
     * Adding an equivalence p = -q to a class p = q causes a contradiction to be reported.
     *
     * @param literal1
     * @param literal2
     * @param origins  the indices of the basic clauses causes this equivalence.
     * @throws Unsatisfiable, if a contradiction occurs
     */
    public synchronized void addEquivalence(int literal1, int literal2, IntArrayList origins) throws Unsatisfiable{
        if(monitoring) {
            monitor.print(monitorId,"Adding new equivalence " +
                    Symboltable.toString(literal1,model.symboltable) + " = " +
                    Symboltable.toString(literal2,model.symboltable) + " @ " +
                    Symboltable.toString(origins,model.symboltable));}
        statistics.derivedClasses++;
        int status = model.status(literal1);
        if(status != 0) {model.add(status*literal2,origins,thread); return;}
        status = model.status(literal2);
        if(status != 0) {model.add(status*literal1,origins,thread); return;}

        EquivalenceClass eqClass1 = getEquivalenceClass(literal1);
        EquivalenceClass eqClass2 = getEquivalenceClass(literal2);

        if(eqClass1 == null && eqClass2 == null) {
            equivalenceClasses.add(new EquivalenceClass(literal1,literal2,origins));
            return;}

        int lit1 = eqClass1 == null ? literal1 : eqClass1.representative;
        int lit2 = eqClass2 == null ? literal2 : eqClass2.representative;
        if(monitoring) {
            if(literal1 != lit1) monitor.print(monitorId,"Literal " +
                    Symboltable.toString(literal1, model.symboltable) + " replaced by " +
                    Symboltable.toString(lit1, model.symboltable));
            if(literal2 != lit2) monitor.print(monitorId,"Literal " +
                    Symboltable.toString(literal2, model.symboltable) + " replaced by " +
                    Symboltable.toString(lit2, model.symboltable));}
        if(lit1 == lit2) return; // class already known
        if(lit1 == -lit2) {
            if(eqClass1 != null) origins = joinIntArrays(origins,eqClass1.origins);
            if(eqClass2 != null) origins = joinIntArrays(origins,eqClass2.origins);
            throw new Unsatisfiable(
                    "Wenn adding new equivalence class " + literal1 + " = " + literal2 +
                            ": Found " + lit1 + " = " + lit2, origins);}




        EquivalenceClass eqvClass = new EquivalenceClass(lit1,lit2,origins);
        int representative = eqvClass.representative;
        int literal = eqvClass.literals.getInt(0);
        boolean found = false;
        for(EquivalenceClass eqClass : equivalenceClasses) {
            if(representative == eqClass.representative) {
                eqClass.literals.add(literal);
                //eqClass.origins.add(origins);
                found = true;
                break;}}
        if(!found) equivalenceClasses.add(eqvClass);
        for(TriConsumer<Integer,Integer,IntArrayList> observer : equivalenceObservers){
            observer.accept(representative,literal,origins);}}

    /** returns null or the equivalence class containing the literal
     *
     * @param literal any literal
     * @return null or the equivalence class containing the literal
     */
    public EquivalenceClass getEquivalenceClass(int literal) {
        for(EquivalenceClass eqClass : equivalenceClasses) {
            if(eqClass.contains(literal) != 0) return eqClass;}
        return null;}

    /** maps literals to their representative in the equivalence class.
     *
     * @param literal a literal
     * @return the literal or the representative of the literal's equivalence class.
     */
    public synchronized int getRepresentative(int literal) {
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
        for(EquivalenceClass eqClass : equivalenceClasses) {
            if(eqClass.contains(literal) != 0) return eqClass.origins;}
        return null;}


    public Inconsistency completeModel() {
        for(EquivalenceClass eqClass : equivalenceClasses) {
            int sign = 0;
            int lit = 0;
            IntArrayList origins = null;
            for(int literal : eqClass.literals) {
                int status = model.status(literal);
                if(status != 0) {
                    if(sign != 0 && status != sign) {
                        return new Inconsistency(problemId,
                                "Equivalence class: " + eqClass.toString(model.symboltable) +
                                ": Literal " + Symboltable.toString(lit,model.symboltable) +
                                " is " + (sign > 0 ? "true" : false) + " but literal " +
                                        Symboltable.toString(literal,model.symboltable) +
                                " is " + (status > 0 ? "true" : "false"));}
                    else {sign = status; lit = literal;
                        origins = model.getOrigin(literal);}}}
            if(sign == 0) return null;

            for(int i = 0; i < eqClass.literals.size(); ++i) {
                int literal = eqClass.literals.getInt(i);
                if(model.status(literal) == 0) {
                    model.addImmediately(sign*literal,joinIntArrays(origins,eqClass.origins));}}}
        return null;}

    /** checks if there is no equivalence class
     *
     * @return true if there is no equivalence class
     */
    public boolean isEmpty() {
        return equivalenceClasses.isEmpty();}

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
    public String toString(@Nullable Symboltable symboltable) {
        StringBuilder string = new StringBuilder();
        int size = equivalenceClasses.size();
        for(int i = 0; i < size; ++i) {
            string.append(equivalenceClasses.get(i).toString(symboltable));
            if(i < size-1) string.append("\n");}
        return string.toString();}


    /** turns the equivalence classes into a string "literal1[origins] = literal2[origins] = ... = representative"
     *
     * @param symboltable or null
     * @return a string representation of the equivalence class with the origins.
     */
    public String infoString(@Nullable Symboltable symboltable) {
        StringBuilder string = new StringBuilder();
        int size = equivalenceClasses.size();
        for(int i = 0; i < size; ++i) {
            string.append(equivalenceClasses.get(i).infoString(symboltable));
            if(i < size-1) string.append("\n");}
        return string.toString();}

    public ArrayList<TriConsumer<Integer, Integer, IntArrayList>> getEquivalenceObservers() {
        return equivalenceObservers;
    }
}
