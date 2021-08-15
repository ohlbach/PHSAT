package Datastructures.Theory;

import Datastructures.Clauses.BasicClauseList;
import Datastructures.Clauses.ClauseType;
import Datastructures.Results.Inconsistency;
import Datastructures.Results.Result;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Management.Monitor;
import Utilities.TriConsumer;
import com.sun.istack.internal.Nullable;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import javafx.util.Pair;

import java.util.ArrayList;
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

    /** The final result: null or Unsatisfiable */
    public Result result = null;

    /** for collecting statistics */
    public EquivalenceStatistics statistics;

    /** The id of the current problem to be solved */
    private String problemId;

    /** stores the equivalence classes */
    private ArrayList<EquivalenceClass> equivalenceClasses = new ArrayList<>();

    /** the global model of true literals */
    private Model model = null;

    /** The threadId of currentThread() */
    private Thread thread = null;

    /** for logging the actions of this class */
    private Monitor monitor;

    /** indicates monitoring is on */
    private boolean monitoring = false;

    /** for distinguishing the monitoring areas */
    private String monitorId = null;

    private int counter = 0;

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
        this.monitor = monitor;
        if(monitor != null) {
            monitoring = true;
            monitorId = problemId+"-EQUIV";
            monitor.addThread(monitorId,"EquivalenceClasses");}}

    /** Any solver which is interested to know about newly derived equivalences can add an observer.
     * The observer is called with (literal1, literal2, origins) as soon as new equivalences
     * literal1 == literal2 are derived.
     *
     * @param observer a TriConsumer for transferring newly derived equivalences.
     */
    public void addEquivalenceObserver(TriConsumer<Integer,Integer,IntArrayList> observer) {
        equivalenceObservers.add(observer);}


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
        if(monitoring) {
            monitor.print(monitorId,"In:   Equivalence " +
                    Symboltable.toString(literal1, model.symboltable) + " = " +
                    Symboltable.toString(literal2, model.symboltable) +
                    (origins == null ? "" : " " + origins.toString()));}
        queue.add(new Pair(new Pair(literal1,literal2),origins));}


    /** Starts the instance in a thread.
     * The thread waits for newly derived unit clauses (via the model) and newly derived
     * equivalences (via the TwoLiteral module) and integrates them into the equivalence classes.
     * During this process, new unit clauses and new equivalences may be derived.
     * The unit clauses are transferred to the model, and the equivalences are made available
     * by the equivalenceObservers.
     * <br>
     * The loop can only be stopped by an interrupt or a contradiction.
     * <br>
     * The result is stored into the variable result.
     */
    public void run() {
        thread = Thread.currentThread();
        model.addObserver(thread,
                (Integer literal, IntArrayList origins) ->
                {if(monitoring) {
                    monitor.print(monitorId,"In:   Unit literal " +
                            Symboltable.toString(literal,model.symboltable) +
                            (origins == null ? "" : " " + origins.toString()));}
                    queue.add(new Pair(literal,origins));});
        while(!Thread.interrupted()) {
            try {
                if(monitoring) {monitor.print(monitorId,"Queue is waiting");}
                Pair<Object, IntArrayList> object = queue.take(); // waits if the queue is empty
                Object key = object.getKey();
                if(key.getClass() == Integer.class) {
                    int literal = (Integer)key;
                    IntArrayList originals = (IntArrayList)object.getValue();
                    integrateTrueLiteral(literal,originals);}
                else {
                    Pair<Integer,Integer> equivalence = (Pair<Integer,Integer>)key;
                    int literal1 = equivalence.getKey(); int literal2 = equivalence.getValue();
                    IntArrayList originals = (IntArrayList)object.getValue();
                    addEquivalence(literal1,literal2,originals);}
                if(monitoring) {
                    monitor.print(monitorId,"Current equivalences:\n" + toString("            ",model.symboltable));}
            }
            catch(InterruptedException ex) {return;}
            catch(Unsatisfiable unsatisfiable) {result = unsatisfiable;}}}


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

        if(monitoring) {
            monitor.print(monitorId,"New equivalence clause: " +
                    BasicClauseList.clauseToString(0,clause,model.symboltable));}

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
            if(representative != literal) origins = joinIntArraysSorted(origins,getOrigins(literal));
            literals.add(representative);}
        if(literals.size() <= 1) {
            if(monitoring) {
                monitor.print(monitorId,"Equivalence clause " + clause[0] + " shrank to '" +
                Symboltable.toString(literals,model.symboltable) + "' and will be ignored.");}
            return; }
        EquivalenceClass eqClass = new EquivalenceClass(++counter, literals,origins);
        eqClass = joinEquivalenceClass(eqClass);
        if(monitoring) {
            monitor.print(monitorId,"Equivalence class " + eqClass.infoString(model.symboltable));}
    }

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
                        if(i != j) addToModel(clause[j],origins);}
                    return true;
                case -1:
                    for(int j = 2; j < clause.length; ++j) {
                        if(i != j) addToModel(-clause[j],origins);}
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
     * @return either the new class or an extended old one.
     */
    protected EquivalenceClass joinEquivalenceClass(EquivalenceClass newClass) {
        int newRepresentative = newClass.representative;
        for(EquivalenceClass oldClass : equivalenceClasses) {
            if(newRepresentative == oldClass.representative) {
                if(monitoring) {
                    monitor.print(monitorId,"merging " +
                            newClass.toString("", model.symboltable) + " into " +
                            oldClass.toString("", model.symboltable));}
                oldClass.literals = joinIntArrays(oldClass.literals,newClass.literals);
                oldClass.origins  = joinIntArraysSorted(oldClass.origins,newClass.origins);
                return oldClass;}
            int sign = newClass.contains(oldClass.representative);
            if(sign != 0) {
                if(monitoring) {
                    monitor.print(monitorId,"merging " +
                            oldClass.toString("", model.symboltable) + " into " +
                            newClass.toString("", model.symboltable));}
                for(int literal : oldClass.literals) {
                    newClass.literals = addInt(newClass.literals,sign*literal);}
                newClass.origins = joinIntArraysSorted(oldClass.origins,newClass.origins);
                equivalenceClasses.remove(oldClass);
                break;}}
        equivalenceClasses.add(newClass);
        return newClass;}

    /** A true literal causes all other equivalent literals to become true.
     *
     * @param literal a true literal
     * @param origins the baisc clause ids causing the literal to become true.
     * @throws Unsatisfiable if a contradiction occurs.
     */
    protected void integrateTrueLiteral(int literal, IntArrayList origins) throws Unsatisfiable {
        if(monitoring) {
            monitor.print(monitorId,"Exec: Unit literal " +
                    Symboltable.toString(literal, model.symboltable) +
                    (origins == null ? "" : " " + origins.toString()));}
        for(EquivalenceClass eqClass : equivalenceClasses) {
            int sign = eqClass.contains(literal);
            if(sign != 0) {
                statistics.trueLiterals++;
                if(monitoring) {
                    monitor.print(monitorId,"Exec: Literal " +
                            Symboltable.toString(literal, model.symboltable) + " -> " +
                            eqClass.toString("",model.symboltable));}
                origins = joinIntArraysSorted(eqClass.origins,origins);
                addToModel(sign* eqClass.representative,origins);
                for(int lit: eqClass.literals) {
                    lit *= sign;
                    if(lit != literal) addToModel(lit,origins);}
                equivalenceClasses.remove(eqClass);
                return;}}}

    /** adds the literal to the model and calls the monitor.
     *
     * @param literal  a true literal
     * @param origins  the basic clause ids for the truth
     * @throws Unsatisfiable if a contradiction is found
     */
    private void addToModel(int literal, IntArrayList origins) throws Unsatisfiable {
        if(monitoring) {
            monitor.print(monitorId,"Exec: Derived true Literal " +
                    Symboltable.toString(literal, model.symboltable) +
                    (origins == null ? " " : " " + origins.toString()));}
        model.add(literal,origins,thread);}

    /** add the equivalence literal1 = literal2 to the equivalence classes, either to an existing one or a new one is created.
     * Adding an equivalence p = -q to a class p = q causes a contradiction to be reported.
     *
     * @param literal1
     * @param literal2
     * @param origins  the indices of the basic clauses causes this equivalence.
     * @throws Unsatisfiable, if a contradiction occurs
     */
    protected synchronized void addEquivalence(int literal1, int literal2, IntArrayList origins) throws Unsatisfiable{
        if(monitoring) {
            monitor.print(monitorId,"Exec: Equivalence " +
                    Symboltable.toString(literal1,model.symboltable) + " = " +
                    Symboltable.toString(literal2,model.symboltable) +
                            (origins == null ? " " : " " + origins.toString()));}
        statistics.derivedClasses++;
        int status = model.status(literal1);
        if(status != 0) {model.add(status*literal2,origins,thread); return;}
        status = model.status(literal2);
        if(status != 0) {model.add(status*literal1,origins,thread); return;}

        EquivalenceClass eqClass1 = getEquivalenceClass(literal1);
        EquivalenceClass eqClass2 = getEquivalenceClass(literal2);
        int rep = 0; int lit = 0;
        int sign = 0;
        if(eqClass1 == null) {
            if(eqClass2 == null) {
                IntArrayList literals = new IntArrayList(2);
                literals.add(literal1); literals.add(literal2);
                EquivalenceClass newClass = new EquivalenceClass(++counter,literals,origins);
                equivalenceClasses.add(newClass);
                rep = newClass.representative; lit = newClass.literals.getInt(0);}
            else {  // eqClass2 != null
                sign = eqClass2.contains(literal2);
                rep = eqClass2.representative; lit = sign*literal1;
                if(monitoring) {
                    monitor.print(monitorId,"Adding literal " +
                            Symboltable.toString(sign*literal1,model.symboltable) + " to " +
                            eqClass2.toString("",model.symboltable));}
                statistics.extendedClasses++;
                eqClass2.addLiteral(sign*literal1,origins);}}
        else { // eqClass1 != null
            sign = eqClass1.contains(literal1);
            if(eqClass2 == null) {
                rep = eqClass1.representative; lit = sign*literal2;
                if(monitoring) {
                    monitor.print(monitorId,"Adding literal " +
                            Symboltable.toString(sign*literal2,model.symboltable) + " to " +
                            eqClass1.toString("",model.symboltable));}
                statistics.extendedClasses++;
                eqClass1.addLiteral(sign*literal2,origins);}
            else {  // eqClass2 != null
                sign *= eqClass2.contains(literal2);
                rep = eqClass1.representative; lit = eqClass2.representative;
                if(lit < rep) {int dummy = lit; lit = rep; rep = dummy;}
                lit *= sign;
                if(monitoring) {
                    monitor.print(monitorId,"Joining the two equivalence classes " +
                            eqClass1.toString(" ",model.symboltable) + " and " +
                            eqClass2.toString(" ",model.symboltable) +
                            (origins == null ? "" : " " + origins.toString()));}
                eqClass1.addEquivalenceClass(eqClass2,sign,origins);
                statistics.joinedClasses++;
                equivalenceClasses.remove(eqClass2);}}
        for(TriConsumer<Integer,Integer,IntArrayList> observer : equivalenceObservers){
            observer.accept(rep,lit,origins);}}

    /** returns null or the equivalence class containing the literal
     *
     * @param literal any literal
     * @return null or the equivalence class containing the literal
     */
    public synchronized EquivalenceClass getEquivalenceClass(int literal) {
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

    /** If a literal in an equivalence class is true, then all other literals are also made true
     *
     * @return an Inconsistency if the resulting model becomes inconsistent (should never happen)
     */
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
                                "Equivalence class: " + eqClass.toString("",model.symboltable) +
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
                    model.addImmediately(sign*literal,joinIntArraysSorted(origins,eqClass.origins));}}}
        return null;}

    /** checks if there is no equivalence class
     *
     * @return true if there is no equivalence class
     */
    public synchronized boolean isEmpty() {
        return equivalenceClasses.isEmpty();}

    /** turns the equivalence classes into a string "literal1 = literal2 = ... = representative\n..."
     *
     * @return a string representation of the equivalence classes.
     */
    public String toString() {return toString("",null);}

    /** turns the equivalence classes into a string "literal1 = literal2 = ... = representative\n..."
     *
     * @param prefix a prefix for the strings
     * @param symboltable or null
     * @return a string representation of the equivalence classes.
     */
    public String toString(String prefix, @Nullable Symboltable symboltable) {
        StringBuilder string = new StringBuilder();
        int size = equivalenceClasses.size();
        for(int i = 0; i < size; ++i) {
            string.append(equivalenceClasses.get(i).toString(prefix,symboltable));
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
