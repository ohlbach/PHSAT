package Datastructures.Theory;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseType;
import Datastructures.Literals.CLiteral;
import Datastructures.Results.Inconsistency;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Datastructures.Task;
import Management.Monitor;
import Management.ProblemSupervisor;
import Utilities.TriConsumer;
import com.sun.istack.internal.Nullable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
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
 * <br>
 * Equivalences are stored as clauses with sorted literals
 * The first literal (with the smallest predicate) is the representative of the equivalence class. <br>
 * The representative is always positive.
 */

public class EquivalenceClasses  {
    private final ProblemSupervisor problemSupervisor;

    /** for collecting statistics */
    public EquivalenceStatistics statistics;

    /** controls the computation of the clause's origins */
    private final boolean trackReasoning;

    /** The id of the current problem to be solved */
    private final String problemId;

    /** The list of Equivalence  clauses */
    private final ArrayList<Clause> clauses = new ArrayList<>();

    /** maps literals to the literal occurrences in clauses.
     * It exploits that equvialence classes are disjoint.
     * Therefore a literal can occur in only a single equivalence clause.
     */
    private final HashMap<Integer,CLiteral> literalIndex = new HashMap<>();

    /** the global model of true literals */
    private final Model model;

    /** for logging the actions of this class */
    private final Monitor monitor;

    /** indicates monitoring is on */
    private boolean monitoring = false;

    /** for distinguishing the monitoring areas */
    private String monitorId = null;

    /** for enumerating the equivalence classes */
    private int counter = 0;

    /** These two types can occur in the task queue */
    private enum TaskType {
        TRUELITERAL, // a new true literal is obtained from the model
        EQUIVALENCE} // a new binary equivalence is found in the TwoLiteral module.

    /** gets the priority for the objects in the queue.
     *
     * @param task the task in the queue
     * @return the priority of the objects in the queue.
     */
    private int getPriority(Task<EquivalenceClasses.TaskType> task) {
        switch(task.taskType) {
            case TRUELITERAL: return Integer.MIN_VALUE + Math.abs((Integer)task.a);
            case EQUIVALENCE: return task.priority;} // this guarantees a deterministic sequence of the tasks
        return 4;}

    /** A queue of newly derived unit literals and binary equivalences.
     * The unit literals are automatically put at the beginning of the queue.
     */
    private final PriorityBlockingQueue<Task<EquivalenceClasses.TaskType>> queue =
            new PriorityBlockingQueue<>(10, Comparator.comparingInt(this::getPriority));

    /** These observers are called for new equivalences: (representative,literal,origins)
     * They are not called for the initial basic equivalence clauses.*/
    private final ArrayList<TriConsumer<Integer,Integer,IntArrayList>> observers = new ArrayList<>();

    /** creates the EquivalenceClasses instance
     *
     * @param problemSupervisor         the problem supervisor
     */
    public EquivalenceClasses(ProblemSupervisor problemSupervisor) {
        problemSupervisor.equivalenceClasses = this;
        this.problemSupervisor = problemSupervisor;
        problemId = problemSupervisor.problemId;
        model = problemSupervisor.model;
        statistics   = new EquivalenceStatistics(problemId);
        trackReasoning = problemSupervisor.globalParameters.trackReasoning;
        monitor = problemSupervisor.globalParameters.monitor;
        if(monitor != null) {
            monitoring = true;
            monitorId = problemId+"-EQV";
            monitor.addThread(monitorId,"EquivalenceClasses");}}

    /** Any solver which is interested to know about newly derived equivalences can add an observer.
     * The observer is called with (literal1, literal2, origins) as soon as new equivalences
     * literal1 == literal2 are derived.
     *
     * @param observer a TriConsumer for transferring newly derived equivalences.
     */
    public void addObserver(TriConsumer<Integer,Integer,IntArrayList> observer) {
        observers.add(observer);}




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
        model.addObserver(Thread.currentThread(),
                (Integer literal, IntArrayList origins) ->
                {if(monitoring) {
                    monitor.print(monitorId,"In:   Unit literal " +
                            Symboltable.toString(literal,model.symboltable) +
                            (origins == null ? "" : " " + origins));}
                    queue.add(new Task<>(TaskType.TRUELITERAL, origins, literal, null));});
        while(!Thread.interrupted()) {
            try {
                if(monitoring) {monitor.print(monitorId,"Queue is waiting\n" + Task.queueToString(queue));}
                Task<TaskType> task = queue.take(); // waits if the queue is empty
                switch(task.taskType){
                    case TRUELITERAL: integrateTrueLiteral((Integer)task.a,task.origins); break;
                    case EQUIVALENCE: integrateEquivalence((Integer)task.a,(Integer)task.b,task.origins); break;}
                if(monitoring) {
                    monitor.print(monitorId,"Current equivalences:\n" +
                            toString(model.symboltable));}}
            catch(InterruptedException ex) {return;}
            catch(Unsatisfiable unsatisfiable) {
                problemSupervisor.setResult(unsatisfiable,"EquivalenceClasses");
                return;}}}


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
        integrateEquivalenceClause(new Clause(++counter,clause));
    }

    private int priority = 0;

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
                    (origins == null ? "" : " " + origins));}
        Task<TaskType> task = new Task<>(TaskType.EQUIVALENCE,origins,literal1,literal2);
        task.priority = ++priority;
        queue.add(task);}


    /** integrates a clause into the equivalence classes.
     * Literals are replaced by representatives of other equivalence classes (if necessary) <br>
     * True and false literals cause all equivalent literals to become true/false <br>
     * Double literals are removed. <br>
     * Complementary literals cause Unsatisfiable to be thrown. <br>
     * Overlapping equivalences are joined.
     *
     * @param clause a new equivalence clause
     * @return null, or the old/new representative
     */
    public Integer integrateEquivalenceClause(Clause clause) throws Unsatisfiable {
        if(monitoring) {
            monitor.print(monitorId,"Exec: equivalence clause: " + clause.toString(0,model.symboltable));}
        clause = normalizeClause(clause);
        if(clause == null) return null;
        clause = joinClause(clause);
        int oldRepresentative = clause.cliterals.get(0).literal;
        sortLiterals(clause);
        insertClause(clause);
        return oldRepresentative;}


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

        if(!clauses.isEmpty()) {  // replacement of equivalent literals
            clause.replaceEquivalences(this,trackReasoning);}

        for(int i = 0; i < cliterals.size(); ++i) {
            CLiteral cliteral = cliterals.get(i);
            int literal = cliteral.literal;
            int sign = model.status(literal);
            if(sign != 0) {
                if(monitoring) {
                    monitor.print(monitorId, "All literals in clause " +
                            clause.toString(0, model.symboltable) + " get the same truth value as " +
                        Symboltable.toString(literal, model.symboltable) );}
                for(CLiteral clit : cliterals) {
                    if(clit != cliteral) {
                        addToModel(sign*clit.literal,
                                trackReasoning ? joinIntArraysSorted(clause.origins,model.getOrigin(literal)) : null);}}
                    return null;} // clause no longer needed

            switch(clause.contains(literal,cliteral)) {
                case +1: clause.remove(cliteral); --i; continue;
                case -1: // p = -p is contradictory
                    throw new Unsatisfiable("Contradictory literals in equivalence class" +
                            clause.toString(0, model.symboltable), clause.origins);}}
        return clause.size() > 1 ? clause : null;}


    /** joins the new (not inserted) equivalence clause to the old ones if there is an overlapping.
     * If there is an overlapping then the old clause is removed from the internal data structures.
     *
     * @param clause A new equivalence clause
     * @return either the new class or an extended old one.
     * @throws Unsatisfiable if a contradiction p = -p occurs.
     */
    protected Clause joinClause(Clause clause) throws Unsatisfiable {
        for(CLiteral cliteral : clause.cliterals) {
            int literal = cliteral.literal;
            for(Clause oldClause : clauses) {
                int sign = oldClause.contains(literal);
                    if(sign != 0) {
                        if(monitoring) monitor.print(monitorId, "Joining equivalence clauses\n" +
                                oldClause.toString(4, model.symboltable) + " and\n" +
                                clause.toString(4, model.symboltable));
                        removeClause(oldClause);
                        for(CLiteral clit : clause) {addLiteral(oldClause,sign*clit.literal);}
                        if(trackReasoning) oldClause.joinOrigins(clause.origins);
                        return oldClause;}}}
        return clause;}


    /** A true literal causes all other equivalent literals to become true.
     *
     * @param literal a true literal
     * @param origins the baisc clause ids causing the literal to become true.
     * @throws Unsatisfiable if a contradiction occurs.
     */
    protected void integrateTrueLiteral(int literal, IntArrayList origins) throws Unsatisfiable {
        if(monitoring) {
            monitor.print(monitorId,"Exec: True literal " +
                    Symboltable.toString(literal, model.symboltable) +
                    (origins == null ? "" : " " + origins));}
        int sign = 1;
        CLiteral cliteral = literalIndex.get(literal);
        if(cliteral == null) {cliteral = literalIndex.get(-literal); sign = -1;}
        if(cliteral != null) {
            if(trackReasoning) origins = joinIntArraysSorted(origins,cliteral.clause.origins);
            removeClause(cliteral.clause);
            for(CLiteral cLiteral : cliteral.clause.cliterals) {
                if(cLiteral != cliteral) {
                    addToModel(sign*cLiteral.literal,origins);}}}}


    /** add the equivalence literal1 = literal2 to the equivalence classes, either to an existing one or a new one is created.
     * Adding an equivalence p = -q to a class p = q causes a contradiction to be reported.
     *
     * @param literal1 literal1 == literal2
     * @param literal2 literal1 == literal2
     * @param origins  the indices of the basic clauses causes this equivalence.
     * @throws Unsatisfiable, if a contradiction occurs
     */
    protected synchronized void integrateEquivalence(int literal1, int literal2, IntArrayList origins) throws Unsatisfiable{
        if(monitoring) {
            monitor.print(monitorId,"Exec: Equivalence " +
                    Symboltable.toString(literal1,model.symboltable) + " = " +
                    Symboltable.toString(literal2,model.symboltable) +
                            (origins == null ? " " : " " + origins));}
        statistics.derivedClasses++;
        Integer oldRepresentative = integrateEquivalenceClause(new Clause(counter++, ClauseType.EQUIV, literal1,literal2, origins));
        if(oldRepresentative == null) return;

        int newRepresentative = getRepresentative(oldRepresentative);
        if(oldRepresentative != newRepresentative) {
            for(TriConsumer<Integer,Integer,IntArrayList> observer : observers){
                observer.accept(newRepresentative,oldRepresentative,origins);}}

        int representative = getRepresentative(literal1);
        if(representative != literal1) {
            for(TriConsumer<Integer,Integer,IntArrayList> observer : observers){
                observer.accept(representative,literal1,origins);}}representative = getRepresentative(literal1);
        if(representative != literal2) {
            for(TriConsumer<Integer,Integer,IntArrayList> observer : observers){
                observer.accept(representative,literal2,origins);}}}

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
                    (origins == null ? "" : " " + origins));}
        model.add(literal,origins,null);}


    /** sorts the literals in the clause.
     * Literals are sorted according to their absolute values.
     * If the first literal is negative, then all literals are inverted.
     *
     * @param clause a new clause
     */
    private void sortLiterals(Clause clause) {
        ArrayList<CLiteral> cliterals = clause.cliterals;
        cliterals.sort(Comparator.comparingInt(clit -> Math.abs(clit.literal)));
        int sign = (cliterals.get(0).literal < 0) ? -1 : +1;
        for(int i = 0; i < cliterals.size(); ++i) {
            CLiteral cliteral = cliterals.get(i);
            cliteral.literal *= sign;
            cliteral.clausePosition = i;}}

    /** maps literals to their representative in the equivalence class.
     *
     * @param literal a literal
     * @return the literal or the representative of the literal's equivalence class.
     */
    public synchronized int getRepresentative(int literal) {
        CLiteral cliteral = literalIndex.get(literal);
        if(cliteral != null) return cliteral.clause.cliterals.get(0).literal;
        cliteral = literalIndex.get(-literal);
        if(cliteral != null) {
            return cliteral.clausePosition == 0 ? literal : -cliteral.clause.cliterals.get(0).literal;}
        return literal;}

    /** maps a literal to the origins of the equivalence containing the literal or its negation.
     * If the literal is already the representative of the class then the origins are null.
     *
     * @param literal any literal
     * @return null or the indices of the basic clauses causing this equivalence.
     */
    public synchronized IntArrayList getOrigins(int literal) {
        CLiteral cliteral = literalIndex.get(literal);
        if(cliteral == null) cliteral = literalIndex.get(-literal);
        return (cliteral == null || cliteral.clausePosition == 0) ? null : cliteral.clause.origins;}

    /** If a literal in an equivalence class is true, then all other literals are also made true
     *
     * @return an Inconsistency if the resulting model becomes inconsistent (should never happen)
     */
    public Inconsistency completeModel() {
        for(Clause clause : clauses) {
            int sign = 0;
            int lit = 0;
            IntArrayList origins = null;
            for(CLiteral cliteral : clause) { // find a true/false literal
                int literal = cliteral.literal;
                int status = model.status(literal);
                if(status != 0) {
                    if(sign != 0 && status != sign) {
                        return new Inconsistency(problemId, // something went terribly wrong
                                "Equivalence class: " + clause.toString(0,model.symboltable) +
                                ": Literal " + Symboltable.toString(lit,model.symboltable) +
                                " is " + (sign > 0 ? "true" : false) + " but literal " +
                                        Symboltable.toString(literal,model.symboltable) +
                                " is " + (status > 0 ? "true" : "false"));}
                    else {sign = status; lit = literal;
                        origins = model.getOrigin(literal);}}}
            if(sign == 0) continue;  // no literal has a truth value.

            if(trackReasoning) origins = joinIntArraysSorted(origins,clause.origins);
            for(CLiteral cliteral : clause) {
                int literal = cliteral.literal;
                if(model.status(literal) == 0) model.addImmediately(sign*literal,origins);}}
        return null;}

    /** inserts the clause into the local data structures.
     *
     * @param clause  the clause to be inserted.
     */
    private void insertClause(Clause clause) {
        ++statistics.clauses;
        clauses.add(clause);
        for(CLiteral cliteral : clause) {literalIndex.put(cliteral.literal, cliteral);}}

    /** removes a clause from the internal lists.
     *
     * @param clause a clause to be removed.
     */
    private void removeClause(Clause clause) {
        --statistics.clauses;
        clauses.remove(clause);
        for(CLiteral cliteral : clause) {literalIndex.remove(cliteral.literal);}}

    /** adds a literal to the clause
     *
     * @param clause a clause
     * @param literal a literal
     */
    private void addLiteral(Clause clause, int literal) {
        CLiteral cliteral = new CLiteral(literal,clause,clause.cliterals.size());
        clause.add(cliteral);
        literalIndex.put(literal,cliteral);}

    /** checks if there is no equivalence class
     *
     * @return true if there is no equivalence class
     */
    public synchronized boolean isEmpty() {
        return clauses.isEmpty();}

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
        string.append("Equivalence Classes of Problem " + problemId + ":\n");
        int width = Integer.toString(counter).length();
        int size = clauses.size();
        for(int i = 0; i < size; ++i) {
            string.append(clauses.get(i).toString(width,symboltable));
            if(i < size-1) string.append("\n");}
        return string.toString();}


    /** turns the equivalence classes into a string "literal1[origins] = literal2[origins] = ... = representative"
     *
     * @param symboltable or null
     * @return a string representation of the equivalence class with the origins.
     */
    public String infoString(@Nullable Symboltable symboltable) {
        StringBuilder string = new StringBuilder();
        if(!clauses.isEmpty()) {string.append(toString(symboltable));}
        if(!literalIndex.isEmpty()) {
            string.append("\nLiteral Index:\n");
            literalIndex.forEach((literal,cliteral) -> string.append("  " + literal + "@"+cliteral.clause.id + "\n"));}
        if(!queue.isEmpty()) {
            string.append("\nEquivalence Classes Queue of Problem " + problemId + ":\n").
                    append(Task.queueToString(queue));}
        return string.toString();}


    /** checks the consistency of the data. */
    public void check() {
        for(Clause clause : clauses) {
            clause.check();
            for(CLiteral cliteral : clause) {
                if(literalIndex.get(cliteral.literal) == null) {
                    System.out.println("Literal Index:  Cliteral: " + cliteral.toString(model.symboltable) +
                            " is not in the literalIndex");}
                else {if(literalIndex.get(cliteral.literal) != cliteral) {
                        System.out.println("Literal Index:  Wrong cliteral: " +
                            literalIndex.get(cliteral.literal).toString(model.symboltable) +
                            " in the literalIndex insread of " + cliteral.toString(model.symboltable));}}}}
        literalIndex.forEach((literal, cliteral) -> {
            if(literal != cliteral.literal) {
                System.out.println("Literal Index: Literal " + Symboltable.toString(literal,model.symboltable) +
                        " != Cliteral.literal: " + cliteral.toString(model.symboltable));}});

        for(Clause clause1 : clauses) {
            for(Clause clause2 : clauses) {
                if(clause1 != clause2 && clause1.overlaps(clause2)) {
                    System.out.println("Clause\n" + clause1.toString(0, model.symboltable) +
                            " overlaps with\n" + clause2.toString(0,model.symboltable));}}}}


}
