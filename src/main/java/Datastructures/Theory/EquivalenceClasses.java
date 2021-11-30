package Datastructures.Theory;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.Connective;
import Datastructures.Literals.CLiteral;
import Datastructures.Results.Result;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Datastructures.Task;
import InferenceSteps.*;
import Management.Monitor;
import Management.ProblemSupervisor;
import com.sun.istack.internal.Nullable;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.concurrent.PriorityBlockingQueue;
import java.util.function.Consumer;


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
    public final ProblemSupervisor problemSupervisor;

    /** for collecting statistics */
    public EquivalenceStatistics statistics;

    /** controls the computation of the clause's origins */
    private final boolean trackReasoning;

    /** The id of the current problem to be solved */
    private final String problemId;

    /** The list of Equivalence  clauses */
    private final ArrayList<Clause> clauses = new ArrayList<>();

    /** maps literals to the literal occurrences in clauses.
     * It exploits that equivalence classes are disjoint.
     * Therefore a literal can occur in only a single equivalence clause.
     */
    private final HashMap<Integer, CLiteral> literalIndex = new HashMap<>();

    /** the global model of true literals */
    public final Model model;

    /** null or a the global symboltable */
    public Symboltable symboltable;

    /** for logging the actions of this class */
    private final Monitor monitor;

    /** indicates monitoring is on */
    private boolean monitoring = false;

    /** for distinguishing the monitoring areas */
    private String monitorId = null;


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
            case EQUIVALENCE: return ((Clause)task.a).id;} // this guarantees a deterministic sequence of the tasks
        return 4;}

    /** A queue of newly derived unit literals and binary equivalences.
     * The unit literals are automatically put at the beginning of the queue.
     */
    private final PriorityBlockingQueue<Task<EquivalenceClasses.TaskType>> queue =
            new PriorityBlockingQueue<>(10, Comparator.comparingInt(this::getPriority));

    /** These observers are called for new equivalences: (representative,literal,origins)
     * They are not called for the initial basic equivalence clauses.*/
    private final ArrayList<Consumer<Clause>> observers = new ArrayList<>();

    /** creates the EquivalenceClasses instance
     *
     * @param problemSupervisor         the problem supervisor
     */
    public EquivalenceClasses(ProblemSupervisor problemSupervisor) {
        problemSupervisor.equivalenceClasses = this;
        this.problemSupervisor = problemSupervisor;
        problemId = problemSupervisor.problemId;
        model = problemSupervisor.model;
        symboltable = model.symboltable;
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
    public void addObserver(Consumer<Clause> observer) {
        observers.add(observer);}

    /** adds the observer to the model.
     * Must be called before the thread is started.
     */
    public void configure() {
        model.addObserver(Thread.currentThread(),this::addTrueLiteral);}



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
        Task<TaskType> task;
        while(!Thread.interrupted()) {
            try {
                if(monitoring) {monitor.print(monitorId,"Queue is waiting\n" + Task.queueToString(queue));}
                task = queue.take(); // waits if the queue is empty
                switch(task.taskType){
                    case TRUELITERAL: integrateTrueLiteral((Integer)task.a, (InferenceStep)task.b); break;
                    case EQUIVALENCE: integrateEquivalence((Clause)task.a,true); break;}
                if(monitoring) {
                    monitor.print(monitorId,"Current equivalences:\n" +
                            toString(symboltable,false));}}
            catch(InterruptedException ex) {return;}
            catch(Unsatisfiable unsatisfiable) {
                problemSupervisor.setResult(unsatisfiable,"EquivalenceClasses");
                return;}}}


    /** adds a basic equivalence basicClause to the equivalence classes.
     * All literals are replaced by the representatives of already existing equivalence classes.
     * Double occurrences are ignored.<br>
     * A contradiction may occur, if for example p,-q is to be added to p,q.<br>
     * In this case an Unsatisfiable exception is thrown.<br>
     * If the new equivalence class overlaps with the  one, the two are joined.
     *
     * @param basicClause a basic equivalence basicClause
     * @throws Unsatisfiable if a contradictory truth value has been discovered.
     */
    public void addBasicEquivalenceClause(int[] basicClause) throws Unsatisfiable {
        assert basicClause.length > 3;
        assert Connective.getType(basicClause[1]) == Connective.EQUIV;
        statistics.basicClauses++;
        Clause clause = new Clause(basicClause);
        if(trackReasoning) {clause.inferenceStep = new ClauseCopy(basicClause,clause);}
        integrateEquivalence(clause,false);}

    /** adds a true literal to the queue
     *
     * @param literal a true literal
     * @param inference which caused the truth
     */
    public void addTrueLiteral(int literal,InferenceStep inference) {
        if(monitoring) {
            monitor.print(monitorId,"In:   True literal " +
                    Symboltable.toString(literal,symboltable));}
        synchronized (this) {queue.add(new Task<>(TaskType.TRUELITERAL, literal, inference));}}

    /** This method is to be called by the TwoLiteral module to announce a newly derived equivalence
     * literal1 = literal2.
     * The equivalences is put into the queue.
     *
     * @param literal1 a literal
     * @param literal2 an equivalent literal
     */
    public void addDerivedEquivalence(int literal1, int literal2, InferenceStep inferenceStep) {
        ++statistics.derivedClasses;
        if(monitoring) {
            monitor.print(monitorId,"In:   Equivalence " +
                    Symboltable.toString(literal1, symboltable) + " = " +
                    Symboltable.toString(literal2, symboltable));}
        Clause clause = new Clause(problemSupervisor.nextClauseId(), Connective.EQUIV,literal1,literal2);
        clause.inferenceStep = inferenceStep;
        Task<TaskType> task = new Task<>(TaskType.EQUIVALENCE,clause,null);
        synchronized (this) {queue.add(task);}}


    /** integrates a clause into the equivalence classes.
     * Literals are replaced by representatives of other equivalence classes (if necessary) <br>
     * True and false literals cause all equivalent literals to become true/false <br>
     * Double literals are removed. <br>
     * Complementary literals cause Unsatisfiable to be thrown. <br>
     * Overlapping equivalences are joined to the new clause (the  clauses are removed).<br>
     * All observers are applied to derived clauses.
     *
     * @param clause a new equivalence clause
     * @param derived is true if the clause is a derived clause (not basic)
     */
    public void integrateEquivalence(Clause clause, boolean derived) throws Unsatisfiable {
        if(monitoring) {
            monitor.print(monitorId,"Exec: equivalence clause: " + clause.toString(0,symboltable));}
        if((clause = normalizeClause(clause)) == null) return;
        sortLiterals(clause);
        clause = joinClause(clause);
        sortLiterals(clause);
        insertClause(clause);
        if(derived) {for(Consumer<Clause> observer : observers) observer.accept(clause);}}


    /** performs a number of transformations and simplifications.
     * replaces literals by representatives in some equivalence class<br>
     * checks for true/false literals<br>
     * checks for double literals p,p (p becomes false)<br>
     * checks for inconsistencies p == -p <br>
     *
     * @param clause  a disjointness clause
     * @return null or the normalized clause
     * @throws Unsatisfiable if a contradiction is discovered.
     */
    protected Clause normalizeClause(Clause clause) throws Unsatisfiable {
        clause = replaceEquivalences(clause);
        if((clause = replaceTruthValues(clause)) == null) return null;
        if((clause = removeDoublesAndInconsistencies(clause)) == null) return null;
        return clause;}

    /** replaces the literals in the Clause by their representatives in the equivalence class.
     * The clause must not be integrated.
     *
     * @param Clause a clause
     * @return the original clause or a new clause with the literals replaced.
     */
    public synchronized Clause replaceEquivalences(Clause Clause) {
        if(isEmpty()) return Clause;
        for(CLiteral cliteral : Clause) {
            int Literal = cliteral.literal;
            int newLiteral = getRepresentative(Literal);
            if(newLiteral != Literal) {
                Clause newClause = Clause.clone(problemSupervisor.nextClauseId());
                newClause.getCLiteral(cliteral.clausePosition).literal = newLiteral;
                if(trackReasoning) {
                    newClause.inferenceStep = new EquivalenceReplacements1(Clause,Literal,newClause,newLiteral,
                            getEClause(Literal));
                    if(monitoring) {monitor.print(monitorId,newClause.inferenceStep.toString(symboltable));}}
                return replaceEquivalences(newClause);}}
        return Clause;}

    /** replaces all double literals and checks for inconsistency.
     *
     * @param clause an unintegrated clause
     * @return null (single literal) or the clause without double literals.
     * @throws Unsatisfiable if the clause contains a contradiction p = -p
     */
    protected Clause removeDoublesAndInconsistencies(Clause clause) throws Unsatisfiable {
        for(int i = 0; i < clause.size(); ++i) {
            int literal = clause.getLiteral(i);
            for(int j = i+1; j < clause.size(); ++j) {
                int otherLiteral = clause.getLiteral(j);
                if(literal == otherLiteral) {clause.removeAtPosition(j--); continue;}
                if(literal == -otherLiteral) {
                    EquivalenceInconsistency eqi = trackReasoning ?
                            new EquivalenceInconsistency(clause,literal,otherLiteral) : null;
                    //throw new Unsatisfiable(eqi);
                }}}
        return clause.size() > 1 ? clause : null;}

    /** a true literal causes the clause to be deleted, a false literal is removed.
     *
     * @param clause the original not integrated clause
     * @return null (true clause) or the unchanged clause or a new shortened clause
     */
    protected Clause replaceTruthValues(Clause clause) throws Unsatisfiable{
        for(CLiteral cLiteral1 : clause) {
            int literal1 = cLiteral1.literal;
            int sign = model.status(literal1);
            if(sign != 0) {
                InferenceStep inferenceLit = model.getInferenceStep(literal1);
                for(CLiteral cLiteral2 : clause) {
                    EquivalentTrueLiteral inference = null;
                    if(cLiteral2 != cLiteral1) {
                        if(trackReasoning) {
                            inference = new EquivalentTrueLiteral(clause,sign*literal1,
                                    sign*cLiteral2.literal,inferenceLit);
                            if(monitoring) monitor.print(monitorId,inference.toString());}
                        ++statistics.derivedTrueLiterals;
                        model.add(sign*cLiteral2.literal,inference,null);}}
                return null;}}
        return clause;}

    /** joins the new (not inserted) equivalence clause to the  ones if there is an overlapping.
     * If there are overlappings then the  clauses are removed from the internal data structures.
     *
     * @param clause A new equivalence clause
     * @return the extended new clause
     * @throws Unsatisfiable if a contradiction p = -p occurs.
     */
    protected Clause joinClause(Clause clause) throws Unsatisfiable {
        for(int i = 0; i < clauses.size(); ++i) {
            Clause Clause = clauses.get(i);
            int[] result = clause.overlaps(Clause);
            if(result != null) {
                ++statistics.joinedClasses;
                int sign = result[0];
                int literal = result[1];
                Clause newClause = clause.clone(problemSupervisor.nextClauseId());
                for(CLiteral cliteral : Clause) newClause.add(sign*cliteral.literal);
                if(trackReasoning) {
                    EquivalenceJoining eqj = new EquivalenceJoining(clause,Clause,literal,newClause);
                    newClause.inferenceStep = eqj;
                    if(monitoring) monitor.print(monitorId,eqj.toString(symboltable));}
                removeClause(Clause); --i;
                clause = newClause;}}
        return removeDoublesAndInconsistencies(clause);}


    /** A true literal causes all other equivalent literals to become true.
     *
     * @param literal a true literal
     * @param inferenceStep which caused the truth of the literal
     * @throws Unsatisfiable if a contradiction occurs.
     */
    protected void integrateTrueLiteral(int literal, InferenceStep inferenceStep) throws Unsatisfiable {
        if(monitoring) {
            monitor.print(monitorId,"Exec: True literal " +
                    Symboltable.toString(literal, symboltable));}
        int sign = 1;
        CLiteral cliteral = literalIndex.get(literal);
        if(cliteral == null) {cliteral = literalIndex.get(-literal); sign = -1;}
        if(cliteral != null) {
            Clause clause = cliteral.clause;
            removeClause(clause);
            InferenceStep inference = null;
            for(CLiteral cLiteral : clause) {
                if(cLiteral != cliteral) {
                    if(trackReasoning) {
                        inference = new EquivalentTrueLiteral(clause,literal,
                                sign*cLiteral.literal,inferenceStep);
                        if(monitoring) monitor.print(monitorId,inference.toString());}
                    model.add(sign*cLiteral.literal,inference,null);}}}}


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

    /** return the Equivalence clause which contains the literal (positive or negative)
     *
     * @param literal any literal
     * @return null or the equivalence clause containing the literal (positive or negative)
     */
    public synchronized Clause getEClause(int literal) {
        CLiteral cliteral = literalIndex.get(literal);
        if(cliteral != null) return cliteral.clause;
        cliteral = literalIndex.get(-literal);
        if(cliteral != null) {return cliteral.clause;}
        return null;}



    /** If a literal in an equivalence class is true, then all other literals are also made true
     * This method is called when a partial model is found, which is supposed to be extendable to a complete model.
     *
     * @throws Unsatisfiable if the resulting model becomes inconsistent (should never happen)
     */
    public void completeModel() throws Unsatisfiable {
        for(Clause clause : clauses) {
            for(CLiteral cliteral : clause) { // find a true/false literal
                int literal = cliteral.literal;
                int status = model.status(literal);
                if(status != 0) {
                    switch(status) {
                        case +1: integrateTrueLiteral(literal, model.getInferenceStep(literal)); break;
                        case -1: integrateTrueLiteral(-literal, model.getInferenceStep(literal)); break;}
                    completeModel();
                    return;}}}}

    /** inserts the clause into the local data structures.
     *
     * @param clause  the clause to be inserted.
     */
    private synchronized void insertClause(Clause clause) {
        ++statistics.clauses;
        clauses.add(clause);
        for(CLiteral cliteral : clause) {literalIndex.put(cliteral.literal, cliteral);}}

    /** removes a clause from the internal lists.
     *
     * @param clause a clause to be removed.
     */
    private synchronized void removeClause(Clause clause) {
        --statistics.clauses;
        clauses.remove(clause);
        clause.setRemoved();
        for(CLiteral cliteral : clause) {literalIndex.remove(cliteral.literal);}}


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
    public String toString() {return toString(null,false);}

    /** turns the equivalence classes into a string "literal1 = literal2 = ... = representative\n..."
     *
     * @param symboltable or null
     * @return a string representation of the equivalence classes.
     */
    public String toString(@Nullable Symboltable symboltable) {
        return toString(symboltable,false);}


    /** turns the equivalence classes into a string "literal1 = literal2 = ... = representative\n..."
     *
     * @param symboltable or null
     * @param info  if true then the origins are added.
     * @return a string representation of the equivalence classes.
     */
    public String toString(@Nullable Symboltable symboltable, boolean info) {
        StringBuilder string = new StringBuilder();
        string.append("Equivalence Classes of Problem " + problemId + ":\n");
        int width = Integer.toString(problemSupervisor.clauseCounter).length()+2;
        int size = clauses.size();
        for(int i = 0; i < size; ++i) {
            if(info) string.append(clauses.get(i).infoString(width,symboltable));
            else string.append(clauses.get(i).toString(width,symboltable));
            if(i < size-1) string.append("\n");}
        return string.toString();}


    /** turns the equivalence classes into a string "literal1[origins] = literal2[origins] = ... = representative"
     *
     * @param symboltable or null
     * @return a string representation of the equivalence class with the origins.
     */
    public String infoString(@Nullable Symboltable symboltable) {
        StringBuilder string = new StringBuilder();
        if(!clauses.isEmpty()) {string.append(toString(symboltable,true));}
        if(!literalIndex.isEmpty()) {
            string.append("\nLiteral Index:\n ");
            literalIndex.forEach((literal,cliteral) -> string.append(
                    Symboltable.toString(literal,symboltable) +
                     "@"+cliteral.clause.id + ","));}
        if(!queue.isEmpty()) {
            string.append("\nEquivalence Classes Queue of Problem " + problemId + ":\n").
                    append(Task.queueToString(queue));}
        return string.toString();}


    /** checks the consistency of the data. */
    public void check(StringBuilder errors) {
        for(Clause clause : clauses) {
            clause.check(errors);
            for(CLiteral cliteral : clause) {
                if(literalIndex.get(cliteral.literal) == null) {
                    System.out.println("Literal Index:  Cliteral: " + cliteral.toString(symboltable) +
                            " is not in the literalIndex");}
                else {if(literalIndex.get(cliteral.literal) != cliteral) {
                        System.out.println("Literal Index:  Wrong cliteral: " +
                            literalIndex.get(cliteral.literal).toString(symboltable) +
                            " in the literalIndex insread of " + cliteral.toString(symboltable));}}}}
        literalIndex.forEach((literal, cliteral) -> {
            if(literal != cliteral.literal) {
                System.out.println("Literal Index: Literal " + Symboltable.toString(literal,symboltable) +
                        " != Cliteral.literal: " + cliteral.toString(symboltable));}});

        for(Clause clause1 : clauses) {
            for(Clause clause2 : clauses) {
                if(clause1 != clause2 && clause1.overlaps(clause2) != null) {
                    System.out.println("Clause\n" + clause1.toString(0, symboltable) +
                            " overlaps with\n" + clause2.toString(0,symboltable));}}}}


}
