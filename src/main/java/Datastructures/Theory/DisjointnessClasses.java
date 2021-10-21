package Datastructures.Theory;

import Datastructures.Clauses.BasicClauseList;
import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseType;
import Datastructures.Literals.CLiteral;
import Datastructures.Literals.HashIndex;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Datastructures.Task;
import InferenceSteps.*;
import Management.Monitor;
import Management.ProblemSupervisor;
import com.sun.istack.internal.Nullable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.*;
import java.util.concurrent.PriorityBlockingQueue;
import java.util.function.Consumer;


/** A disjointness clause is a set of literals which are pairwise contradictory.
 * Such a clause may come from the input data, or be derived from binary clauses.
 * The class works as Thread in parallel to the other solvers. <br>
 * A disjointness p != q != r is essentially a shortcut for two-literal clauses <br>
 * -p,-q and -p,-r and -q,-r. <br>
 * Therefore they are in fact redundant. <br>
 * However, disjointness clauses are the basis for multi-resolutions!
 *
 * Created by Ohlbach on 21.09.2018.
 */
public class DisjointnessClasses {

    public final ProblemSupervisor problemSupervisor;  // supervises the problem solution
    private final String problemId;                    // the current problem's id
    public  final DisjointnessStatistics statistics;   // the statistics of this thread
    private final boolean trackReasoning;              // controls the computation of the clause's origins
    public final boolean monitoring;                   // activates monitoring
    public final Monitor monitor;                      // for logging the actions of this class
    public final String monitorId;                     // for distinguishing the monitoring areas
    public final Model model;                          // the global model
    public final Symboltable symboltable;              // its symboltable
    public final EquivalenceClasses equivalenceClasses;// The equivalence classes thread
    private final ArrayList<Clause> clauses;            // The list of Disjointness clauses
    private final HashIndex literalIndex;               // maps literals to the literal occurrences in clauses
    private final HashMap<Integer, ArrayList<CLiteral>> disjointnesses = new HashMap<>();// maps literals to disjoint literals
    private final ArrayList<Consumer<Clause>> observers = new ArrayList<>();             // called in integrateDisjointnessClause for a new clause
    private enum TaskType {TRUELITERAL, INSERTCLAUSE, EQUIVALENCE}   //types of tasks in the queue
    private final PriorityBlockingQueue<Task<TaskType>> queue =      // a queue of tasks
            new PriorityBlockingQueue<>(10, Comparator.comparingInt(this::getPriority));

    /**
     * gets the priority for the objects in the queue.
     *
     * @param task the task in the queue
     * @return the priority of the objects in the queue.
     */
    private int getPriority(Task<TaskType> task) {
        switch (task.taskType) {
            case TRUELITERAL:
                return Integer.MIN_VALUE;
            case EQUIVALENCE:
                return Integer.MIN_VALUE + 1;
            case INSERTCLAUSE:
                return ((Clause) task.a).id;
        } // makes the sequence deterministic
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
        symboltable = model.symboltable;
        equivalenceClasses = problemSupervisor.equivalenceClasses;
        clauses = new ArrayList<>();
        literalIndex = new HashIndex();
        statistics = new DisjointnessStatistics(problemId);
        trackReasoning = problemSupervisor.globalParameters.trackReasoning;
        monitor = problemSupervisor.globalParameters.monitor;
        monitoring = monitor != null;
        monitorId = problemId + "DISJ";}

    /** Any solver which is interested to know about newly derived disjointnesses can add an observer.
     * The observer is called with a new disjointness clause
     *
     * @param observer a Consumer for transferring newly derived equivalences.
     */
    public void addObserver(Consumer<Clause> observer) {
        observers.add(observer);}

    /** installs the observers in the other modules
     * must be called before the thread starts.
     */
    public void configure() {
        model.addObserver(Thread.currentThread(), this::addTrueLiteral);
        equivalenceClasses.addObserver(this::addEquivalence);}

    /** This method is started as thread.
     * It reads and executes tasks from the queue
     * It can only be stopped by an interrupt or when a contradiction is found.
     */
    public void run() {
        Task<TaskType> task;
        while (!Thread.interrupted()) {
            try {
                if (monitoring) {
                    monitor.print(monitorId, "Queue is waiting\n" + Task.queueToString(queue));}
                task = queue.take();
                switch (task.taskType) {
                    case TRUELITERAL:
                        integrateTrueLiteral((Integer)task.a, (InferenceStep)task.b);
                        break;
                    case INSERTCLAUSE:
                        integrateDisjointnessClause((Clause)task.a);
                        break;
                    case EQUIVALENCE:
                        integrateEquivalence((Clause)task.a);}}
            catch (InterruptedException ex) {return;}
            catch (Unsatisfiable unsatisfiable) {
                problemSupervisor.setResult(unsatisfiable, "Disjointness");
                return;}}}

    /** adds a basic disjointness clause to the queue.
     *
     * @param basicClause a basic disjointness clause with at least 3 disjoint literals
     */
    public void addDisjointnessClause(int[] basicClause) {
        assert basicClause.length > 4;
        //assert basicClause[1] == ClauseType.DISJOINT.ordinal() || basicClause[1] == ClauseType.XOR.ordinal();
        ++statistics.basicClauses;
        Clause clause = new Clause(problemSupervisor.nextClauseId(),basicClause);
        if(trackReasoning) {
            clause.inferenceStep = new ClauseCopy(basicClause,clause);
            if (monitoring) {
                monitor.print(monitorId, "In:   disjointness clause: " +
                        BasicClauseList.clauseToString(0, basicClause, symboltable));}}
        synchronized (this) {queue.add(new Task<>(TaskType.INSERTCLAUSE, clause, null));}}

    /** adds a true literal to the queue
     *
     * @param literal a true literal
     * @param inferenceStep for deriving the literal
     */
    public void addTrueLiteral(int literal, InferenceStep inferenceStep) {
        ++statistics.trueLiterals;
        if(monitoring) {
            monitor.print(monitorId, "In:   True literal " +
                    Symboltable.toString(literal, symboltable));}
        synchronized (this) {queue.add(new Task<>(TaskType.TRUELITERAL, literal, inferenceStep));}}

    /** adds a new derived disjointness p,q,r to the queue
     *
     * @param literals some literals
     * @param inference  which caused the derivation of the disjoints
     */
    public void addDerivedDisjoints(IntArrayList literals, InferenceStep inference) {
        ++statistics.derivedDisjointesses;
        if (monitoring) {
            monitor.print(monitorId, "In:   Disjointness " +
                    Symboltable.toString(literals, symboltable));}
        // change
        Clause clause = new Clause(problemSupervisor.nextClauseId(), ClauseType.ATLEAST, literals);

        clause.inferenceStep = inference;
        synchronized (this) {queue.add(new Task<>(TaskType.INSERTCLAUSE, clause, null));}}

      /** adds a new equivalence clause to the queue
     *
     * @param clause  an equivalence clause
     */

    public void addEquivalence(Clause clause) {
        ++statistics.equivalences;
        if(monitoring) {
            monitor.print(monitorId,"In:   Equivalence " + clause.toString(0,symboltable));}
        synchronized (this) {queue.add(new Task<>(TaskType.EQUIVALENCE, clause,null));}}

    /** simplifies a disjointness clause and inserts them into the internal datastructures. <br>
     *  The following phenomena are treated: <br>
     *  double literals, complementary literals, true/false literals <br>
     *  overlappings with other clauses <br>
     *  subsumptions (forward, backward) <br>
     *  resolutions with other clauses <br>
      *
     * @param clause a new disjointness clause
     */
    protected void integrateDisjointnessClause(Clause clause) throws Unsatisfiable {
        if(monitoring) {
            monitor.print(monitorId,"Exec: disjointness clause: " + clause.toString(0,symboltable));}
        if((clause = normalizeClause(clause)) == null) return;
        Clause subsumer = isSubsumed(clause);
        if(subsumer != null) {
            if(monitoring) {
                monitor.print(monitorId,"New clause is subsumed by " +
                        subsumer.toString(0,symboltable));}
            return;}
        clause = extendNormalizedClause(clause);
        if(!clauses.isEmpty()) {resolveClause(clause);}
        removeSubsumedClauses(clause);
        sortLiterals(clause);
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
    protected Clause normalizeClause(Clause clause) throws Unsatisfiable {
        clause = equivalenceClasses.replaceEquivalences(clause);
        if((clause = removeDoubles(clause)) == null) return null;
        return replaceTruthValues(clause);}

    /** replaces all double literals and checks for inconsistency.
     * p != p  is false and must be removed from the clause <br>
     * p != -p is true and therefore all other literals become false<br>
     * The original clause is not changed.
     *
     * @param clause an unintegrated clause
     * @return null (single literal) or a clause without double and complementary literals.
     * @throws Unsatisfiable if the clause contains a contradiction p = -p
     */
    protected Clause removeDoubles(Clause clause) throws Unsatisfiable {
        if(clause.size() < 2) return null;
        InferenceStep step = null; // we must first check for complementary literals.
        for(int i = 0; i < clause.size(); ++i) {
            int literal = clause.getLiteral(i);
            for(int j = i+1; j < clause.size(); ++j) {
                int otherLiteral = clause.getLiteral(j);
                if(literal == -otherLiteral) { // p != -p is true. All other literals become false
                    for(CLiteral cLiteral : clause) {
                        int nextLiteral = -cLiteral.literal;
                        if (Math.abs(literal) == Math.abs(nextLiteral)) continue;
                        if(trackReasoning) {
                            step = new DisjointnessComplementary(clause,literal,nextLiteral);
                            if(monitoring) monitor.print(monitorId,step.toString());}
                        model.add(nextLiteral,step,null);}
                    return null;}}}
        for(int i = 0; i < clause.size(); ++i) {  // now we can check for double literals.
            int literal = clause.getLiteral(i);
            for(int j = i+1; j < clause.size(); ++j) {
                int otherLiteral = clause.getLiteral(j);
                if(literal == otherLiteral) { // p != p is false and has to be removed
                    if(trackReasoning) {
                        step = new DisjointnessDouble(clause,literal);
                        if(monitoring) monitor.print(monitorId,step.toString(symboltable));}
                    model.add(-literal,step,null);
                    return removeDoubles(clause.cloneExcept(problemSupervisor.nextClauseId(),literal));}}}
        return clause;}

    /** a true literal causes all other literals to become false, a false literal is removed.
     *
     * @param clause the original not integrated clause
     * @return null (empty clause) or the unchanged clause or a new shortened clause
     */
    protected Clause replaceTruthValues(Clause clause) throws Unsatisfiable{
        for(int i = 0; i < clause.size(); ++i) { // check for true literals
            int literal1 = clause.getLiteral(i);
            if(model.isTrue(literal1)) {
                InferenceStep step1 = model.getInferenceStep(literal1);
                InferenceStep step2 = null;
                for(int j = 0; j < clause.size(); ++j) {
                    if(i == j) continue;
                    int literal2 = clause.getLiteral(j);
                    if(trackReasoning) {
                        step2 = new DisjointnessTrueLiteral(clause,literal1,-literal2,step1);
                        if(monitoring) monitor.print(monitorId,step2.toString(symboltable));}
                    model.add(-literal2,step2,null);}
                return null;}}
        for(int i = 0; i < clause.size(); ++i) { // check for false literals
            InferenceStep step2;
            int literal = clause.getLiteral(i);
            if(model.isFalse(literal)) {
                InferenceStep step1 = model.getInferenceStep(literal);
                Clause newClause = clause.clone(problemSupervisor.nextClauseId(), i--);
                if(trackReasoning) {
                    step2 = new DisjointnessFalseLiteral(clause,newClause,literal,step1);
                    newClause.inferenceStep = step2;
                    if(monitoring) monitor.print(monitorId,step2.toString(symboltable));}
                clause = newClause;}}
        return clause.size() <= 1 ? null : clause;}

    /** binary resolution works for disjointness clauses in the same way as for disjunctions.
     * All resolvents are put into the task queue.
     *
     * @param clause the clause to be tested
     */
    private void resolveClause(Clause clause) {
        for(int i = 0; i < clause.size(); ++i) {
            int literal = clause.getLiteral(i);
            ArrayList<CLiteral> clits = literalIndex.get(-literal);
            if(clits == null) continue;
            for(CLiteral parentCLiteral : clits) {
                Clause parent2 = parentCLiteral.clause;
                Clause resolvent = clause.clone(problemSupervisor.nextClauseId(),i);
                for(CLiteral clit2 : parent2)
                    if(clit2 != parentCLiteral) resolvent.add(clit2.literal);
                    ++statistics.resolutions;
                if(trackReasoning) {
                    BinaryResolution inf = new BinaryResolution(clause,parent2,literal,resolvent);
                    resolvent.inferenceStep = inf;
                    if(monitoring) monitor.print(monitorId,inf.toString(symboltable));}
                synchronized (this) {queue.add(new Task<>(TaskType.INSERTCLAUSE,resolvent,null));}}}}

    /** tries to extend a clause of disjoint literals.
     * Example: literals = p,q,r
     * Find a literal s such that p != s, q != s and r !=s holds.
     * This literal is added to p,q,r.
     *
     * @param clause a disjointness clause
     * @return the clause itself or an extended clause.
     */
    private Clause extendNormalizedClause(Clause clause) {
        CLiteral cliteral1 = clause.cliterals.get(0);
        int literal1 = cliteral1.literal;
        ArrayList<CLiteral> disjoints = disjointnesses.get(literal1);
        if(disjoints == null || disjoints.isEmpty()) return clause; // no extension possible
        for(CLiteral cliteral2 : disjoints) {
            int literal2 = cliteral2.literal; // literal2 is a candidate for extension
            if(model.status(literal2) != 0) continue;
            ArrayList<CLiteral> cLiterals = new ArrayList<>();
            boolean found = true;
            for(CLiteral cliteral11 : clause.cliterals) { // all other literals must also be disjoint to literal2
                 //if(cliteral11 == cliteral1) continue;
                 if(cliteral11.literal == -literal2) continue;
                CLiteral clit = areDisjoint(literal2,cliteral11.literal);
                if(clit == null) {found = false; break;}
                cLiterals.add(clit);}
            if(found) {
                Clause newClause = clause.clone(problemSupervisor.nextClauseId());
                newClause.add(literal2);
                ++statistics.extendedClasses;
                if(trackReasoning) {
                    DisjointnessExtension dise = new DisjointnessExtension(clause,literal2,newClause,cLiterals);
                    newClause.inferenceStep = dise;
                    if(monitoring) {monitor.print(monitorId,dise.toString(symboltable));}}
                clause = newClause;}}
        return clause;}

    /** checks of a new disjointness class is a subset of an already existing one.
     * In this case the new class is superfluous
     *
     * @param clause a disjointness clause
     * @return null or the subsumer class
     */
    private Clause isSubsumed(Clause clause) {
        for(Clause dClause : clauses) {
            if(clause.isSubset(dClause)) {++statistics.backwardSubsumed; return dClause;}}
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
        for(Clause clause2 : subsumed) {
            ++statistics.forwardSubsumed;
            removeClause(clause2);
            literalIndex.removeClause(clause2);}}


    /** A true literal p in a disjointness clause p,q,r causes q,r to become false.
     * A false literal p in a disjointness clause p,q,r causes p to be removed <br>
     * All literals which get a truth value are removed form the disjointness classes.
     *
     * @param literal a true literal
     * @throws Unsatisfiable if a contradiction is found.
     */
    protected synchronized void integrateTrueLiteral(int literal, InferenceStep inferenceStep) throws Unsatisfiable {
        if (monitoring) {
            monitor.print(monitorId, "Exec: true literal: " +
                    Symboltable.toString(literal, symboltable));}

        ArrayList<CLiteral> cLiterals = literalIndex.get(literal);
        if(cLiterals != null) { // clauses with literal cause the other literals to become false.
            for(CLiteral cliteral1 : cLiterals) {
                Clause clause = cliteral1.clause;
                removeClause(clause);
                for(CLiteral cliteral2 : clause.cliterals) {
                    if(cliteral1 != cliteral2) {
                        int literal2 = -cliteral2.literal;
                        DisjointnessTrueLiteral dis = null;
                        if(trackReasoning) {
                            dis = new DisjointnessTrueLiteral(clause,literal,literal2,inferenceStep);
                            if(monitoring) {monitor.print(monitorId,dis.toString(symboltable));}}
                        model.add(literal2,dis,null);}}}
            literalIndex.removeClauses(literal);}

        cLiterals = literalIndex.get(-literal);
        literalIndex.removeClauses(-literal);
        if(cLiterals != null) { // clauses with literal cause the other literals to become false.
            for(CLiteral cliteral1 : cLiterals) {
                Clause clause = cliteral1.clause;
                Clause newClause = clause.clone(problemSupervisor.nextClauseId(),cliteral1.clausePosition);
                if(trackReasoning) {
                    newClause.inferenceStep = new DisjointnessFalseLiteral(clause,newClause,-literal,inferenceStep);
                    if(monitoring) monitor.print(monitorId,newClause.inferenceStep.toString(symboltable));}
                removeClause(clause);
                insertClause(newClause);}}}




    /** removes all clauses with literal and creates a new Task which does the replacements
     *
     * @param eqClause an equivalence clause
     */
    protected synchronized void integrateEquivalence(Clause eqClause) {
        if(monitoring) {
            monitor.print(monitorId,"Exec: equivalence " + eqClause.toString(0, symboltable));}
        for(int position = 1; position < eqClause.size(); ++position) {
            int literal = eqClause.getLiteral(position);
            for(int i = 1; i <= 2; ++i) {
                literalIndex.forEach(literal,(cLiteral -> {
                    Clause clause = cLiteral.clause;
                    removeClause(clause);
                    queue.add(new Task<>(TaskType.INSERTCLAUSE, clause, null));}));
                literalIndex.removeClauses(literal);
                literal = -literal;}}}


    /** checks if two literals are disjoint
     *
     * @param literal1 a literal
     * @param literal2 a literal
     * @return the CLiteral of literal2 if they are disjoint.
     */
    public CLiteral areDisjoint(int literal1, int literal2) {
        assert(literal1 != -literal2);
        ArrayList<CLiteral> result = disjointnesses.get(literal1);
        if (result == null) {return null;}
        for(CLiteral cliteral : result) {
            if(cliteral.literal == literal2) return cliteral;}
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
                    disjointnesses.computeIfAbsent(cliteral1.literal, k-> new ArrayList<>()).add(cliteral2);}}

    /** removes a clause from the internal lists, except from the literalIndex,.
     *
     * @param clause a clause to be removed.
     */
    private void removeClause(Clause clause) {
        --statistics.clauses;
        clauses.remove(clause);
        clause.setRemoved();
        for(CLiteral cliteral1 : clause) {
            for(CLiteral cliteral2 : clause)
                if(cliteral1 != cliteral2) disjointnesses.get(cliteral1.literal).remove(cliteral2);}}

    /** sorts the literals in the clause.
     * Literals are sorted according to their absolute values.
     *
     * @param clause a new clause
     */
    private void sortLiterals(Clause clause) {
        ArrayList<CLiteral> cliterals = clause.cliterals;
        cliterals.sort(Comparator.comparingInt(clit -> Math.abs(clit.literal)));
        for(int i = 0; i < cliterals.size(); ++i) {
            CLiteral cliteral = cliterals.get(i);
            cliteral.clausePosition = i;}}

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
        int width = Integer.toString(problemSupervisor.clauseCounter).length()+2;
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
            int width = Integer.toString(problemSupervisor.clauseCounter).length() +2;
            string.append("Disjointness Classes of Problem "+problemId+":\n");
            for(Clause clause : clauses) {
                string.append(clause.infoString(width,symboltable)).append("\n");}}
        if(!disjointnesses.isEmpty()) {
            string.append("\nDisjoint Literals of Problem "+problemId+":\n");
            disjointnesses.forEach((literal, disjoints) -> {
                string.append(Symboltable.toString(literal,symboltable)).append(": ");
                for(CLiteral clit : disjoints){
                    string.append(Symboltable.toString(clit.literal  ,symboltable)).append("@").append(clit.clause.id).
                            append(",");}
                string.append("\n");});}
        if(!queue.isEmpty()) {
            string.append("Disjointnesses Queue of Problem "+problemId+":").append(Task.queueToString(queue));}
        return string.toString();}
    }