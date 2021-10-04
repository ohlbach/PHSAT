package Datastructures.TwoLiteral;

import Datastructures.Clauses.BasicClauseList;
import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseType;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Datastructures.Task;
import Datastructures.Theory.*;
import InferenceSteps.*;
import Management.Monitor;
import Management.ProblemSupervisor;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.concurrent.PriorityBlockingQueue;
import java.util.function.Consumer;

/** This class maintains two-literal clauses.
 *  The clause set is kept resolution complete, but minimized as far as possible.
 *  (subsumption, replacement resolution, tautologies are recognized) <br>
 *  The following structures are recognized: <br>
 *  - Derived unit literals (are put into the model and may cause a snowball effect) <br>
 *  - Derived Equivalences are p = q  are put into the EquivalenceClasses <br>
 *  - Derived Disjunctions p != q != r are put into the DisjointnessClasses <br>
 */

public class TwoLitClauses {
    ProblemSupervisor problemSupervisor;

    /** stores the clauses */
    private final ArrayList<TwoLitClause> clauses = new ArrayList<>();

    /** maps a literal to the clauses containing this literal. */
    private final HashMap<Integer,ArrayList<TwoLitClause>> literalIndex = new HashMap<>();

    /** name of the problem */
    private final String problemId;

    /** the global model */
    private final Model model;

    private final Symboltable symboltable;

    /** the global set of equivalence classes */
    private final EquivalenceClasses equivalenceClasses;

    /** the global set of disjointness classes */
    private final DisjointnessClasses disjointnessClasses;

    /** counts various aspects */
    private final TwoLitStatistics statistics;

    /** for monitoring actions */
    private final Monitor monitor;

    /** An identifier for the monitor */
    private final String monitorId;

    /** monitoring is active only if this variable is true */
    public final boolean monitoring;

    /** If true then the origins (basic clause ids) are tracked */
    private final boolean trackReasoning;

    /** An observer is called when a new two-literal clause is derived */
    private final ArrayList<Consumer<TwoLitClause>> observers = new ArrayList<>();

    private enum TaskType {
        TRUELITERAL,  // a new true literal must be integrated
        TWOLITCLAUSE} // a new or old two-literal clause must be simplified and resolved.

    /** A queue of tasks
     * The unit literals are automatically put at the beginning of the queue.
     */
    private final PriorityBlockingQueue<Task<TwoLitClauses.TaskType>> queue =
            new PriorityBlockingQueue<>(10, Comparator.comparingInt(this::getPriority));


    /** gets the priority for the objects in the queue.
     *
     * @param task the task in the queue
     * @return the priority of the objects in the queue.
     */
    private int getPriority(Task<TwoLitClauses.TaskType> task) {
        switch(task.taskType) {
            case TRUELITERAL:   return Integer.MIN_VALUE;
            case TWOLITCLAUSE:  return ((TwoLitClause)task.a).id;}
        return Integer.MAX_VALUE;}

    /** creates a new instance
     *
     * @param problemSupervisor contains all the parameters for the problem.
     */
    public TwoLitClauses(ProblemSupervisor problemSupervisor) {
        problemSupervisor.twoLitClauses = this;
        this.problemSupervisor = problemSupervisor;
        problemId = problemSupervisor.problemId;
        model = problemSupervisor.model;
        symboltable = model.symboltable;
        equivalenceClasses = problemSupervisor.equivalenceClasses;
        disjointnessClasses = problemSupervisor.disjointnessClasses;
        monitor = problemSupervisor.globalParameters.monitor;
        monitoring = monitor != null;
        monitorId = problemId+"-2Lit";
        statistics = new TwoLitStatistics(problemId);
        trackReasoning = problemSupervisor.globalParameters.trackReasoning;
    }

    /** adds an observer which is called when a two-literal clause is inserted
     *
     * @param observer the observer
     */
    public void addObserver(Consumer<TwoLitClause> observer) {
        observers.add(observer);}


    public void configure() {
        model.addObserver(Thread.currentThread(),this::addTrueLiteral);
        equivalenceClasses.addObserver(this::addEquivalence);
    }

    /** This method is started as thread.
     * It reads and executes tasks from the queue
     * It can only be stopped by an interrupt or when a contradiction is found.
     *
     */
    public void run() {
        while(!Thread.interrupted()) {
            try {
                if(monitoring) {
                    String q = "Queue is waiting\n";
                        if(!queue.isEmpty())   q += "  Queue: "+ Task.queueToString(queue) + "\n";
                        if(!clauses.isEmpty()) q += toString("  ",model.symboltable) ;
                    monitor.print(monitorId,q);}

                Task<TaskType> task = queue.take();
                switch (task.taskType) {
                    case TRUELITERAL: integrateTrueLiteral((Integer)task.a); break;
                    case TWOLITCLAUSE: integrateClause((TwoLitClause) task.a,(boolean)task.b); break;}

                if(monitoring && !clauses.isEmpty()) {monitor.print(monitorId,toString("",model.symboltable));}}
            catch(InterruptedException ex) {return;}
            catch(Unsatisfiable unsatisfiable) {
                problemSupervisor.setResult(unsatisfiable,"TwoLitClauses");
                return;}}}

    /** puts a two-literal basicClause into the queue
     *
     * @param basicClause a basic basicClause
     */
    public void addBasicClause(int[] basicClause) {
        assert basicClause.length == 4;
        assert ClauseType.getType(basicClause[1]) == ClauseType.OR;
        if(monitoring) {
            monitor.print(monitorId,"In:   basic basicClause " +
                    BasicClauseList.clauseToString(0,basicClause, model.symboltable));}
        TwoLitClause clause = new TwoLitClause(problemSupervisor.nextClauseId(),basicClause[2],basicClause[3]);
        if(trackReasoning) {clause.inferenceStep = new ClauseCopy(basicClause,clause);}
        synchronized (this) {queue.add(new Task<>(TaskType.TWOLITCLAUSE, clause, false));}}


    /** puts a derived two-literal clause into the queue
     *
     * @param literal1 a literal
     * @param literal2 a literal
     */
    public void addDerivedClause(int literal1, int literal2, InferenceStep inferenceStep) {
        if(monitoring) {
            monitor.print(monitorId,"In:   derived clause " +
                    Symboltable.toString(literal1,model.symboltable) + "," +
                    Symboltable.toString(literal2,model.symboltable));}
        TwoLitClause clause = new TwoLitClause(problemSupervisor.nextClauseId(),literal1,literal2);
        clause.inferenceStep = inferenceStep;
        synchronized (this) {queue.add(new Task<>(TaskType.TWOLITCLAUSE, clause, true));}}

    /** adds a two-literal disjunction to the data structures and performs all simplifications and inferences.
     * If a contradiction is encountered the inconsistencyReporter is called and the method stops.
     *
     * @param clause a TwoLit clause
     * @throws Unsatisfiable if a contradiction is encountered
     */
    protected void integrateClause(TwoLitClause clause, boolean derived) throws Unsatisfiable {
        if(monitoring) {
            monitor.print(monitorId,"Exec: clause " + clause.toString("",model.symboltable));}
        clause = normalizeClause(clause);
        if(clause != null && !isSubsumed(clause)) {
            insertClause(clause);
            if(derived) for(Consumer<TwoLitClause> observer : observers) observer.accept(clause);}}

    /** puts a true literal into the queue.
     *
     * @param literal a true literal
     * @param inferenceStep for making the literal true
     */
    public void addTrueLiteral(int literal, InferenceStep inferenceStep){
        if(monitoring) {
            monitor.print(monitorId,"In:   Unit literal " +
                Symboltable.toString(literal,model.symboltable));}
        synchronized (this) {queue.add(new Task<>(TaskType.TRUELITERAL, literal, inferenceStep));}}


    /** generates all unit resolvents and removes the clauses with the literal from the data structures
     *
     * @param literal a true literal
     * @throws Unsatisfiable if a contradiction is found.
     */
    private void integrateTrueLiteral(int literal) throws Unsatisfiable {
        if(monitoring) {
            monitor.print(monitorId,"Exec: true literal " +
                    Symboltable.toString(literal,model.symboltable));}

        ArrayList<TwoLitClause> clauseList = literalIndex.get(literal); // remove all true literals
        if(clauseList != null) { // remove all true clauses
            for(TwoLitClause clause : clauseList) {clauses.remove(clause);}
            literalIndex.remove(literal);}

        clauseList = literalIndex.get(-literal);
        if(clauseList != null) {   // false literals yield new true literals
            for(TwoLitClause clause : clauseList) {
                replaceTruthValues(clause);
                clauses.remove(clause);}
            literalIndex.remove(-literal);}}


    /** All clauses with literals which must be replaced because of the equivalence clause are put into the queue.
     *
     * @param eqClause an equivalence clause
     */
    public void addEquivalence(Clause eqClause)  {
        if(monitoring) {
            monitor.print(monitorId,"In:     equivalence " + eqClause.toString(2,model.symboltable));}
        for(int position = 1; position < eqClause.size(); ++position) {
            int literal = eqClause.getLiteral(position);
            for(int j = 1; j <= 2; ++j) {
                ArrayList<TwoLitClause> clauses = literalIndex.get(literal);
                if(clauses != null) {
                    for(TwoLitClause clause : clauses) {
                        removeClause(clause);
                        synchronized (this) {queue.add(new Task<>(TaskType.TWOLITCLAUSE,clause,true));}}}
                literal = -literal;}}}


    /** The literals in the clause are replaced by equivalent ones (if necessary).
     * The clause may become a tautology, which is ignored.
     * Ihe clause may become a unit clause, which is put into the model.
     *
     * @param clause a new clause
     * @return null or a normalized version of the clause
     * @throws Unsatisfiable if a contradiction is found.
     */
    protected TwoLitClause normalizeClause(TwoLitClause clause) throws Unsatisfiable {
        clause = replaceEquivalentLiterals(clause);
        if((clause = removeDoubles(clause)) == null)      return null;
        if((clause = replaceTruthValues(clause)) == null) return null;
        return clause;}

    /** replaces the two literals by the representatives of their equivalence class (if necessary)
     *
     * @param clause a new clause
     * @return the old or the new clause with the replacements.
     */
    protected TwoLitClause replaceEquivalentLiterals(TwoLitClause clause) {
        int literal1 = clause.literal1;
        int literal2 = clause.literal2;
        int representative1 = equivalenceClasses.getRepresentative(literal1);
        int representative2 = equivalenceClasses.getRepresentative(literal2);
        if(literal1 == representative1 && literal2 == representative2) return clause;
        TwoLitClause newClause = new TwoLitClause(problemSupervisor.nextClauseId(),representative1,representative2);
        if(trackReasoning) {
            Clause eClause1 = equivalenceClasses.getEClause(literal1);
            Clause eClause2 = equivalenceClasses.getEClause(literal2);
            EquivalenceReplacements2 er = new EquivalenceReplacements2(clause,newClause,literal1,representative1, eClause1,
                    literal2,representative2,eClause2);
            newClause.inferenceStep = er;
            if(monitoring) monitor.print(monitorId,er.toString(symboltable));}
        return newClause;}

    /** checks the clause for double or complementary literals.
     * p,p merges to p\n
     * p,-p is a tautology
     *
     * @param clause a two-lit clause
     * @return null or the clause
     * @throws Unsatisfiable if a contradiction is detected.
     */
    protected TwoLitClause removeDoubles(TwoLitClause clause) throws Unsatisfiable {
        int literal1 = clause.literal1;
        int literal2 = clause.literal2;
        if(literal1 == -literal2) {return null;} // tautology
        if(literal1 == literal2) {
            if(monitoring) {
                monitor.print(monitorId,"Clause " + clause.toString("",model.symboltable) + " is a unit clause " +
                        Symboltable.toString(literal1, model.symboltable));}
            model.add(literal1,clause.inferenceStep,null); // send back to me
            return null;}
        return clause;}

    /** a true literal causes the clause to be true, a false literal causes the other literal to become true.
     *
     * @param clause the original not integrated clause
     * @return null or the unchanged clause
     */
    protected TwoLitClause replaceTruthValues(TwoLitClause clause) throws Unsatisfiable {
        int literal1 = clause.literal1;
        int literal2 = clause.literal2;
        if(model.isTrue(literal1) || model.isTrue(literal2)) return null; // true clause
        for(int i = 1; i <= 2; ++i) {
            if(model.isFalse(literal1)) {
                UnitResolution2 inf = null;
                if(trackReasoning) {
                    inf = new UnitResolution2(clause,literal1,model.getInferenceStep(literal1));
                    if(monitoring) monitor.print(monitorId,inf.toString(symboltable));}
                model.add(literal2,inf,null);
                return null;}
            literal1 = clause.literal2;
            literal2 = clause.literal1;}
        return clause;}

    /** The method looks for equivalences and disjointnesses
     *
     * @param clause a new clause (not yet internalized)
     * @return true if the clause survived.
     */
    private boolean findStructures(TwoLitClause clause) {
        TwoLitClause partner = findClause(-clause.literal1,-clause.literal2);
        if(partner != null) {
            EquivalenceDerivation eqd = null;
            if(trackReasoning) {
                eqd = new EquivalenceDerivation(clause,partner);
                if(monitoring) monitor.print(monitorId,eqd.toString(symboltable));}
            equivalenceClasses.addDerivedEquivalence(clause.literal1, -clause.literal2,eqd);
            removeClause(partner);
            return false;}
        findDisjointnesses(clause);
        return true;}

    /** searches for equivalences and disjointnesses, and
     * inserts the clause into the clauses list and the clausMap
     * Finally it generates all binary resolvents with the clause.
     *
     * @param clause a new two-literal clause
     */
    private void insertClause(TwoLitClause clause){
        if(!findStructures(clause)) return;
        clauses.add(clause);
        literalIndex.computeIfAbsent(clause.literal1, k -> new ArrayList<>()).add(clause);
        literalIndex.computeIfAbsent(clause.literal2, k -> new ArrayList<>()).add(clause);
        statistics.twoLitlauses++;
        addResolvents(clause);}

    /** removes the clause from the internal data structures
     *
     * @param clause a clause to be removed.
     */
    private void removeClause(TwoLitClause clause) {
        literalIndex.get(clause.literal1).remove(clause);
        literalIndex.get(clause.literal2).remove(clause);
        clauses.remove(clause);}

    /** checks if the clause is subsumed by another clause in the list.
     *
     * @param clause two-literal clause to be tested for subsumption
     * @return true if the clause is subsumed by another clause.
     */
    private boolean isSubsumed(TwoLitClause clause) {
        return findClause(clause.literal1,clause.literal2) != null;}

    /** searches for a clause with the given literals
     *
     * @param literal1 a literal
     * @param literal2 a literal
     * @return null or the clause with the literals.
     */
    private TwoLitClause findClause(int literal1, int literal2) {
        ArrayList<TwoLitClause> clauses = literalIndex.get(literal1);
        if(clauses == null) {return null;}
        for(TwoLitClause clause : clauses) {
            if(literal2 == clause.literal1 || literal2 == clause.literal2) {return clause;} }
        return null;}

    /** The method puts all binary resolvents with the clause into the queue.
     *
     * @param clause a new clause
     */
    protected void addResolvents(TwoLitClause clause) {
        int literal1 = clause.literal1;
        int literal2 = clause.literal2;
        for(int i = 1; i <= 2; ++i) {
            ArrayList<TwoLitClause>  parents = literalIndex.get(-literal1);
            if(parents != null) {
                for(TwoLitClause parent : parents) {
                    int literal3 = (literal1 == -parent.literal1) ? parent.literal2 : parent.literal1;
                    if(literal2 == -literal3) continue; // yields a tautology
                    statistics.resolvents++;
                    TwoLitClause resolvent = new TwoLitClause(problemSupervisor.nextClauseId(),literal2,literal3);
                    if(trackReasoning) {
                        BinaryResolution2 inf = new BinaryResolution2(clause,parent,resolvent);
                        resolvent.inferenceStep = inf;
                        if(monitoring) monitor.print(monitorId,inf.toString(symboltable));}
                    synchronized (this) {queue.add(new Task<>(TaskType.TWOLITCLAUSE, resolvent, true));}}}
            literal1 = clause.literal2;
            literal2 = clause.literal1;}}



    /** tries to find a triple of disjoint literals.
     *  Three clauses: p,q  and p,r and q,r mean that -p,-q,-r are disjoint
     *  The triple is inserted into the disjointnessClasses.
     *
     * @param clause a potential partner of the triple
     */
    protected void findDisjointnesses(TwoLitClause clause) { // clause = p,q
        int literal1 = clause.literal1;
        int literal2 = clause.literal2;
        for(int i = 1; i <= 2; ++i) {
            ArrayList<TwoLitClause> clauses = literalIndex.get(literal1);
            if(clauses != null) {
                for(TwoLitClause clause2 : clauses) {
                    if(clause2 != clause) {             // clause2 = p,r
                        int literal3 = (clause2.literal1 == literal1) ? clause2.literal2 : clause2.literal1;
                        TwoLitClause clause3 = findClause(literal2,literal3);
                        if(clause3 != null) {           // clause3 = q,r
                            IntArrayList literals = new IntArrayList();
                            literals.add(-literal1); literals.add(-literal2); literals.add(-literal3);
                            statistics.disjointnesses++;
                            DisjointnessDerivation inf = null;
                            if(trackReasoning) {
                                inf = new DisjointnessDerivation(literals,clause,clause2,clause3);
                                if(monitoring) monitor.print(monitorId,inf.toString(symboltable));}
                            disjointnessClasses.addDerivedDisjoints(literals,inf);
                            return;}}}}
            literal1 = clause.literal2;
            literal2 = clause.literal1;}}

    /** checks if there are no two-literal clauses
     *
     * @return true if there are no clauses.
     */
    public boolean isEmpty() {
        return clauses.isEmpty();}

    /** turns the clauses into a string
     *
     * @param symboltable null or a symboltable
     * @return the clauses as string
     */
    public String toString(String prefix,Symboltable symboltable) {
        StringBuilder string = new StringBuilder();
        string.append(prefix);
        string.append("Two-Literal clauses of problem " + problemId + ":\n");
        for(int i = 0; i < clauses.size(); ++i) {
            string.append(prefix + clauses.get(i).toString("  ", symboltable));
            if(i < clauses.size()-1) string.append("\n");}
        return string.toString();}

    /** turns the clauses into a string using the symboltable
     *
     * @return the clauses as string
     */
    public String toString() {
        return toString("",model.symboltable);}

    /** turns the clauses into a string using numbers
     *
     * @return the clauses as string
     */
    public String toNumbers() {
        return toString("",null);}

    /** turns the clauses and the clauseMap into a string
     *
     * @param symboltable null or a symboltable
     * @return the data structures as string
     */
    public String infoString(Symboltable symboltable) {
        StringBuilder string = new StringBuilder();
        string.append(this);
        string.append("\nClause Map:\n");
        literalIndex.forEach((literal, clauses) -> {
            string.append("  "+Symboltable.toString(literal,symboltable)).append(": ");
            String prefix = " ";
            for(int i = 0; i < clauses.size(); ++i) {
                string.append(clauses.get(i).infoString(prefix,symboltable)).append("\n");
                prefix = "      ";}});
        if(!queue.isEmpty()) {string.append("Queue:").append(Task.queueToString(queue));}
        return string.toString();}

}
