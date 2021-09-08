package Datastructures.TwoLiteral;

import Datastructures.Clauses.BasicClauseList;
import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseType;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Datastructures.Task;
import Datastructures.Theory.*;
import Management.Monitor;
import Management.ProblemSupervisor;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.concurrent.PriorityBlockingQueue;
import java.util.function.Consumer;

import static Utilities.Utilities.joinIntArraysSorted;
import static Utilities.Utilities.stdoutLogger;

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

    /** for enumerating the clauses */
    private int counter = 0;

    /** stores the clauses */
    private final ArrayList<TwoLitClause> clauses = new ArrayList<>();

    /** maps a literal to the clauses containing this literal. */
    private final HashMap<Integer,ArrayList<TwoLitClause>> literalIndex = new HashMap<>();

    /** name of the problem */
    private final String problemId;

    /** the global model */
    private final Model model;

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

    /** removes the observer
     *
     * @param observer for observing bew two-literal clauses
     */
    public void removeObserver(Consumer<TwoLitClause> observer) {
        observers.remove(observer);}

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
                    case TRUELITERAL: integrateTrueLiteral((Integer)task.a,task.origins); break;
                    case TWOLITCLAUSE: integrateClause((TwoLitClause) task.a,(boolean)task.b); break;}

                if(monitoring && !clauses.isEmpty()) {monitor.print(monitorId,toString("",model.symboltable));}}
            catch(InterruptedException ex) {return;}
            catch(Unsatisfiable unsatisfiable) {
                problemSupervisor.setResult(unsatisfiable,"TwoLitClauses");
                return;}}}

    /** puts a two-literal clause into the queue
     *
     * @param clause a basic clause
     */
    public void addBasicClause(int[] clause) {
        assert clause.length == 4;
        assert ClauseType.getType(clause[1]) == ClauseType.OR;
        if(monitoring) {
            monitor.print(monitorId,"In:   basic clause " +
                    BasicClauseList.clauseToString(0,clause, model.symboltable));}
        TwoLitClause clause2 = new TwoLitClause(++counter,clause[2],clause[3], IntArrayList.wrap(new int[]{clause[0]}));
        synchronized (this) {queue.add(new Task<>(TaskType.TWOLITCLAUSE, null, clause2, false));}}


    /** puts a derived two-literal clause into the queue
     *
     * @param literal1 a literal
     * @param literal2 a literal
     * @param origins the basic clause ids causing the derivation of this literal.
     */
    public void addDerivedClause(int literal1, int literal2, IntArrayList origins) {
        if(monitoring) {
            monitor.print(monitorId,"In:   derived clause " +
                    Symboltable.toString(literal1,model.symboltable) + "," +
                    Symboltable.toString(literal2,model.symboltable));}
        TwoLitClause clause = new TwoLitClause(++counter,literal1,literal2,origins);
        synchronized (this) {queue.add(new Task<>(TaskType.TWOLITCLAUSE, null, clause, true));}}

    /** adds a two-literal disjunction to the data structures and performs all simplifications and inferences.
     * If a contradiction is encountered the inconsistencyReporter is called and the method stops.
     *
     * @param clause a TwoLit clause
     * @throws Unsatisfiable if a contradiction is encountered
     */
    protected void integrateClause(TwoLitClause clause, boolean derived) throws Unsatisfiable {
        if(monitoring) {
            monitor.print(monitorId,"Exec: clause " + clause.toString("",model.symboltable));}

        if(normalizeClause(clause) && !isSubsumed(clause)) {
            insertClause(clause);
            if(derived) for(Consumer<TwoLitClause> observer : observers) observer.accept(clause);}}

    /** puts a true literal into the queue.
     *
     * @param literal a true literal
     * @param origins the basic clause ids causing the derivation of the true literal.
     */
    public void addTrueLiteral(int literal, IntArrayList origins){
        if(monitoring) {
            monitor.print(monitorId,"In:   Unit literal " +
                Symboltable.toString(literal,model.symboltable));}
        synchronized (this) {queue.add(new Task<>(TaskType.TRUELITERAL, origins, literal, null));}
        System.out.println("QQQ " + Task.queueToString(queue));}


    /** generates all unit resolvents and removes the clauses with the literal from the data structures
     *
     * @param literal a true literal
     * @param origins the list of basic clause ids for the truth of the literal
     * @throws Unsatisfiable if a contradiction is found.
     */
    private void integrateTrueLiteral(int literal, IntArrayList origins) throws Unsatisfiable {
        if(monitoring) {
            monitor.print(monitorId,"Exec: true literal " +
                    Symboltable.toString(literal,model.symboltable));}

        ArrayList<TwoLitClause> clauseList = literalIndex.get(literal); // remove all true literals
        if(clauseList != null) { // remove all true clauses
            for(TwoLitClause clause : clauseList) {clauses.remove(clause);}
            literalIndex.remove(literal);}

        literal = -literal;
        clauseList = literalIndex.get(literal);
        if(clauseList != null) {   // false literals yield new true literals
            for(TwoLitClause clause : clauseList) {
                int literal2 = (clause.literal1 == literal) ? clause.literal2 : clause.literal1;
                if(monitoring) {
                    monitor.print(monitorId,"False literal " +
                            Symboltable.toString(literal, model.symboltable) + " and clause " +
                            clause.toString("", model.symboltable) + " yields new unit literal " +
                            Symboltable.toString(literal2, model.symboltable));}
                model.add(literal2,joinIntArraysSorted(clause.origins,origins),null);
                clauses.remove(clause);}
            literalIndex.remove(literal);}}


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
                        synchronized (this) {queue.add(new Task<>(TaskType.TWOLITCLAUSE,null,clause,true));}}}
                literal = -literal;}}}


    /** The literals in the clause are replaced by equivalent ones (if necessary).
     * The clause bay become a tautology, which is ignored.
     * Ihe clause may become a unit clause, which is put into the model.
     *
     * @param clause a new clause
     * @return true if the clause survived (no tautology and no unit clause).
     * @throws Unsatisfiable if a contradiction is found.
     */
    protected boolean normalizeClause(TwoLitClause clause) throws Unsatisfiable {
        IntArrayList origins = null;
        replaceEquivalentLiterals(clause);

        int literal1 = clause.literal1;
        int literal2 = clause.literal2;

        if(literal1 == -literal2) {return false;} // tautology

        if(literal1 == literal2) {
            if(monitoring) {
                monitor.print(monitorId,"Clause " + clause.toString("",model.symboltable) + " is a unit clause " +
                        Symboltable.toString(literal1, model.symboltable));}
            model.add(literal1,clause.origins,null); // send back to me
            return false;}

        for(int i = 1; i <= 2; ++i) {
            switch(model.status(literal1)) {
                case +1: return false;
                case -1:
                    if(monitoring) {
                        monitor.print(monitorId,"Clause " + clause.toString("",model.symboltable) + ": false literal " +
                                Symboltable.toString(literal1, model.symboltable) + " yields  unit clause " +
                                Symboltable.toString(literal2, model.symboltable));}
                    if(trackReasoning) origins = joinIntArraysSorted(clause.origins, model.getOrigin(-literal1));
                    model.add(literal2, origins,null); // send back to me
                    return false;} // clause not needed
            literal1 = clause.literal2;
            literal2 = clause.literal1;}

        return true;}

    /** replaces the two literals by the representatives of their equivalence class (if necessary)
     *
     * @param clause a new clause
     */
    private void replaceEquivalentLiterals(TwoLitClause clause) {
        int literal1 = clause.literal1;
        int literal2 = clause.literal2;
        int representative1 = equivalenceClasses.getRepresentative(literal1);
        int representative2 = equivalenceClasses.getRepresentative(literal2);
        if(literal1 != representative1) {
            clause.literal1 = representative1;
            if(trackReasoning) clause.joinOrigins(equivalenceClasses.getOrigins(representative1));}
        if(literal2 != representative2) {
            clause.literal2 = representative2;
            if(trackReasoning) clause.joinOrigins(equivalenceClasses.getOrigins(representative2));}}


    /** The method looks for equivalences and disjointnesses
     *
     * @param clause a new clause (not yet internalized)
     * @return true if the clause survived.
     */
    private boolean findStructures(TwoLitClause clause) {
        TwoLitClause partner = findEquivalence(clause);
        if(partner != null) {
            equivalenceClasses.addDerivedEquivalence(clause.literal1, -clause.literal2,
                    joinIntArraysSorted(clause.origins,partner.origins));
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
    private void insertClause(TwoLitClause clause) throws Unsatisfiable{
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

    /** checks if the clause is together with an existing clause denote an equivalence.
     *  a new clause p,q together with an old clause -p,-q mean p == -q
     *
     * @param clause two-literal clause to be tested for equivalence
     * @return null or the partner clause
     */
    private TwoLitClause findEquivalence(TwoLitClause clause) {
        TwoLitClause partner = findClause(-clause.literal1,-clause.literal2);
        if(partner == null) return null;
        statistics.equivalences++;
        if(monitoring) {
            monitor.print(monitorId,"Equivalence found: " +
                    Symboltable.toString(clause.literal1,model.symboltable) + " = " +
                    Symboltable.toString(-clause.literal2,model.symboltable));}
        return partner;}


    /** The method puts all binary resolvents with the clause into the queue.
     *
     * @param clause a new clause
     */
    protected void addResolvents(TwoLitClause clause) throws Unsatisfiable {
        IntArrayList origins = null;
        int literal1 = clause.literal1;
        int literal2 = clause.literal2;
        for(int i = 1; i <= 2; ++i) {
            ArrayList<TwoLitClause>  parents = literalIndex.get(-literal1);
            if(parents != null) {
                for(TwoLitClause parent : parents) {
                    int literal3 = (literal1 == -parent.literal1) ? parent.literal2 : parent.literal1;
                    if(literal2 == -literal3) continue; // yields a tautology
                    if(literal2 == literal3) {
                        statistics.unitClauses++;
                        if(monitoring) {
                            monitor.print(monitorId,"Clauses\n" +
                                    clause.toString("  ",model.symboltable) + " and\n" +
                                    parent.toString("  ",model.symboltable) + " yield unit clause: " +
                                    Symboltable.toString(literal2,model.symboltable));}
                        if(trackReasoning) origins = joinIntArraysSorted(clause.origins,parent.origins);
                        model.add(literal2,origins,null);
                        return;}  // clause becomes true anyway
                    statistics.resolvents++;
                    if(trackReasoning) origins = joinIntArraysSorted(clause.origins,parent.origins);
                    TwoLitClause resolvent = new TwoLitClause(++counter,literal2,literal3,origins);
                    if(monitoring) {
                        monitor.print(monitorId,"Clauses \n" +
                                clause.toString("  ",model.symboltable) + " and\n" +
                                parent.toString("  ",model.symboltable) + " yields resolvent\n " +
                                resolvent.toString("  ",model.symboltable));}
                    synchronized (this) {queue.add(new Task<>(TaskType.TWOLITCLAUSE, null, resolvent, true));}}}
            literal1 = clause.literal2;
            literal2 = clause.literal1;}}



    /** tries to find a triple of disjoint literals.
     *  Three clauses: p,q  and p,r and q,r mean that -p,-q,-r are disjoint
     *  The triple is inserted into the disjointnessClasses.
     *
     * @param clause a potential partner of the triple
     */
    protected void findDisjointnesses(TwoLitClause clause) { // clause = p,q
        IntArrayList origins = null;
        int literal1 = clause.literal1;
        int literal2 = clause.literal2;
        for(int i = 1; i <= 2; ++i) {
            ArrayList<TwoLitClause> clauses = literalIndex.get(literal1);
            if(clauses != null) {
                for(TwoLitClause clause2 : clauses) {
                    if(clause2 != clause) {             // clause2 = p,r
                        int literal3 = (clause.literal1 == literal1) ? literal2 : literal1;
                        TwoLitClause clause3 = findClause(literal2,literal3);
                        if(clause3 != null) {           // clause3 = q,r
                            IntArrayList literals = new IntArrayList();
                            literals.add(-literal1); literals.add(-literal2); literals.add(-literal3);
                            if(trackReasoning) {origins = joinIntArraysSorted(clause.origins,clause2.origins);
                                                origins = joinIntArraysSorted(origins,clause3.origins);}
                            statistics.disjointnesses++;
                            if(monitoring) {
                                monitor.print(monitorId,"Disjointness found: " +
                                    Symboltable.toString(-literal1,model.symboltable) + " != " +
                                    Symboltable.toString(-literal2,model.symboltable)+ " != " +
                                    Symboltable.toString(-literal3,model.symboltable));}
                            disjointnessClasses.addDerivedDisjoints(literals,origins);
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
                string.append(clauses.get(i).toString(prefix,symboltable)).append("\n");
                prefix = "      ";}});
        if(!queue.isEmpty()) {string.append("Queue:").append(Task.queueToString(queue));}
        return string.toString();}

}
