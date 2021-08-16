package Datastructures.TwoLiteral;

import Datastructures.Clauses.BasicClauseList;
import Datastructures.Clauses.ClauseType;
import Datastructures.Results.Result;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Datastructures.Theory.*;
import Management.Monitor;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import javafx.util.Pair;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.concurrent.PriorityBlockingQueue;
import java.util.function.Consumer;

import static Utilities.Utilities.joinIntArraysSorted;

/** This class maintains two-literal clauses.
 *  The clause set is kept resolution complete, but minimized as far as possible.
 *  (subsumption, replacement resolution, tautologies are recognized) <br>
 *  The following structures are recognized: <br>
 *  - Derived unit literals (are put into the model and may cause a snowball effect) <br>
 *  - Derived Equivalences are p = q  are put into the EquivalenceClasses <br>
 *  - Derived Disjunctions p != q != r are put into the DisjointnessClasses <br>
 */

public class TwoLitClauses {

    /** for enumerating the clauses */
    int counter = 0;

    ArrayList<TwoLitClause> clauses = new ArrayList<>();

    /** maps a literal to the clauses containing this literal. */
    HashMap<Integer,ArrayList<TwoLitClause>> clauseMap = new HashMap<>();

    String problemId;

    /** the global model */
    Model model;

    /** the global set of equivalence classes */
    EquivalenceClasses equivalenceClasses;

    /** the global set of disjointness classes */
    DisjointnessClasses disjointnessClasses;

    /** counts various aspects */
    TwoLitStatistics statistics;

    /** The current thread */
    public Thread thread;

    private Thread supervisorThread;

    /** for monitoring actions */
    public Monitor monitor;

    private String monitorId;

    /** if monitoring is active */
    public boolean monitoring;

    /** Unsatisfiable if a contradiction is found */
    public Result result = null;

    private ArrayList<Consumer<TwoLitClause>> observers = new ArrayList<>();

    /** A queue of newly derived unit literals, newly derived binary disjointnesses and basic disjointness clauses
     * The unit literals are automatically put at the beginning of the queue.
     */
    private final PriorityBlockingQueue<Pair<Object,IntArrayList>> queue =
            new PriorityBlockingQueue<>(10,(Pair<Object,IntArrayList> o1, Pair<Object,IntArrayList> o2) -> {
                Class cl = o1.getKey().getClass();
                if(cl == Integer.class)   {return 0;}    // true literal
                if(cl == int[].class)     {return 1;}    // basic clause
                 return 3;});                             // Equivalence class

    public TwoLitClauses(Model model, EquivalenceClasses equivalenceClasses,
                        DisjointnessClasses disjointnessClasses, String problemId, Monitor monitor, Thread supervisorThread) {
        this.problemId = problemId;
        this.model = model;
        this.equivalenceClasses = equivalenceClasses;
        this.disjointnessClasses = disjointnessClasses;
        this.monitor = monitor;
        monitoring = monitor != null;
        monitorId = problemId+"-TwoLit";
        statistics = new TwoLitStatistics(problemId);
        thread = Thread.currentThread();
        this.supervisorThread = supervisorThread;
    }

    /** adds an observer which is called when a two-literal clause is inserted
     *
     * @param observer the observer
     */
    public void addObserver(Consumer<TwoLitClause> observer) {
        observers.add(observer);}

    /** removes the observer
     *
     * @param observer
     */
    public void removeObserver(Consumer<TwoLitClause> observer) {
        observers.remove(observer);}


    /** This method is started as thread.
     * It reads and executes tasks from the queue
     * It can only be stopped by an interrupt or when a contradiction is found.
     *
     */
    public void run() {
        thread = Thread.currentThread();
        model.addObserver(thread,
                (Integer literal, IntArrayList origins) -> addTrueLiteral(literal,origins));

        equivalenceClasses.addEquivalenceObserver((representative, literal, origins) ->
                addEquivalence(representative,literal,origins));

        while(!Thread.interrupted()) {
            try {
                if(monitoring) {monitor.print(monitorId,"Queue is waiting");}
                Pair<Object, IntArrayList> object = queue.take();
                Object key = object.getKey();
                Class cl = key.getClass();
                IntArrayList origins = object.getValue();
                if(cl == Integer.class) {integrateTrueLiteral((Integer)key,origins); continue;}
                if(cl == int[].class) {integrateBasicClause((int[])key,null); continue;}
                 if(cl == Pair.class) {
                    integrateEquivalence((Integer)((Pair<?, ?>) key).getKey(), (Integer)((Pair<?, ?>) key).getValue(), origins);
                    continue;}
                if(monitoring) {
                    monitor.print(monitorId,toString("",model.symboltable));}}
            catch(InterruptedException ex) {return;}
            catch(Unsatisfiable unsatisfiable) {
                result = unsatisfiable;
                supervisorThread.interrupt();}}}

    /** puts a two-literal clause into the queue
     *
     * @param clause
     */
    public void addBasicClause(int[] clause) {
        assert clause.length == 4;
        assert ClauseType.getType(clause[1]) == ClauseType.OR;
        if(monitoring) {
            monitor.print(monitorId,"In:   basic clause" +
                    BasicClauseList.clauseToString(0,clause, model.symboltable));}
        queue.add(new Pair(clause,null)); }


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
        int[] clause = new int[]{-1,ClauseType.OR.ordinal(),literal1,literal2};
        queue.add(new Pair(clause,origins));}

    /** adds a two-literal disjunction to the data structures and performs all simplifications and inferences.
     * If a contradiction is encountered the inconsistencyReporter is called and the method stops.
     *
     * @param basicClause
     * @throws Unsatisfiable if a contradiction is encountered
     */
    protected void integrateBasicClause(int[] basicClause, IntArrayList origins) throws Unsatisfiable {
        if(monitoring) {
            monitor.print(monitorId,"Exec: clause " +
                    BasicClauseList.clauseToString(0,basicClause, model.symboltable));}
        if(origins == null && basicClause[0] >= 0) {
            origins = new IntArrayList(); origins.add(basicClause[0]);}
        TwoLitClause clause = new TwoLitClause(++counter,basicClause[2],basicClause[3],origins);
        if(normalizeClause(clause) && !isSubsumed(clause)) insertClause(clause);}

    /** puts a true literal into the queue.
     *
     * @param literal a true literal
     * @param origins the basic clause ids causing the derivation of the true literal.
     */
    public void addTrueLiteral(int literal, IntArrayList origins) {
        if(monitoring) {
            monitor.print(monitorId,"In:   true literal " +
                Symboltable.toString(literal,model.symboltable));}
        queue.add(new Pair(literal,origins));}


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
        for(int i = 0; i < clauses.size(); ++i) {
            TwoLitClause clause = clauses.get(i);
            if(clause.literal1 == literal || clause.literal2 == literal) {
                clauses.remove(i--); continue;}
            if(clause.literal1 == -literal) {
                model.add(clause.literal2,joinIntArraysSorted(clause.origins,origins),null);
                clauses.remove(i--); continue;}
            if(clause.literal2 == -literal) {
                model.add(clause.literal1,joinIntArraysSorted(clause.origins,origins),null);
                clauses.remove(i--); continue;}}
        clauseMap.remove(literal);
        clauseMap.remove(-literal);}

    /** puts an equivalence into the queue
     *
     * @param representative a literal
     * @param literal        a literal
     * @param origins        the basic clause ids for the equivalence.
     */
    public void addEquivalence(int representative, int literal, IntArrayList origins) {
        if(monitoring) {
        monitor.print(monitorId,"In:   equivalence" +
                Symboltable.toString(representative,model.symboltable) + " = " +
                Symboltable.toString(literal,model.symboltable));}
        queue.add(new Pair<>(new Pair(representative,literal),origins));}

    /** replaces in all clauses the 'literal' with 'representative
     *
     * @param representative a literal
     * @param literal        a literal
     * @param origins        the basic clause ids for the equivalence
     * @throws Unsatisfiable if a contradiction is found.
     */
    protected void integrateEquivalence(int representative, int literal, IntArrayList origins) throws Unsatisfiable {
        if(monitoring) {
            monitor.print(monitorId,"Exec:   equivalence" +
                    Symboltable.toString(representative,model.symboltable) + " = " +
                    Symboltable.toString(literal,model.symboltable));}
        for(int j = 1; j <= 2; ++j) {
            ArrayList<TwoLitClause> clausesLit = clauseMap.get(literal);
            if(clausesLit != null) {
                for(int i = 0; i < clausesLit.size(); ++i) {
                    TwoLitClause clause = clausesLit.get(i);
                    switch(replaceLiteral(clause,representative,literal,origins)) {
                        case 1: // tautology
                            clausesLit.remove(i--);
                            clauses.remove(clause);
                            continue;
                        case 2:  // double literal
                            if(monitoring) {
                                monitor.print(monitorId,"Double literal " +
                                        Symboltable.toString(clause.literal1,model.symboltable));}
                            model.add(clause.literal1,joinIntArraysSorted(clause.origins,origins),null);
                            clausesLit.remove(i--);
                            clauses.remove(clause);
                            continue;}}
                clauseMap.remove(literal);}
            literal = -literal;
            representative = -representative;}
    }

    /** replaces the literal by the equivalent representative in the clause,
     *  and the corresponding data structures
     *
     * @param clause          a clause containing literal
     * @param representative  a literal
     * @param literal         a literal
     * @param origins         the clause ids of the equivalence
     * @return 1: tautology, 2: double literal, 3: none of these
     */
    private int replaceLiteral(TwoLitClause clause, int representative, int literal, IntArrayList origins) {
        if(clause.literal1 == literal) {clause.literal1 = representative;}
        else {clause.literal2 = representative;}
        if(clause.literal1 == -clause.literal2) return 1;
        if(clause.literal1 ==  clause.literal2) return 2;

        clause.origins = joinIntArraysSorted(clause.origins,origins);
        ArrayList<TwoLitClause> clausesLit = clauseMap.get(representative);
        if(clausesLit == null) {
            clausesLit = new ArrayList<>();
            clauseMap.put(representative,clausesLit);}
        clausesLit.add(clause);
        return 3;}

    /** The literals in the clause are replaced by equivalent ones (if necessary).
     * The clause bay become a tautology, which is ignored.
     * Ihe clause may become a unit clause, which is put into the model.
     * The clause may replacement resolve with another two-literal clause,
     * which is put into the model.
     *
     * @param clause a new clause
     * @return true if the clause survived (no tautology and no unit clause).
     * @throws Unsatisfiable if a contradiction is found.
     */
    protected boolean normalizeClause(TwoLitClause clause) throws Unsatisfiable {
        replaceEquivalentLiterals(clause);

        int literal1 = clause.literal1;
        int literal2 = clause.literal2;

        if(literal1 == -literal2) {return false;} // tautology

        if(literal1 == literal2) {                // merge
            model.add(literal1,clause.origins,null); // send back to me
            return false;}

        switch(model.status(literal1)) {
            case +1: return false;
            case -1: model.add(literal2,
                        joinIntArraysSorted(clause.origins, model.getOrigin(-literal1)),null); // send back to me
                return false;}

        switch(model.status(literal2)) {
            case +1: return false;
            case -1: model.add(literal1,
                        joinIntArraysSorted(clause.origins, model.getOrigin(-literal2)),null); // send back to me
                return false;}
        return true;}

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
        ArrayList<TwoLitClause> clausesLit = clauseMap.get(clause.literal1);
        if(clausesLit == null) {clausesLit = new ArrayList<>(); clauseMap.put(clause.literal1,clausesLit);}
        clausesLit.add(clause);

        clausesLit = clauseMap.get(clause.literal2);
        if(clausesLit == null) {clausesLit = new ArrayList<>(); clauseMap.put(clause.literal2,clausesLit);}
        clausesLit.add(clause);

        statistics.twoLitlauses++;
        for(Consumer<TwoLitClause> observer : observers) observer.accept(clause);
        addResolvents(clause);}

    /** removes the clause from the internal data structures
     *
     * @param clause a clause to be removed.
     */
    private void removeClause(TwoLitClause clause) {
        clauseMap.get(clause.literal1).remove(clause);
        clauseMap.get(clause.literal2).remove(clause);
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
     * @param literal1
     * @param literal2
     * @return null or the clause with the literals.
     */
    private TwoLitClause findClause(int literal1, int literal2) {
        ArrayList<TwoLitClause> clauses = clauseMap.get(literal1);
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
        int literal1 = clause.literal1;
        int literal2 = clause.literal2;
        for(int i = 1; i <= 2; ++i) {
            ArrayList<TwoLitClause>  parents = clauseMap.get(-literal1);
            if(parents != null) {
                for(TwoLitClause parent : parents) {
                    int literal3 = (literal1 == -parent.literal1) ? parent.literal2 : parent.literal1;
                    if(literal2 == -literal3) continue;
                    if(literal2 == literal3) {
                        statistics.unitClauses++;
                        if(monitoring) {
                            monitor.print(monitorId,"Clause " +
                                    clause.toString("",model.symboltable) + " and " +
                                    parent.toString("",model.symboltable) + " yields " +
                                    Symboltable.toString(literal2,model.symboltable));}
                        model.add(literal2,joinIntArraysSorted(clause.origins,parent.origins),null);
                        continue;}
                    statistics.resolvents++;
                    addDerivedClause(literal2,literal3,joinIntArraysSorted(clause.origins,parent.origins));}}
            literal1 = clause.literal2;
            literal2 = clause.literal1;}}


    /** replaces the two literals by the representatives of their equivalence class (if necessary)
     *
     * @param clause a new clause
     */
    protected void replaceEquivalentLiterals(TwoLitClause clause) {
        int literal1 = clause.literal1;
        int literal2 = clause.literal2;
        int representative1 = equivalenceClasses.getRepresentative(literal1);
        int representative2 = equivalenceClasses.getRepresentative(literal2);
        if(literal1 != representative1) {
            clause.literal1 = representative1;
            clause.origins = joinIntArraysSorted(clause.origins,equivalenceClasses.getOrigins(representative1));}
        if(literal2 != representative2) {
            clause.literal2 = representative2;
            clause.origins = joinIntArraysSorted(clause.origins,equivalenceClasses.getOrigins(representative2));}}



    /** tries to find a triple of disjoint literals.
     *  Three clauses: p,q  and p,r and q,r mean that -p,-q,-r are disjoint
     *  The triple is inserted into the disjointnessClasses.
     *
     * @param clause a potential partner of the triple
     */
    protected void findDisjointnesses(TwoLitClause clause) {
        int literal1 = clause.literal1;
        int literal2 = clause.literal2;
        int predicate1 = Math.abs(literal1);
        int predicate2 = Math.abs(literal2);
        clauseMap.forEach((literal,clauses) -> {
            int predicate = Math.abs(literal);
            if(predicate1 == predicate || predicate2 == predicate) return;
            TwoLitClause clause2 = findClause(literal1,literal);
            if(clause2 == null) return;
            TwoLitClause clause3 = findClause(literal2,literal);
            if(clause3 == null) return;
            IntArrayList literals = new IntArrayList();
            literals.add(-literal1); literals.add(-literal2); literals.add(-literal);
            IntArrayList origins = joinIntArraysSorted(clause.origins,clause2.origins);
            origins = joinIntArraysSorted(origins,clause3.origins);
            statistics.disjointnesses++;
            if(monitoring) {
                monitor.print(monitorId,"Disjointness found: " +
                        Symboltable.toString(-literal1,model.symboltable) + " != " +
                        Symboltable.toString(-literal2,model.symboltable)+ " != " +
                        Symboltable.toString(-literal,model.symboltable));}
            disjointnessClasses.addDerivedDisjoints(literals,origins);});
    }

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
        string.append(toString());
        string.append("\nClause Map:\n");
        clauseMap.forEach((literal,clauses) -> {
            string.append("  "+Symboltable.toString(literal,symboltable)).append(": ");
            String prefix = " ";
            for(int i = 0; i < clauses.size(); ++i) {
                string.append(clauses.get(i).toString(prefix,symboltable)).append("\n");
                prefix = "      ";}});
        return string.toString();}

}
