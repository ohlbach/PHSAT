
package Datastructures.Clauses.AllClauses;

import Datastructures.Clauses.InputClauses;
import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseStructure;
import Datastructures.Clauses.Connective;
import Datastructures.Clauses.Simplifiers.ClauseSimplifier;
import Datastructures.Literals.CLiteral;
import Datastructures.Literals.LitAlgorithms;
import Datastructures.Results.Result;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Results.UnsatisfiableClause;
import Datastructures.Symboltable;
import Datastructures.Task;
import Datastructures.Theory.EquivalenceClasses;
import Datastructures.Theory.Model;
import Datastructures.TwoLiteral.TwoLitClauses;
import InferenceSteps.InferenceStep;
import Management.Monitor.Monitor;
import Management.Monitor.MonitorLife;
import Management.ProblemSupervisor;
import Utilities.BucketSortedIndex;
import Utilities.BucketSortedList;

import java.util.*;
import java.util.concurrent.PriorityBlockingQueue;
import java.util.function.IntSupplier;

public class InitializerSimplifier {
    /** supervises the problem solution */
    public final ProblemSupervisor problemSupervisor;
    private final String problemId;
    private final Thread thread;

    public final Model model;
    public EquivalenceClasses equivalenceClasses;
    public final boolean monitoring;
    public final MonitorLife monitor;
    public final String monitorId;
    public final boolean trackReasoning;
    public final Symboltable symboltable;
    public TwoLitClauses twoLitClauses;
    public ClauseSimplifier clauseSimplifier;
    private IntSupplier nextId;

    private int maxClauseLength = 0;
    private int timestamp = 1;
    public ClausesStatistics statistics;

    private final HashMap<Integer,Clause> clauses = new HashMap<>();
    public final BucketSortedIndex<CLiteral> literalIndex;

    // static methods for the initializer part
    public static String helpInitializer() {
        return "Simplifier: parameters:\n" +
                "seed:   for the random number generator      (default: 0)\n";}

    private static final HashSet<String> keysInitializer = new HashSet<>(); // contains the allowed keys in the specification.
    static { // these are the allowed keys in the specification.
        Collections.addAll(keysInitializer, "name", "seed", "flips", "jumps", "type", "solver");}

    /** parses a HashMap with key-value pairs<br>
     *
     * @param parameters  the parameters with the keys "seed", "flips", "jumps"
     * @param errors      for error messages
     * @param warnings    for warnings
     * @return            a list of HashMaps with these keys.
     */
    public static HashMap<String,Object> parseParametersInitializer(HashMap<String,String> parameters, Monitor errors, Monitor warnings) {
        for (String key : parameters.keySet()) {
            if (!keysInitializer.contains(key)) {
                warnings.print("test","Simplifier: unknown key in parameters: " + key + "\n" +
                        "        allowed keys: seed, flips, jumps.\n");
            }
        }
        HashMap<String, Object> list = new HashMap();
        String seeds = parameters.get("seed");
        if (seeds == null) seeds = "0";
        return list;
    }


    // static methods for the simplifier part
    public static String help() {
        return "Simplifier: parameters:\n" +
                "seed:   for the random number generator      (default: 0)\n";}

    private static final HashSet<String> keys = new HashSet<>(); // contains the allowed keys in the specification.
    static { // these are the allowed keys in the specification.
        Collections.addAll(keys, "name", "seed", "flips", "jumps", "type", "solver");}

    /** parses a HashMap with key-value pairs<br>
     *
     * @param parameters  the parameters with the keys "seed", "flips", "jumps"
     * @param errors      for error messages
     * @param warnings    for warnings
     * @return            a list of HashMaps with these keys.
     */
    public static ArrayList<HashMap<String,Object>> parseParameters(HashMap<String,String> parameters, StringBuilder errors, StringBuilder warnings) {
        for (String key : parameters.keySet()) {
            if (!keys.contains(key)) {
                warnings.append("Simplifier: unknown key in parameters: " + key + "\n" +
                        "        allowed keys: seed, flips, jumps.\n");
            }
        }
        ArrayList<HashMap<String, Object>> list = new ArrayList<>();
        String seeds = parameters.get("seed");
        if (seeds == null) seeds = "0";
        return list;
    }



    private enum TaskType {
        TRUELITERAL, EQUIVALENCE, INTEGRATE_CLAUSE, INTEGRATE_SHORTENED_CLAUSE, TWOLITCLAUSE, SIMPLIFYALL, SIMPLIFYOTHERS,
        MRESOLUTION
    }

    /** gets the priority for the objects in the queue.
     *
     * @param task the objects in the queue
     * @return the priority of the objects in the queue.
     */
    private int getPriority(Task<InitializerSimplifier.TaskType> task) {
        switch(task.taskType) {
            case TRUELITERAL:  return 0;
            case INTEGRATE_SHORTENED_CLAUSE: return ((Clause)task.a).size();
            case INTEGRATE_CLAUSE: return ((Clause)task.a).size();
            case EQUIVALENCE:  return 100;
            case SIMPLIFYALL:  return 102;
            case SIMPLIFYOTHERS: return 103 + ((Clause)task.a).size();
            case MRESOLUTION: return 200;
        }
        return -1;}

    /** A queue of newly derived unit literals, newly derived binary equivalences and disjointness clauses
     * The unit literals are automatically put at the beginning of the queue.
     */
    private final PriorityBlockingQueue<Task<InitializerSimplifier.TaskType>> queue =
            new PriorityBlockingQueue<>(10, Comparator.comparingInt(this::getPriority));

    /** constructs a new AllClauses instance
     *
     * @param problemSupervisor    coordinates several solvers.
     */
    public InitializerSimplifier(Integer solverNumber, HashMap<String,Object> solverParameters, ProblemSupervisor problemSupervisor) {
         this.problemSupervisor = problemSupervisor;
        problemSupervisor.clauses = this;
        problemId = problemSupervisor.problemId;
        thread = Thread.currentThread();
        model = problemSupervisor.model;
        symboltable = null; //model.symboltable;
        equivalenceClasses = problemSupervisor.equivalenceClasses;
        twoLitClauses = problemSupervisor.twoLitClauses;
        monitor = null; //problemSupervisor.globalParameters.monitor;
        monitoring = monitor != null;
        monitorId = problemId + "Clauses";
        if(monitoring) nextId = problemSupervisor::nextClauseId;
        statistics = new ClausesStatistics("Clauses");
        trackReasoning = problemSupervisor.globalParameters.trackReasoning;
        literalIndex = new BucketSortedIndex<CLiteral>(model.predicates+1,
                (cLiteral->cLiteral.literal),
                (cLiteral->cLiteral.clause.size()));
        clauseSimplifier = new ClauseSimplifier(this,thread);
    }

    public Result initialize() {
        return null;
    }

    public void integrateBasicClauses() throws Unsatisfiable {
        try{
            InputClauses basicClauses = problemSupervisor.inputClauses;
            for(int[] basicClause : basicClauses.conjunctions) {
                integrateAnd(new Clause(basicClause));}
            for(int[] basicClause : basicClauses.equivalences) {
                equivalenceClasses.addBasicEquivalenceClause(basicClause);}
            model.addObserver(this::addTrueLiteral);
            equivalenceClasses.addObserver(this::addEquivalence);
            for(int[] basicClause : basicClauses.disjunctions) {
                integrateClause(new Clause(basicClause));}
            for(int[] basicClause : basicClauses.atleasts) {
                integrateClause(new Clause(basicClause));}
            for(int[] basicClause : basicClauses.atmosts) {
                integrateClause(new Clause(basicClause));}
            for(int[] basicClause : basicClauses.exacltys) {
                integrateClause(new Clause(basicClause));}
            for(int[] basicClause : basicClauses.intervals) {
                for(Clause clause : Clause.intervalClause(problemSupervisor::nextClauseId,basicClause))
                integrateClause(clause);}}
        catch(Unsatisfiable unsatisfiable) {
             unsatisfiable.problemId   = problemId;
             unsatisfiable.solverClass = InitializerSimplifier.class;
             unsatisfiable.solverId    = "Clauses";
             unsatisfiable.statistic   = statistics;
             unsatisfiable.symboltable = symboltable;
             throw unsatisfiable;}}

    /** puts the literals of an AND-clause into the model
     *
     * @param clause a basic AND-clause [id,type,literal1,...]
     * @throws Unsatisfiable if there are contradictory literals
     */
    private void integrateAnd(Clause clause) throws Unsatisfiable {
        InferenceStep step = clause.inferenceStep;
        for(CLiteral cLiteral : clause.cliterals) {
            model.add(cLiteral.literal,step);}}


    /** simplifies and integrates a clause into the local data structures
     *
     * @param clause a new clause
     * @throws Unsatisfiable if a contradiction is discovered.
     */
    private void integrateClause(Clause clause) throws Unsatisfiable {
        clause = clause.replaceEquivalences(equivalenceClasses,nextId);
        clause = clause.removeComplementaryLiterals(nextId);
        if(clause.structure == ClauseStructure.TAUTOLOGY) return;
        if(clause.connective == Connective.AND) {integrateAnd(clause); return;}
        clause = clause.removeTrueFalseLiterals(model::status,nextId);
        switch(clause.structure) {
            case TAUTOLOGY: return;
            case CONTRADICTORY: throw new UnsatisfiableClause(clause);}
        if(clause.connective == Connective.AND) {integrateAnd(clause); return;}
        if(isSubsumed(clause)) return;
        ArrayList<Clause> clauses = clause.splitOffMultiples(nextId,trackReasoning);
        if(clauses != null) {for(Clause cl : clauses) {integrateClause(cl);}}
        removeSubsumedClauses(clause);
        insertClause(clause);
        if(clause.connective == Connective.OR && clause.size() == 2) {twoLitClauses.addDerivedClause(clause);}
    }

    /** simplifies and integrates a clause into the local data structures
     *
     * @param clause a new clause
     * @throws Unsatisfiable if a contradiction is discovered.
     */
    private void integrateShortenedClause(Clause clause) throws Unsatisfiable {
        if(isSubsumed(clause)) return;
        ArrayList<Clause> clauses = clause.splitOffMultiples(nextId,trackReasoning);
        if(clauses != null) {for(Clause cl : clauses) {integrateClause(cl);}}
        removeSubsumedClauses(clause);
        insertClause(clause);
        if(clause.connective == Connective.OR && clause.size() == 2) {twoLitClauses.addDerivedClause(clause);}
    }

    /** puts a true literal into the queue.
     *
     * @param literal a true literal
     * @param step the inference step for making the literal true
     */
    public void addTrueLiteral(int literal, InferenceStep step){
        if(monitoring) {
            monitor.print(monitorId,"In:   Unit literal " +
            Symboltable.toString(literal,null));}; //model.symboltable));}
        queue.add(new Task<>(InitializerSimplifier.TaskType.TRUELITERAL, literal, step));}

    /** puts an equivalence into the queue
     *
     * @param eClause an equivalence clause
     */
    public void addEquivalence(Clause eClause) {
        if(monitoring) {
            monitor.print(monitorId,
                    "In:   equivalence " + eClause.toString(0,null));}//model.symboltable));}
        queue.add(new Task<>(TaskType.EQUIVALENCE,eClause,null));}


    /** inserts the clause into the local data structures.
     *
     * @param clause  the clause to be inserted.
     */
    protected void insertClause(Clause clause) {
        maxClauseLength = Math.max(maxClauseLength,clause.expandedSize());
        clauses.put(clause.id,clause);
        for(CLiteral cliteral : clause) {literalIndex.add(cliteral);}
        switch(clause.structure) {
            case NEGATIVE: ++statistics.negativeClauses; break;
            case POSITIVE: ++statistics.positiveClauses; break;}
        ++statistics.clauses[clause.connective.ordinal()];}

    /** removes a clause from the internal lists
     *
     * @param clause a clause to be removed
     */
    private void removeClause(Clause clause) {
        switch(clause.structure) {
            case NEGATIVE: --statistics.negativeClauses; break;
            case POSITIVE: --statistics.positiveClauses; break;}
        --statistics.clauses[clause.connective.ordinal()];
        for(CLiteral cliteral : clause) literalIndex.remove(cliteral);
        clauses.remove(clause.id);}

    /** removes the iterator's next clause from the index and the clauses
     *
     * @param cliteral iterator's next cliteral;
     * @param iterator an iterator over the literal index.
     * @return cliteral;
     */
    private CLiteral removeClause(CLiteral cliteral, BucketSortedList<CLiteral>.BucketIterator iterator)  {
        Clause clause = cliteral.clause;
        switch(clause.structure) {
            case NEGATIVE: --statistics.negativeClauses; break;
            case POSITIVE: --statistics.positiveClauses; break;}
        --statistics.clauses[clause.connective.ordinal()];
        iterator.remove();
        for(CLiteral clit : clause) if(clit != cliteral) literalIndex.remove(cliteral);
        clauses.remove(clause.id);
        return cliteral;}


    /** works off the queue
     * new true literals <br>
     * new equivalences <br>
     * new disjointnesses <br>
     */
    public void run() {
        while(!Thread.interrupted()) {
            try {
                if(monitoring) {monitor.print(monitorId,"Queue is waiting:\n"+Task.queueToString(queue));}
                Task<InitializerSimplifier.TaskType> task = queue.take(); // waits if the queue is empty
                if(monitoring) System.out.println("Next Task: " + task.toString());
                switch (task.taskType) {
                    case TRUELITERAL:
                        integrateTrueLiteral((Integer)task.a);
                        break;
                    case EQUIVALENCE:
                        integrateEquivalence((Clause)task.a);
                        break;
                    case INTEGRATE_CLAUSE:
                        integrateClause((Clause)task.a);
                        break;
                    case INTEGRATE_SHORTENED_CLAUSE:
                        integrateShortenedClause((Clause)task.a);
                        break;
                }
            /*    if(clausesFinished && queue.isEmpty()) {
                    if(clauses.isEmpty()) throw new Satisfiable(model);
                    checkSatisfiabiliy();
                    checkPurity();}
            */
            }
            catch(InterruptedException ex) {return;}
            catch(Result result) {problemSupervisor.announceResult(result,"AllClauses"); return;}}}

    private static int[] quantifierTypes = new int[]{
            Connective.ATLEAST.ordinal(),Connective.ATMOST.ordinal(),Connective.EXACTLY.ordinal()};

    /** applies a true literal to all clauses.
     * Clauses containing the literal are removed.<br>
     * In clauses containing the negated literal, this literal is removed.<br>
     * For the resulting clause a INSERTCLAUSE Task is generated.
     *
     * @param literal a true literal
     * @throws Unsatisfiable if a contradiction is found.
     */
    private void integrateTrueLiteral(int literal) throws Unsatisfiable {
        BucketSortedList<CLiteral>.BucketIterator iterator;
        for(int sign = -1; sign <= +1; sign += 2) {
            iterator = literalIndex.popIterator(sign*literal);
            while (iterator.hasNext()) {
                CLiteral cLiteral = iterator.next();
                removeClause(cLiteral, iterator);
                Clause clause = cLiteral.clause;
                Clause clause1 = clause.removeTrueFalseLiterals(model::status,nextId);
                if(clause1.structure == ClauseStructure.TAUTOLOGY) continue;
                if(clause1.connective == Connective.AND) {
                    addTrueLiteral(clause1.getLiteral(0),clause1.inferenceStep);
                    continue;}
                queue.add(new Task<>(TaskType.INTEGRATE_SHORTENED_CLAUSE, clause,null));}
            literalIndex.pushIterator(sign*literal, iterator);}}

    /** checks subsumption and intersection between an old clause and a new clause
     * The old clause subsumes the new one: ignore the new one <br>
     * @param clause a new clause (not yet integrated)
     * @return null (clause subsumed) or, the possibly cloned or modified, clause
     */

    protected boolean isSubsumed(Clause clause)  {
        Clause subsumer = LitAlgorithms.isSubsumed(clause,literalIndex,timestamp);
        timestamp += maxClauseLength+1;
        if(subsumer == null) return false;
        ++statistics.forwardSubsumptions;

        if(trackReasoning) {
            InferenceStep step = new InfSubsumption(subsumer,clause);
            clause.inferenceStep = step;
            if(monitoring) monitor.print(monitorId, step.toString(symboltable));}
        return true;}

    private final ArrayList<Clause> subsumedClauses = new ArrayList<>();

    /** removes all clauses which are subsumed by the given clauses.
     *
     * @param subsumer a potential subsumer
     */
    protected void removeSubsumedClauses(Clause subsumer) {
        LitAlgorithms.subsumes(subsumer,literalIndex,timestamp, subsumedClauses);
        timestamp += maxClauseLength+1;
        for(Clause subsumed : subsumedClauses) {
            removeClause(subsumed);
            ++statistics.backwardSubsumptions;
            if(trackReasoning) {
                InferenceStep step = new InfSubsumption(subsumer,subsumed);
                subsumed.inferenceStep = step;
                if(monitoring) monitor.print(monitorId, step.toString(symboltable));}}}

    /** integrates the consequences of an equivalence clause.
     * All clauses containing a literal of the equivalence clause are removed and an
     * INTEGRATE_CLAUSE task is generated.
     *
     * @param eClause an equivalence clause
     */
    protected void integrateEquivalence(Clause eClause) {
        assert eClause.connective == Connective.EQUIV;
        BucketSortedList<CLiteral>.BucketIterator iterator;
        for(int i = 1; i < eClause.size(); ++i) {
            int literal = eClause.getLiteral(i);
            for(int sign = -1; sign <= +1; sign +=2) {
                iterator = literalIndex.popIterator(sign*literal);
                while (iterator.hasNext()) {
                    CLiteral cLiteral = iterator.next();
                    removeClause(cLiteral, iterator);
                    queue.add(new Task<>(TaskType.INTEGRATE_CLAUSE, cLiteral.clause,null));}
                literalIndex.pushIterator(sign*literal,iterator);}}}

}

