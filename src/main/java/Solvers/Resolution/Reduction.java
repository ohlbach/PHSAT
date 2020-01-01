package Solvers.Resolution;

import Coordinator.Tasks.Task;
import Coordinator.Tasks.TaskQueue;
import Datastructures.Clauses.Clause;
import Datastructures.Literals.CLiteral;
import Datastructures.Literals.LitAlgorithms;
import Datastructures.Results.*;
import Datastructures.Statistics.Statistic;
import Datastructures.Symboltable;
import Datastructures.Theory.EquivalenceClasses;
import Datastructures.Theory.Transformers;
import Management.ProblemSupervisor;
import Solvers.Solver;
import Utilities.BucketSortedIndex;
import Utilities.BucketSortedList;

import java.util.*;
import java.util.function.Consumer;
import java.util.function.Function;

/** The class implements a SAT-Solver with the resolution principle.
 * Created by ohlbach on 18.10.2018.
 *
 * Resolution is combined with various simplification techniques: <br>
 *     - Propagation of unit clauses <br>
 *     - forward and backward subsumption<br>
 *     - forward and backward replacement resolution<br>
 *        <br>
 * Four different strategies are available: <br>
 *     - INPUT:    only resolvents with input clauses are allowed (complete only for Horn clauses)
 *     - SOS:      a percentage of the input clauses make up the Set of Support (SOS)<br>
 *                 resolvents are put into the SOS<br>
 *     - POSITIVE: one parent clause must be a positive clause<br>
 *     - NEGATIVE: one parent clause must be a negative clause. <br>
 *  <br>
 *  Unit clauses are exchanged between different solvers. <br>
 *  Therefore various resolution solvers can operate in parallel and exchange unit clauses as intermediate results.
 */
public class Reduction extends Solver {

    boolean checkConsistency = true;

    private static HashSet<String> keys = new HashSet<>(); // contains the allowed keys in the specification.
    static { // these are the allowed keys in the specification.
        for(String key : new String[]{"name", "type", "solver"}) {
            keys.add(key);}}

    /** parses a HashMap with key-value pairs:<br>
     *
     * @param parameters  the parameters with the keys "seed", "strategy", "percentageOfSOSClauses", "limit"
     * @param errors      for error messages
     * @param warnings    for warnings
     * @return            a list of HashMaps with keys "seed" and "sos", "limit".
     */
    public static ArrayList<HashMap<String,Object>> parseParameters(HashMap<String,String> parameters, StringBuffer errors, StringBuffer warnings){
        for(String key : parameters.keySet()) {
            if(!keys.contains(key)) {warnings.append("Reduction: unknown key in parameters: " + key + "\n");}}
        ArrayList<HashMap<String,Object>> list = new ArrayList<>();
        HashMap<String,Object> map = new HashMap<>();
        map.put("name","Reduction");
        list.add(map);
        return list;}

    /** gives a string with descriptions of the available parameters.
     *
     * @return a description of the available parameters.
     */
    public static String help() {
        return "There are no parameters";}


    /** maintains the tasks to be executed */
    private TaskQueue taskQueue = null;

    /** collects statistical information */
    public ResolutionStatistics statistics;

    /** for forming the ids of the new clauses */
    private int[] id = new int[]{0};

    /** constructs a new Resolution solver.
     *
     * @param solverNumber         for distinguishing different solvers of the same type, but different parameters
     * @param solverParameters     contains the parameters for controlling the solver
     * @param problemSupervisor    coordinates several solvers.
     */
    public Reduction(Integer solverNumber, HashMap<String,Object> solverParameters, ProblemSupervisor problemSupervisor) {
        super(solverNumber,solverParameters, problemSupervisor);}

    /** This method controls the entire resolution sequence
     * 1. All local data are initialized <br>
     * 2. All basic clauses are transformed to clauses and distributed to the primary and secondary clause lists. <br>
     *    Equivalences are turned to equivalence classes <br>
     *    All literals are mapped to the representatives in their classes.<br>
     * 3. The initial causes are put into the task queue for simplifications.<br>
     * 4. Resolution and simplification is started until <br>
     *     - a contradiction is formed or <br>
     *     - the clauses became empty or<br>
     *     - the resolution limit is exceeded or <br>
     *     - the thread is interrupted.<br>
     *
     * @return the result of the resolution sequence.
     */
    public Result solve() {
        super.initialize();
        globalParameters.log(solverId + " for problem " + problemId + " started");
        long time = System.currentTimeMillis();
        initializeData();
        Result result = null;
        try{result = initializeClauses();
            if(result == null) {result = reduce();}}
        catch(InterruptedException ex) {
            globalParameters.log("Resolution " + combinedId + " interrupted.\n");
            result = new Aborted("Resolution aborted after ");}
        statistics.elapsedTime = System.currentTimeMillis() - time;
        System.out.println("RESULT " + result.toString());
        problemSupervisor.finished(this, result, "done");
        return result;}

    /** one resolution parent is always chosen from this list */
    private BucketSortedList<Clause> clauses;

    /** maps literals (numbers) to their occurrences in clauses */
    public BucketSortedIndex<CLiteral<Clause>> literalIndex;

    /** If the strategy is SOS, then this number determines how many randomly chosen clauses are put into the clauses list */
    private int percentageOfSOSClauses = 0;


    /** is set false after all initial clauses are integrated */
    private boolean initializing = true;

    /** for optimizing subsumption and replacement resolution operations */
    private int timestamp = 1;

    private int maxClauseLength = 3;


    /** initializes resolution specific data structures*/
    private void initializeData() {
        clauses      = new BucketSortedList<Clause>(clause->clause.size());
        literalIndex = new BucketSortedIndex<CLiteral<Clause>>(predicates+1,
                (cLiteral->cLiteral.literal),
                (cLiteral->cLiteral.clause.size()));
        taskQueue    = new TaskQueue(combinedId,monitor);}

    /** EquivalenceClasses manage equivalent literals.
     *  In each equivalence class the literals are mapped to their representatives,
     *  which is always the predicate with the smallest number.
     */
    private EquivalenceClasses equivalenceClasses = null;

    /** If an equivalence p = -p occurs or can be derived, this function is called.
     *  It adds an Unsatisfiable task to the task queue.
     */
    private Consumer<String> contradictionHandler = ((reason)->{
        taskQueue.add(new Task(0,(()-> new Unsatisfiable(reason)), (()->reason)));});

    /** This function is called when a new disjunction is to be inserted.
     *  It generates a simplifyBackwards task.
     */
    private Consumer<Clause> insertHandler = (
            clause -> {insertClause(clause,"Initial clause");
                if(clause.size() > 1) {
                    taskQueue.add(new Task(basicClauseList.maxClauseLength-clause.size()+3, // longer clauses should be
                        (()-> {simplifyBackwards(clause); return null;}),    // checked first for subsumption and replacement resolution
                        (()-> "Simplify initial clause " + clause.toString())));}});

    private int maxInputId = 0;
    /** This method translates all basic clauses into Clause data structures.
     *  Equivalent literals are replaced by their representatives.
     *
     * @return possibly Unsatisfiable
     * @throws InterruptedException
     */
    private Result initializeClauses() throws InterruptedException {
        if(basicClauseList.equivalences != null) {
            equivalenceClasses = Transformers.prepareEquivalences(basicClauseList,contradictionHandler,symboltable);
            if(!taskQueue.isEmpty()) {Result result = taskQueue.run(); if(result != null) {return result;}}}

        Transformers.prepareConjunctions(basicClauseList,equivalenceClasses,
                (literal-> addTrueLiteralTask(literal, true, "Initial Conjunction")));
        if(Thread.interrupted()) {throw new InterruptedException();}
        Transformers.prepareDisjunctions(basicClauseList,id,equivalenceClasses,insertHandler);
        Transformers.prepareXors     (basicClauseList,id,equivalenceClasses,insertHandler);
        Transformers.prepareDisjoints(basicClauseList,id,equivalenceClasses,insertHandler);
        initializing = false;
        if(Thread.interrupted()) {throw new InterruptedException();}
        if(checkConsistency) {check("initializeClauses");}
        maxInputId = id[0];
        purityAndElimination();
        return taskQueue.run();}

    private Result reduce() throws InterruptedException {
        for(int level = 2;; ++level) {
            if(Thread.interrupted()) {throw new InterruptedException();}
            int maxLevel = level;
            for(Clause clause : clauses) {
                taskQueue.add(new Task(maxClauseLength - clause.size() + 3,
                        (() -> {reduceClause(clause,maxLevel); return null;}),
                        (() -> "reducing clause " + clause.toString())));}
            Result result = taskQueue.run();
            if(result != null) {return result;}}
        }

    private void reduceClause(Clause clause, int maxLevel) {
        if(clause.removed) {return ;}
        timestamp += maxClauseLength + maxLevel + 2;
        CLiteral<Clause> cliteral = LitAlgorithms.canBRemoved(clause,literalIndex,timestamp,maxLevel);
        if(cliteral == null) {return;}
        if(removeLiteral(cliteral)) {
            taskQueue.add(new Task(maxClauseLength - clause.size() + 3,
                    (() -> {simplifyForward(clause); return null;}),
                    (() -> "forward simplification of clause " + clause.toString())));
            if(monitoring) {
              monitor.print(combinedId,"Clause " + clause.toString() + " lost literal" + cliteral.literal);}}
        if(clause.size() == 2) {
            ++statistics.exportedBinaryClauses;
            problemSupervisor.forwardBinaryClause(this, clause.getLiteral(0), clause.getLiteral(1));
            return;}
        ++statistics.exportedOtherClauses;
        problemSupervisor.forwardClause(this,clause.getLiterals());}


    public void newBinaryClause(int literal1, int literal2) {
        ++statistics.importedBinaryClauses;
        taskQueue.add(new Task(2,
                (()-> {importBinaryClause(literal1,literal2); return null;}),
                (()-> "importing binary clause " + literal1 + " " + literal2)));}


    private void importBinaryClause(int literal1, int literal2) {
        Clause clause = new Clause(++id[0],2);
        clause.add(new CLiteral(literal1));
        clause.add(new CLiteral(literal2));
        clause.setStructure();
        simplifyBackwards(clause);
        if(clause.removed) {return;}
        insertClause(clause, "imported clause");
        simplifyForward(clause);
    }

    public void newClause(int[] literals) {
        ++statistics.importedOtherClauses;
        taskQueue.add(new Task(literals.length,
                (()-> {importClause(literals); return null;}),
                (()-> "importing clause " + Arrays.toString(literals))));}


    private void importClause(int[] literals) {
        Clause clause = new Clause(++id[0],literals.length);
        for(int literal : literals) {clause.add(new CLiteral(literal));}
        clause.setStructure();
        simplifyBackwards(clause);
        if(clause.removed) {return;}
        insertClause(clause, "imported clause");
        simplifyForward(clause);
    }



    /** checks if the clause is subsumed or some of its literals can be resolved away by replacement resolution.
     * If the clause is subsumed, it is removed from the clause lists and the literal index <br>
     *  - if the clause is in the clauses and the subsumer is in the secondary clauses, <br>
     *    the subsumer is moved to the clauses
     *    <br>
     *  - Example for replacement resolution:<br>
     *      p,q,r<br>
     *      -p,r <br>
     *    p in the first clause can be removed. <br>
     *    In more complex examples, several literals can be removed at once.<br>
     *    If the resulting clause is a unit clause, it generates a newTrueLiteral task.
     *    <br>
     *    Removing clauses may produce pure literals, whose negation can be made true and therefore
     *    generates a newTrueLiteral task.<br>
     *  - Shortened clauses may trigger forward subsumptions and forward replacement resolutions.
     *    Therefore thy generate a corresponding task.
     *
     * @param clause
     */
    private void simplifyBackwards(Clause clause) {
        if(clause.removed) {return;}
        timestamp += maxClauseLength +1;
        Clause subsumer = LitAlgorithms.isSubsumed(clause,literalIndex,timestamp);
        if(subsumer != null) {
            ++statistics.backwardSubsumptions;
            removeClause(clause,0);
            if(monitoring) {
                monitor.print(combinedId,"Clause \n  " + clause.toString() + " is subsumed by \n  " + subsumer.toString());}
            return;}

        timestamp += maxClauseLength +1;
        Object[] replacements = LitAlgorithms.replacementResolutionBackwards(clause,literalIndex,timestamp);

        while(replacements != null) { // several literals may become resolved away
             CLiteral<Clause> cLiteral = (CLiteral<Clause>)replacements[0];
            ++statistics.backwardReplacementResolutions;
            if(monitoring) {
                monitor.print(combinedId,
                        "Literal " + cLiteral.literal + " in clause \n  " + clause.toString() + " resolved away by clause \n  "
                        + ((Clause)replacements[1]).toString());}
            if(removeLiteral(cLiteral)) {
                timestamp += maxClauseLength +1;
                replacements = LitAlgorithms.replacementResolutionBackwards(clause,literalIndex,timestamp);}}}



    /** just used in simplifyForward */
    private ArrayList<Clause> clauseList = new ArrayList<>();
    /** just used in simplifyForward */
    private ArrayList<CLiteral<Clause>> literalList = new ArrayList<>();

    /** does forward subsumption and replacement resolution
     *
     * @param clause
     */
    private void simplifyForward(Clause clause) {
        if(clause.removed) {return;}
        clauseList.clear();
        timestamp += maxClauseLength +1;
        LitAlgorithms.subsumes(clause,literalIndex,timestamp,clauseList);
        for(Clause subsumedClause : clauseList) {
            ++statistics.forwardSubsumptions;
            if(monitoring) {monitor.print(combinedId,"Clause \n  " + clause.toString() + "  subsumes \n  " + subsumedClause.toString());}
            removeClause(subsumedClause,0);}

        literalList.clear();
        timestamp += maxClauseLength +1;
        LitAlgorithms.replacementResolutionForward(clause,literalIndex,timestamp,literalList);
        for(CLiteral<Clause> cLiteral : literalList) {
            ++statistics.forwardReplacementResolutions;
            if(monitoring) {
                monitor.print(combinedId,"Literal " + cLiteral.literal + " in clause \n  " + cLiteral.clause.toString() +
                        " resolved away by resolvent \n  " +clause.toString());}
            removeLiteral(cLiteral);
            Clause otherClause = cLiteral.clause;
            if(!otherClause.removed) {
                taskQueue.add(new Task(otherClause.size()+3,
                        (()->{simplifyForward(otherClause);return null;}),
                        (()->"Forward Simplification for shortened clause " + otherClause.toString())));}}}



    /** This method is called be the problemSupervisor, when another solver derives a true literal.
         * It generates a trueLiteral task.
         *
         * @param literal a new true literal
         */
    public void newTrueLiteral(int literal) {
        ++statistics.importedUnitClauses;
        --statistics.derivedUnitClauses;
        addTrueLiteralTask(literal, false,"Imported from another solver");}


    /** turns the literal into a trueLiteralTask.
     * If it is a unit resolvent then it is forwarded to the problem supervisor.
     *
     * @reason  for monitoring the tasks
     * @param literal a unit literal.
     */
    private void addTrueLiteralTask(int literal, boolean forward, String reason) {
        taskQueue.add(new Task(1,
                (()->processTrueLiteral(literal)),
                (()->reason + ": " + (symboltable == null ? literal : symboltable.getLiteralName(literal)))));
        ++statistics.derivedUnitClauses;
        if(forward && !initializing) problemSupervisor.forwardTrueLiteral(this,literal);}

    /** computes the consequences of a new true literal
     * - all clauses with this literal are removed <br>
     * - pure literals cause new true literals <br>
     * - all occurrences of the negated literal are removed<br>
     * - for each shortened clause which became a unit clause, a new task is created.<br>
     * - if the primary clauses became empty, the model is completed
     *
     * @param literal a new true literal
     * @return the result of a model completion or null
     */
    private Result processTrueLiteral(int literal) {
        //System.out.println("PL START " + literal);
        //System.out.println(toString());
        switch(model.status(literal)) {
            case -1: return new Unsatisfiable(model,literal);
            case +1: return null;}
        model.add(literal);
        if(false) {
            ArrayList<int[]> falseClauses = basicClauseList.falseClausesInPartial(model);
            if(falseClauses != null) {
                System.out.println("ErrorCheck: the following basic clauses are false in the model");
                System.out.println(model.toString());
                for(int[] clause : falseClauses) {
                    System.out.println(basicClauseList.clauseToString(clause));}
                System.exit(1);}}
        Iterator<CLiteral<Clause>> iterator = literalIndex.iterator(literal);
        while(iterator.hasNext()) {
            Clause clause = iterator.next().clause;
            removeClause(clause,literal);}

        for(CLiteral<Clause> cLiteral : literalIndex.getAllItems(-literal)) {
            removeLiteral(cLiteral);}
        literalIndex.clearBoth(Math.abs(literal));
        //System.out.println("PL END " + literal);
        //System.out.println(toString());
        return null;}


    private ArrayList<Object[]> eliminatedLiterals = new ArrayList<>();

    private void processElimination(int eliminateLiteral) {
        int size01p = literalIndex.size01(eliminateLiteral);
        int size01n = literalIndex.size01(-eliminateLiteral);
        if(size01p != 1 || size01n == 0) {return;}
        //System.out.println("Start Elimination " + eliminateLiteral );
        //System.out.println(toString());
        Clause clause  = literalIndex.getAllItems(eliminateLiteral).get(0).clause;
        ArrayList<CLiteral<Clause>> literals = clause.cliterals;
        boolean inPrimary = clauses.contains(clause);
        for(CLiteral<Clause> otherCliteral : literalIndex.getAllItems(-eliminateLiteral)) {
            boolean tautology = false;
            Clause otherClause = otherCliteral.clause;
            Clause newClause = new Clause(++id[0]);
            ArrayList<CLiteral<Clause>> newLiterals = new ArrayList<>();
            for(CLiteral<Clause> literal : literals) {
                if(literal.literal != eliminateLiteral) {newLiterals.add(new CLiteral(literal.literal,newClause,newLiterals.size()));}}
            for(CLiteral<Clause> literal : otherClause.cliterals) {
                if(literal.literal == -eliminateLiteral) {continue;}
                int contained = LitAlgorithms.contains(newLiterals,literal.literal);
                if(contained > 0) {continue;}
                if(contained < 0) {tautology = true; break;}
                newLiterals.add(new CLiteral(literal.literal,newClause,newLiterals.size()));}
            if(tautology) {removeClause(otherClause,0); continue;}
            newClause.cliterals = newLiterals;
            newClause.setStructure();
            simplifyBackwards(newClause);
            if(newClause.removed) {removeClause(otherClause,0); continue;}
            boolean inp = inPrimary || clauses.contains(otherClause);
            removeClause(otherClause,0);
            insertClause(newClause,"Literal " + eliminateLiteral + " eliminated");}
        eliminatedLiterals.add(new Object[]{clause.cliterals,eliminateLiteral});
        removeClause(clause,0);
        literalIndex.clearBoth(Math.abs(eliminateLiteral));
        if(checkConsistency) {check("processElimination");}
        //System.out.println("End Elimination " + eliminateLiteral + "@" + clause.toString());
        //System.out.println(toString());
    }

    /** completes a model after resolution has finished.
     * Strategy INPUT or SOS: all remaining clauses should be true <br>
     * Strategy POSITIVE: all remaining clauses are negative of mixed. <br>
     * Choose an unassigned negative literal. <br>
     *
     * Strategy NEGATIVE: all remaining clauses are positive of mixed. <br>
     * Choose an unassigned positive literal. <br>
     *
     * @return Satisfiable or Erraneous (if something went wrong).
     */
    private Result completeModel() {
        System.out.println("Completing Model\n"+toString());
        if(model.size() == predicates) {return new Satisfiable(model);}
        boolean isPositive = true;

        completeEliminations();
        Result result = equivalenceClasses.completeModel(model);
        if(result != null) {return result;}
        result = checkModel();
        if(result != null) {return result;}
        return new Satisfiable(model);}

    private void completeEliminations() {
        for(int i = eliminatedLiterals.size()-1; i >= 0; --i) {
            Object[] els = eliminatedLiterals.get(i);
            ArrayList<CLiteral<Clause>> literals = (ArrayList<CLiteral<Clause>>)els[0];
            int literal = (int)els[1];
            if(model.status(literal) != 0) {continue;}
            boolean satisfied = false;
            for(CLiteral<Clause>cliteral : literals) {
                int lit = cliteral.literal;
                if(lit != literal && model.status(lit) == 1) {satisfied = true; break;}}
            model.add(satisfied ? -literal : literal);}}


    /** counts the number of clauses in the resolution solver */
    private int clauseCounter = 0;

    /** inserts the clause into the local data structures.
     * - If the clause is a unit clause, it generates a trueLiteralTask<br>
     * - If it is an initial clause, it generates a simplifyBackwards task.<br>
     *   These tasks are sorted such that longer clauses are simplified first (subsumption and replacement resolution).
     *
     * @param clause  the clause to be inserted.
     */
    private void insertClause(Clause clause, String reason) {
        switch(clause.size()) {
            case 1: addTrueLiteralTask(clause.getLiteral(0),true,reason); return;
            case 2: findEquivalence(clause);}
        ++clauseCounter;
        maxClauseLength = Math.max(maxClauseLength,clause.size());
        clauses.add(clause);
        for(CLiteral<Clause> cLiteral : clause) {literalIndex.add(cLiteral);}
        if(checkConsistency) {check("insertClause");}}


    /** removes a clause from primary/secondary clauses and from the literal index (except ignoreLiteral)
     *
     * @param clause        the clause to be removed
     * @param ignoreLiteral the literal not to remove from the index
     */
    private void removeClause(Clause clause, int ignoreLiteral) {
        if(clause.removed) {return;}
        --clauseCounter;
        clauses.remove(clause);
        for(CLiteral<Clause> cLiteral : clause) {
            if(cLiteral.literal != ignoreLiteral) {
                literalIndex.remove(cLiteral);}}
        clause.removed = true;
        if(checkConsistency) {check("removeClause");}}


    /** removes the literal from its clause and the literal index.
     * - If the clause is already marked 'removed' nothing happens. <br>
     * - If the shortened clause is a unit clause, it generates a trueLiteralTask, and is removed.
     *
     * @param cLiteral the literal to be removed
     * @return true if the clause is still there.
     */
    private boolean removeLiteral(CLiteral<Clause> cLiteral) {
        Clause clause = cLiteral.clause;
        if(clause.removed) {return false;}
        boolean isOld = clause.getPosition() >= 0;
        if(isOld) {
            clauses.remove(clause);
            for(CLiteral<Clause> cliteral : clause) literalIndex.remove(cliteral);}
        clause.remove(cLiteral);
        if(clause.size() == 1) {
            addTrueLiteralTask( clause.getLiteral(0),true,
                    "New true literal derived from clause " + clause.toString());
            removeClause(clause,0);
            return false;}
        if(isOld) {
            for(CLiteral<Clause> cliteral : clause) literalIndex.add(cliteral); // literals are inserted into different buckets
            clauses.add(clause);}
        if(checkConsistency) {check("removeLiteral");}
        clause.id = ++id[0];
        return true;}



    private ArrayList<Integer> zeros = new ArrayList<>();
    private ArrayList<Integer> ones = new ArrayList<>();

    /** This method checks all predicates for purity and elimination.
     * A literal p is pure if there are no clauses with p any more. <br>
     * In this case -p can be made true. <br>
     * A literal p can be eliminated if it occurs only once in the clauses, say in clause C.
     * In this case all clauses with -p can be replaced with their resolvent with C.
     */
    private void purityAndElimination() {
        while(literalIndex.size01(predicates,zeros,ones)) {
            for(int literal : zeros) {
                int sizep = literalIndex.size01(literal);
                if(sizep == 0) {
                    if(monitoring) {monitor.print(combinedId, "Eliminating pure literal " + -literal);}
                    processTrueLiteral(-literal);}}
            for(int literal : ones)  {processElimination(literal);}}}

    /** checks if the clause is part of an equivalence like (p,q) and (-p,-q)
     *
     * @param clause a clause to be checked (p,q)
     * @return true if there is another clause (-p,-q)
     */
    private boolean isEquivalence(Clause clause) {
        if(clause.size() != 2) {return false;}
        ArrayList<CLiteral<Clause>> clits1 = literalIndex.getItems(-clause.getCLiteral(0).literal,2);
        ArrayList<CLiteral<Clause>> clits2 = literalIndex.getItems(-clause.getCLiteral(1).literal,2);
        if(clits1 == null || clits2 == null) {return false;}
        timestamp += maxClauseLength +1;
        for(CLiteral<Clause> clit : clits1) {clit.clause.timestamp = timestamp;}
        for(CLiteral<Clause> clit : clits2) {if(clit.clause.timestamp == timestamp) {return true;}}
        return false;}

    /** This method checks if the clause is part of an equivalence (p,q) (-p,-q)
     * If this is the case: <br>
     *     - -p == q is inserted into the equivalence classes <br>
     *     - a processEquivalence task is generated
     *
     * @param clause the clause to be checked
     * @return true if an equivalence has been found.
     */
    private boolean findEquivalence(Clause clause) {
        if(!isEquivalence(clause)) {return false;}
        int literal1 = -clause.getCLiteral(0).literal;
        int literal2 =  clause.getCLiteral(1).literal;
        if(literal1 < 0) {literal1 = -literal1; literal2 = -literal2;}
        int fromliteral = literal1; int toliteral = literal2;
        if(!equivalenceClasses.addEquivalence(fromliteral,toliteral)) {return false;}
        taskQueue.add(new Task(2,(()->processEquivalence(fromliteral,toliteral)),
                (()-> "Replacing equivalent literal: "+ fromliteral + " -> " + toliteral)));
        ++statistics.equivalences;
        return true;}

    private ArrayList<Clause> replacedClauses = new ArrayList<>();

    /** This method replaces all occurrences of fromLiteral by toLiteral.
     *  Generated tautologies are ignored.<br>
     *  Double literals are avoided. <br>
     *  The new clauses are backwards simplified.
     *
     * @param fromLiteral antecedent
     * @param toLiteral   succedent
     * @return            null
     */
    private Result processEquivalence(int fromLiteral, int toLiteral) {
        //System.out.println("START EQUIVALENCE " + fromLiteral + " -> " + toLiteral);
        //System.out.println(toString());
        int fromStatus = model.status(fromLiteral);
        int toStatus   = model.status(toLiteral);
        if(fromStatus != 0 && toStatus != 0 && fromStatus != toStatus) {
            return new Unsatisfiable(model,toLiteral);}
        if(fromStatus != 0) {
            addTrueLiteralTask((fromStatus == 1 ? toLiteral : -toLiteral),true,
                    "equivalent literals " + fromLiteral + " " + toLiteral);
            return null;}
        if(toStatus != 0) {
            addTrueLiteralTask((toStatus == 1 ? fromLiteral : -fromLiteral),true,
                    "equivalent literals " + fromLiteral + " " + toLiteral);
            return null;}
        replacedClauses.clear();
        for(CLiteral<Clause> cliteral : literalIndex.getAllItems(fromLiteral)) {
            Clause clause = cliteral.clause;
            Clause newClause = new Clause(++id[0],clause.size());
            boolean tautology = false;
            for(CLiteral cLiteral : clause.cliterals) {
                int literal = cLiteral.literal;
                if(literal == toLiteral) {continue;}
                if(literal == -toLiteral) {tautology = true; break;}
                if(literal == fromLiteral) {literal = toLiteral;}
                newClause.add(new CLiteral(literal));}
            removeClause(clause,0);
            if(!tautology) {replacedClauses.add(newClause);}}

        for(CLiteral<Clause> cliteral : literalIndex.getAllItems(-fromLiteral)) {
            Clause clause = cliteral.clause;
            Clause newClause = new Clause(++id[0],clause.size());
            boolean tautology = false;
            for(CLiteral cLiteral : clause.cliterals) {
                int literal = cLiteral.literal;
                if(literal == -toLiteral) {continue;}
                if(literal == toLiteral) {tautology = true; break;}
                if(literal == -fromLiteral) {literal = -toLiteral;}
                newClause.add(new CLiteral(literal));}
            removeClause(clause,0);
            if(!tautology) {replacedClauses.add(newClause);}}

        literalIndex.clearBoth(fromLiteral);
        for(Clause clause : replacedClauses) {
            simplifyBackwards(clause);
            if(!clause.removed) {
                insertClause(clause,"Literal " + fromLiteral + " -> " + toLiteral);}}
        for(Clause clause : replacedClauses) {if(!clause.removed) {simplifyForward(clause);}}

        //System.out.println("END EQUIVALENCE " + fromLiteral + " -> " + toLiteral);
        //System.out.println(toString());
        return null;}




    /** return the entire statistics information
     *
     * @return the entire statistics information for the resolution solver.
     */
    public Statistic getStatistics() {return statistics;}

    /** lists the clauses and the literal index as a string.
     *
     * @return the clauses and the literal index as a string.
     */
    public String toString() {return toString(null);}

    /** lists the clauses and the literal index as a string.
     *
     * @param symboltable a symboltable or null
     * @return the clauses and the literal index as a string.
     */
    public String toString(Symboltable symboltable) {
        Function<Clause,String> clauseString = (clause->clause.toString(symboltable));
        Function<CLiteral<Clause>,String> literalString = (cliteral->cliteral.toString(symboltable,clause->Integer.toString(clause.id)));
        StringBuilder st = new StringBuilder();
        st.append("Resolution:\n");
        if(!clauses.isEmpty()) {
            st.append("Clauses:\n").append(clauses.toString(clauseString)).append("\n");}
        if(model != null && !model.isEmpty()) {
            st.append("Model:\n").append(model.toString(symboltable)).append("\n\n");}
        st.append("Literal Index:\n").append(literalIndex.toString(literalString));
        if(!taskQueue.isEmpty()) {
            st.append("\nTask Queue:\n").append(taskQueue.toString());}
        if(!eliminatedLiterals.isEmpty()) {
            st.append("Eliminated Literals:\n");
            for(Object[] elms : eliminatedLiterals) {
                st.append("  "+elms[1].toString() + " from " + ((ArrayList<CLiteral>)elms[0]).toString()+"\n");}}
        return st.toString();}

    /** collects information about the control parameters
     *
     * @return a string with information about the control parameters.
     */
    public String parameters() {return "";}

    public void check(String info) {
        clauses.check(info + ":'clauses'");
        literalIndex.check(info+":'literal index'");

        for(Clause clause : clauses) {
            for(CLiteral cLiteral : clause) {
                if(!literalIndex.contains(cLiteral)) {
                    System.out.println("Error: "+info+ " literal " + cLiteral.literal + " in clause " + clause.toString() + " is not in the index.");}}}
    }
}
