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
public class Reduction extends ResolutionReduction {

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


    /** constructs a new Resolution solver.
     *
     * @param solverNumber         for distinguishing different solvers of the same type, but different parameters
     * @param solverParameters     contains the parameters for controlling the solver
     * @param problemSupervisor    coordinates several solvers.
     */
    public Reduction(Integer solverNumber, HashMap<String,Object> solverParameters, ProblemSupervisor problemSupervisor) {
        super(solverNumber,solverParameters, problemSupervisor);}

    /** contains all the clauses, sorted according to the clause length*/
    private BucketSortedList<Clause> clauses;


    /** initializes resolution specific data structures*/
    protected void initializeData() {
        clauses      = new BucketSortedList<Clause>(clause->clause.size());
        literalIndex = new BucketSortedIndex<CLiteral<Clause>>(predicates+1,
                (cLiteral->cLiteral.literal),
                (cLiteral->cLiteral.clause.size()));
        taskQueue    = new TaskQueue(combinedId,monitor);}



    protected Result doTheWork() throws InterruptedException {
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
    protected void simplifyBackwards(Clause clause) {
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
    protected void simplifyForward(Clause clause) {
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
    protected void addTrueLiteralTask(int literal, boolean forward, String reason) {
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
    protected Result processTrueLiteral(int literal) {
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
    protected void insertClause(Clause clause, String reason) {
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
    protected void removeClause(Clause clause, int ignoreLiteral) {
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





    /** lists the clauses and the literal index as a string.
     *
     * @param symboltable a symboltable or null
     * @return the clauses and the literal index as a string.
     */
    public String toString(Symboltable symboltable) {
        Function<Clause,String> clauseString = (clause->clause.toString(symboltable));
        Function<CLiteral<Clause>,String> literalString = (cliteral->cliteral.toString(symboltable,clause->Integer.toString(clause.id)));
        StringBuilder st = new StringBuilder();
        st.append("Reduction:\n");
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
