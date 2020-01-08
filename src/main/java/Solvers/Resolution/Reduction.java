package Solvers.Resolution;

import Coordinator.Tasks.Task;
import Coordinator.Tasks.TaskQueue;
import Datastructures.Clauses.Clause;
import Datastructures.Literals.CLiteral;
import Datastructures.Literals.LitAlgorithms;
import Datastructures.Results.*;
import Datastructures.Symboltable;
import Management.ProblemSupervisor;
import Utilities.BucketSortedIndex;
import Utilities.BucketSortedList;

import java.util.*;
import java.util.function.Function;

/** The class implements recursive replacement resolution on the input clauses
 * Created by ohlbach on 5.1.2020
 *
 * Recursive Replacement Resolution means,
 * a sequence of resolutions cause a clause which is just one literal shorter than the given one.
 * The search is done by iterative deepening.
 * The main purpose of the class is to support other solvers with reduced input clauses.
 *
 */
public class Reduction extends ResolutionReduction {

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
    void initializeData() {
        clauses      = new BucketSortedList<Clause>(clause->clause.size());
        literalIndex = new BucketSortedIndex<CLiteral<Clause>>(predicates+1,
                            (cLiteral->cLiteral.literal),
                            (cLiteral->cLiteral.clause.size()));
        taskQueue    = new TaskQueue(combinedId,monitor);
        statistics = new ResolutionStatistics(combinedId);}


    Result doTheWork() throws InterruptedException {
        for(int level = 2;level < clauses.size(); ++level) {
            System.out.println("REDUCTION LEVEL " + level);
            if(Thread.interrupted()) {throw new InterruptedException();}
            if(clauses.isEmpty()) {return completeModel();}
            if(monitoring) {monitor.print(combinedId,"Reduction enters level " + level);}
            int maxLevel = level;
            for(Clause clause : clauses) {
                taskQueue.add(new Task(maxClauseLength - clause.size() + priorityShift,
                        (() -> {reduceClause(clause,maxLevel); return null;}),
                        (() -> "reducing clause " + clause.toString())));}
            Result result = taskQueue.run();
            if(result != null) {return result;}
            if((result = purityAndElimination()) != null) {return result;}}
        return null;}


    /** does recursive replacement resolution down to maxLevel
     *
     * @param clause    the clause to be investigated
     * @param maxLevel  the maximum search depth
     */
    private void reduceClause(Clause clause, int maxLevel) {
        if(clause.removed) {return ;}
        CLiteral<Clause> cliteral = LitAlgorithms.canBRemoved(clause,literalIndex,timestamp,maxLevel);
        timestamp += maxClauseLength + maxLevel + 2;
        if(cliteral == null) {return;}
        if(monitoring) {
            monitor.print(combinedId,"Reduction removes literal " + cliteral.toString(symboltable) + " from clause "+
            clause.toString(symboltable));}
        ++statistics.reductions;
        removeLiteral(cliteral);
        if(checkConsistency) {check("reduceClause");}
        analyseShortenedClause(clause);}

    /** just returns the clause
     *
     * @param clause not used
     * @return  the clauses
     */
    BucketSortedList<Clause> getClauseList(Clause clause) {return clauses;}

    /** checks if there are no clauses any more
     *
     * @return true if there are no clauses any more.
     */
    boolean clausesEmpty() {return clauses.isEmpty();}


    /** just removes the clause
     *
     * @param clause   the clause to be removed
     * @param subsumer  not used.
     */
    void replaceClause(Clause clause, Clause subsumer) {
        removeClause(clause,0);}



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
        if(!equivalenceClasses.isEmpty()) {st.append(equivalenceClasses.toString());}
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
