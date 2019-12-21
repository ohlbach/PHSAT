package Solvers;

import Datastructures.Clauses.BasicClauseList;
import Datastructures.Clauses.Clause;
import Datastructures.Literals.CLiteral;
import Datastructures.Results.Result;
import Datastructures.Statistics.Statistic;
import Datastructures.Symboltable;
import Datastructures.Theory.Model;
import Management.GlobalParameters;
import Management.Monitor;
import Management.ProblemSupervisor;
import Solvers.RandomWalker.WalkerCommunicative;
import Solvers.RandomWalker.WalkerIsolated;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;

/** This is the superclass of all solver classes.
 * Its static methods maintain information about all solver classes
 *
 * Its instance methods proved the interface to the ProblemSupevisor.
 *
 * Created by ohlbach on 09.10.2018.
 */
public abstract class Solver {

    /* Static data and methods
       *********************** */

    /** the list of all solver types */
    public static String[] solvers = new String[]{"walker isolated", "walker communicative","resolution"};

    /** checks if the name is a solver name
     *
     * @param name  a string
     * @return true if the name is the name of a solver.
     */
    public static boolean isSolver(String name) {
        for(String solver : solvers) {if(name.equals(solver)) {return true;}}
        return false;}


    /** maps the solver names to the solver classes.
     * This method must be extended when a new solver class is added.
     *
     * @param solverName a solver name
     * @return the solver class, or null
     */
    public static Class solverClass(String solverName) {
        switch (solverName) {
            case "walker isolated":      return WalkerIsolated.class;
            case "walker communicative": return WalkerCommunicative.class;
            case "resolution":           return Solvers.Resolution.Resolution.class;
            default: return null;}}

    /** collects all the help-strings for all solver classes
     *
     * @return the collected help string for all solver classes
     */
    public static String help() {
        StringBuilder st = new StringBuilder();
        st.append("The following solver types are available:\n");
        for(String solver : solvers) {
            st.append(solver).append(":\n");
            st.append(help(solver)).append("\n");}
        return st.toString();}

    /** returns the help-string for the generator with the given name
     *
     * @param solverName a solver name
     * @return its help-string
     */
    public static String help(String solverName) {
        Class clazz = solverClass(solverName);
        if(clazz == null) {return "Unknown Solver Class: " +solverName;}
        try{
            Method helper = clazz.getMethod("help");
            return (String)helper.invoke(null);}
        catch(Exception ex) {ex.printStackTrace();System.exit(1);}
        return null;}


    /** parses the string-type parameters into sequences of objects
     *
     * @param solverName       the solver name
     * @param parameters a key-value map with parameters as strings
     * @param errors     for collecting error messages
     * @param warnings   for collecting warning messages
     * @return           a list of key-value maps where the values are objects.
     */
    public static ArrayList<HashMap<String,Object>> parseParameters(String solverName, HashMap<String,String> parameters,
                                                                    StringBuffer errors, StringBuffer warnings) {
        Class clazz = solverClass(solverName);
        if(clazz == null) {errors.append("Unknown solver class: " + solverName+"\n"); return null;}
        try{
            Method parser = clazz.getMethod("parseParameters",HashMap.class,StringBuffer.class, StringBuffer.class);
            return (ArrayList<HashMap<String,Object>>)parser.invoke(null,parameters,errors,warnings);}
        catch(Exception ex) {ex.printStackTrace();System.exit(1);}
        return null;}

    /** constructs a new solver of the given type
     *
     * @param solverName       the solver name
     * @param solverParameters a key-value map with parameters as strings
     * @param problemSupervisor the central processor
     * @return           a new solver
     */
    public static Solver construct(String solverName, int solverNumber, HashMap<String,Object> solverParameters, ProblemSupervisor problemSupervisor) {
        Class clazz = solverClass(solverName);
        try{
            Constructor constructor = clazz.getConstructor(Integer.class,HashMap.class,ProblemSupervisor.class);
            return (Solver)constructor.newInstance(solverNumber,solverParameters,problemSupervisor);}
        catch(Exception ex) {ex.printStackTrace();System.exit(1);}
        return null;}


    /** Instance data and methods
       *************************  */

    public String solverId = "solverId";

    public String problemId = "problemId";

    public String combinedId = "combinedId";

    public Monitor monitor = null;
    protected boolean monitoring = false;

    /** the supervisor which coordinates the work of all solvers or a given problem */
    protected final ProblemSupervisor problemSupervisor;

    protected  GlobalParameters globalParameters;

    /** all control parameters for the solvers */
    protected  HashMap<String,Object> solverParameters;

    protected  BasicClauseList basicClauseList;

    protected  int predicates;

    protected Model model;

    protected  Symboltable symboltable;

    protected Integer solverNumber;


    /** constructs a solver as an instance of the Processor class.
     *
     * @param solverNumber       to distinguish different solvers of the same type, but different parameters,
     * @param solverParameters   the control parameters for the solver
     * @param problemSupervisor the central processor.
     */
    public Solver(Integer solverNumber, HashMap<String,Object> solverParameters, ProblemSupervisor problemSupervisor) {
        this.solverNumber = solverNumber;
        this.solverParameters   = solverParameters;
        this.problemSupervisor  = problemSupervisor;}

    protected void initialize() {
        solverId                = (String)solverParameters.get("solverId");
        problemId               = problemSupervisor.problemId;
        combinedId              = problemId+"@"+solverId + ":" + solverNumber;
        globalParameters        = problemSupervisor.globalParameters;
        basicClauseList         = problemSupervisor.basicClauseList;
        predicates              = basicClauseList.predicates;
        symboltable             = basicClauseList.symboltable;
        monitor                 = globalParameters.monitor;
        monitoring              = monitor.monitoring;
        model                   = new Model(predicates);
        if(monitoring) {
            monitor.addThread(combinedId,"Monitor for problem " + problemId + " and solver " + solverId);}}


    /** This method is called when another solver found a new true literal.
     *  It should be overwritten in the solver classes, because otherwise this important information is lost.
     *
     * @param literal a new true literal
     */
    public void newTrueLiteral(int literal) {}

    /** This method is called when another solver found a new binary clause.
     * It need be overwritten in the solver class when it uses implication DAGs.
     *
     * @param literal1 the first literal of the clause
     * @param literal2 the second literal of the clause
     */
    public void newBinaryClause(int literal1, int literal2) {}


    /** The key method, which has to be implemented by the solvers.
     * It is supposed to find a model or a contradiction in the clauses.
     *
     * @return Un/Satisfiable or null
     */
    public abstract Result solve();

    public abstract Statistic getStatistics();


    /** This method checks if all literals are true or all literals are false in a model
     *
     * @param clause  a clause
     * @param model   a model
     * @return +1 if all literals are true, -1 if all literals are false, otherwise 0.
     */
    public int statusInModel(Clause clause, Model model) {
        int trueLiterals = 0;
        int falseLiterals = 0;
        for(CLiteral cliteral : clause) {
            int status = model.status(cliteral.literal);
            if(status == 1) {++trueLiterals; continue;}
            if(status == 0) {++falseLiterals;}}
        int size = clause.size();
        if(trueLiterals == size) {return 1;}
        if(falseLiterals == size) {return -1;}
        return 0;}

    /** This method checks if some literals are true or all literals are false in a model
     *
     * @param clause  a clause
     * @param model   a model
     * @return true if the clause is true in the model
     */
    public boolean trueInModel(Clause clause, Model model) {
        for(CLiteral cliteral : clause) {
            if(model.isTrue(cliteral.literal)) {return true;} }
        return false;}








}
