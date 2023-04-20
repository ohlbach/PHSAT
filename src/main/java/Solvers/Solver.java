package Solvers;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.InputClauses;
import Datastructures.Literals.CLiteral;
import Datastructures.Results.Erraneous;
import Datastructures.Results.Result;
import Datastructures.Statistics.Statistic;
import Datastructures.Symboltable;
import Datastructures.Theory.Model;
import Management.GlobalParameters;
import Management.Monitor.Monitor;
import Management.ProblemSupervisor;
import Solvers.RecursiveSearch.RecursiveSearcher;
import Solvers.Simplifier.Simplifier;
import Solvers.Walker.Walker;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;

/** This is the superclass of all solver classes.
 * Its static methods maintain information about all solver classes.
 *
 * Its instance methods proved the interface to the ProblemSupervisor.
 *
 * Created by ohlbach on 09.10.2018.
 */
public abstract class Solver {

    /* Static data and methods
       *********************** */

    /** the list of all solver types */
    public static String[] solvers = new String[]{"simplifier","walker","recursiveSearch","resolution"};

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
            case "Simplifier":
            case "simplifier":      return Simplifier.class;
            case "Walker":
            case "walker":          return Walker.class;
            case "RecursiveSearch":
            case "recursiveSearch": return RecursiveSearcher.class;
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
     * @param parameterList the parameters
     * @param errors        for collecting error messages
     * @param warnings      for collecting warning messages
     * @return              a list of solvers
     */
    public static ArrayList<Solver> makeSolvers(ArrayList<HashMap<String,String>> parameterList,
                                                StringBuilder errors, StringBuilder warnings) {
        ArrayList<Solver> solvers = new ArrayList<>();
        for(HashMap<String,String> parameters: parameterList) {
            String type = parameters.get("solver");
            if(type == null) continue;
            Class clazz = solverClass(type);
            if(clazz == null) {
                errors.append("Solver: Unknown solver class: ").append(type); continue;}
            try{
                Method makeSolver = clazz.getMethod("makeSolvers",HashMap.class,
                        ArrayList.class, StringBuilder.class, StringBuilder.class);
                makeSolver.invoke(null,parameters,solvers,errors,warnings);}
            catch(Exception ex) {ex.printStackTrace();System.exit(1);}}
        return solvers;}





    /** Instance data and methods
       *************************  */

    /** the solver's identifier. */
    public String solverId = "solverId";

    /** in case there are more solvers of the same type. */
    protected Integer solverNumber;

    /** the problem's identifier. */
    public String problemId = "problemId";

    /** combines the solver and problem identifiers. */
    public String combinedId = "combinedId";

    /** the supervisor which coordinates the work of all solvers or a given problem. */
    public ProblemSupervisor problemSupervisor;

    /** the global parameters. */
    protected  GlobalParameters globalParameters;

    /** all control parameters for the solvers. */
    protected  HashMap<String,Object> solverParameters;

    /** the input clauses. */
    protected InputClauses inputClauses;

    /** the number of predicates in the problem. */
    public  int predicates;

    /** the global model (from the problemSupervisor). */
    public Model model;

    /** the symboltable */
    public  Symboltable symboltable = null;

    /** controls the application of inferenceSteps. */
    protected boolean trackReasoning = true;

    /** a monitor for showing the actions */
    public Monitor monitor = null;

    /** true if the monitor is active */
    protected boolean monitoring = false;

    /** an identifier for the monitor */
    protected String monitorId;

    Thread myThread;

    /** constructs a solver as an instance of the Processor class.
     *
     * @param solverNumber       to distinguish different solvers of the same type, but different parameters,
     * @param solverParameters   the control parameters for the solver
     */
    public Solver(Integer solverNumber, HashMap<String,Object> solverParameters) {
        this.solverNumber = solverNumber;
        this.solverParameters   = solverParameters;}

    public Solver() {}

    /** initializes the parameters which are common to all solvers.
     *
     * @param problemSupervisor the supervisor for the problem.
     */
    public void initialize(Thread thread,ProblemSupervisor problemSupervisor) {
        this.myThread              = thread;
        this.problemSupervisor     = problemSupervisor;
        solverId                   = (String)solverParameters.get("name");
        problemId                  = problemSupervisor.problemId;
        combinedId                 = problemId+"@"+solverId + ":" + solverNumber;
        globalParameters           = problemSupervisor.globalParameters;
        inputClauses               = problemSupervisor.inputClauses;
        predicates                 = inputClauses.predicates;
        symboltable                = inputClauses.symboltable;
        monitor                    = problemSupervisor.monitor;
        monitoring                 = monitor != null;
        model                      = problemSupervisor.model;
        }

        public String getSolverId() {return solverId;}


    /** The key method, which has to be implemented by the solvers.
     * It is supposed to find a model or a contradiction in the clauses.
     *
     * @return Un/Satisfiable or null
     */
    public abstract Result solveProblem(ProblemSupervisor problemSupervisor);

    /** @return the statistics of the solver */
    public abstract Statistic getStatistics();


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

    /** The method checks if the model satisfies the basic clauses.
     *
     * @return null or an Erraneous Result
     */
    public Result checkModel(Model model) {
        ArrayList<int[]> falseClauses = inputClauses.falseClausesInModel(model);
        if(falseClauses != null) {return new Erraneous(problemId,"Solver",model,falseClauses,symboltable);}
        else {return null;}}




}
