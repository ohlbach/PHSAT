package Solvers;

import Datastructures.Clauses.Clause;
import Datastructures.Literals.CLiteral;
import Datastructures.Results.Erraneous;
import Datastructures.Results.Result;
import Datastructures.Statistics.Statistic;
import Datastructures.Symboltable;
import Datastructures.Theory.Model;
import Management.GlobalParameters;
import Management.Monitor.Monitor;
import Management.Parameters;
import Management.ProblemSupervisor;
import Solvers.Backtracker.Backtracker;
import Solvers.Normalizer.Normalizer;
import Solvers.ResolutionOld.Resolution;
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
    public static String[] solvers = new String[]{"walker","backtracker","resolution"};

    /** checks if the name is a solver name
     *
     * @param name  a string
     * @return true if the name is the name of a solver.
     */
    public static boolean isSolver(String name) {
        for(String solver : solvers) {if(name.toLowerCase().equals(solver)) {return true;}}
        return false;}


    /** maps the solver names to the solver classes.<br>
     * This method must be extended when a new solver class is added.
     *
     * @param solverName a solver name
     * @return the solver class, or null
     */
    public static Class solverClass(String solverName) {
        switch (solverName.toLowerCase()) {
            case "resolution":  return Resolution.class;
            case "walker":      return Walker.class;
            case "backtracker": return Backtracker.class;
            default: return null;}}

    /**
     * Sets the default values for the solvers based on the module values.
     *
     * @param moduleValues A HashMap containing the module values.
     */
    public static void setDefaults(HashMap<String,ArrayList<String>> moduleValues) {
        for(String solver : solvers) {
            ArrayList<String> values = moduleValues.get(solver);
            if(values != null) {
                try{solverClass(solver).getMethod("setDefaults",ArrayList.class).invoke(null,values);}
                catch(Exception ignore) {}}}}


    /** Analyses the parameters and generates the corresponding solvers.
     *
     * @param parameterList the parameters
     * @return  a list of solvers
     */
    public static ArrayList<Solver> makeSolvers(ArrayList<Parameters> parameterList) {
        ArrayList<Solver> solvers = new ArrayList<>();
        for(Parameters parameters: parameterList) {
            if(!(Boolean)parameters.parameters.get(0).value) continue;
            Class solverClass = solverClass(parameters.name);
            if(solverClass == null) {
                System.err.println("System Error: unknown solver: " + parameters.name);
                new Exception().printStackTrace();System.exit(1);}
            try{
                Method makeSolver = solverClass.getMethod("makeSolvers",Parameters.class,ArrayList.class);
                makeSolver.invoke(null,parameters,solvers);}
            catch(Exception ex) {
                System.err.println("System Error: unknown method: makeSolvers\n" + ex.getMessage());
                ex.printStackTrace();System.exit(1);}}
        return solvers;}





    /** Instance data and methods
       *************************  */

    /** the solver's identifier. */
    public String solverId = "solverId";

    /** in case there are more solvers of the same type. */
    public Integer solverNumber;

    /** the problem's identifier. */
    public String problemId = "problemId";

    /** combines the solver and problem identifiers. */
    public String combinedId = "combinedId";

    /** the supervisor which coordinates the work of all solvers or a given problem. */
    public ProblemSupervisor problemSupervisor;

    /** the global parameters. */
    protected GlobalParameters globalParameters;

    /** if contains the normalized clauses */
    protected Normalizer normalizer;

    /** the start time of the solver. */
    public long startTime;

    /** the number of predicates in the problem. */
    public int predicates;

    /** the global model (from the problemSupervisor). */
    public Model model;

    /** the symboltable */
    public  Symboltable symboltable = null;

    /** controls the application of inferenceSteps. */
    public boolean trackReasoning = true;

    /** a monitor for showing the actions */
    public Monitor monitor = null;

    /** true if the monitor is active */
    public boolean monitoring = false;

    /** an identifier for the monitor */
    protected String monitorId;

    public Thread myThread;

    /** the list of solver classes */
    public static ArrayList<Class> solverClasses = new ArrayList<>();
    static{
        solverClasses.add(Solvers.Walker.Walker.class);
        solverClasses.add(Solvers.Backtracker.Backtracker.class);
    }

    /**
     * Constructs a list of Parameters objects by invoking the "makeParameter" method in each generator class.
     *
     * @return an ArrayList of Parameters objects
     */
    public static ArrayList<Parameters> makeParameters() {
        ArrayList<Parameters> parameters = new ArrayList<>();
        for(Class clazz : solverClasses) {
            try{
                Method method = clazz.getMethod("makeParameter");
                parameters.add((Parameters)method.invoke(null));}
            catch(Exception ignored){}}
        return parameters;}

    /** constructs a solver as an instance of the Processor class.
     *
     * @param solverNumber       to distinguish different solvers of the same type, but different parameters,
     */
    public Solver(Integer solverNumber) {
        this.solverNumber = solverNumber;}



    public Solver() {}

    /** initializes the parameters which are common to all solvers.
     *
     * @param thread the solver's thread.
     * @param problemSupervisor the supervisor for the problem.
     */
    public void initialize(Thread thread,ProblemSupervisor problemSupervisor) {
        this.myThread              = thread;
        this.problemSupervisor     = problemSupervisor;
        problemId                  = problemSupervisor.problemId;
        combinedId                 = problemId+"@"+solverId + ":" + solverNumber;
        globalParameters           = problemSupervisor.globalParameters;
        normalizer                 = problemSupervisor.normalizer;
        predicates                 = problemSupervisor.inputClauses.predicates;
        symboltable                = problemSupervisor.inputClauses.symboltable;
        monitoring                 = problemSupervisor.monitor != null;
        monitor                    = problemSupervisor.monitor;
        model                      = problemSupervisor.model;
        trackReasoning             = globalParameters.trackReasoning;
        }


    /** The key method, which has to be implemented by the solvers.<br>
     * It is supposed to find a model or a contradiction in the clauses.
     *
     * @return Un/Satisfiable or null
     */
    public Result solveProblem() {return null;};

    /** returns the solver's statistics.
     *
     * @return the statistics of the solver */
    public Statistic getStatistics() {return null;};


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
        ArrayList<int[]> falseClauses = problemSupervisor.inputClauses.falseClausesInModel(model);
        if(falseClauses != null) {return new Erraneous(problemId,"Solver", startTime, model,falseClauses,symboltable);}
        else {return null;}}




}
