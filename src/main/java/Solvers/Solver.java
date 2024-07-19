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
 * <br>
 * Its static methods maintain information about all solver classes.<br>
 * Its instance methods provide the interface to the ProblemSupervisor.
 */
public abstract class Solver {

    // Static data and methods
    // ***********************

    /** the list of all solver types */
    public static String[] solvers = new String[]{"walker","backtracker","resolution"};

    /** the list of solver classes */
    public static ArrayList<Class> solverClasses = new ArrayList<>();
    static{
        solverClasses.add(Solvers.Walker.Walker.class);
        solverClasses.add(Solvers.Backtracker.Backtracker.class);
    }

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
        return switch (solverName.toLowerCase()) {
            case "resolution"  -> Resolution.class;
            case "walker"      -> Walker.class;
            case "backtracker" -> Backtracker.class;
            default -> null;};}

    /**
     * Sets the default values for the solvers based on the module values.
     *
     * @param moduleValues A HashMap containing the module values.
     */
    public static void setDefaults(HashMap<String,ArrayList<String>> moduleValues) {
        for(String solver : solvers) {
            Class solverClass = solverClass(solver);
            if(solverClass == null) {
                System.err.println("setDefaults: unknown solver class " + solver + ".");
                System.exit(1);}
            ArrayList<String> values = moduleValues.get(solver);
            if(values != null) {
                try{solverClass(solver).getMethod("setDefaults",ArrayList.class).invoke(null,values);}
                catch(Exception ignore) {
                    System.err.println("No Method 'setDefaults' for solver " + solver + " found\n"+
                            "Either unknown solver, or incomplete implementation of the solver class.");
                    System.exit(1);}}}}


    /** Analyses the parameters and generates the corresponding solvers.
     *
     * @param parameterList the parameters
     * @return  a list of solvers
     */
    public static ArrayList<Solver> makeSolvers(ArrayList<Parameters> parameterList) {
        ArrayList<Solver> solvers = new ArrayList<>();
        if(parameterList.isEmpty()) {
            System.err.println("No Solver parameters provided. System stops.");
            System.exit(1);}
        for(Parameters parameters: parameterList) {
            if(!(Boolean)parameters.parameters.get(0).value) continue;
            Class solverClass = solverClass(parameters.name);
            if(solverClass == null) {
                System.err.println("System Error: unknown solver: " + parameters.name);
                new Exception().printStackTrace(); System.exit(1);}
            try{
                Method makeSolver = solverClass.getMethod("makeSolvers",Parameters.class,ArrayList.class);
                makeSolver.invoke(null,parameters,solvers);}
            catch(Exception ex) {
                System.err.println("System Error: unknown method: makeSolvers\n" + ex.getMessage());
                ex.printStackTrace();System.exit(1);}}
        return solvers;}

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
            catch(Exception ignored){
                System.err.println("Unknown method 'makeParameter' for solver class: " + clazz.getName()+
                        "\nProbably incomplete implementation of the solver class.");
                System.exit(1);}}
        return parameters;}


    // Instance data and methods
    // *************************

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
    public long solverStartTime;

    /** the number of predicates in the problem. */
    public int predicates;

    /** the global model (from the problemSupervisor). */
    public Model model;

    /** the symboltable */
    public  Symboltable symboltable = null;

    /** controls the application of inferenceSteps. */
    public boolean trackReasoning = true;

    /** controls the verification of inference steps. */
    public boolean verify = false;

    /** a monitor for showing the actions */
    public Monitor monitor = null;

    /** true if the monitor is active */
    public boolean monitoring = false;

    /** an identifier for the monitor */
    protected String monitorId;

    /** the thread which operates the solver */
    public Thread myThread;

    /** specifies a reason for an interrupt */
    protected InterruptReason interruptReason;

    /** each solver which uses ClauseList will wait until a new true literal has been processed.*/
    public void waitForTrueLiteralProcessing() {}

    /** causes the solvers to continue their work */
    public void continueProcessing(){}

    /** constructs a solver as an instance of the Processor class.
     *
     * @param solverNumber  to distinguish different solvers of the same type, but different parameters,
     */
    public Solver(Integer solverNumber) {
        this.solverNumber = solverNumber;}


    /** initializes the parameters which are common to all solvers.
     * <br>
     * This method is called in the problemSupervisor's thread.
     *
     * @param problemSupervisor the supervisor for the problem.
     */
    public void initialize(ProblemSupervisor problemSupervisor) {
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
        verify                     = globalParameters.verify;
        }

    /** to be called when another solver has found a solution.
     *  It interrupts myThread.
     */
    public synchronized void problemSolved() {
        interruptReason = InterruptReason.PROBLEMSOLVED;
        myThread.interrupt();}

    /** The key method, which has to be implemented by the solvers.<br>
     * It is supposed to find a model or a contradiction in the clauses.<br>
     * This method is called in the solver's own thread.
     *
     * @return Un/Satisfiable or null
     */
    public Result solveProblem() {return null;}

    /** returns the solver's statistics.
     *
     * @return the statistics of the solver */
    public Statistic getStatistics() {return null;}


    /** This method checks if some predicates are true or all predicates are false in a model
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
        if(falseClauses != null) {return new Erraneous(problemId,"Solver", solverStartTime, model,falseClauses,symboltable);}
        else {return null;}}




}
