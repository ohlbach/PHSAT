package Management;

import Datastructures.Clauses.AllClauses.InitializerSimplifier;
import Datastructures.Clauses.InputClauses;
import Datastructures.Clauses.Quantifier;
import Datastructures.Results.*;
import Datastructures.Theory.EquivalenceClasses.EquivalenceClasses;
import Datastructures.Theory.Model;
import InferenceSteps.InfInputClause;
import Management.Monitor.Monitor;
import ProblemGenerators.ProblemGenerator;
import Solvers.Simplifier.Simplifier;
import Solvers.Solver;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.HashMap;

/** A problem supervisor solves a single problem by using several cooperating solvers in parallel threads.
 * Created by ohlbach on 09.10.2018.
 */
public class ProblemSupervisor {
    public String jobname;
    public String problemId;

    public InputClauses inputClauses;
    public GlobalParameters globalParameters;
    public HashMap<String,Object> problemParameters;
    public HashMap<String,Object> initializeParameters;
    public ArrayList<HashMap<String,Object>> solverParameters;
    public InitializerSimplifier initializer;
    public Result result = null;
    private String solver = null;
    Thread[] threads;
    Result[] results;
    int numberOfSolvers;
    public ProblemDistributor problemDistributor;

    public Model model;
    public EquivalenceClasses equivalenceClasses;
    public Thread equivalenceThread;

    public Simplifier simplifier;
    public Thread simplifierThread;

    public Thread supervisorThread;

    private ArrayList<Solver> solvers;

    public SupervisorStatistics statistics = null;
    private ProblemGenerator problemGenerator;

    private boolean trackReasoning;

    public Monitor monitor;

    public QuSatJob quSatJob;

    public ProblemSupervisor(QuSatJob quSatJob, GlobalParameters globalParameters, ProblemGenerator problemGenerator,
                             ArrayList<Solver> solvers) {
        this.quSatJob         = quSatJob;
        this.globalParameters = globalParameters;
        jobname               = globalParameters.jobname;
        trackReasoning        = globalParameters.trackReasoning;
        //problemId             = problemGenerator.inputClauses.problemName;
        this.problemGenerator = problemGenerator;
        this.solvers          = solvers;
    }


    int predicates;
    public int clauseCounter = 0;

    public synchronized int nextClauseId() {
        return ++clauseCounter;}

    public void solveProblem()  {
        monitor = quSatJob.getMonitor(problemId);
        try {
            inputClauses = problemGenerator.generateProblem(null);
            if(globalParameters.showClauses && globalParameters.logstream != null) quSatJob.printlog(inputClauses.toString());
            model = new Model(inputClauses.predicates);
            readConjunctions(inputClauses.conjunctions);
            startEquivalenceClasses();
            if(globalParameters.simplifier) startSimplifier();
            numberOfSolvers = solvers.size();
            statistics.solvers = numberOfSolvers;
            threads = new Thread[numberOfSolvers];
            results = new Result[numberOfSolvers];
            for(int i = 0; i < numberOfSolvers; ++i) {
                int j = i;
               threads[i] = new Thread(() -> {
                   Result result = solvers.get(j).solveProblem();
                   finished(result);});}
            for(int i = 0; i < numberOfSolvers; ++i) {threads[i].start();}
            for(int i = 0; i < numberOfSolvers; ++i) {threads[i].join();}
            equivalenceThread.join();
            if(simplifierThread != null) simplifierThread.join();}
        catch(Exception ex) {}
        globalParameters.logstream.println("Solvers finished for problem " + problemId);}

    /** inserts the initial conjunctions (if any) into the model
     *
     * @param conjunctions a list of input clauses
     * @throws Unsatisfiable if the conjunctions are contradictory
     */
    private void readConjunctions(ArrayList<int[]> conjunctions) throws Unsatisfiable {
        for(int[] inputClause : conjunctions) {
            assert inputClause[1] == Quantifier.AND.ordinal();
            for(int i = 2; i < inputClause.length; ++i) {
                model.add(inputClause[i],trackReasoning ? new InfInputClause(inputClause[0]) : null);}}}

    private void startEquivalenceClasses() throws Unsatisfiable{
        equivalenceClasses = new EquivalenceClasses(this,monitor);
        equivalenceClasses.readEquivalences(inputClauses.equivalences);
        equivalenceThread = new Thread(()->{
            Result result = equivalenceClasses.solveProblem();
            finished(result);});
        equivalenceThread.start();
    }

    private void startSimplifier() throws Result {
        simplifier = new Simplifier(this);
        simplifierThread = new Thread(() -> {
            Result result = simplifier.solveProblem();
            finished(result);});
        simplifierThread.start();}

    /** This method is called by the solvers to indicate that they have done their job or gave up.
     * If the solver succeeded (satisfiable or unsatisfiable) then all other solvers are interrupted. <br>
     * Some messages are logged.
     *
     * @param result    the result of the solver's work.
     */
    public synchronized void finished(Result result) {
        if(result == null) return;
        if(result instanceof Satisfiable) checkModel((Satisfiable) result);
        this.result = result;
        Class solver = result.solver;
        globalParameters.logstream.println("Solver " + solver.getSimpleName() + " finished  work at problem " + problemId);
        if(result.message != null && !result.message.isEmpty()) {globalParameters.logstream.println(result.message);}
        if(threads != null) {for(Thread thread : threads) {thread.interrupt();}}
        if(simplifierThread != null) simplifierThread.interrupt();
        equivalenceThread.interrupt();
        if(result instanceof Aborted)    {++statistics.aborted;}
        if(result instanceof Erraneous ) {++statistics.erraneous;}}

    /** checks the model against the input clauses.
     * If some clauses are false in this model, they are printed and the system exits.
     *
     * @param satisfiable the result of a SAT-search.
     */
    protected void checkModel(Satisfiable satisfiable) {
        ArrayList<int[]> falseClauses = inputClauses.falseClausesInModel(satisfiable.model);
        if(!falseClauses.isEmpty()) {
            System.out.println("Wrong model derived by " + satisfiable.solver.getSimpleName() +
                    "for problem " + problemId + "\n  " +
                    model.toString() +
                    "\n  False Clauses:\n");
            System.out.println(InputClauses.toString(falseClauses,inputClauses.symboltable));
            System.out.println("Process Terminates");
            System.exit(1);}}

    /** The method collects the individual solver statistics into an array of Statistic-objects:
     * statistics[0]    = supervisor statistics <br>
     * statistics[1...] = solver statistics
     *
     * @return the array of Statistics objects.
     */
    public static void printStatistics(GlobalParameters globalParameters, ArrayList<ProblemSupervisor> problemSupervisors) {
       String statistics = globalParameters.statistic;

    }

    /** prints the result to the PrintStream
     *
     * @param out a PrintStream
     */
    public void reportResult(PrintStream out) {
        out.println("Result for problem " + problemId + ":");
        if(result == null) {out.println("   no result");}
        else {out.println("  "+result.toString());}}


    public void announceResult(Result unsatisfiable,String source) {};

}
