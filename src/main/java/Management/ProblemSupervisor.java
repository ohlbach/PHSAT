package Management;

import Datastructures.Clauses.InputClauses;
import Datastructures.Results.Aborted;
import Datastructures.Results.Erraneous;
import Datastructures.Results.Result;
import Datastructures.Results.Satisfiable;
import Datastructures.Theory.Model;
import Management.Monitor.Monitor;
import ProblemGenerators.ProblemGenerator;
import Solvers.Normalizer.Normalizer;
import Solvers.Solver;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.HashMap;

/** A problem supervisor solves a single problem by using several cooperating solvers in parallel threads.
 * Created by ohlbach on 09.10.2018.
 */
public class ProblemSupervisor {
    /** the name of the job. */
    public String jobname;
    /** the name of the problem. */
    public String problemId;
    /** the input clauses. */
    public InputClauses inputClauses;
    /** the global parameters. */
    public GlobalParameters globalParameters;
    /** the problem parameters.*/
    public HashMap<String,Object> problemParameters;
    /** the solver parameters. */
    public ArrayList<HashMap<String,Object>> solverParameters;
    /** the final result */
    public Result result = null;

    private long startTime;

    /** stores all the solver threads */
    Thread[] threads;
    /** the number of solvers */
    int numberOfSolvers;

    /** the global model */
    public Model model;
    private ArrayList<Solver> solvers;


    public SupervisorStatistics statistics = null;
    private ProblemGenerator problemGenerator;

    private boolean trackReasoning;

    public Monitor monitor;

    public QuSatJob quSatJob;

    public Normalizer normalizer;

    public ProblemSupervisor(QuSatJob quSatJob, GlobalParameters globalParameters, ProblemGenerator problemGenerator,
                             ArrayList<Solver> solvers) {
        this.quSatJob         = quSatJob;
        this.globalParameters = globalParameters;
        jobname               = globalParameters.jobname;
        trackReasoning        = globalParameters.trackReasoning;
        this.problemGenerator = problemGenerator;
        this.solvers          = solvers;
        statistics            = new SupervisorStatistics("Supervisor");
    }


    /** for enumerating the clauses */
    public int clauseCounter = 0;

    /** for enumerating the clauses
     *
     * @return the next free clause identifier
     */
    public synchronized int nextClauseId() {
        return ++clauseCounter;}

    /** starts the solvers in parallel threads and waits for their results.
     */
    public void solveProblem()  {
        startTime = System.nanoTime();
        StringBuilder errors = new StringBuilder();
        try {
            inputClauses = problemGenerator.generateProblem(errors);
            problemId = inputClauses.problemId;
            clauseCounter = inputClauses.nextId-1;
            if(errors.length() > 1) {
                System.out.println("Error when reading/generating problem '" + problemId + "'");
                System.out.println(errors);
                System.out.println(problemGenerator.toString());
                System.out.println("System is aborted.");
                System.exit(1);}
            monitor   = quSatJob.getMonitor(problemId);
            if(!globalParameters.cnfFile.equals("none")) inputClauses.makeCNFFile(globalParameters.jobDirectory,globalParameters.cnfFile);
            if(globalParameters.logstream != null) {
                boolean infoOnly = !globalParameters.showClauses;
                globalParameters.logstream.println(inputClauses.toString(inputClauses.symboltable,infoOnly));}
            model = new Model(inputClauses.predicates);
            normalizer = new Normalizer(this);
            Result result = normalizer.solveProblem(this);
            if(result != null)  {finished(result); return;}
            numberOfSolvers = solvers.size();
            threads = new Thread[numberOfSolvers];
            for(int i = 0; i < numberOfSolvers; ++i) {int j = i;
                threads[i] = new Thread(() -> finished(solvers.get(j).solveProblem(this)));
                solvers.get(i).initialize(threads[i],this);}
            for(int i = 0; i < numberOfSolvers; ++i) {threads[i].start();}
            for(int i = 0; i < numberOfSolvers; ++i) {threads[i].join();}}
        catch(Exception ex) {
            System.out.println(ex);
            ex.printStackTrace();
            System.exit(0);}
        if(globalParameters.logstream != null) {
            float time = (float)(System.nanoTime() - startTime) / (float)1000.0;
            globalParameters.logstream.println("Solvers finished the problem " + problemId +" in " + time + "μs");}}


    /** This method is called to indicate that they have done their job or gave up.
     * If the solver succeeded (satisfiable or unsatisfiable) then all other solvers are interrupted. <br>
     * Some messages are logged.
     */
    public synchronized void finished(Result result) {
        if(result == null) return;
        if(result instanceof Aborted) {
            if(result.message != null && !result.message.isEmpty()) {quSatJob.printlog(result.message);}
            ++statistics.aborted; return;}
        if(result instanceof Erraneous ) {
            if(result.message != null && !result.message.isEmpty()) {quSatJob.printlog(result.message);}
            ++statistics.erraneous; return;}
        if(result instanceof Satisfiable) checkModel((Satisfiable) result);
        this.result = result;
        quSatJob.printlog("Solver " + result.solverId + " finished  work at problem " + problemId);
        quSatJob.printlog("Result:\n"+result.toString(inputClauses.symboltable));
        if(threads != null) {for(Thread thread : threads) {thread.interrupt();}}}

    /** checks the model against the input clauses.
     * If some clauses are false in this model, they are printed and the system exits.
     *
     * @param satisfiable the result of a SAT-search.
     */
    protected void checkModel(Satisfiable satisfiable) {
        ArrayList<int[]> falseClauses = inputClauses.falseClausesInModel(satisfiable.model);
        if(falseClauses.isEmpty()) {
            globalParameters.logstream.println("Problem " + problemId + ": model successfully checked.");}
        else {
            System.out.println("Wrong model derived by " + satisfiable.solverId +
                    " for problem " + problemId + "\n  " +
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


}
