package Management;

import Datastructures.Clauses.AllClauses.InitializerSimplifier;
import Datastructures.Clauses.InputClauses;
import Datastructures.Clauses.Simplifiers.ClauseSimplifier;
import Datastructures.Results.*;
import Datastructures.Statistics.Statistic;
import Datastructures.Theory.EquivalenceClasses;
import Datastructures.Theory.Model;
import Datastructures.TwoLiteral.TwoLitClauses;
import ProblemGenerators.ProblemGenerator;
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
    public Controller controller;

    public Model model;
    public EquivalenceClasses equivalenceClasses;
    public ClauseSimplifier clauseSimplifier;
    public Thread equivalenceThread;

    public TwoLitClauses twoLitClauses;
    public Thread twoLitThread;

    public InitializerSimplifier clauses;
    public Thread allClausesThread;

    public Thread supervisorThread;

    private ArrayList<Solver> solvers;

    public SupervisorStatistics statistics = null;
    private ProblemGenerator problemGenerator;

    public ProblemSupervisor(GlobalParameters globalParameters, ProblemGenerator problemGenerator,
                             ArrayList<Solver> solvers) {
        this.globalParameters = globalParameters;
        jobname               = globalParameters.jobname;
        problemId             = (String)problemParameters.get("name");
        this.problemGenerator = problemGenerator;
        this.solvers          = solvers;
    }

    public int clauseCounter = 0;

    public synchronized int nextClauseId() {
        return ++clauseCounter;}

    public void solveProblem()  {
        try{
            numberOfSolvers = solvers.size();
            statistics.solvers = numberOfSolvers;
            threads = new Thread[numberOfSolvers];
            results = new Result[numberOfSolvers];
            for(int i = 0; i < numberOfSolvers; ++i) {
                int j = i;
                threads[i] = new Thread(() -> {results[j] = solvers.get(j).solveProblem(inputClauses);});}
            for(int i = 0; i < numberOfSolvers; ++i) {threads[i].start();}
            for(int i = 0; i < numberOfSolvers; ++i) {threads[i].join();}}
        catch(Exception ex) {}
        globalParameters.logstream.println("Solvers finished for problem " + problemId);}



    /** This method is called by the solvers to indicate that they have done their job or gave up.
     * If the solver succeeded (satisfiable or unsatisfiable) then all other solvers are interrupted. <br>
     * Some messages are logged.
     *
     * @param solver    which finished its work.
     * @param result    the result of the solver's work.
     * @param message   an extra message to explain the result.
     */
    public synchronized void finished(Solver solver, Result result, String message) {
        this.result = result;
        globalParameters.logstream.println("Solver " + solver.solverId + " finished  work at problem " + problemId);
        if(message != null && !message.isEmpty()) {globalParameters.logstream.println(message);}
        if(result instanceof Satisfiable || result instanceof Unsatisfiable) {
            for(Thread thread : threads) {thread.interrupt();}
            return;}
        if(result instanceof Aborted)    {++statistics.aborted;}
        if(result instanceof Erraneous ) {++statistics.erraneous;}}


    /** The method collects the individual solver statistics into an array of Statistic-objects:
     * statistics[0]    = supervisor statistics <br>
     * statistics[1...] = solver statistics
     *
     * @return the array of Statistics objects.
     */
    public Statistic[] collectStatistics() {
        Statistic[] statistics = new Statistic[2+solvers.size()];
        statistics[0] = this.statistics;
        statistics[1] = inputClauses.getStatistics(problemId);
        for(int i = 0; i < solvers.size(); ++i) {statistics[i+2] = solvers.get(i).getStatistics();}
        return statistics;
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
