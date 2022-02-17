package Management;

import Datastructures.Clauses.AllClauses.InitializerSimplifier;
import Datastructures.Clauses.BasicClauseList;
import Datastructures.Clauses.Simplifiers.ClauseSimplifier;
import Datastructures.Results.Result;
import Datastructures.Statistics.Statistic;
import Datastructures.Results.*;
import Datastructures.Theory.EquivalenceClasses;
import Datastructures.Theory.Model;
import Datastructures.TwoLiteral.TwoLitClauses;
import Generators.Generator;
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

    public BasicClauseList basicClauseList;
    public GlobalParameters globalParameters;
    public HashMap<String,Object> problemParameters;
    public ArrayList<HashMap<String,Object>> solverParameters;
    public Result result = null;
    private String solver = null;
    Thread[] threads;
    Solver[] solvers;
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

    private Monitor monitor;
    private String monitorId;
    private boolean monitoring;


    public SupervisorStatistics statistics = null;

    public ProblemSupervisor(Controller controller,
                             GlobalParameters globalParameters,
                             HashMap<String,Object> problemParameters,
                             ArrayList<HashMap<String,Object>> solverParameters) {
        this.controller             = controller;
        jobname                     = controller.jobname;
        problemId                   = (String)problemParameters.get("name");
        this.globalParameters       = globalParameters;
        this.problemParameters      = problemParameters;
        this.solverParameters       = solverParameters;
        statistics                  = new SupervisorStatistics(problemId);
        supervisorThread            = Thread.currentThread();
        monitor                     = globalParameters.monitor;
        monitoring                  = monitor != null;
        monitorId                   = problemId+"PS";
    }

    public int clauseCounter = 0;

    public synchronized int nextClauseId() {
        return ++clauseCounter;}

    public void solveProblem(Monitor errors, Monitor warnings)  {
        if(!generateProblem(errors, warnings)) return;
        clauses = new InitializerSimplifier(1,null,this);
        try{
            equivalenceThread = new Thread(()-> equivalenceClasses.run());
            equivalenceThread.start();
            twoLitThread = new Thread(() -> twoLitClauses.run());
            twoLitThread.start();
            allClausesThread = new Thread(() -> clauses.run());
            allClausesThread.start();


            if(result != null) {return;}
            numberOfSolvers = solverParameters.size();
            solvers = new Solver[numberOfSolvers];
            statistics.solvers = numberOfSolvers;
            Statistic[] solverStatistics = new Statistic[numberOfSolvers];
            for(int i = 0; i < numberOfSolvers; ++i) {
                HashMap<String,Object> solverParameter = solverParameters.get(i);
                solvers[i] = Solver.construct((String)solverParameter.get("type"),i,solverParameter,this);}
            threads = new Thread[numberOfSolvers];
            results = new Result[numberOfSolvers];
            for(int i = 0; i < numberOfSolvers; ++i) {
                int j = i;
                threads[i] = new Thread(() -> {results[j] = solvers[j].solve();});}
            for(int i = 0; i < numberOfSolvers; ++i) {threads[i].start();}
            for(int i = 0; i < numberOfSolvers; ++i) {threads[i].join();}}
        catch (InterruptedException e) {}

        for(Solver solver : solvers) {
            System.out.println(solver.getStatistics().toString(false));
        }
        globalParameters.log("Solvers finished for problem " + problemId);}


    /** reads or generates the SAT-clauses
     *
     * @return true if method succeeded, false if an error has occurred
     */
    public boolean generateProblem(Monitor errors, Monitor warnings) {
        String type = (String)problemParameters.get("type");
        basicClauseList = Generator.generate(type,problemParameters,this,errors,warnings);
        return basicClauseList != null;}


    /** a thread which found a solution calls this method to set the result and interrupt all other threads
     *
     * @param result of the search
     * @param solver who found the solution.
     */
    public synchronized void setResult(Result result, String solver) {
        interruptAll();
        this.result = result;
        this.solver = solver;
        if(monitoring) {
            monitor.print(monitorId,"Result of solver " + solver + ":\n" + result.toString());}}

    /** interrupts all threads */
    private void interruptAll() {
        supervisorThread.interrupt();
        if(equivalenceThread != null)  equivalenceThread.interrupt();
        if(twoLitThread != null)       twoLitThread.interrupt();
        if(allClausesThread != null)   allClausesThread.interrupt();
    }



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
        globalParameters.log("Solver " + solver.solverId + " finished  work at problem " + problemId);
        if(message != null && !message.isEmpty()) {globalParameters.log(message);}
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
        Statistic[] statistics = new Statistic[2+solvers.length];
        statistics[0] = this.statistics;
        statistics[1] = basicClauseList.getStatistics(problemId);
        for(int i = 0; i < solvers.length; ++i) {statistics[i+2] = solvers[i].getStatistics();}
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




}
