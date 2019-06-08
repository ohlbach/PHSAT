package Management;

import Coordinator.CentralProcessor;
import Coordinator.PreProcessor;
import Datastructures.Clauses.BasicClauseList;
import Datastructures.Results.Result;
import Datastructures.Statistics.DataStatistics;
import Datastructures.Statistics.ProblemStatistics;
import Datastructures.Statistics.Statistic;
import Datastructures.Results.*;
import Generators.Generator;
import Solvers.Solver;

import java.io.PrintStream;
import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;

/**
 * Created by ohlbach on 09.10.2018.
 */
public class ProblemSupervisor {
    int problemNumber = 0;
    public String problemId;

    BasicClauseList basicClauseList;
    public GlobalParameters globalParameters;
    public HashMap<String,Object> problemParameters;
    public ArrayList<HashMap<String,Object>> solverParameters;
    PreProcessor preProcessor;
    CentralProcessor centralProcessor;
    Thread centralThread;
    Result result;
    Thread[] threads;
    Solver[] solvers;
    Result[] results;
    int numberOfSolvers;
    int activeSolvers;

    public ProblemStatistics statistics = null;

    public ProblemSupervisor(int problemNumber,GlobalParameters globalParameters,
                             HashMap<String,Object> problemParameters,
                             ArrayList<HashMap<String,Object>> solverParameters) {
        this.problemNumber          = problemNumber;
        Object name                 = problemParameters.get("name");
        this.problemId              = (name == null) ? "P"+problemNumber : (String)name;
        this.globalParameters       = globalParameters;
        this.problemParameters      = problemParameters;
        this.solverParameters       = solverParameters;
        statistics                  = new ProblemStatistics(problemId);
    }

    /** reads or generates the SAT-clauses
     *
     * @param errors   for error messages
     * @param warnings for warnings
     * @return true if method succeeded, false if an error has ocurred
     */
    public boolean generateProblem(StringBuffer errors, StringBuffer warnings) {
        String type = (String)problemParameters.get("type");
        basicClauseList = Generator.generate(type,problemParameters,errors,warnings);
        return basicClauseList != null;}

    public Result preprocessProblem() {
        globalParameters.log("Preprocessor starts for problem " + problemId);
        basicClauseList.addToStatistics(statistics);
        preProcessor = new PreProcessor(this,problemParameters,basicClauseList);
        result = preProcessor.prepareClauses();
        globalParameters.log("Preprocessor finished for problem " + problemId);
        return result;}


    public void solveProblem() {
        centralProcessor = new CentralProcessor(preProcessor);
        numberOfSolvers = solverParameters.size();
        activeSolvers = numberOfSolvers;
        solvers = new Solver[numberOfSolvers];
        statistics.solvers = numberOfSolvers;
        for(int i = 0; i < numberOfSolvers; ++i) {
            HashMap<String,Object> solverParameter = solverParameters.get(i);
            solvers[i] = Solver.construct((String)solverParameter.get("type"),solverParameter,centralProcessor);}
        threads = new Thread[numberOfSolvers];
        results = new Result[numberOfSolvers];
        for(int i = 0; i < numberOfSolvers; ++i) {
            int j = i;
            threads[i] = new Thread(() -> {results[j] = solvers[j].solve();});}
        centralThread = new Thread(()->centralProcessor.processTasks());
        centralThread.start();
        for(int i = 0; i < numberOfSolvers; ++i) {threads[i].start();}
        try {centralThread.join();
            for(int i = 0; i < numberOfSolvers; ++i) {threads[i].join();}}
        catch (InterruptedException e) {}
        globalParameters.log("Processors finished for problem " + problemId);
        }

    /** This method is called by the solvers to indicate that they have done their job.
     * If the solver succeeded (satisfiable or unsatisfiable) then the centralProcessor and all other solvers are interrupted. <br>
     * If the solver gave up, or caused an error, then the activeSolver counter is decreased. <br>
     * As soon as it reached zero the centralProcessor is interrupted. <br>
     * Some messages are logged.
     *
     * @param result    the result of the solver's work
     * @param solverId  the solver who called the method
     * @param problemId the problem the solver tackled
     * @param message   an extra message to explain the result.
     */
    public synchronized void finished(Result result, String solverId, String problemId, String message) {
        this.result = result;
        globalParameters.log("Solver " + solverId + " finished  work at problem " + problemId);
        if(message != null && !message.isEmpty()) {globalParameters.log(message);}
        --activeSolvers;
        if(result instanceof Satisfiable || result instanceof Unsatisfiable) {
            centralThread.interrupt();
            for(int i = 0; i < numberOfSolvers; ++i) {threads[i].interrupt();}
            return;}
        if(result instanceof Aborted)    {statistics.incAborted();}
        if(result instanceof Erraneous ) {statistics.incErraneous();}
        if(activeSolvers == 0) {centralThread.interrupt();}}


    /** The method collects the individual solver statistics into an array of Statistic-objects:
     * statistics[0]    = supervisor statistics <br>
     * statistics[1]    = preprocessor statistics <br>
     * statistics[2]    = central processor statistics <br>
     * statistics[3...] = solver statistics
     *
     * @return the array of Statistics objects.
     */
    public Statistic[] collectStatistics() {
        int solvLength = 0;
        if(solvers != null) {solvLength = solvers.length;}
        Statistic[] statistics = new Statistic[(centralProcessor == null ? 2 : 3)+solvLength];
        statistics[0] = this.statistics;
        statistics[1] = preProcessor.statistics;
        if(centralProcessor != null) {statistics[2] = centralProcessor.statistics;}
        for(int i = 0; i < solvLength; ++i) {statistics[i+3] = solvers[i].statistics;}
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
