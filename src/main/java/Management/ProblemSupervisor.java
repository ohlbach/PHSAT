package Management;

import Coordinator.CentralProcessor;
import Coordinator.PreProcessor;
import Datastructures.Clauses.BasicClauseList;
import Datastructures.Results.Result;
import Datastructures.Statistics.ProblemStatistics;
import Datastructures.Statistics.Statistic;
import Generators.Generator;
import Solvers.Solver;

import java.io.PrintStream;
import java.util.ArrayList;
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
    Result result;
    Thread[] threads;
    Solver[] solvers;
    Result[] results;
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
        int numberOfSolvers = solverParameters.size();
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
        Thread centralThread = new Thread(()->centralProcessor.processTasks());
        centralThread.start();
        for(int i = 0; i < numberOfSolvers; ++i) {threads[i].start();}
        try {centralThread.join();} catch (InterruptedException e) {}
        for(int i = 0; i < numberOfSolvers; ++i) {threads[i].interrupt();}
        globalParameters.log("Processors finished for problem " + problemId);
        }

        public synchronized void aborted(String  solverId) {
            globalParameters.log("Solver " + solverId + " gave up on problem " + problemId);
            ++statistics.aborted;
    }

    public Statistic[] collectStatistics() {
        int solvLength = 0;
        if(solvers != null) {solvLength = solvLength;}
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
