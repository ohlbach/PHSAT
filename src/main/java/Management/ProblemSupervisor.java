package Management;

import Coordinator.CentralProcessor;
import Coordinator.PreProcessor;
import Datastructures.Clauses.BasicClauseList;
import Datastructures.Results.Result;
import Datastructures.Statistics.ProblemStatistics;
import Generators.Generator;
import Generators.StringClauseSetGenerator;
import Solvers.Solver;

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
    ProblemStatistics statistics  = new ProblemStatistics();

    public ProblemSupervisor(int problemNumber,GlobalParameters globalParameters,
                             HashMap<String,Object> problemParameters,
                             ArrayList<HashMap<String,Object>> solverParameters) {
        this.problemNumber    = problemNumber;
        Object name = problemParameters.get("problem");
        this.problemId = (name == null) ? "P"+problemNumber : (String)name;
        this.globalParameters = globalParameters;
        this.problemParameters = problemParameters;
        this.solverParameters = solverParameters;
        globalParameters.supervisor = this;
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
        basicClauseList.addStatistics(statistics);
        preProcessor = new PreProcessor(globalParameters,problemParameters,basicClauseList);
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
            solvers[i] = Solver.construct((String)solverParameter.get("type"),i,globalParameters,solverParameter,centralProcessor);}
        threads = new Thread[numberOfSolvers];
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




}
