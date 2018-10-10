package Management;

import Coordinator.CentralProcessor;
import Coordinator.Preprocessor;
import Datastructures.Clauses.BasicClauseList;
import Datastructures.Results.Result;
import Generators.Generator;
import Solvers.Solver;

import java.util.ArrayList;
import java.util.HashMap;

/**
 * Created by ohlbach on 09.10.2018.
 */
public class ProblemSupervisor {

    static enum Status {
        waiting, running, aborted, satisfiable, unsatisfiable;}

    Status status = Status.waiting;
    BasicClauseList basicClauseList;
    public HashMap<String,Object> globalParameters;
    public HashMap<String,Object> problemParameters;
    public ArrayList<HashMap<String,Object>> solverParameters;
    Preprocessor preprocessor;
    CentralProcessor centralProcessor;
    StringBuffer errors = new StringBuffer();
    StringBuffer warnings = new StringBuffer();
    Result result;
    Thread[] threads;
    Solver[] solvers;
    Result[] results;

    public ProblemSupervisor(
            HashMap<String,Object> globalParameters,
            HashMap<String,Object> problemParameters,
                             ArrayList<HashMap<String,Object>> solverParameters) {
        this.globalParameters = globalParameters;
        this.problemParameters = problemParameters;
        this.solverParameters = solverParameters;}

    public void solve() {
        String type = (String)problemParameters.get("type");
        basicClauseList = Generator.generate(type,problemParameters,errors,warnings);
        if(basicClauseList == null) {return;}
        preprocessor = new Preprocessor(globalParameters,problemParameters,basicClauseList);
        result = preprocessor.prepareClauses();
        if(result == null) {return;}
        centralProcessor = new CentralProcessor(preprocessor);
        int size = solverParameters.size();
        solvers = new Solver[size];
        for(int i = 0; i < size; ++i) {
            HashMap<String,Object> solverParameter = solverParameters.get(i);
            solvers[i] = Solver.construct((String)solverParameter.get("type"),i,globalParameters,solverParameter,centralProcessor);}
        threads = new Thread[size];
        for(int i = 0; i < size; ++i) {
            int j = i;
            threads[i] = new Thread(() -> {results[j] = solvers[j].solve(errors,warnings);});}
        Thread centralThread = new Thread(()->centralProcessor.processTasks());
        centralThread.start();
        for(int i = 0; i < size; ++i) {threads[i].start();}
        try {centralThread.join();} catch (InterruptedException e) {}
        for(int i = 0; i < size; ++i) {threads[i].interrupt();}
        for(int i = 0; i < size; ++i) {
            try {threads[i].join();} catch (InterruptedException e) {}
        }

    }









}
