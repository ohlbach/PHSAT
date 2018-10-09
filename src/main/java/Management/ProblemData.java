package Management;

import Coordinator.Preprocessor;
import Datastructures.Clauses.BasicClauseList;
import Solvers.SolverData;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Objects;

/**
 * Created by ohlbach on 09.10.2018.
 */
public class ProblemData {

    static enum Status {
        waiting, running, aborted, satisfiable, unsatisfiable;}

    Status status = Status.waiting;
    String sourceType;
    BasicClauseList basicClauseList;
    public HashMap<String,Object> problemParameters;
    public ArrayList<HashMap<String,Object>> solverParameters;
    Preprocessor preprocessor;
    ArrayList<SolverData> solverData;

    public ProblemData(HashMap<String,Object> problemParameters,
            ArrayList<HashMap<String,Object>> solverParameters) {
        this.problemParameters = problemParameters;
        this.solverParameters = solverParameters;}





}
