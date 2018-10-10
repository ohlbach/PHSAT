package Solvers;

import Management.ProblemSupervisor;

import java.util.HashMap;

/**
 * Created by ohlbach on 09.10.2018.
 */
public class SolverData {
    String solverType;
    ProblemSupervisor problemData;
    HashMap<String,Object> solverParameters;
    Thread thread;

}
