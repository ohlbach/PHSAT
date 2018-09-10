package Management;

import Datastructures.Model;
import Utilities.Utilities;
import org.junit.Test;

import java.util.ArrayList;
import java.util.HashMap;

import static org.junit.Assert.*;

/**
 * Created by ohlbach on 10.09.2018.
 */
public class SolverControlTest {

    public static class TestSolver {
        public static void solve(HashMap<String,Object> solverControl, HashMap<String,Object> problemControl, Model globalModel,
                                 StringBuffer errors, StringBuffer warnings) {
            problemControl.put("SOLVED",""+solverControl.get("ID")+problemControl.get("size"));}}



    static public class Generator{
        public static ArrayList<HashMap<String,Object>> parseParameters(HashMap<String,String> parameters, StringBuffer errors, StringBuffer warnings){
            ArrayList<HashMap<String,Object>> list = new ArrayList<>();
            String number = parameters.get("number");
            Integer n = Utilities.parseInteger("place",number,errors);
            for(int i = 1; i <= n; ++i) {
                HashMap<String,Object> control = new HashMap<>();
                control.put("size",i);
                list.add(control);}
            return list;}

        public static void generate(HashMap<String,Object> parameters, StringBuffer errors, StringBuffer warnings) {
            parameters.put("number",10*(Integer)parameters.get("size"));}
    }

    @Test
    public void solveProblem() throws Exception {
        System.out.println("solveProblem");
        ArrayList<HashMap<String,Object>> solverParameters = new ArrayList<>();
        for(int i = 0; i < 5; ++i) {
            HashMap<String,Object> map = new HashMap<>();
            map.put("ID",i);
            solverParameters.add(map);}
        KVAnalyser anal = new KVAnalyser(null,null);
        anal.solverParameters = solverParameters;
        HashMap<String,Object> solverControl = new HashMap<>();
        solverControl.put("class",TestSolver.class);
        HashMap<String,Object> problemControl = new HashMap<>();
        problemControl.put("number",5);
        SolverControl sctr = new SolverControl(null,null);
        sctr.solveProblem(problemControl);

    }

    @Test
    public void solveProblems() throws Exception {

    }

    @Test
    public void solve() throws Exception {
        System.out.println("solve");
        HashMap<String,Object> solverControl = new HashMap<>();
        solverControl.put("class",TestSolver.class);
        HashMap<String,Object> problemControl = new HashMap<>();
        problemControl.put("number",5);
        SolverControl sctr = new SolverControl(null,null);
        sctr.solve(solverControl,problemControl,null);
        assertEquals(5,(int)(Integer)problemControl.get("SOLVED"));
    }

    @Test
    public void solve1() throws Exception {

    }

}