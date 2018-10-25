package Management;

import Datastructures.Theory.Model;
import Utilities.Utilities;

import java.util.ArrayList;
import java.util.HashMap;

/**
 * Created by ohlbach on 10.09.2018.
 */
public class SolverControlTest {
    static StringBuffer st = new StringBuffer();

    public static class TestSolver {
        public TestSolver(Integer n) {}

        public static void solve(HashMap<String,Object> solverControl, HashMap<String,Object> problemControl, Model globalModel,
                                 StringBuffer errors, StringBuffer warnings) {
            st.append("SOLVED "+solverControl.get("ID")+ " " +problemControl.get("size")+"\n");}}



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
/*
    @Test
    public void solveProblem() throws Exception {
        System.out.println("solveProblem");
        ArrayList<HashMap<String,Object>> solverParameters = new ArrayList<>();
        for(int i = 0; i < 5; ++i) {
            HashMap<String,Object> map = new HashMap<>();
            map.put("ID",i);
            map.put("class", TestSolver.class );
            solverParameters.add(map);}
        KVAnalyser anal = new KVAnalyser(null,null);
        anal.solverParameters = solverParameters;
        HashMap<String,Object> applicationParameters = new HashMap<>();
        applicationParameters.put("class",TestSolver.class);
        HashMap<String,Object> problemControl = new HashMap<>();
        problemControl.put("size",5);
        problemControl.put("class",SolverControlTest.class.getClasses()[0]);
        problemControl.put("predicates",10);
        SolverController sctr = new SolverController(null,null);
        sctr.kvAnalyser = anal;
        sctr.solveProblem(problemControl);
        System.out.println("ST " +st.toString());


    }

    @Test
    public void solveProblems() throws Exception {

    }

    @Test
    public void distributeProblems() throws Exception {
        System.out.println("distributeProblems");
        HashMap<String,Object> applicationParameters = new HashMap<>();
        applicationParameters.put("class",TestSolver.class);
        HashMap<String,Object> problemControl = new HashMap<>();
        problemControl.put("number",5);
        SolverController sctr = new SolverController(null,null);
        sctr.distributeProblems(1,applicationParameters,problemControl,null);
        assertEquals(5,(int)(Integer)problemControl.get("SOLVED"));
    }

    @Test
    public void solve1() throws Exception {

    }
*/
}