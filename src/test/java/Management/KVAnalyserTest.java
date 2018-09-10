package Management;

import Datastructures.Clauses.Clause;
import Utilities.Utilities;
import org.junit.Test;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Objects;

import static org.junit.Assert.*;

/**
 * Created by Ohlbach on 03.09.2018.
 */
public class KVAnalyserTest {
    static public class Generator{
        public static  ArrayList<HashMap<String,Object>> parseParameters(HashMap<String,String> parameters, StringBuffer errors, StringBuffer warnings){
             HashMap<String,Object> control = new HashMap<>();
            String number = parameters.get("number");
            Integer n = Utilities.parseInteger("place",number,errors);
            control.put("number",n);
            ArrayList<HashMap<String,Object>> list = new ArrayList<>();
            list.add(control);
            return list;
        }}


    static public class Solver{
        public static  ArrayList<HashMap<String,Object>> parseParameters(HashMap<String,String> parameters, StringBuffer errors, StringBuffer warnings){
             HashMap<String,Object> control = new HashMap<>();
            String number = parameters.get("solvers");
            Integer n = Utilities.parseInteger("solvers",number,errors);
            control.put("solvers",n);
            ArrayList<HashMap<String,Object>> list = new ArrayList<>();
            list.add(control);
            return list;
        }
    }

    static HashMap<String,Class> classMap = new HashMap<>();
    static {classMap.put("generator",KVAnalyserTest.class.getClasses()[1]);
        classMap.put("solver",KVAnalyserTest.class.getClasses()[0]);
    }



    @Test
    public void analyse() throws Exception {
        KVParser kvParser = new KVParser("global","problem","solver" );
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
        KVAnalyser kvAnalyser = new KVAnalyser(errors,warnings);
        String test =
                "Header 1\n" +
                        "Header 2\n" +
                        "problem generator\n" +
                        "number = 33\n" +
                        "\n" +
                        "global\n" +
                        "parallel\n" +
                        "problem generator\n" +
                        "many : 4\n" +
                        "solver solver\n" +
                        "solvers : 6";
        kvParser.parseString(test);
        //System.out.println(kvParser);
        kvAnalyser.analyse(kvParser,classMap);
        //System.out.println("ERR " + kvAnalyser.errors);
        //System.out.println("AN " + kvAnalyser.toString());
        assertEquals(33, kvAnalyser.problemParameters.get(0).get("number"));
        assertEquals(Runtime.getRuntime().availableProcessors(), kvAnalyser.globalParameters.get("parallel"));
        assertEquals(6, kvAnalyser.solverParameters.get(0).get("solvers"));
        //System.out.println(kvAnalyser.solverParameters);
        //System.out.println("E " + errors.toString());
        //System.out.println("W "+warnings.toString());


    }

}