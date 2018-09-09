package Generators;

import org.junit.Test;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Random;

import static org.junit.Assert.*;

/**
 * Created by ohlbach on 06.09.2018.
 */
public class RandomClauseSetGeneratorTest {
    @Test
    public void parseProblemParameters1() throws Exception {
        System.out.println("parseProblemParameters 1");
        HashMap<String,String> parameters = new HashMap<>();
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
        parameters.put("predicates","10");
        parameters.put("clauses","30");
        parameters.put("length","3");
        ArrayList<HashMap<String,Object>> map = RandomClauseSetGenerator.parseProblemParameters(parameters,errors,warnings);
        //System.out.println("E\n"+errors.toString());
        //System.out.println("W\n"+warnings.toString());
        //System.out.println(map);
        assertEquals(1,map.size());
        assertEquals(10,map.get(0).get("predicates"));
        assertEquals(30,map.get(0).get("clauses"));
        assertEquals(3,map.get(0).get("length"));
        assertTrue((Boolean)map.get(0).get("precise"));
        assertEquals(0,map.get(0).get("seed"));
        assertNull(map.get(0).get("dBlocks"));
        assertNull(map.get(0).get("dLengths"));
    }

    @Test
    public void parseProblemParameters2() throws Exception {
        System.out.println("parseProblemParameters 2");
        HashMap<String,String> parameters = new HashMap<>();
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
        parameters.put("predicates","100");
        parameters.put("cpRatio","4.3");
        parameters.put("length","3");
        ArrayList<HashMap<String,Object>> map = RandomClauseSetGenerator.parseProblemParameters(parameters,errors,warnings);
        //System.out.println("E\n"+errors.toString());
        //System.out.println("W\n"+warnings.toString());
        //System.out.println(map);
        assertEquals(1,map.size());
        assertEquals(100,map.get(0).get("predicates"));
        assertEquals(430,map.get(0).get("clauses"));
        assertEquals(3,map.get(0).get("length"));
        assertTrue((Boolean)map.get(0).get("precise"));
        assertEquals(0,map.get(0).get("seed"));
        assertNull(map.get(0).get("dBlocks"));
        assertNull(map.get(0).get("dLengths"));
    }

    @Test
    public void parseProblemParameters3() throws Exception {
        System.out.println("parseProblemParameters 3");
        HashMap<String,String> parameters = new HashMap<>();
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
        parameters.put("predicates","100");
        parameters.put("clauses","400");
        parameters.put("length","3");
        parameters.put("precise","false");
        ArrayList<HashMap<String,Object>> map = RandomClauseSetGenerator.parseProblemParameters(parameters,errors,warnings);
        //System.out.println("E\n"+errors.toString());
        //System.out.println("W\n"+warnings.toString());
        //System.out.println(map);
        assertEquals(1,map.size());
        assertEquals(100,map.get(0).get("predicates"));
        assertEquals(400,map.get(0).get("clauses"));
        assertEquals(3,map.get(0).get("length"));
        assertFalse((Boolean)map.get(0).get("precise"));
        assertEquals(0,map.get(0).get("seed"));
        assertNull(map.get(0).get("dBlocks"));
        assertNull(map.get(0).get("dLengths"));
    }

    @Test
    public void parseProblemParameters4() throws Exception {
        System.out.println("parseProblemParameters 4");
        HashMap<String,String> parameters = new HashMap<>();
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
        parameters.put("predicates","100");
        parameters.put("clauses","400");
        parameters.put("length","3");
        parameters.put("precise","false");
        parameters.put("dLength","10");
        ArrayList<HashMap<String,Object>> map = RandomClauseSetGenerator.parseProblemParameters(parameters,errors,warnings);
        //System.out.println("E\n"+errors.toString());
        //System.out.println("W\n"+warnings.toString());
        //System.out.println(map);
        assertEquals(1,map.size());
        assertEquals(100,map.get(0).get("predicates"));
        assertEquals(400,map.get(0).get("clauses"));
        assertEquals(3,map.get(0).get("length"));
        assertFalse((Boolean)map.get(0).get("precise"));
        assertEquals(0,map.get(0).get("seed"));
        assertEquals(1,map.get(0).get("dBlocks"));
        assertEquals(10,map.get(0).get("dLengths"));
    }

    @Test
    public void parseProblemParameters5() throws Exception {
        System.out.println("parseProblemParameters 5");
        HashMap<String,String> parameters = new HashMap<>();
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
        parameters.put("predicates","100");
        parameters.put("clauses","400");
        parameters.put("length","3");
        parameters.put("precise","false");
        parameters.put("dBlocks","2");
        parameters.put("dLength","10");
        ArrayList<HashMap<String,Object>> map = RandomClauseSetGenerator.parseProblemParameters(parameters,errors,warnings);
        //System.out.println("E\n"+errors.toString());
        //System.out.println("W\n"+warnings.toString());
        //System.out.println(map);
        assertEquals(1,map.size());
        assertEquals(100,map.get(0).get("predicates"));
        assertEquals(400,map.get(0).get("clauses"));
        assertEquals(3,map.get(0).get("length"));
        assertFalse((Boolean)map.get(0).get("precise"));
        assertEquals(0,map.get(0).get("seed"));
        assertEquals(2,map.get(0).get("dBlocks"));
        assertEquals(10,map.get(0).get("dLengths"));
    }

    @Test
    public void parseProblemParameters6() throws Exception {
        System.out.println("parseProblemParameters 6");
        HashMap<String,String> parameters = new HashMap<>();
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
        parameters.put("predicates","10,20");
        parameters.put("clauses","30");
        parameters.put("length","3");
        ArrayList<HashMap<String,Object>> map = RandomClauseSetGenerator.parseProblemParameters(parameters,errors,warnings);
        //System.out.println("E\n"+errors.toString());
        //System.out.println("W\n"+warnings.toString());
        //System.out.println(map);
        assertEquals(2,map.size());
        assertEquals(10,map.get(0).get("predicates"));
        assertEquals(30,map.get(0).get("clauses"));
        assertEquals(3,map.get(0).get("length"));
        assertTrue((Boolean)map.get(0).get("precise"));
        assertEquals(0,map.get(0).get("seed"));
        assertNull(map.get(0).get("dBlocks"));
        assertNull(map.get(0).get("dLengths"));

        assertEquals(20,map.get(1).get("predicates"));
        assertEquals(30,map.get(1).get("clauses"));
        assertEquals(3,map.get(1).get("length"));
        assertTrue((Boolean)map.get(1).get("precise"));
        assertEquals(0,map.get(1).get("seed"));
        assertNull(map.get(1).get("dBlocks"));
        assertNull(map.get(1).get("dLengths"));
    }

    @Test
    public void parseProblemParameters7() throws Exception {
        System.out.println("parseProblemParameters 7");
        HashMap<String,String> parameters = new HashMap<>();
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
        parameters.put("predicates","10,20");
        parameters.put("clauses","30 to 40 step 5");
        parameters.put("length","3");
        ArrayList<HashMap<String,Object>> map = RandomClauseSetGenerator.parseProblemParameters(parameters,errors,warnings);
        //System.out.println("E\n"+errors.toString());
        //System.out.println("W\n"+warnings.toString());
        //System.out.println(map);
        assertEquals(6,map.size());
        assertEquals(10,map.get(0).get("predicates"));
        assertEquals(30,map.get(0).get("clauses"));
        assertEquals(3,map.get(0).get("length"));
        assertTrue((Boolean)map.get(0).get("precise"));
        assertEquals(0,map.get(0).get("seed"));
        assertNull(map.get(0).get("dBlocks"));
        assertNull(map.get(0).get("dLengths"));

        assertEquals(20,map.get(1).get("predicates"));
        assertEquals(30,map.get(1).get("clauses"));
        assertEquals(3,map.get(1).get("length"));
        assertTrue((Boolean)map.get(1).get("precise"));
        assertEquals(0,map.get(1).get("seed"));
        assertNull(map.get(1).get("dBlocks"));
        assertNull(map.get(1).get("dLengths"));

        assertEquals(20,map.get(5).get("predicates"));
        assertEquals(40,map.get(5).get("clauses"));
    }

    @Test
    public void parseProblemParameters8() throws Exception {
        System.out.println("parseProblemParameters 8");
        HashMap<String,String> parameters = new HashMap<>();
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
        parameters.put("predicates","10,20");
        parameters.put("cpRatio","4 to 4.2 step 0.1");
        parameters.put("length","3");
        ArrayList<HashMap<String,Object>> map = RandomClauseSetGenerator.parseProblemParameters(parameters,errors,warnings);
        //System.out.println("E\n"+errors.toString());
        //System.out.println("W\n"+warnings.toString());
        //System.out.println(map);
        assertEquals(6,map.size());
        assertEquals(10,map.get(0).get("predicates"));
        assertEquals(40,map.get(0).get("clauses"));
        assertEquals(3,map.get(0).get("length"));
        assertTrue((Boolean)map.get(0).get("precise"));
        assertEquals(0,map.get(0).get("seed"));
        assertNull(map.get(0).get("dBlocks"));
        assertNull(map.get(0).get("dLengths"));

        assertEquals(20,map.get(1).get("predicates"));
        assertEquals(80,map.get(1).get("clauses"));
        assertEquals(3,map.get(1).get("length"));
        assertTrue((Boolean)map.get(1).get("precise"));
        assertEquals(0,map.get(1).get("seed"));
        assertNull(map.get(1).get("dBlocks"));
        assertNull(map.get(1).get("dLengths"));

        assertEquals(10,map.get(2).get("predicates"));
        assertEquals(41,map.get(2).get("clauses"));

        assertEquals(20,map.get(5).get("predicates"));
        assertEquals(84,map.get(5).get("clauses"));
    }

    @Test
    public void parseProblemParameters9() throws Exception {
        System.out.println("parseProblemParameters 9");
        HashMap<String,String> parameters = new HashMap<>();
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
        parameters.put("predicates","10");
        parameters.put("clauses","30");
        parameters.put("length","3");
        parameters.put("precise", "false,true");
        ArrayList<HashMap<String,Object>> map = RandomClauseSetGenerator.parseProblemParameters(parameters,errors,warnings);
        //System.out.println("E\n"+errors.toString());
        //System.out.println("W\n"+warnings.toString());
        //System.out.println(map);
        assertEquals(2,map.size());
        assertEquals(10,map.get(0).get("predicates"));
        assertEquals(30,map.get(0).get("clauses"));
        assertEquals(3,map.get(0).get("length"));
        assertFalse((Boolean)map.get(0).get("precise"));
        assertEquals(0,map.get(0).get("seed"));
        assertNull(map.get(0).get("dBlocks"));
        assertNull(map.get(0).get("dLengths"));

        assertEquals(10,map.get(1).get("predicates"));
        assertEquals(30,map.get(1).get("clauses"));
        assertEquals(3,map.get(1).get("length"));
        assertTrue((Boolean)map.get(1).get("precise"));
        assertEquals(0,map.get(1).get("seed"));
        assertNull(map.get(1).get("dBlocks"));
        assertNull(map.get(1).get("dLengths"));
    }

    @Test
    public void generate1() throws Exception {
        System.out.println("generate 1");
        HashMap<String,String> parameters = new HashMap<>();
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
        parameters.put("predicates","10");
        parameters.put("clauses","30");
        parameters.put("length","3");
        parameters.put("precise", "true");
        HashMap<String,Object> map = RandomClauseSetGenerator.parseProblemParameters(parameters,errors,warnings).get(0);

        RandomClauseSetGenerator.generate(map,errors,warnings);
        System.out.println(map.get("clauses").toString());
    }

    @Test
    public void generate2() throws Exception {
        System.out.println("generate 2");
        HashMap<String,String> parameters = new HashMap<>();
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
        parameters.put("predicates","10");
        parameters.put("clauses","30");
        parameters.put("length","3");
        parameters.put("precise", "false");
        HashMap<String,Object> map = RandomClauseSetGenerator.parseProblemParameters(parameters,errors,warnings).get(0);

        RandomClauseSetGenerator.generate(map,errors,warnings);
        System.out.println(map.get("clauses").toString());
    }

    @Test
    public void generate3() throws Exception {
        System.out.println("generate 3");
        HashMap<String,String> parameters = new HashMap<>();
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
        parameters.put("predicates","10");
        parameters.put("clauses","30");
        parameters.put("length","3");
        parameters.put("precise", "true");
        parameters.put("dBlocks","2");
        parameters.put("dLength","5");
        ArrayList<HashMap<String,Object>> maps = RandomClauseSetGenerator.parseProblemParameters(parameters,errors,warnings);
        System.out.println(errors.toString());
        RandomClauseSetGenerator.generate(maps.get(0),errors,warnings);
        System.out.println(maps.get(0).get("clauses").toString());
    }

}