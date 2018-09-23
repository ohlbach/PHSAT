package Generators;
import Datastructures.Clauses.BasicClauseList;
import org.junit.Test;

import java.util.ArrayList;
import java.util.HashMap;

import static org.junit.Assert.assertEquals;

/**
 * Created by ohlbach on 27.08.2018.
 */
public class StringClauseSetGeneratorTest {
    @Test
    public void generate1() throws Exception {
        System.out.println("generate 1");
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
        String clauses =
                "p,q\n" +
                "-p,q";
        HashMap<String,String> map = new HashMap<>();
        map.put("disjunctions",clauses);
        ArrayList<HashMap<String,Object>> params = StringClauseSetGenerator.parseParameters(map,errors,warnings);
        HashMap<String,Object> result = StringClauseSetGenerator.generate(params.get(0),errors,warnings);
        //System.out.println(((BasicClauseList)result.get("disjunctions")).toString(true));
        assertEquals("1: p,q\n" +
                "2: -p,q\n", ((BasicClauseList)result.get("disjunctions")).toString(true));
    }

    @Test
    public void generate2() throws Exception {
        System.out.println("generate with disjointness");
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
        String clauses =
                        "p q\n" +
                        "-p q\n" +
                        "disjoint p q";
        HashMap<String,String> map = new HashMap<>();
        map.put("disjunctions",clauses);
        ArrayList<HashMap<String,Object>> params = StringClauseSetGenerator.parseParameters(map,errors,warnings);
        HashMap<String,Object> result = StringClauseSetGenerator.generate(params.get(0),errors,warnings);
        //System.out.println(((BasicClauseList)result.get("disjunctions")).toString(true));
        assertEquals("1: p,q\n" +
                "2: -p q\n" +
                "3: disjoint p q\n", ((BasicClauseList)result.get("disjunctions")).toString(true));

       // System.out.println(StringClauseSetGenerator.help());
    }

}