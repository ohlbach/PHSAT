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
     public void help() {
         System.out.println(StringClauseSetGenerator.help());
     }

    @Test
    public void generate1() throws Exception {
        System.out.println("generate 1");
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        String clauses =
                "p,q\n" +
                "-p,q";
        HashMap<String,String> map = new HashMap<>();
        map.put("disjunctions",clauses);
        ArrayList<HashMap<String,Object>> params = StringClauseSetGenerator.parseParameters(map,errors,warnings);
        BasicClauseList bcl = StringClauseSetGenerator.generate(params.get(0),errors,warnings);
        //System.out.println(((BasicClauseList)result.get("disjunctions")).toString(true));
        assertEquals("String-generated clauses:\n" +
                "p,q\n" +
                "-p,q\n" +
                "Disjunctions:\n" +
                "1 : p | q\n" +
                "2 : -p | q\n", bcl.toString());
    }

    @Test
    public void generate2() throws Exception {
        System.out.println("generate complex");
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        String clauses =
                        "p q\n" +
                        "-p q\n" +
                        "$a p q\n" +
                                "$x r s\n" + "$e p s\n" + "$d q a";
        HashMap<String,String> map = new HashMap<>();
        map.put("disjunctions",clauses);
        ArrayList<HashMap<String,Object>> params = StringClauseSetGenerator.parseParameters(map,errors,warnings);
        BasicClauseList result = StringClauseSetGenerator.generate(params.get(0),errors,warnings);
        assertEquals("String-generated clauses:\n" +
                "p q\n" +
                "-p q\n" +
                "$a p q\n" +
                "$x r s\n" +
                "$e p s\n" +
                "$d q a\n" +
                "Disjunctions:\n" +
                "1 : p | q\n" +
                "2 : -p | q\n" +
                "Conjunctions:\n" +
                "3 : p & q\n" +
                "Xor:\n" +
                "4 : r x s\n" +
                "Disjoints:\n" +
                "6 : q /= a\n" +
                "Equivalences:\n" +
                "5 : p = s\n", result.toString());

    }

}