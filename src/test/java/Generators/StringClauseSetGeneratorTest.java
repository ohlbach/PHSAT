package Generators;
import Datastructures.Clauses.BasicClauseList;
import Datastructures.Clauses.Connective;
import Datastructures.Symboltable;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
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
    public void parseLineSingle()  {
        System.out.println("parseLine single");
        StringBuilder errors = new StringBuilder();
        Symboltable symboltable = new Symboltable(5);
        int[] clause = StringClauseSetGenerator.parseLine("& p,q, -r,-p",10,symboltable,errors);
        assertEquals("[10, 1, 1, 2, -3, -1]", Arrays.toString(clause));
        clause = StringClauseSetGenerator.parseLine("&p,q, -r,-p",11,symboltable,errors);
        assertEquals("[11, 1, 1, 2, -3, -1]", Arrays.toString(clause));
        clause = StringClauseSetGenerator.parseLine("=p,q, -r,-p",12,symboltable,errors);
        assertEquals("[12, 2, 1, 2, -3, -1]", Arrays.toString(clause));
        symboltable = new Symboltable(10);
        clause = StringClauseSetGenerator.parseLine("& 3,5,-1, -2",13,symboltable,errors);
        assertEquals("[13, 1, 3, 5, -1, -2]", Arrays.toString(clause));

        System.out.println("Errors");
        clause = StringClauseSetGenerator.parseLine("& p?,q;-r,-p",14,symboltable,errors);
        System.out.println(errors);}

    @Test
    public void parseWithQuantification()  {
        System.out.println("parseWithQuantification");
        StringBuilder errors = new StringBuilder();
        Symboltable symboltable = new Symboltable(5);
        int[] clause = StringClauseSetGenerator.parseLine("<= 3 p,q, -r,-p", 10, symboltable, errors);
        assertEquals(clause[1], Connective.ATMOST.ordinal());
        assertEquals("[10, 5, 3, 1, 2, -3, -1]", Arrays.toString(clause));
        symboltable = new Symboltable(5);
        clause = StringClauseSetGenerator.parseLine(">= 3 4,5,-6", 11, symboltable, errors);
        assertEquals("[11, 4, 3, 4, 5, -6]", Arrays.toString(clause));
        System.out.println("Errors");
        symboltable = new Symboltable(5);
        clause = StringClauseSetGenerator.parseLine("<= 3 p,q, -r;-p", 12, symboltable, errors);
        System.out.println(errors.toString());
        errors = new StringBuilder();
        clause = StringClauseSetGenerator.parseLine("<= 3a p,q, -r;-p", 13, symboltable, errors);
        System.out.println(errors.toString());

    }
    @Test
    public void parseInterval() {
        System.out.println("parseInterval");
        StringBuilder errors = new StringBuilder();
        Symboltable symboltable = new Symboltable(5);
        int[] clause = StringClauseSetGenerator.parseLine("[2,3] p,q, -r,-p", 10, symboltable, errors);
        assertEquals(clause[1], Connective.INTERVAL.ordinal());
        assertEquals("[10, 3, 2, 3, 1, 2, -3, -1]",Arrays.toString(clause));
        symboltable = new Symboltable(5);
        System.out.println("Errors");
        clause = StringClauseSetGenerator.parseLine("[2a,3a] p,q, -r,-p", 10, symboltable, errors);
        System.out.println(errors);
        errors = new StringBuilder();
        clause = StringClauseSetGenerator.parseLine("[3,2] p,q, -r; -p", 10, symboltable, errors);
        System.out.println(errors);
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