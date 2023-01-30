package ProblemGenerators;

import Datastructures.Clauses.Connective;
import Datastructures.Symboltable;
import org.junit.Test;

import java.util.Arrays;

import static org.junit.Assert.assertEquals;

/**
 * Created by ohlbach on 27.08.2018.
 */
public class StringClauseSetGeneratorTest {
    String errorPrefix = "Error: ";

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
        clause = StringClauseSetGenerator.parseLine("ep,q, -r,-p",12,symboltable,errors);
        assertEquals("[12, 2, 1, 2, -3, -1]", Arrays.toString(clause));
        symboltable = new Symboltable(10);
        clause = StringClauseSetGenerator.parseLine("& 3,5,-1, -2",13,symboltable,errors);
        assertEquals("[13, 1, 3, 5, -1, -2]", Arrays.toString(clause));

        clause = StringClauseSetGenerator.parseLine("e 3,5,-1, -2",13,symboltable,errors);
        assertEquals("[13, 2, 3, 5, -1, -2]", Arrays.toString(clause));

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

        clause = StringClauseSetGenerator.parseLine("= 3 4,5,-6", 11, symboltable, errors);
        assertEquals("[11, 6, 3, 4, 5, -6]", Arrays.toString(clause));


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
        clause = StringClauseSetGenerator.parseLine("[2a,3a] p,q, -r,-p", 10, symboltable,errors);
        System.out.println(errors);
        errors = new StringBuilder();
        clause = StringClauseSetGenerator.parseLine("[3,2] p,q, -r; -p", 10, symboltable, errors);
        System.out.println(errors);
    }
    @Test
    public void parseOr() {
        System.out.println("parseOr");
        StringBuilder errors = new StringBuilder();
        Symboltable symboltable = new Symboltable(5);
        int[] clause = StringClauseSetGenerator.parseLine("p,q, -r,-p", 10, symboltable, errors);
        System.out.println(errors);
        assertEquals("[10, 0, 1, 2, -3, -1]", Arrays.toString(clause));
    }
/*
    @Test
    public void generate() throws Exception {
        System.out.println("generate");
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        String clauses = "p 5\n" +
                        "p,q\n" +
                "<= 2 -p,q,r\n" +
                ">= 3 q,-r,s";
        HashMap<String,String> map = new HashMap<>();
        map.put("clauses",clauses);
        ArrayList<HashMap<String,Object>> params = StringClauseSetGenerator.parseParameters(map,errors,warnings);
        BasicClauseList bcl = StringClauseSetGenerator.generate(params.get(0), problemSupervisor, errors,warnings);
        assertEquals("String Generator\n" +
                "Disjunctions:\n" +
                "  1: p,q\n" +
                "Quantifieds:\n" +
                "M-2: 2 -p,q,r\n" +
                "L-3: 3 q,-r,s\n", bcl.toString());
    }
*/
}