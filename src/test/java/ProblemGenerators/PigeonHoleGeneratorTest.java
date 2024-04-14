package ProblemGenerators;

import Datastructures.Clauses.InputClauses;
import Datastructures.Clauses.Quantifier;
import Datastructures.Symboltable;
import Utilities.Interval;
import junit.framework.TestCase;
import org.junit.Test;

import java.util.ArrayList;
import java.util.HashMap;

/**
 * Created by ohlbach on 09.09.2018.
 */
public class PigeonHoleGeneratorTest extends TestCase  {

    private String mkString(ArrayList<Object[]>list) {
        StringBuilder str = new StringBuilder();
        for(Object[] o : list) {
            Quantifier q = (Quantifier)o[0];
            str.append(q.toString()).append(" ");
            Object limit = o[1];
            if(limit instanceof Integer) {str.append(limit).append(",");}
            else {str.append(((Interval)limit).toString()).append(",");}}
        return str.toString();}

    public void testParseCapacity() {
        System.out.println("parseCapacity");
        StringBuilder errors = new StringBuilder();
        ArrayList<Object[]> results;

        results = PigeonHoleGenerator.parseCapacity("2",errors);
        assertTrue(errors.isEmpty());
        assertEquals("= 2,", mkString(results));

        results = PigeonHoleGenerator.parseCapacity("<2",errors);
        assertTrue(errors.isEmpty());
        assertEquals("<= 1,", mkString(results));

        results = PigeonHoleGenerator.parseCapacity(">2",errors);
        assertTrue(errors.isEmpty());
        assertEquals(">= 3,", mkString(results));

        results = PigeonHoleGenerator.parseCapacity("= 2",errors);
        assertTrue(errors.isEmpty());
        assertEquals("= 2,", mkString(results));

        results = PigeonHoleGenerator.parseCapacity("[2,3]",errors);
        System.out.println(errors.toString());
        assertTrue(errors.isEmpty());
        assertEquals("i [2,3],", mkString(results));


        results = PigeonHoleGenerator.parseCapacity("< 2, <= 2, > 2, >= 2, =2, [2,3]",errors);
        System.out.println(errors.toString());
        assertTrue(errors.isEmpty());
        assertEquals("<= 1,<= 2,>= 3,>= 2,= 2,i [2,3],", mkString(results));
    }

    @Test
    public void makeProblemGenerator()  {
        System.out.println("makeProblemGenerator with errors");
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        HashMap<String,String> params = new HashMap<>();
        ArrayList<ProblemGenerator> generators = new ArrayList<>();
        params.put("holes","4");
        params.put("pigeons", "5");
        params.put("capacities", "<= 2");
        PigeonHoleGenerator.makeProblemGenerator(params,generators, errors,warnings);
        System.out.println("Errors:\n" + errors);
        System.out.println("Warnings:\n" + warnings);
        //assertEquals(2, generators.size());
        for(ProblemGenerator generator: generators) {
            System.out.println(generator.toString());
            System.out.println("\n\n");
            InputClauses inputClauses = generator.generateProblem(null);
            Symboltable symboltable = null;
            System.out.println(inputClauses.toString());
            System.out.println("\n\n");}
        }

}