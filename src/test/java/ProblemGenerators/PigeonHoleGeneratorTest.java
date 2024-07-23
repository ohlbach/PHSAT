package ProblemGenerators;

import Datastructures.Clauses.Quantifier;
import Utilities.Interval;
import junit.framework.TestCase;

import java.util.ArrayList;

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
/*
        results = PigeonHoleGenerator.parseCapacity("2",errors);
        assertTrue(errors.isEmpty());
        assertEquals("= 2,", mkString(results));

        results = PigeonHoleGenerator.parseCapacity("<2",errors);
        assertTrue(errors.isEmpty());
        assertEquals("<= 1,", mkString(results));

        results = PigeonHoleGenerator.parseCapacity(">2",errors);
        assertTrue(errors.isEmpty());
        assertEquals(">= 3,", mkString(results));

        results = PigeonHoleGenerator.parseCapacity("2,3",errors);
        assertTrue(errors.isEmpty());
        assertEquals("= 2,= 3,", mkString(results));

        results = PigeonHoleGenerator.parseCapacity("[2,3]",errors);
        System.out.println(errors.toString());
        assertTrue(errors.isEmpty());
        assertEquals("i [2,3],", mkString(results));


        results = PigeonHoleGenerator.parseCapacity("< 2, <= 2, > 2, >= 2, 2, [2,3]",errors);
        System.out.println(errors.toString());
        assertTrue(errors.isEmpty());
        assertEquals("<= 1,<= 2,>= 3,>= 2,= 2,i [2,3],", mkString(results));
        */

    }


}