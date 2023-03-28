package Solvers.Walker;

import junit.framework.TestCase;

public class PredicateTest extends TestCase {

    public void testTestToString() {
        System.out.println("constructor");
        Predicate predicate5 = new Predicate(5);
        assertEquals("5",predicate5.toString());

        Predicate predicate4 = new Predicate(4);
        Predicate predicate6 = new Predicate(6);
        predicate5.previousPredicate = predicate4;
        predicate5.nextPredicate = predicate6;

        assertEquals("5:-4+6",predicate5.toString());

        predicate5.predicate = 0;
        assertEquals("0:-4+6",predicate5.toString());
        assertEquals("4",predicate4.toString());


    }
}