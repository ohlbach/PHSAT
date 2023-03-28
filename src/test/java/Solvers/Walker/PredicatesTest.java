package Solvers.Walker;

import junit.framework.TestCase;

public class PredicatesTest extends TestCase {

    public void testAddToFront() {
        System.out.println("addToFront");
        Predicates predicates = new Predicates(5);
        assertEquals("",predicates.toString());
        predicates.addToFront(2);
        assertEquals("2,",predicates.toString());
        predicates.addToFront(3);
        assertEquals("3,2,",predicates.toString());
        predicates.addToFront(1);
        assertEquals("1,3,2,",predicates.toString());
    }

    public void testAddToBack() {
        System.out.println("addToBack");
        Predicates predicates = new Predicates(5);
        assertEquals("",predicates.toString());
        predicates.addToBack(2);
        assertEquals("2,",predicates.toString());
        predicates.addToBack(3);
        assertEquals("2,3,",predicates.toString());
        predicates.addToBack(1);
        assertEquals("2,3,1,",predicates.toString());
    }

    public void testAddMixed() {
        System.out.println("addMixed");
        Predicates predicates = new Predicates(5);
        assertEquals("",predicates.toString());
        predicates.addToFront(2);
        assertEquals("2,",predicates.toString());
        predicates.addToBack(3);
        assertEquals("2,3,",predicates.toString());
        predicates.addToFront(1);
        assertEquals("1,2,3,",predicates.toString());
    }


    public void testRemove() {
        System.out.println("remove");
        Predicates predicates = new Predicates(5);
        assertEquals("",predicates.toString());
        predicates.addToFront(2);
        predicates.remove(2);
        assertEquals("",predicates.toString());

        predicates.addToFront(2);
        predicates.addToBack(3);
        predicates.remove(2);
        assertEquals("3,",predicates.toString());
        predicates.remove(3);
        assertEquals("",predicates.toString());

        predicates.addToBack(1);
        predicates.addToBack(2);
        predicates.addToBack(3);
        predicates.remove(2);
        assertEquals("1,3,",predicates.toString());
        predicates.remove(3);
        assertEquals("1,",predicates.toString());
        predicates.remove(1);
        assertEquals("",predicates.toString());

        predicates.addToFront(1);
        predicates.addToFront(2);
        predicates.addToFront(3);
        assertEquals("3,2,1,",predicates.toString());
        predicates.remove(3);
        assertEquals("2,1,",predicates.toString());
        predicates.remove(1);
        assertEquals("2,",predicates.toString());
        predicates.remove(2);
        assertEquals("",predicates.toString());

        predicates.addToBack(1);
        predicates.addToBack(2);
        predicates.addToBack(3);
        predicates.addToBack(4);
        predicates.remove(2);
        assertEquals("1,3,4,",predicates.toString());
        predicates.remove(3);
        assertEquals("1,4,",predicates.toString());

    }
}