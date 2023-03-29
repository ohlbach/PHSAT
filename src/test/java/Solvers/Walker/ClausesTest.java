package Solvers.Walker;

import Datastructures.Clauses.Quantifier;
import junit.framework.TestCase;

public class ClausesTest extends TestCase {
    private static int cOr = Quantifier.OR.ordinal();
    public void testAddToFront() {
        System.out.println("addToFront");
        Clauses clauses = new Clauses();
        Clause clause = new Clause(new int[]{1,cOr,1,2,3});
        clauses.addToFront(clause);
        assertEquals("1: 1v2v3\n",clauses.toString());
        clause = new Clause(new int[]{2,cOr,-1,-2,-3});
        clauses.addToFront(clause);
        assertEquals("2: -1v-2v-3\n" +
                "1: 1v2v3\n",clauses.toString());
    }

    public void testAddToBack() {
        System.out.println("addToBack");
        Clauses clauses = new Clauses();
        Clause clause = new Clause(new int[]{1,cOr,1,2,3});
        clauses.addToBack(clause);
        assertEquals("1: 1v2v3\n",clauses.toString());
        clause = new Clause(new int[]{2,cOr,-1,-2,-3});
        clauses.addToBack(clause);
        assertEquals("1: 1v2v3\n" +
                "2: -1v-2v-3\n",clauses.toString());
    }

    public void testAddMixed() {
        System.out.println("addMixed");
        Clauses clauses = new Clauses();
        Clause clause = new Clause(new int[]{1,cOr,1,2,3});
        clauses.addToBack(clause);
        assertEquals("1: 1v2v3\n",clauses.toString());
        clause = new Clause(new int[]{2,cOr,-1,-2,-3});
        clauses.addToFront(clause);
        assertEquals("2: -1v-2v-3\n" +
                "1: 1v2v3\n",clauses.toString());
        clause = new Clause(new int[]{3,cOr,4,5});
        clauses.addToBack(clause);
        assertEquals("2: -1v-2v-3\n" +
                "1: 1v2v3\n" +
                "3: 4v5\n",clauses.toString());
        clause = new Clause(new int[]{4,cOr,-4,-5});
        clauses.addToFront(clause);
        assertEquals("4: -4v-5\n" +
                "2: -1v-2v-3\n" +
                "1: 1v2v3\n" +
                "3: 4v5\n",clauses.toString());
        assertEquals(4,clauses.size());
    }

    public void testRemove1() {
        System.out.println("remove1");
        Clauses clauses = new Clauses();
        Clause clause1 = new Clause(new int[]{1,cOr,1,2,3});
        clauses.addToFront(clause1);
        assertTrue(clause1.isInList);
        clauses.remove(clause1);
        assertEquals(0,clauses.size());
        assertFalse(clause1.isInList);

        clauses.addToBack(clause1);
        assertTrue(clause1.isInList);
        assertEquals(1,clauses.size());

        Clause clause2 = new Clause(new int[]{2,cOr,-1,-2,-3});
        clauses.addToBack(clause2);
        assertEquals("1: 1v2v3\n" +
                "2: -1v-2v-3\n",clauses.toString());
        clauses.remove(clause2);
        assertEquals(1,clauses.size());
        assertEquals("1: 1v2v3\n",clauses.toString());

        clauses.addToBack(clause2);
        Clause clause3 = new Clause(new int[]{3,cOr,4,5});
        clauses.addToBack(clause3);
        assertEquals(3,clauses.size());
        clauses.remove(clause2);
        assertEquals(2,clauses.size());
        assertEquals("1: 1v2v3\n" +
                "3: 4v5\n",clauses.toString());

        clauses.remove(clause1);
        assertEquals(1,clauses.size());
        assertEquals("3: 4v5\n",clauses.toString());
    }

    public void testRemove2() {
        System.out.println("remove 2");
        Clauses clauses = new Clauses();
        Clause clause1 = new Clause(new int[]{1, cOr, 1, 2});
        clauses.addToBack(clause1);
        Clause clause2 = new Clause(new int[]{2, cOr, 3,4});
        clauses.addToBack(clause2);
        Clause clause3 = new Clause(new int[]{3, cOr, 5,6});
        clauses.addToBack(clause3);
        Clause clause4 = new Clause(new int[]{4, cOr, 7,8});
        clauses.addToBack(clause4);
        Clause clause5 = new Clause(new int[]{5, cOr, 9,10});
        clauses.addToBack(clause5);
        assertEquals(5,clauses.size());
        clauses.remove(clause3);
        clauses.remove(clause2);
        clauses.remove(clause4);
        assertEquals(2,clauses.size());
        assertEquals("1: 1v2\n" +
                "5: 9v10\n",clauses.toString());

    }
}