package Solvers.Resolution;

import Datastructures.Clauses.Quantifier;
import Datastructures.Symboltable;
import junit.framework.TestCase;

public class ClausesTest extends TestCase {

    static int cOr = Quantifier.OR.ordinal();
    static int cAtleast = Quantifier.ATLEAST.ordinal();

    static Symboltable symboltable = new Symboltable(10);

    static {
        symboltable.setName(1, "p");
        symboltable.setName(2, "q");
        symboltable.setName(3, "r");
    }

    public void testAddClause() {
        System.out.println("addClause");
        Clauses clauses = new Clauses();
        assertEquals("", clauses.toString());
        assertTrue(clauses.isEmpty());
        assertEquals(0, clauses.size());
        Clause clause1 = new Clause(new int[]{10, cOr, 1, 2, 3});
        clauses.addClause(clause1);
        assertEquals("10: 1v2v3\n", clauses.toString());
        assertEquals("10: pvqvr\n", clauses.toString(symboltable));
        assertFalse(clauses.isEmpty());
        assertEquals(1, clauses.size());
        assertEquals(clause1, clauses.firstClause);
        assertNull(clause1.previousClause);
        assertNull(clause1.nextClause);

        Clause clause2 = new Clause(new int[]{100, cAtleast, 2, 1, 1, 2, 2, 3});
        clauses.addClause(clause2);
        assertEquals(" 10: pvqvr\n" +
                "100: >= 2 p^2,q^2,r\n", clauses.toString(symboltable));
        assertEquals(2, clauses.size());

        Clause clause3 = new Clause(new int[]{1000, cOr, 4, 5});
        clauses.addClause(clause3);
        assertEquals("  10: pvqvr\n" +
                " 100: >= 2 p^2,q^2,r\n" +
                "1000: 4v5\n", clauses.toString(symboltable));
        assertEquals(3, clauses.size());
        assertEquals(1,clauses.status);
        //System.out.println(clauses.numbers());
    }

    public void testRemoveClause() {
        System.out.println("removeClause");
        Clauses clauses = new Clauses();
        Clause clause1 = new Clause(new int[]{10, cOr, 1, 2, 3});
        assertEquals(0, clauses.removeClause(clause1));
        clauses.addClause(clause1);
        assertEquals(0, clauses.removeClause(clause1));
        assertEquals("", clauses.toString(symboltable));
        assertEquals(0, clauses.size());

        clauses.addClause(clause1);
        Clause clause2 = new Clause(new int[]{100, cOr, -1, -2, -3});
        clauses.addClause(clause2);
        assertEquals(" 10: pvqvr\n" +
                "100: -pv-qv-r\n", clauses.toString(symboltable));
        assertEquals(1, clauses.removeClause(clause2));
        assertEquals("10: pvqvr\n", clauses.toString(symboltable));
        clauses.addClause(clause2);
        assertEquals(1, clauses.removeClause(clause1));
        assertEquals("100: -pv-qv-r\n", clauses.toString(symboltable));
        clauses.addClause(clause1);
        Clause clause3 = new Clause(new int[]{1000, cOr, 4, 5});
        clauses.addClause(clause3);
        assertEquals(" 100: -pv-qv-r\n" +
                "  10: pvqvr\n" +
                "1000: 4v5\n", clauses.toString(symboltable));
        assertEquals(2, clauses.removeClause(clause1));
        assertEquals(" 100: -pv-qv-r\n" +
                "1000: 4v5\n", clauses.toString(symboltable));

        assertEquals(0,clauses.status);
        //System.out.println(clauses.numbers());

    }

    public void testIterating() {
        System.out.println("iterating");
        Clauses clauses = new Clauses();
        Clause clause1 = new Clause(new int[]{1, cOr, 1, 2, 3});
        clauses.addClause(clause1);
        Clause clause2 = new Clause(new int[]{2, cOr, 1, 2, 3});
        clauses.addClause(clause2);
        Clause clause3 = new Clause(new int[]{3, cOr, 1, 2, 3});
        clauses.addClause(clause3);
        Clause clause4 = new Clause(new int[]{4, cOr, 1, 2, 3});
        clauses.addClause(clause4);
        String s = "";
        Clause c = clauses.firstClause;
        while (c != null) {
            s += c.id;
            c = c.nextClause;}
        assertEquals("1234", s);

        s = "";
        c = clauses.firstClause.nextClause;
        clauses.removeClause(clause2);
        while (c != null) {
            if(!c.exists) {c = c.nextClause; continue;}
            s += c.id;
            c = c.nextClause;}
        assertEquals("34", s);

        s = "";
        clauses.removeClause(clause3);
        c = clauses.firstClause.nextClause;
        while (c != null) {
            if(!c.exists) {c = c.nextClause; continue;}
            s += c.id;
            c = c.nextClause;}
        assertEquals("4", s);

        s = "";
        c = clauses.firstClause;
        while (c != null) {
            if(!c.exists) {c = c.nextClause; continue;}
            s += c.id;
            c = c.nextClause;}
        assertEquals("14", s);

        assertEquals("1: 1v2v3\n" +
                "4: 1v2v3\n",clauses.toString());
    }

    }