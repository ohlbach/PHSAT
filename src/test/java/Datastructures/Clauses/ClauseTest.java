package Datastructures.Clauses;

import Datastructures.Literals.CLiteral;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;

import static org.junit.Assert.*;

/**
 * Created by ohlbach on 26.08.2018.
 */
public class ClauseTest {


    private static Symboltable makeSymboltable() {
        Symboltable symboltable = new Symboltable(10);
        symboltable.setName(1, "a");
        symboltable.setName(2, "b");
        symboltable.setName(3, "c");
        symboltable.setName(4, "d");
        symboltable.setName(5, "e");
        symboltable.setName(6, "f");
        symboltable.setName(7, "g");
        symboltable.setName(8, "p");
        symboltable.setName(9, "q");
        symboltable.setName(10, "r");
        return symboltable;
    }

    @Test
    public void inference() {
        System.out.println("inference step");
        Clause clause = new Clause(100, Connective.OR);
        assertEquals(100, clause.id);
        assertEquals("Input 100", clause.inferenceStep.toString());
    }

    @Test
    public void basicClauseOR() {
        System.out.println("basicClause primitve features");
        Symboltable st = makeSymboltable();
        int[] basicClause = {10, 0, 1, 2, 3}; // OR
        Clause clause = new Clause(basicClause);
        assertEquals("10: 1,2,3", clause.toString());
        assertEquals("10:  a,b,c", clause.toString(5, st));
        assertSame(clause.structure, ClauseStructure.POSITIVE);
        assertEquals(10, clause.id);
        assertSame(clause.connective, Connective.OR);
        assertEquals(0, clause.quAmount);
        assertEquals(1, clause.getLiteral(0));
        assertEquals(2, clause.getLiteral(1));
        assertEquals(3, clause.getLiteral(2));

        assertEquals(1, clause.getCLiteral(0).literal);
        assertEquals(2, clause.getCLiteral(1).literal);
        assertEquals(3, clause.getCLiteral(2).literal);

        IntArrayList literals = new IntArrayList();
        for (CLiteral cLiteral : clause) {
            literals.add(cLiteral.literal);
        }
        assertEquals("[1, 2, 3]", literals.toString());
        assertEquals(3, clause.size());
        assertFalse(clause.isEmpty());
        assertTrue(clause.isPositive());
        assertFalse(clause.isNegative());
        assertEquals("[1, 2, 3]", Arrays.toString(clause.getLiterals()));

        basicClause = new int[]{10, 0, -1, -2, -3}; // OR

        clause = new Clause(basicClause);
        assertSame(ClauseStructure.NEGATIVE, clause.structure);

        basicClause = new int[]{11, 0, -1, 2, -3}; // OR
        clause = new Clause(basicClause);
        assertEquals("11:  -a,b,-c", clause.toString(5, st));
        assertSame(ClauseStructure.MIXED, clause.structure);
    }

    @Test
    public void basicClauseTypes() {
        System.out.println("basicClause differnt types");
        Symboltable st = makeSymboltable();
        int[] basicClause = {10, 1, 1, 2, 3}; // AND
        Clause clause = new Clause(basicClause);
        assertSame(Connective.AND, clause.connective);
        assertEquals("A-10: 1&2&3", clause.toString());
        assertEquals("A-10:  a&b&c", clause.toString(5, st));

        basicClause = new int[]{11, 2, 1, 2, 3}; // EQUIV
        clause = new Clause(basicClause);
        assertSame(Connective.EQUIV, clause.connective);
        assertEquals("E-11: 1=2=3", clause.toString());
        assertEquals("E-11:  a=b=c", clause.toString(5, st));

        basicClause = new int[]{12, 3, 2, 1, -2, 3}; // ATLEAST
        clause = new Clause(basicClause);
        assertEquals(2, clause.quAmount);
        assertSame(Connective.ATLEAST, clause.connective);
        assertEquals("L-12: ATLEAST 2: 1,-2,3", clause.toString());
        assertEquals("L-12:  ATLEAST 2: a,-b,c", clause.toString(5, st));


        basicClause = new int[]{13, 4, 2, 1, -2, 3}; // ATMOST
        clause = new Clause(basicClause);
        assertEquals(2, clause.quAmount);
        assertSame(Connective.ATMOST, clause.connective);
        assertEquals("M-13: ATMOST 2: 1,-2,3", clause.toString());
        assertEquals("M-13:  ATMOST 2: a,-b,c", clause.toString(5, st));

        basicClause = new int[]{14, 5, 2, 1, -2, 3}; // EXACTLY
        clause = new Clause(basicClause);
        assertEquals(2, clause.quAmount);
        assertSame(Connective.EXACTLY, clause.connective);
        assertEquals("X-14: EXACTLY 2: 1,-2,3", clause.toString());
        assertEquals("X-14: EXACTLY 2: 1,-2,3", clause.toNumbers());
        assertEquals("X-14:  EXACTLY 2: a,-b,c", clause.toString(5, st));
    }


    @Test
    public void addCLiteral() {
        System.out.println("add");
        Clause cl = new Clause(1, Connective.OR);
        assertEquals(0, cl.size());
        CLiteral lit = new CLiteral(5);
        cl.add(lit);
        assertEquals(1, cl.size());
        CLiteral lit1 = new CLiteral(5);
        cl.add(lit1);
        assertEquals(2, cl.size());
        CLiteral lit2 = new CLiteral(-5);
        cl.add(lit2);
        cl.setStructure();
        assertEquals(3, cl.size());
        assertEquals("1:   5,5,-5", cl.toString(5, null));
        assertEquals(ClauseStructure.MIXED, cl.structure);
        assertTrue(cl.hasDoubles());
        assertTrue(cl.hasComplementaries());
        assertEquals(3, cl.size());
    }

    @Test
    public void addLiterals() {
        System.out.println("add Literals");
        IntArrayList lits = new IntArrayList();
        lits.add(1);
        lits.add(2);
        lits.add(3);
        Clause cl = new Clause(1, Connective.ATLEAST, 2, lits);
        assertEquals("L-1: ATLEAST 2: 1,2,3", cl.toString());
    }

    @Test
    public void addLiteralsAND() {
        System.out.println("add Literals AND");
        IntArrayList lits = new IntArrayList();
        lits.add(1);
        lits.add(2);
        lits.add(3);
        Clause cl = new Clause(1, Connective.AND, lits);
        assertEquals("A-1: 1&2&3", cl.toString());
    }

    @Test
    public void addCLiterals() {
        System.out.println("add CLiterals");
        ArrayList<CLiteral> lits = new ArrayList<>();
        lits.add(new CLiteral(1));
        lits.add(new CLiteral(-2));
        lits.add(new CLiteral(3));
        Clause cl = new Clause(1, Connective.ATMOST, 2, lits);
        assertEquals("M-1: ATMOST 2: 1,-2,3", cl.toString());
        assertSame(ClauseStructure.MIXED, cl.structure);
    }

    @Test
    public void addCLiteralsOR() {
        System.out.println("add CLiterals OR");
        ArrayList<CLiteral> lits = new ArrayList<>();
        lits.add(new CLiteral(1));
        lits.add(new CLiteral(-2));
        lits.add(new CLiteral(3));
        Clause cl = new Clause(1, Connective.OR, lits);
        assertEquals("1: 1,-2,3", cl.toString());
        assertSame(ClauseStructure.MIXED, cl.structure);
    }

    @Test
    public void addCLiteralsList() {
        System.out.println("add CLiterals ...");
        Clause cl = new Clause(1, Connective.OR, -1, 2, -3);
        assertEquals("1: -1,2,-3", cl.toString());
        assertSame(ClauseStructure.MIXED, cl.structure);
        cl = new Clause(1, Connective.ATLEAST, 2, -1, 2, -3);
        assertEquals("L-1: ATLEAST 2: -1,2,-3", cl.toString());
        assertSame(ClauseStructure.MIXED, cl.structure);
    }

    @Test
    public void cloneTest() {
        System.out.println("clone");
        Clause cl1 = new Clause(1, Connective.OR, 1, 2, 3);
        Clause cl2 = cl1.clone(2);
        assertEquals("2: 1,2,3", cl2.toString());
        Clause cl3 = cl1.clone(3, 0);
        assertEquals("3: 2,3", cl3.toString());
        Clause cl4 = cl1.clone(4, 1);
        assertEquals("4: 1,3", cl4.toString());

        Clause cl5 = new Clause(5, Connective.EXACTLY, 2, -1, -2, -3);
        Clause cl6 = cl5.clone(6, 2);
        assertEquals("X-6: EXACTLY 2: -1,-2", cl6.toString());
        Clause cl7 = cl5.cloneExcept(7, -2);
        assertEquals("X-7: EXACTLY 2: -1,-3", cl7.toString());
    }

    @Test
    public void contains() {
        System.out.println("contains");
        Clause cl = new Clause(1, Connective.OR, 5, -6, 7);

        assertEquals(+1, cl.contains(5));
        assertEquals(-1, cl.contains(-5));
        assertEquals(+1, cl.contains(-6));
        assertEquals(+1, cl.contains(7));
        assertEquals(-1, cl.contains(6));
        assertEquals(0, cl.contains(8));

        assertEquals(0, cl.contains(5, cl.getCLiteral(0)));

        assertEquals(5, cl.getLiteral(0));
        assertEquals(-6, cl.getLiteral(1));
        assertEquals(7, cl.getLiteral(2));
    }


    @Test
    public void isSubset() {
        System.out.println("isSubset");

        Clause cl1 = new Clause(1, Connective.OR, 5, -6, 7);
        Clause cl2 = new Clause(2, Connective.OR, 5, -6, 7);
        assertTrue(cl1.isSubset(cl2));

        Clause cl3 = new Clause(2, Connective.OR, 7, 5);
        assertTrue(cl3.isSubset(cl1));
        assertFalse(cl1.isSubset(cl3));
        Clause cl4 = new Clause(2, Connective.OR, 5, 6, 7);
        assertFalse(cl1.isSubset(cl4));

        Clause cl5 = new Clause(2, Connective.AND, 5, -6, 7);
        assertFalse(cl1.isSubset(cl5));
    }

    @Test
    public void overlaps() {
        System.out.println("overlaps");

        Clause cl1 = new Clause(1, Connective.OR, 1, 2, 3);
        Clause cl2 = new Clause(2, Connective.OR, 4, 5, 6);
        assertNull(cl1.overlaps(cl2));

        Clause cl3 = new Clause(3, Connective.OR, 3, 4, 5);
        assertEquals("[1, 3]", Arrays.toString(cl1.overlaps(cl3)));
        assertEquals("[1, 3]", Arrays.toString(cl3.overlaps(cl1)));

        Clause cl4 = new Clause(4, Connective.OR, -2, -3, 4);
        assertEquals("[-1, 2]", Arrays.toString(cl1.overlaps(cl4)));
        assertEquals("[-1, -2]", Arrays.toString(cl4.overlaps(cl1)));
    }


    @Test
    public void removeLiteral() {
        System.out.println("remove");
        Clause c1 = new Clause(1, Connective.OR, 1, 2, 3);
        c1.remove(c1.getCLiteral(1));
        assertEquals("1: 1,3", c1.toString());
        c1.remove(c1.getCLiteral(1));
        assertEquals("1: 1", c1.toString());
        c1.remove(c1.getCLiteral(0));
        assertEquals("1: ", c1.toString());
        assertTrue(c1.isEmpty());

        c1 = new Clause(2, Connective.OR, 1, 2, 3);
        c1.remove(c1.getCLiteral(2));
        assertEquals("2: 1,2", c1.toString());
        c1.remove(c1.getCLiteral(0));
        assertEquals("2: 2", c1.toString());
    }

    @Test
    public void removeAtPosition() {
        System.out.println("removeAtPosition");
        Clause c1 = new Clause(1, Connective.OR, 1, 2, 3);
        c1.removeAtPosition(1);
        assertEquals("1: 1,3", c1.toString());
        CLiteral lit = c1.getCLiteral(1);
        assertEquals(1, lit.clausePosition);
    }

    @Test
    public void doubles() {
        System.out.println("double tautology");
        Clause c1 = new Clause(1, Connective.OR, 5, -6, -5, -6, -6);
        assertTrue(c1.hasDoubles());
        assertTrue(c1.hasComplementaries());
        assertTrue(c1.removeDoubles());
        assertEquals("1: 5,-6,-5", c1.toString());
    }

    @Test
    public void infoString() {
        System.out.println("infoString");
        Clause cl1 = new Clause(1, Connective.OR, 1, 2, 3);
        assertEquals("1: 1,2,3 [1]",cl1.infoString(0,null));
        Symboltable st = makeSymboltable();
        assertEquals("1:   a,b,c [1]",cl1.infoString(5,st));
    }

    @Test
    public void check() {
        System.out.println("check");
        StringBuilder errors = new StringBuilder();
        Clause cl1 = new Clause(1, Connective.ATLEAST, 4, 1, 2, 3);
        assertFalse(cl1.check(errors));
        assertEquals("Clause 1: Quantifier 4 is not between 1 and 3\n",errors.toString());

        errors = new StringBuilder();
        cl1 = new Clause(1, Connective.ATLEAST, 2, 1, 2, 3);
        cl1.structure = ClauseStructure.MIXED;
        assertFalse(cl1.check(errors));
        assertEquals("Clause 1: Clause has wrong structure: MIXED, and not POSITIVE\n",errors.toString());

        errors = new StringBuilder();
        cl1 = new Clause(1, Connective.ATLEAST, 2, 1, 2, 3);
        cl1.cliterals.set(2,new CLiteral(2,cl1,4));
        assertFalse(cl1.check(errors));
        assertEquals("Clause 1: Literal 2 has wrong position 4 instead of 2\n",errors.toString());

        cl1 = new Clause(1, Connective.ATLEAST, 2, 1, 2, 3);
        assertTrue(cl1.check(errors));}
}