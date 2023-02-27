package Solvers.Simplifier;

import Datastructures.Clauses.Connective;
import Datastructures.Symboltable;
import junit.framework.TestCase;

import java.util.ArrayList;

public class ClauseTest extends TestCase {
    static int cOr = Connective.OR.ordinal();
    static int cAtleast = Connective.ATLEAST.ordinal();

    static Symboltable symboltable = new Symboltable(10);
    static {
        symboltable.setName(1,"p");
        symboltable.setName(2,"q");
        symboltable.setName(3,"r");
        symboltable.setName(4,"s");
    }

    public void testConstructorInputClause1() {
        System.out.println("constructor InputClause Disjunction");
        int[] c1 = new int[]{10,cOr,1,2,-3};
        Clause clause1 = new Clause(c1);
        //System.out.println(clause1.toString());
        //System.out.println(clause1.toString(symboltable,10));
        assertEquals(10,clause1.id);
        assertEquals(Connective.OR,clause1.connective);
        assertTrue(clause1.exists);
        assertTrue(clause1.isDisjunction);
        assertEquals(1,clause1.quantifier);
        assertEquals(3,clause1.size());
        assertEquals(3,clause1.expandedSize);
        assertEquals(3,clause1.expandedSize());
        assertFalse(clause1.hasMultiplicities);
        assertNull(clause1.previousClause);
        assertNull(clause1.nextClause);
        assertEquals(0,clause1.timestamp);
        assertEquals("Input Clause Id: 10",clause1.inferenceStep.toString(symboltable));

        assertEquals(1,clause1.findLiteral(1).literal);
        assertEquals(2,clause1.findLiteral(2).literal);
        assertEquals(-3,clause1.findLiteral(-3).literal);
        assertNull(clause1.findLiteral(3));
        assertEquals(clause1,clause1.findLiteral(1).clause);

        ArrayList<Literal> trueLiterals = new ArrayList(5);
        trueLiterals.add(null);
        assertEquals(0,clause1.findTrueLiterals(trueLiterals));
        assertTrue(trueLiterals.isEmpty());
    }

    public void testConstructorInputClause2() {
        System.out.println("constructor InputClause Atleast");
        int[] c1 = new int[]{11, cAtleast, 2, 1, 2, 1, -3, 2, -3,4};
        Clause clause = new Clause(c1);
        //System.out.println(clause.toString());
        //System.out.println(clause.toString(symboltable,10));
        assertEquals("11: >= 2 p^2,q^2,-r^2,s",clause.toString(symboltable,0));
        assertEquals(11, clause.id);
        assertEquals(Connective.ATLEAST, clause.connective);
        assertEquals(4,clause.size());
        assertEquals(7,clause.expandedSize());
        assertFalse(clause.isDisjunction);
        assertTrue(clause.hasMultiplicities);
        ArrayList<Literal> trueLiterals = new ArrayList(5);
        assertEquals(0,clause.findTrueLiterals(trueLiterals));
        assertTrue(trueLiterals.isEmpty());
    }

    public void testFindTrueLiterals() {
        System.out.println("findTrueLiterals");
        int[] c1 = new int[]{12, cAtleast, 4, 1, 2, 1, 2,3};
        Clause clause = new Clause(c1);
        assertEquals("12: >= 4 p^2,q^2,r",clause.toString(symboltable,0));
        ArrayList<Literal> trueLiterals = new ArrayList(5);
        assertEquals(4,clause.findTrueLiterals(trueLiterals));
        assertEquals(1,trueLiterals.get(0).literal);
        assertEquals(2,trueLiterals.get(1).literal);
        assertTrue(clause.removeLiterals(trueLiterals));
        assertEquals("12: >= 0 r",clause.toString(symboltable,0));

        int[] c2 = new int[]{13, cAtleast, 3, 1, 2, 1, 2,3};
        clause = new Clause(c2);
        assertEquals(0,clause.findTrueLiterals(trueLiterals));
        int[] c3 = new int[]{12, cAtleast, 9, 1,1,1,1,1,1,1,3,3,3,4,4,4};
        clause = new Clause(c3);
        assertEquals("12: >= 9 p^7,r^3,s^3",clause.toString(symboltable,0));
        assertEquals(7,clause.findTrueLiterals(trueLiterals));
        assertFalse(clause.removeLiterals(trueLiterals));
        assertEquals("12: rvs",clause.toString(symboltable,0));

    }
    public void testRemoveLiteral() {
        System.out.println("removeLiteral");
        int[] c1 = new int[]{12, cOr, 1,2,3};
        Clause clause1 = new Clause(c1);
        Literal literal1 = clause1.findLiteral(1);
        clause1.removeLiteral(literal1,false);
        assertEquals("12: 2v3",clause1.toString());
        assertEquals(2,clause1.size());
        assertEquals(2,clause1.expandedSize);

        int[] c2 = new int[]{13, cAtleast,2 , 1,1,2,2,3};
        Clause clause2 = new Clause(c2);
        Literal literal2 = clause2.findLiteral(1);
        clause2.removeLiteral(literal2,false);
        assertEquals("13: >= 2 2^2,3",clause2.toString());

        Literal literal3 = clause2.findLiteral(2);
        clause2.removeLiteral(literal3,true);
        assertEquals("13: 3",clause2.toString());
        assertTrue(clause2.isDisjunction);
        assertEquals(1, clause2.expandedSize());
        assertFalse(clause2.hasMultiplicities);
        assertNull(literal3.clause);
    }

    public void testConstructorQuantified() {
        System.out.println("constructor Quantified");
        Clause clause = new Clause(10,Connective.ATLEAST, 3, 1,3,2,2,3,1);
        assertEquals("10: >= 3 1^3,2^2,3",clause.toString());
        assertEquals(6,clause.expandedSize);
        clause = new Clause(11,Connective.ATLEAST, 2, 1,3,2,2,3,1);
        assertEquals("11: >= 2 1^2,2^2,3",clause.toString());
        assertTrue(clause.hasMultiplicities);
        assertEquals(2,clause.quantifier);
        assertEquals(5,clause.expandedSize);
    }
    public void testConstructorOr() {
        System.out.println("constructor Or");
        Clause clause = new Clause(10, 1, 2, -3);
        assertEquals("10: 1v2v-3", clause.toString());
        assertEquals(3,clause.expandedSize);
    }

    public void testDivideByGCD() {
        System.out.println("divide by GCD");
        Clause clause = new Clause(new int[]{10, cAtleast, 2, 1,1,1,1,2,2,2,2,2,2});
        clause.divideByGCD();
        assertEquals("10: 1v2", clause.toString());
        clause = new Clause(new int[]{11, cAtleast, 2, 1,1,2,2,3,3});
        clause.divideByGCD();
        assertEquals("11: 1v2v3", clause.toString());
        clause = new Clause(new int[]{12, cAtleast, 4, 1,1,1,1,2,2,3,3});
        clause.divideByGCD();
        assertEquals("12: >= 2 1^2,2,3", clause.toString());


    }
}