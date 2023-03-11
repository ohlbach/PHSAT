package Solvers.Simplifier;

import Datastructures.Clauses.Quantifier;
import Datastructures.Symboltable;
import junit.framework.TestCase;

import java.util.ArrayList;

public class ClauseTest extends TestCase {
    static int cOr = Quantifier.OR.ordinal();
    static int cAtleast = Quantifier.ATLEAST.ordinal();

    static ArrayList<Literal> removedLiterals = new ArrayList<>();

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
        assertEquals(Quantifier.OR,clause1.quantifier);
        assertTrue(clause1.exists);
        assertTrue(clause1.isDisjunction);
        assertEquals(1,clause1.limit);
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
    }

    public void testConstructorInputClause2() {
        System.out.println("constructor InputClause Atleast");
        int[] c1 = new int[]{11, cAtleast, 2, 1, 2, 1, -3, 2, -3,4};
        Clause clause = new Clause(c1);
        //System.out.println(clause.toString());
        //System.out.println(clause.toString(symboltable,10));
        assertEquals("11: >= 2 p^2,q^2,-r^2,s",clause.toString(symboltable,0));
        assertEquals(11, clause.id);
        assertEquals(Quantifier.ATLEAST, clause.quantifier);
        assertEquals(4,clause.size());
        assertEquals(7,clause.expandedSize());
        assertFalse(clause.isDisjunction);
        assertTrue(clause.hasMultiplicities);
        ArrayList<Literal> trueLiterals = new ArrayList(5);
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
        assertFalse(clause2.removeLiteral(literal3,true));
        assertFalse(literal3.clause.exists);
    }

    public void testConstructorQuantified() {
        System.out.println("constructor Quantified");
        Clause clause = new Clause(10, Quantifier.ATLEAST, 3, 1,3,2,2,3,1);
        assertEquals("10: >= 3 1^3,2^2,3",clause.toString());
        assertEquals(6,clause.expandedSize);
        clause = new Clause(11, Quantifier.ATLEAST, 2, 1,3,2,2,3,1);
        assertEquals("11: >= 2 1^2,2^2,3",clause.toString());
        assertTrue(clause.hasMultiplicities);
        assertEquals(2,clause.limit);
        assertEquals(5,clause.expandedSize);
    }
    public void testConstructorOr() {
        System.out.println("constructor Or");
        Clause clause = new Clause(10, 1, 2, -3);
        assertEquals("10: 1v2v-3", clause.toString());
        assertEquals(3,clause.expandedSize);
    }
    public void testRemoveComplementaryLiterals() {
        System.out.println("removeComplementaryLiterals");
        Clause clause = new Clause(new int[]{10, cAtleast, 3, 1, 1, -1, -1, 2,3});
        assertFalse(clause.removeComplementaryLiterals());
        assertEquals("10: 2v3",clause.toString());

        clause = new Clause(new int[]{11, cAtleast, 3, 1, 1, -1, -1, 2,3,-1});
        assertFalse(clause.removeComplementaryLiterals());
        assertEquals("11: -1v2v3",clause.toString());

        clause = new Clause(new int[]{12, cAtleast, 2, 1, 1, -1, -1, 2,3,1});
        assertTrue(clause.removeComplementaryLiterals());

        clause = new Clause(new int[]{13, cAtleast, 2, 1, 1, -1, -1});
        assertTrue(clause.removeComplementaryLiterals());

        clause = new Clause(new int[]{14, cAtleast, 3, 1, 1, -1, -1});
        assertTrue(clause.removeComplementaryLiterals());

        clause = new Clause(new int[]{15, cAtleast, 3, 1, 2,-1,3,-2,4});
        assertFalse(clause.removeComplementaryLiterals());
        assertEquals("15: 3v4",clause.toString());

        clause = new Clause(new int[]{16, cOr, 1, 2, -1, 3});
        assertTrue(clause.removeComplementaryLiterals());

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
        assertEquals("12: >= 2 1^2,2,3", clause.toString());}

    public void testReduceByTrueLiterals() {
        System.out.println("reduceByTrueLiterals");
        removedLiterals.clear();
        Clause clause = new Clause(new int[]{10, cAtleast, 5, 1, 1, 3,4, 2, 2});
        ArrayList<Literal> literals = clause.reduceByTrueLiterals(removedLiterals);
        assertEquals("10: 3v4", clause.toString());
        assertEquals(2,literals.size());
        assertEquals(2,literals.get(1).literal);
        String st = "";
        for(Literal literalObject: removedLiterals) st += literalObject.literal;
        assertEquals("12",st);
        clause = new Clause(new int[]{11, cAtleast, 4, 1, 1, 3,4, 2, 2});
        assertNull(clause.reduceByTrueLiterals(removedLiterals));

    }
        public void testReduceToEssentialLiterals() {
        System.out.println("reduceToEssentialLiterals");
        removedLiterals.clear();
        Clause clause = new Clause(new int[]{10, cAtleast, 2, 1, 1, 3, 2, 2});
        assertTrue(clause.reduceToEssentialLiterals(removedLiterals));
        assertEquals("10: 1v2", clause.toString());
        String st = "";
        for(Literal literalObject: removedLiterals) st += literalObject.literal;
        assertEquals("3",st);

        removedLiterals.clear();
        clause = new Clause(new int[]{10, cAtleast, 2, 1, 1, 3, 4, 2, 2});
        assertFalse(clause.reduceToEssentialLiterals(removedLiterals));
        assertEquals("10: >= 2 1^2,3,4,2^2", clause.toString());
        assertTrue(removedLiterals.isEmpty());
    }

    public void testReplaceEquivalenceTwo() {
        System.out.println("replaceEquivalenceTwo");
        Clause clause = new Clause(new int[]{10, cOr, 1,2});
        assertFalse(clause.replaceEquivalenceTwo(3,2));
        assertEquals("10: 1v3",clause.toString());

        assertTrue(clause.replaceEquivalenceTwo(1,3));
        assertEquals("10: 1v3",clause.toString());
    }

    public void testReplaceEquivalenceMore() {
        System.out.println("replaceEquivalenceMore");
        Clause clause = new Clause(new int[]{10, cAtleast, 2, 1,1,2,2,3,3});
        assertFalse(clause.replaceEquivalenceMore(4,2));
        assertEquals("10: >= 2 1^2,4^2,3^2",clause.toString());

        assertTrue(clause.replaceEquivalenceMore(1,4));
        assertEquals("10: >= 2 1^2,3^2",clause.toString());
    }


}