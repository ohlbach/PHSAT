package Solvers.Walker;

import Datastructures.Clauses.Quantifier;
import junit.framework.TestCase;

public class ClauseTest extends TestCase {
    private static int cOr = Quantifier.OR.ordinal();
    private static int cAtleast = Quantifier.ATLEAST.ordinal();
    private static int cAtmost = Quantifier.ATMOST.ordinal();
    private static int cExactly = Quantifier.EXACTLY.ordinal();
    private static int cInterval = Quantifier.INTERVAL.ordinal();

    private Clause makeClause(int[] inputClause) {
        Solvers.Normalizer.Clause clause = new Solvers.Normalizer.Clause(inputClause,false,null,null);
        return new Clause(clause);}
    public void testConstructor() {
        System.out.println("constructor");
        Clause clause = makeClause(new int[]{1,cOr,1,2,3});
        assertEquals("1: 1v2v3",clause.toString());
        assertEquals(1,clause.min);
        assertEquals(3,clause.max);
        assertFalse(clause.isTrue());
        assertFalse(clause.isFalse());

        clause = makeClause(new int[]{2,cAtleast,2,1,2,3,1,2,3});
        assertEquals("2: >=2 1^2,2^2,3^2",clause.toString());
        assertEquals(2,clause.min);
        assertEquals(6,clause.max);
        assertEquals(3,clause.size());
        assertEquals(6,clause.expandedSize);

        clause = makeClause(new int[]{3,cAtmost,2,1,2,3,1,2,3});
        assertEquals("3: <=2 1^2,2^2,3^2",clause.toString());
        assertEquals(0,clause.min);
        assertEquals(2,clause.max);

        clause = makeClause(new int[]{4,cExactly,2,1,2,3,1,2,3});
        assertEquals("4: =2 1^2,2^2,3^2",clause.toString());
        assertEquals(2,clause.min);
        assertEquals(2,clause.max);

        clause = makeClause(new int[]{5,cInterval,2,3,1,2,3,1,2,3});
        assertEquals("5: [2,3] 1^2,2^2,3^2",clause.toString());
        assertEquals(2,clause.min);
        assertEquals(3,clause.max);
    }

    public void testIsTrueFalse() {
        System.out.println("isTrue, isFalse");
        Clause clause = makeClause(new int[]{1,cInterval,0,6,1,2,3,1,2,3});
        assertTrue(clause.isTrue());
        assertFalse(clause.isFalse());

        clause = makeClause(new int[]{2,cAtleast,7,1,2,3,1,2,3});
        assertFalse(clause.isTrue());
        assertTrue(clause.isFalse());

        clause = makeClause(new int[]{3,cInterval,3,2,1,2,3,1,2,3});
        assertFalse(clause.isTrue());
        assertTrue(clause.isFalse());

        clause = makeClause(new int[]{4,cAtmost,-1,1,2,3,1,2,3});
        assertFalse(clause.isTrue());
        assertTrue(clause.isFalse());
    }


    public void testFindLiteral() {
        System.out.println("findLiteral");
        Clause clause = makeClause(new int[]{1,cInterval,0,6,1,2,3,1,2,3});
        Literal literalObject = clause.findLiteral(2);
        assertEquals(2,literalObject.literal);
        assertTrue(literalObject.clause == clause);
        assertNull(clause.findLiteral(-2));
    }

   

    }