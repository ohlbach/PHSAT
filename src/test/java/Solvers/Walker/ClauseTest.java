package Solvers.Walker;

import Datastructures.Clauses.Quantifier;
import Datastructures.Results.Unsatisfiable;
import junit.framework.TestCase;

public class ClauseTest extends TestCase {
    private static int cOr = Quantifier.OR.ordinal();
    private static int cAtleast = Quantifier.ATLEAST.ordinal();
    private static int cAtmost = Quantifier.ATMOST.ordinal();
    private static int cExactly = Quantifier.EXACTLY.ordinal();
    private static int cInterval = Quantifier.INTERVAL.ordinal();

    public void testConstructor() {
        System.out.println("constructor");
        Clause clause = new Clause(new int[]{1,cOr,1,2,3});
        assertEquals("1: 1v2v3",clause.toString());
        assertEquals(1,clause.min);
        assertEquals(3,clause.max);
        assertFalse(clause.isTrue());
        assertFalse(clause.isFalse());

        clause = new Clause(new int[]{2,cAtleast,2,1,2,3,1,2,3});
        assertEquals("2: >=2 1^2,2^2,3^2",clause.toString());
        assertEquals(2,clause.min);
        assertEquals(6,clause.max);
        assertEquals(3,clause.size());
        assertEquals(6,clause.expandedSize);

        clause = new Clause(new int[]{3,cAtmost,2,1,2,3,1,2,3});
        assertEquals("3: <=2 1^2,2^2,3^2",clause.toString());
        assertEquals(0,clause.min);
        assertEquals(2,clause.max);

        clause = new Clause(new int[]{4,cExactly,2,1,2,3,1,2,3});
        assertEquals("4: =2 1^2,2^2,3^2",clause.toString());
        assertEquals(2,clause.min);
        assertEquals(2,clause.max);

        clause = new Clause(new int[]{5,cInterval,2,3,1,2,3,1,2,3});
        assertEquals("5: [2,3] 1^2,2^2,3^2",clause.toString());
        assertEquals(2,clause.min);
        assertEquals(3,clause.max);
    }

    public void testIsTrueFalse() {
        System.out.println("isTrue, isFalse");
        Clause clause = new Clause(new int[]{1,cInterval,0,6,1,2,3,1,2,3});
        assertTrue(clause.isTrue());
        assertFalse(clause.isFalse());

        clause = new Clause(new int[]{2,cAtleast,7,1,2,3,1,2,3});
        assertFalse(clause.isTrue());
        assertTrue(clause.isFalse());

        clause = new Clause(new int[]{3,cInterval,3,2,1,2,3,1,2,3});
        assertFalse(clause.isTrue());
        assertTrue(clause.isFalse());

        clause = new Clause(new int[]{4,cAtmost,-1,1,2,3,1,2,3});
        assertFalse(clause.isTrue());
        assertTrue(clause.isFalse());
    }


    public void testFindLiteral() {
        System.out.println("findLiteral");
        Clause clause = new Clause(new int[]{1,cInterval,0,6,1,2,3,1,2,3});
        Literal literalObject = clause.findLiteral(2);
        assertEquals(2,literalObject.literal);
        assertTrue(literalObject.clause == clause);
        assertNull(clause.findLiteral(-2));
    }

    public void testRemoveComplementaryLiterals() throws Unsatisfiable {
        System.out.println("removeComplementaryLiterals");
        int[] inputClause = new int[]{1,cOr,1,2,3};
        Clause clause = new Clause(inputClause);
        clause.removeComplementaryLiterals();
        assertEquals("1: 1v2v3",clause.toString());
        inputClause = new int[]{2,cOr,1,2,3,-2};
        clause = new Clause(inputClause);
        clause.removeComplementaryLiterals();
        assertEquals("2: 1v3",clause.toString());

        inputClause = new int[]{3,cAtleast,3,1,2,3,-2};
        clause = new Clause(inputClause);
        clause.removeComplementaryLiterals();
        assertEquals("3: >=2 1,3",clause.toString());

        inputClause = new int[]{4,cInterval,0,2,1,2,3,-2,-3,4};
        clause = new Clause(inputClause);
        clause.removeComplementaryLiterals();
        assertEquals("4: -1&-4",clause.toString());

        inputClause = new int[]{5,cInterval,0,2,1,2,3,-2,-3,-1};
        clause = new Clause(inputClause);
        assertTrue(clause.removeComplementaryLiterals());

        inputClause = new int[]{6,cAtleast,2,1,-1,2,-2,3,4};
        clause = new Clause(inputClause);
        assertTrue(clause.removeComplementaryLiterals());

        inputClause = new int[]{7,cAtleast,2,1,1,1,2,3,-1,-1};
        clause = new Clause(inputClause);
        assertTrue(clause.removeComplementaryLiterals());

        inputClause = new int[]{8,cAtleast,3,1,1,1,2,3,-1,-1};
        clause = new Clause(inputClause);
        assertFalse(clause.removeComplementaryLiterals());
        assertEquals("8: 1v2v3",clause.toString());

        inputClause = new int[]{9,cAtleast,3,1,1,2,3,-1,-1,-1};
        clause = new Clause(inputClause);
        assertFalse(clause.removeComplementaryLiterals());
        assertEquals("9: 2v3v-1",clause.toString());

        inputClause = new int[]{10,cAtleast,3,1,2,-1,-2,3,4};
        clause = new Clause(inputClause);
        assertFalse(clause.removeComplementaryLiterals());
        assertEquals("10: 3v4",clause.toString());


        inputClause = new int[]{11,cInterval,0,2,1,2,3,-2,-3,-1,4};
        clause = new Clause(inputClause);
        try{clause.removeComplementaryLiterals();}
        catch(Unsatisfiable uns) {
            System.out.println(uns.toString());
        }
    }

    public void testReduceMultiplicities() throws Unsatisfiable {
        System.out.println("reduceMultiplicities");
        Clause clause = new Clause(new int[]{1, cOr, 1, 2, 3});
        clause.reduceMultiplicities();
        assertEquals("1: 1v2v3",clause.toString());

        clause = new Clause(new int[]{2, cOr, 1, 2, 1,  3, 1, 3});
        clause.reduceMultiplicities();
        assertEquals("2: 1v2v3",clause.toString());

        clause = new Clause(new int[]{3, cInterval, 2,3, 1, 2, 1, 1, 3, 1, 3});
        clause.reduceMultiplicities();
        assertEquals("3: [2,3] 1^2,2,3^2",clause.toString());

        clause = new Clause(new int[]{4, cAtmost, 4, 1,1,1,2,3});
        clause.reduceMultiplicities();
        assertEquals("4: <=2 1,2,3",clause.toString());

        clause = new Clause(new int[]{5, cAtmost, 4, 1,1,2,3,4});
        clause.reduceMultiplicities();
        assertEquals("5: <=3 1,2,3,4",clause.toString());


    }

    }