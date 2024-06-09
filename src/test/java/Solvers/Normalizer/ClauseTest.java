package Solvers.Normalizer;

import Datastructures.Clauses.Quantifier;
import Datastructures.Symboltable;
import Management.Monitor.Monitor;
import Management.Monitor.MonitorLife;
import junit.framework.TestCase;


public class ClauseTest extends TestCase {
    int nand = Quantifier.AND.ordinal();
    int nor = Quantifier.OR.ordinal();
    int natl = Quantifier.ATLEAST.ordinal();
    int natm = Quantifier.ATMOST.ordinal();
    int nex = Quantifier.EXACTLY.ordinal();
    int nint = Quantifier.INTERVAL.ordinal();

    static Symboltable symboltable = new Symboltable(10);
    static Monitor monitor = new MonitorLife();
    static {
        symboltable.setName(1,"p");
        symboltable.setName(2,"q");
        symboltable.setName(3,"r");
        symboltable.setName(4,"s");

    }
    static NormalizerStatistics statistics = new NormalizerStatistics(null);
    /*
    static Clause makeClause(int[] inputClause) {
        return new Clause(inputClause,false,statistics,null,null);
    }

    public void testConstructor() {
        System.out.println("Constructor");
        int[] clause = new int[]{5,nor,1,-2,3};
        Clause clause1 = makeClause(clause);
        assertEquals("5: 1,-2,3", clause1.description(null,0));
        assertEquals("5: p,-q,r", clause1.description(symboltable,0));

        clause = new int[]{6,natl,2,1,-2,1,-2,-2,3};
        clause1 = makeClause(clause);
        assertEquals("6: >=2 1^2,-2^3,3", clause1.description(null,0));

        clause = new int[]{7,natm,3,1,-2,1,-2,-2,3,3,4};
        clause1 = makeClause(clause);
        assertEquals("7: <=3 1^2,-2^3,3^2,4", clause1.description(null,0));


        clause = new int[]{8,nex,1,3,-4};
        clause1 = makeClause(clause);
        assertEquals("8: =1 3,-4", clause1.description(null,0));

        clause = new int[]{8,nint,1,2,3,-4,6};
        clause1 = makeClause(clause);
        assertEquals("8: [1,2] 3,-4,6", clause1.description(null,0));

        clause = new int[]{9,nor};
        clause1 = makeClause(clause);
        assertEquals("9: ", clause1.description(null,0));
    }

    public void testClassifyClause() {
        System.out.println("classifyClause");
        Clause clause = makeClause(new int[]{5, nor, 1});
        assertEquals(Quantifier.AND,clause.quantifier);
        clause = makeClause(new int[]{6, natl, 2, 1,2});
        assertEquals(Quantifier.AND,clause.quantifier);
        clause = makeClause(new int[]{7, nint, 2,2, 1,2,3});
        assertEquals(Quantifier.EXACTLY,clause.quantifier);
        clause = makeClause(new int[]{8, nint, 3,3, 1,2,3});
        assertEquals(Quantifier.AND,clause.quantifier);
        clause = makeClause(new int[]{9, nint, 0,3, 1,2,3});
        assertTrue(clause.isTrue);
        clause = makeClause(new int[]{10, nint, 1,3, 1,2,3});
        assertEquals(Quantifier.OR,clause.quantifier);
        clause = makeClause(new int[]{11, nint, 2,3, 1,2,3});
        assertEquals(Quantifier.ATLEAST,clause.quantifier);
        clause = makeClause(new int[]{12, natl, 4, 1,1,2,2});
        assertEquals(Quantifier.AND,clause.quantifier);
        clause = makeClause(new int[]{12, nint, 2,4, 1,1,2,2,3});
        assertEquals(Quantifier.INTERVAL,clause.quantifier);
    }



    public void testApplyTrueLiteral() {
        System.out.println("applyTrueLiteral");
        StringBuilder errors = new StringBuilder();
        Clause clause1 = makeClause(new int[]{5, natl, 1, 2, 3});
        clause1.applyTrueLiteral(2, false, statistics,monitor, null);
        assertTrue(clause1.isTrue);
        clause1 = makeClause(new int[]{6, nor, 1, 2, 3});
        clause1.applyTrueLiteral(-2, false,statistics, monitor, null);
        assertFalse(clause1.isTrue);
        assertEquals("6.1: 1,3",clause1.description(null,0));
        clause1 = makeClause(new int[]{7, natl, 2, 1, 2,2, 3});
        clause1.applyTrueLiteral(-2, false,statistics, monitor, null);
        assertFalse(clause1.isTrue);
        assertEquals("7.1: & 1,3",clause1.description(null,0));
        clause1 = makeClause(new int[]{8, natl, 2, 1, 2,2, 3});
        clause1.applyTrueLiteral(2, false,statistics, monitor, null);
        assertTrue(clause1.isTrue);

        clause1 = makeClause(new int[]{9, nint, 2,4, 1,1, 2,2, 3,3,4,4});
        clause1.applyTrueLiteral(2, false,statistics, monitor, null);
        assertFalse(clause1.isTrue);
        assertEquals("9.1: <=2 1^2,3^2,4^2",clause1.description(null,0));

        InferenceStep iniStep = new InfInputClause(10);
        clause1 = makeClause(new int[]{10, nint, 2,4, 1,1, 2,2, 3,3,4,4});
        clause1.applyTrueLiteral(-2, true,statistics, monitor, null);
        assertFalse(clause1.isTrue);
        assertEquals("10.1: [2,4] 1^2,3^2,4^2",clause1.description(null,0));
        System.out.println(clause1.inferenceSteps.get(0).description(clause1,null));
        assertTrue(clause1.inferenceSteps.get(0).verify(clause1,null,errors));

        clause1 = makeClause(new int[]{11, natl, 2,1,2,2});
        iniStep = new InfInputClause(11);
        clause1.applyTrueLiteral(-2, true,statistics, monitor, null);
        assertTrue(clause1.isFalse);
        assertFalse(clause1.isTrue);
        System.out.println(clause1.inferenceSteps.get(0).description(clause1,null));
        assertTrue(clause1.inferenceSteps.get(0).verify(clause1,null,errors));
    }

    public void testReplaceEquivalences() {
        System.out.println("replace equivalences");
        StringBuilder errors = new StringBuilder();
        Clause clause1 = makeClause(new int[]{5, nor, 1, 2, 3});
        clause1.applyEquivalentLiteral(4, 2, null, true,statistics,monitor,null);
        assertFalse(clause1.isTrue);
        assertFalse(clause1.isFalse);
        assertEquals("5.1: 1,4,3",clause1.description(null,0));
        System.out.println(clause1.inferenceSteps.get(0).description(clause1,null));
        assertTrue(clause1.inferenceSteps.get(0).verify(clause1,null,errors));

        clause1 = makeClause(new int[]{6, nor, 1, 2, 3});
        clause1.applyEquivalentLiteral(1, 3, null, true,statistics,monitor,null);
        assertFalse(clause1.isTrue);
        assertFalse(clause1.isFalse);
        assertEquals("6.2: 1,2",clause1.description(null,0));
        //System.out.println(clause1.inferenceSteps.get(0).description(clause1,null));
        assertTrue(clause1.inferenceSteps.get(0).verify(clause1,null,errors));
        System.out.println(clause1.deductions(null));

        clause1 = makeClause(new int[]{7, nor, 1, 2, 3});
        clause1.applyEquivalentLiteral(-1, 3, null, true,statistics,monitor,null);
        assertTrue(clause1.isTrue);
        assertFalse(clause1.isFalse);
        assertTrue(clause1.inferenceSteps.get(0).verify(clause1,null,errors));
        System.out.println(clause1.deductions(null));

        clause1 = makeClause(new int[]{8, natm, 2, 1, 2,2, 3});
        Clause conj = clause1.applyEquivalentLiteral(2, 3, null, true,statistics,monitor,null);
        assertTrue(clause1.isTrue);
        assertFalse(clause1.isFalse);
        assertNotNull(conj);
        assertEquals("8.2: & -2",conj.description(null,0));
        assertEquals("8.1: <=2 1",clause1.description(null,0));

        assertTrue(clause1.inferenceSteps.get(0).verify(clause1,null,errors));
        System.out.println(clause1.deductions(null));

        clause1 = makeClause(new int[]{9, natm, 2, 1, 2,2, 3});
        conj = clause1.applyEquivalentLiteral(-2, 3, null, true,statistics,monitor,null);
        assertFalse(clause1.isTrue);
        assertFalse(clause1.isFalse);
        assertNull(conj);
        assertEquals("9.2: <=1 1,2",clause1.description(null,0));

        assertTrue(clause1.inferenceSteps.get(0).verify(clause1,null,errors));
        System.out.println(clause1.deductions(null));
    }

    public void testRemoveLiteralAtPosition() {
        System.out.println("remove literal");
        StringBuilder errors = new StringBuilder();
        Clause clause1 = makeClause(new int[]{5, nint, 1,2, 1, 2, 3});
        clause1.removeLiteral(1, true, statistics,monitor,  null);
        assertTrue(clause1.isTrue);
        assertEquals("5.1: [0,2] 2,3", clause1.description(null, 0));
        System.out.println(clause1.inferenceSteps.get(0).description(clause1, null));
        assertTrue(clause1.inferenceSteps.get(0).verify(clause1, null, errors));

        clause1 = makeClause(new int[]{6, nint, 4,6, 1,1, 2, 3,4,5,6,7,8});
        clause1.removeLiteral(1, true, statistics,monitor,  null);
        assertFalse(clause1.isTrue);
        assertEquals("6.1: [2,6] 2,3,4,5,6,7,8", clause1.description(null, 0));
        System.out.println(clause1.inferenceSteps.get(0).description(clause1, null));
        assertTrue(clause1.inferenceSteps.get(0).verify(clause1, null, errors));
    }
*/
    }