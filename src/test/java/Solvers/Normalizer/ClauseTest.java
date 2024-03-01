package Solvers.Normalizer;

import Datastructures.Clauses.Quantifier;
import Datastructures.Symboltable;
import InferenceSteps.InfInputClause;
import InferenceSteps.InferenceStep;
import Management.Monitor.Monitor;
import Management.Monitor.MonitorLife;
import Solvers.Normalizer.NMInferenceSteps.NMInferenceStep;
import junit.framework.TestCase;

import java.util.Arrays;


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
    
    static Clause makeClause(int[] inputClause) {
        return new Clause(inputClause,false,null,null);
    }

    public void testConstructor() {
        System.out.println("Constructor");
        int[] clause = new int[]{5,nor,1,-2,3};
        Clause clause1 = makeClause(clause);
        assertEquals("5: 1,-2,3", clause1.toString(null,0));
        assertEquals("5: p,-q,r", clause1.toString(symboltable,0));
        assertFalse(clause1.hasMultiplicities());
        assertTrue(clause1.isDisjunction());

        assertEquals("[5, 0, 0, 1, 3, 3, 1, 1, -2, 1, 3, 1]", Arrays.toString(clause1.toIntArray()));

        clause = new int[]{6,natl,2,1,-2,1,-2,-2,3};
        clause1 = makeClause(clause);
        assertEquals("6: >=2 1^2,-2^3,3", clause1.toString(null,0));
        assertTrue(clause1.hasMultiplicities());
        assertFalse(clause1.isDisjunction());

        clause = new int[]{7,natm,3,1,-2,1,-2,-2,3,3,4};
        clause1 = makeClause(clause);
        assertEquals("7: <=3 1^2,-2^3,3^2,4", clause1.toString(null,0));

        assertEquals("[7, 0, 4, 0, 3, 8, 1, 2, -2, 3, 3, 2, 4, 1]", Arrays.toString(clause1.toIntArray()));

        clause = new int[]{8,nex,1,3,-4};
        clause1 = makeClause(clause);
        assertEquals("8: =1 3,-4", clause1.toString(null,0));

        clause = new int[]{8,nint,1,2,3,-4,6};
        clause1 = makeClause(clause);
        assertEquals("8: [1,2] 3,-4,6", clause1.toString(null,0));

        assertEquals("[8, 0, 6, 1, 2, 3, 3, 1, -4, 1, 6, 1]", Arrays.toString(clause1.toIntArray()));

        clause = new int[]{9,nor};
        clause1 = makeClause(clause);
        assertEquals("9: ", clause1.toString(null,0));
    }

    public void testIsTrue() {
        System.out.println("isTrue");
        Clause clause1 = makeClause(new int[]{5, nor, 1,2,3});
        assertTrue(clause1.isTrue(literal -> literal == 1));
        assertTrue(clause1.isTrue(literal -> literal == 2));
        assertTrue(clause1.isTrue(literal -> literal == 3));
        assertFalse(clause1.isTrue(literal -> literal == -2));

        clause1 = makeClause(new int[]{6, natl, 2, 1,2,2,3});
        assertFalse(clause1.isTrue(literal -> literal == 1));
        assertTrue(clause1.isTrue(literal -> literal == 2));
        assertTrue(clause1.isTrue(literal -> literal != 2));

        clause1 = makeClause(new int[]{7, natm, 2, 1,2,2,2,3});
        assertFalse(clause1.isTrue(literal -> literal == 2));
        assertTrue(clause1.isTrue(literal -> literal == 4));

        clause1 = makeClause(new int[]{8, nint, 2, 3, 1,2,2,2,3});
        assertFalse(clause1.isTrue(literal -> literal == 1));
        assertTrue(clause1.isTrue(literal -> literal != 2));
        assertTrue(clause1.isTrue(literal -> literal == 2));

        clause1 = makeClause(new int[]{9, nor, 1,2,3});
        clause1.quantifier = Quantifier.AND;
        assertFalse(clause1.isTrue(literal -> literal == 1));
        assertTrue(clause1.isTrue(literal -> literal != 4));
        assertFalse(clause1.isTrue(literal -> literal != 2));
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

    public void testRemoveMultiplicities() {
        System.out.println("removeMultiplicities");
        int[] clause = new int[]{5,nor,1,-2,3};
        Clause clause1 = makeClause(clause);
        assertFalse(clause1.removeMultiplicities(false, null, null));
        assertEquals("5: 1,-2,3",clause1.toString(null,0));

        clause = new int[]{6,nor,1,-2,1,3,-2,1};
        clause1 = makeClause(clause);
        assertTrue(clause1.removeMultiplicities(true, monitor, null));
        assertEquals("6.1: 1,-2,3",clause1.toString(null,0));
        assertEquals("removeMultiplicities: 6: p^3,-q^2,r => 6.1: p,-q,r\n",clause1.deductions(symboltable));

        clause = new int[]{7,natl,1,-2,1,3,-2,1};
        clause1 = makeClause(clause);
        assertTrue(clause1.removeMultiplicities(true, monitor, null));
        assertEquals("7.1: -2,1,3",clause1.toString(null,0));
        assertEquals("removeMultiplicities: 7: -q^2,p^2,r => 7.1: -q,p,r\n",clause1.deductions(symboltable));

        clause = new int[]{8,nint,2,3,1,1,1,2,2,3,3,3};
        clause1 = makeClause(clause);
        assertTrue(clause1.removeMultiplicities(true, monitor, null));
        assertEquals("8.1: [2,3] 1^2,2^2,3^2",clause1.toString(null,0));

        clause = new int[]{9,natm,2,1,1,1,2,2,3,3,3};
        clause1 = makeClause(clause);
        assertFalse(clause1.removeMultiplicities(true, monitor, null));
    }





    public void testRemoveComplementaries() {
        System.out.println("removeComplementaries");
        int[] clause = new int[]{5,nor,1,-2,3};
        Clause clause1 = makeClause(clause);
        clause1.removeComplementaries(false, null, null);
        assertEquals("5: 1,-2,3", clause1.toString(null,0));

        clause = new int[]{6,nor,1,-2,3,-1,2};
        clause1 = makeClause(clause);
        assertTrue(clause1.removeComplementaries(false, null, null));

        clause = new int[]{7,natl,5,1,1,-2,3,-1,2,2};
        clause1 = makeClause(clause);
        clause1.removeComplementaries(false, null, null);
        assertEquals(Quantifier.AND,clause1.quantifier);
        assertEquals("7.1: & 1,3,2", clause1.toString(null,0));

        Monitor monitor = new MonitorLife();
        StringBuilder errors = new StringBuilder();
        clause = new int[]{8,nint,4,5,3,1,1,-2,3,-1,2,2,-2};
        clause1 = makeClause(clause);
        clause1.removeComplementaries(true, monitor, null);
        assertEquals("8.1: 3,1", clause1.toString(null,0));
        assertTrue(clause1.inferenceSteps.get(0).verify(clause1,null,errors));
    }

    public void testDivideByGCD() {
        System.out.println("divideByGCD");
        StringBuilder errors = new StringBuilder();
        Clause clause1 = makeClause(new int[]{5, natl, 4, 1,1,2,2,3,3});
        assertTrue(clause1.divideByGCD(true,monitor,null));
        assertEquals("5.1: >=2 1,2,3", clause1.toString(null, 0));
        assertTrue(clause1.inferenceSteps.get(0).verify(clause1,null,errors));
        assertEquals(3,clause1.expandedSize);

        Clause clause2 = makeClause(new int[]{5, natl, 2, 1,-2,3});
        assertFalse(clause1.inferenceSteps.get(0).verify(clause2,null,errors));
        System.out.println(errors.toString());

        clause1 = makeClause(new int[]{6, natl, 2, 1,1,2,2,3,3});
        assertTrue(clause1.divideByGCD(true,monitor,null));
        assertEquals("6.1: 1,2,3", clause1.toString(null, 0));
        assertTrue(clause1.inferenceSteps.get(0).verify(clause1,null,errors));

        clause1 = makeClause(new int[]{6, natl, 2, 1,1,2,2,3,3,3});
        assertFalse(clause1.divideByGCD(true,monitor,null));
        assertEquals("6: >=2 1^2,2^2,3^3", clause1.toString(null, 0));
    }

    public void testReduceToEssentialLiterals() {
        System.out.println("reduceToEssentialLiterals");
        StringBuilder errors = new StringBuilder();
        Clause clause1 = makeClause(new int[]{5, natl, 2, 1, 1, 2, 2, 3});
        assertTrue(clause1.reduceToEssentialLiterals(true, monitor, null));
        assertEquals("5.1: 1,2",clause1.toString(null,0));
        NMInferenceStep step = clause1.inferenceSteps.get(0);
        assertTrue(step.verify(clause1,null,errors));

        Clause clause2 = makeClause(new int[]{6, nor, 1,3});
        assertFalse(step.verify(clause2,null,errors));
        System.out.println(errors.toString());

        clause1 = makeClause(new int[]{2, natl, 2, 1, 1, 2, 2, 3,4});
        assertFalse(clause1.reduceToEssentialLiterals(true, monitor, null));

    }
    public void testApplyTrueLiteral() {
        System.out.println("applyTrueLiteral");
        StringBuilder errors = new StringBuilder();
        Clause clause1 = makeClause(new int[]{5, natl, 1, 2, 3});
        clause1.applyTrueLiteral(2, null, false, monitor, null);
        assertTrue(clause1.isTrue);
        clause1 = makeClause(new int[]{6, nor, 1, 2, 3});
        clause1.applyTrueLiteral(-2, null, false, monitor, null);
        assertFalse(clause1.isTrue);
        assertEquals("6.1: 1,3",clause1.toString(null,0));
        clause1 = makeClause(new int[]{7, natl, 2, 1, 2,2, 3});
        clause1.applyTrueLiteral(-2, null, false, monitor, null);
        assertFalse(clause1.isTrue);
        assertEquals("7.1: & 1,3",clause1.toString(null,0));
        clause1 = makeClause(new int[]{8, natl, 2, 1, 2,2, 3});
        clause1.applyTrueLiteral(2, null, false, monitor, null);
        assertTrue(clause1.isTrue);

        clause1 = makeClause(new int[]{9, nint, 2,4, 1,1, 2,2, 3,3,4,4});
        clause1.applyTrueLiteral(2, null, false, monitor, null);
        assertFalse(clause1.isTrue);
        assertEquals("9.1: <=2 1^2,3^2,4^2",clause1.toString(null,0));

        InferenceStep iniStep = new InfInputClause(10);
        clause1 = makeClause(new int[]{10, nint, 2,4, 1,1, 2,2, 3,3,4,4});
        clause1.applyTrueLiteral(-2, iniStep, true, monitor, null);
        assertFalse(clause1.isTrue);
        assertEquals("10.1: [2,4] 1^2,3^2,4^2",clause1.toString(null,0));
        System.out.println(clause1.inferenceSteps.get(0).toString(clause1,null));
        assertTrue(clause1.inferenceSteps.get(0).verify(clause1,null,errors));

        clause1 = makeClause(new int[]{11, natl, 2,1,2,2});
        iniStep = new InfInputClause(11);
        clause1.applyTrueLiteral(-2, iniStep, true, monitor, null);
        assertTrue(clause1.isFalse);
        assertFalse(clause1.isTrue);
        System.out.println(clause1.inferenceSteps.get(0).toString(clause1,null));
        assertTrue(clause1.inferenceSteps.get(0).verify(clause1,null,errors));
    }

    }