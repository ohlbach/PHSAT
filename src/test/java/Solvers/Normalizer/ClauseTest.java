package Solvers.Normalizer;

import Datastructures.Clauses.Quantifier;
import Datastructures.Symboltable;
import Management.Monitor.Monitor;
import Management.Monitor.MonitorLife;
import Solvers.Normalizer.NMInferenceSteps.NMInferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;
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

    public void testConstructor() {
        System.out.println("Constructor");
        int[] clause = new int[]{5,nor,1,-2,3};
        Clause clause1 = new Clause(clause);
        assertEquals("5: 1,-2,3", clause1.toString(null,0));
        assertEquals("5: p,-q,r", clause1.toString(symboltable,0));
        assertFalse(clause1.hasMultiplicities());
        assertTrue(clause1.isDisjunction());

        assertEquals("[5, 0, 0, 1, 3, 3, 1, 1, -2, 1, 3, 1]", Arrays.toString(clause1.toIntArray()));

        clause = new int[]{6,natl,2,1,-2,1,-2,-2,3};
        clause1 = new Clause(clause);
        assertEquals("6: >=2 1^2,-2^3,3", clause1.toString(null,0));
        assertTrue(clause1.hasMultiplicities());
        assertFalse(clause1.isDisjunction());

        clause = new int[]{7,natm,3,1,-2,1,-2,-2,3,3,4};
        clause1 = new Clause(clause);
        assertEquals("7: <=3 1^2,-2^3,3^2,4", clause1.toString(null,0));

        assertEquals("[7, 0, 4, 0, 3, 8, 1, 2, -2, 3, 3, 2, 4, 1]", Arrays.toString(clause1.toIntArray()));

        clause = new int[]{8,nex,1,3,-4};
        clause1 = new Clause(clause);
        assertEquals("8: =1 3,-4", clause1.toString(null,0));

        clause = new int[]{8,nint,1,2,3,-4,6};
        clause1 = new Clause(clause);
        assertEquals("8: [1,2] 3,-4,6", clause1.toString(null,0));

        assertEquals("[8, 0, 6, 1, 2, 3, 3, 1, -4, 1, 6, 1]", Arrays.toString(clause1.toIntArray()));

        clause = new int[]{9,nor};
        clause1 = new Clause(clause);
        assertEquals("9: ", clause1.toString(null,0));
    }

    public void testIsTrue() {
        System.out.println("isTrue");
        Clause clause1 = new Clause(new int[]{5, nor, 1,2,3});
        assertTrue(clause1.isTrue(literal -> literal == 1));
        assertTrue(clause1.isTrue(literal -> literal == 2));
        assertTrue(clause1.isTrue(literal -> literal == 3));
        assertFalse(clause1.isTrue(literal -> literal == -2));

        clause1 = new Clause(new int[]{6, natl, 2, 1,2,2,3});
        assertFalse(clause1.isTrue(literal -> literal == 1));
        assertTrue(clause1.isTrue(literal -> literal == 2));
        assertTrue(clause1.isTrue(literal -> literal != 2));

        clause1 = new Clause(new int[]{7, natm, 2, 1,2,2,2,3});
        assertFalse(clause1.isTrue(literal -> literal == 2));
        assertTrue(clause1.isTrue(literal -> literal == 4));

        clause1 = new Clause(new int[]{8, nint, 2, 3, 1,2,2,2,3});
        assertFalse(clause1.isTrue(literal -> literal == 1));
        assertTrue(clause1.isTrue(literal -> literal != 2));
        assertTrue(clause1.isTrue(literal -> literal == 2));

        clause1 = new Clause(new int[]{9, nor, 1,2,3});
        clause1.quantifier = Quantifier.AND;
        assertFalse(clause1.isTrue(literal -> literal == 1));
        assertTrue(clause1.isTrue(literal -> literal != 4));
        assertFalse(clause1.isTrue(literal -> literal != 2));
    }

    public void testRemoveMultiplicities() {
        System.out.println("removeMultiplicities");
        int[] clause = new int[]{5,nor,1,-2,3};
        Clause clause1 = new Clause(clause);
        assertFalse(clause1.removeMultiplicities(false, null, null));
        assertEquals("5: 1,-2,3",clause1.toString(null,0));

        clause = new int[]{6,nor,1,-2,1,3,-2,1};
        clause1 = new Clause(clause);
        assertTrue(clause1.removeMultiplicities(true, monitor, null));
        assertEquals("6.1: 1,-2,3",clause1.toString(null,0));
        assertEquals("removeMultiplicities: 6: p^3,-q^2,r => 6.1: p,-q,r\n",clause1.deductions(symboltable));

        clause = new int[]{7,natl,1,-2,1,3,-2,1};
        clause1 = new Clause(clause);
        assertTrue(clause1.removeMultiplicities(true, monitor, null));
        assertEquals("7.1: >=1 -2,1,3",clause1.toString(null,0));
        assertEquals("removeMultiplicities: 7: >=1 -q^2,p^2,r => 7.1: >=1 -q,p,r\n",clause1.deductions(symboltable));

        clause = new int[]{8,nint,2,3,1,1,1,2,2,3,3,3};
        clause1 = new Clause(clause);
        assertTrue(clause1.removeMultiplicities(true, monitor, null));
        assertEquals("8.1: [2,3] 1^2,2^2,3^2",clause1.toString(null,0));

        clause = new int[]{9,natm,2,1,1,1,2,2,3,3,3};
        clause1 = new Clause(clause);
        assertFalse(clause1.removeMultiplicities(true, monitor, null));
    }

    public void testRemoveMultiplicitiesVerify() {
        System.out.println("removeMultiplicities Verify");
        int[] clause = new int[]{7,natl,2,-2,-2,1,3,3,-2,1,1};
        //symboltable = null;
        Clause clause1 = new Clause(clause);
        assertTrue(clause1.removeMultiplicities(true, monitor, symboltable));
        assertEquals("7.1: >=2 -q^2,p^2,r^2",clause1.toString(symboltable,0));
        StringBuilder errors = new StringBuilder();
        assertTrue(clause1.inferenceSteps.get(0).verify(clause1,symboltable,errors));

        System.out.println("NEW");
        clause1 = new Clause(new int[]{8,natl,2,1,2,2,2,3});
        Clause clause2 =  new Clause(new int[]{9,natl,2,1,2,3});
        assertTrue(clause1.removeMultiplicities(true, monitor, symboltable));
        assertFalse(clause1.inferenceSteps.get(0).verify(clause2,symboltable,errors));
        System.out.println(errors.toString());

    }
    public void testNMInferenceStepIsTrue() {
        System.out.println("NMInferenceStep Verification isTrue");
        IntArrayList literals = IntArrayList.wrap(new int[]{1,2,3});
        assertFalse(NMInferenceStep.isTrue(0,1,literals));
        assertTrue(NMInferenceStep.isTrue(0,-1,literals));
        assertTrue(NMInferenceStep.isTrue(1,1,literals));
        assertFalse(NMInferenceStep.isTrue(1,-1,literals));
        assertTrue(NMInferenceStep.isTrue(3,1,literals)); // pattern 11
        assertFalse(NMInferenceStep.isTrue(3,-1,literals));
        assertFalse(NMInferenceStep.isTrue(4,1,literals)); // pattern 100
        assertTrue(NMInferenceStep.isTrue(4,-1,literals));
        assertTrue(NMInferenceStep.isTrue(4,3,literals));
        assertFalse(NMInferenceStep.isTrue(4,-3,literals));
        assertTrue(NMInferenceStep.isTrue(5,3,literals));  // pattern 101
        assertFalse(NMInferenceStep.isTrue(5,-3,literals));

        assertEquals("", NMInferenceStep.model(0,literals,null));
        assertEquals("1,3,", NMInferenceStep.model(5,literals,null));
        literals.set(1,-2);
        assertEquals("1,-2,3,", NMInferenceStep.model(7,literals,null));
        assertEquals("-2,", NMInferenceStep.model(2,literals,null));
    }
    public void testRemoveComplementariesVerify() {
        System.out.println("removeComplementaries Verify");
        Clause clause1 = new Clause(new int[]{5,natl,3,1,-1,2,-2,3,4});
        Clause clause2 = new Clause(new int[]{6,natl,1,3,4});
        StringBuilder errors = new StringBuilder();
        NMInferenceStep step = new NMInferenceStep("removeComplementaries", clause1);
        assertTrue(step.verify(clause2, symboltable, errors));

        Clause clause3 = new Clause(new int[]{7,natl,2,3,4});
        assertFalse(step.verify(clause3, symboltable, errors));
        //System.out.println(errors.toString());
        errors = new StringBuilder();

        clause1 = new Clause(new int[]{7,natl,2,1,2,2,-2});
        step = new NMInferenceStep("removeComplementaries",clause1);
        clause2 = new Clause(new int[]{8,natl,1,1,2});
        assertTrue(step.verify(clause2, symboltable, errors));

        clause3 = new Clause(new int[]{9,natl,1,1,-2});
        assertFalse(step.verify(clause3, symboltable, errors));
        System.out.println(errors.toString());
    }

    public void testRemoveComplementaries() {
        System.out.println("removeComplementaries");
        int[] clause = new int[]{5,nor,1,-2,3};
        Clause clause1 = new Clause(clause);
        clause1.removeComplementaries(false, null, null);
        assertEquals("5: 1,-2,3", clause1.toString(null,0));

        clause = new int[]{6,nor,1,-2,3,-1,2};
        clause1 = new Clause(clause);
        assertTrue(clause1.removeComplementaries(false, null, null));

        clause = new int[]{7,natl,5,1,1,-2,3,-1,2,2};
        clause1 = new Clause(clause);
        clause1.removeComplementaries(false, null, null);
        assertEquals("7.1: >=3 1,3,2", clause1.toString(null,0));

        Monitor monitor = new MonitorLife();
        StringBuilder errors = new StringBuilder();
        clause = new int[]{8,nint,4,5,3,1,1,-2,3,-1,2,2,-2};
        clause1 = new Clause(clause);
        clause1.removeComplementaries(true, monitor, null);
        assertEquals("8.1: 3,1", clause1.toString(null,0));
        assertTrue(clause1.inferenceSteps.get(0).verify(clause1,null,errors));
    }


}