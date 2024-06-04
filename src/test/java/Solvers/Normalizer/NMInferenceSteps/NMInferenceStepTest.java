package Solvers.Normalizer.NMInferenceSteps;

import Datastructures.Clauses.Quantifier;
import Datastructures.Symboltable;
import Management.Monitor.Monitor;
import Management.Monitor.MonitorLife;
import Solvers.Normalizer.Clause;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import junit.framework.TestCase;

public class NMInferenceStepTest extends TestCase {

    int nand = Quantifier.AND.ordinal();
    int nor = Quantifier.OR.ordinal();
    int natl = Quantifier.ATLEAST.ordinal();
    int natm = Quantifier.ATMOST.ordinal();
    int nex = Quantifier.EXACTLY.ordinal();
    int nint = Quantifier.INTERVAL.ordinal();

    static Symboltable symboltable = new Symboltable(10);
    static {
        symboltable.setName(1,"p");
        symboltable.setName(2,"q");
        symboltable.setName(3,"r");
        symboltable.setName(4,"s");

    }
    static Clause makeClause(int[] inputClause) {
        //return new Clause(inputClause,false,null,null);
        return null;
    }
    static Monitor monitor = new MonitorLife();

    public void testIsTrue() {
        System.out.println("isTrue");
        IntArrayList literals = IntArrayList.wrap(new int[]{1,2,3});
        NMInferenceStep step = new NMInferenceStep(null,null);
        assertFalse(step.isTrue(0,1,literals));
        assertTrue(step.isTrue(0,-1,literals));
        assertTrue(step.isTrue(1,1,literals));
        assertFalse(step.isTrue(1,-1,literals));
        assertTrue(step.isTrue(3,1,literals)); // pattern 11
        assertFalse(step.isTrue(3,-1,literals));
        assertFalse(step.isTrue(4,1,literals)); // pattern 100
        assertTrue(step.isTrue(4,-1,literals));
        assertTrue(step.isTrue(4,3,literals));
        assertFalse(step.isTrue(4,-3,literals));
        assertTrue(step.isTrue(5,3,literals));  // pattern 101
        assertFalse(step.isTrue(5,-3,literals));
        }


    public void testModel() {
        System.out.println("model");
        IntArrayList literals = IntArrayList.wrap(new int[]{1,2,3});
        assertEquals("", NMInferenceStep.model(0,literals,null));
        assertEquals("1,3,", NMInferenceStep.model(5,literals,null));
        literals.set(1,-2);
        assertEquals("1,-2,3,", NMInferenceStep.model(7,literals,null));
        assertEquals("-2,", NMInferenceStep.model(2,literals,null));
    }

    public void testVerify()  {
        System.out.println("removeComplementaries Verify");
        Clause clause1 = makeClause(new int[]{5,natl,3,1,-1,2,-2,3,4});
        Clause clause2 = makeClause(new int[]{6,natl,1,3,4});
        StringBuilder errors = new StringBuilder();
        NMInferenceStep step = new NMInferenceStep("removeComplementaries", clause1.simpleClone());
        assertTrue(step.verify(clause2, symboltable, errors));

        Clause clause3 = makeClause(new int[]{7,natl,2,3,4});
        assertFalse(step.verify(clause3, symboltable, errors));
        System.out.println(errors.toString());
        errors = new StringBuilder();

        clause1 = makeClause(new int[]{7,natl,2,1,2,2,-2});
        step = new NMInferenceStep("removeComplementaries",clause1.simpleClone());
        clause2 = makeClause(new int[]{8,natl,1,1,2});
        assertTrue(step.verify(clause2, symboltable, errors));

        clause3 = makeClause(new int[]{9,natl,1,1,-2});
        assertFalse(step.verify(clause3, symboltable, errors));
        System.out.println(errors.toString());
        }

    public void testRemoveMultiplicitiesVerify() {
        System.out.println("removeMultiplicities Verify");
        int[] clause = new int[]{7,natl,2,-2,-2,1,3,3,-2,1,1};
        //symboltable = null;
        Clause clause1 = makeClause(clause);
       // assertTrue(clause1.removeMultiplicities(true, monitor, symboltable));
        assertEquals("7.1: >=2 -q^2,p^2,r^2",clause1.toString(symboltable,0));
        StringBuilder errors = new StringBuilder();
        //assertTrue(clause1.inferenceSteps.get(0).verify(clause1,symboltable,errors));

        //System.out.println("NEW");
        clause1 = makeClause(new int[]{8,natl,2,1,2,2,2,3});
        Clause clause2 =  makeClause(new int[]{9,natl,2,1,2,3});
       // assertTrue(clause1.removeMultiplicities(true, monitor, symboltable));
        //assertFalse(clause1.inferenceSteps.get(0).verify(clause2,symboltable,errors));
        System.out.println(errors.toString());

    }
    }
