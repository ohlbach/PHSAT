package Solvers.Normalizer;

import Datastructures.Clauses.Quantifier;
import Datastructures.Symboltable;
import Management.Monitor.Monitor;
import Management.Monitor.MonitorLife;
import junit.framework.TestCase;

import java.util.Arrays;


public class ClauseTest extends TestCase {
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
        assertEquals("RemoveMultiplicities: 6: p^3,-q^2,r => 6.1: p,-q,r\n",clause1.deductions(symboltable));

        clause = new int[]{7,natl,1,-2,1,3,-2,1};
        clause1 = new Clause(clause);
        assertTrue(clause1.removeMultiplicities(true, monitor, null));
        assertEquals("7.1: >=1 -2,1,3",clause1.toString(null,0));
        assertEquals("RemoveMultiplicities: 7: >=1 -q^2,p^2,r => 7.1: >=1 -q,p,r\n",clause1.deductions(symboltable));

        clause = new int[]{8,nint,2,3,1,1,1,2,2,3,3,3};
        clause1 = new Clause(clause);
        assertTrue(clause1.removeMultiplicities(true, monitor, null));
        assertEquals("8.1: [2,3] 1^2,2^2,3^2",clause1.toString(null,0));

        clause = new int[]{9,natm,2,1,1,1,2,2,3,3,3};
        clause1 = new Clause(clause);
        assertFalse(clause1.removeMultiplicities(true, monitor, null));
    }

    public void testRemoveMultiplicitiesVerify() {
        System.out.println("removeMultiplicities Vrify");
        int[] clause = new int[]{7,natl,2,-2,-2,1,3,3,-2,1,1};
        symboltable = null;
        Clause clause1 = new Clause(clause);
        assertTrue(clause1.removeMultiplicities(true, monitor, symboltable));
        //assertEquals("7.1: >=2 -q^2,p^2,r^2",clause1.toString(symboltable,0));
        StringBuilder errors = new StringBuilder();
        assertTrue(clause1.inferenceSteps.get(0).verify(clause1,symboltable,errors));
        System.out.println("NEW");
        Clause clause2 =  new Clause(new int[]{7,natl,2,-2,1,1,3});
        System.out.println(clause2.toString(null,0));
        assertFalse(clause1.inferenceSteps.get(0).verify(clause2,symboltable,errors));
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
        clause1.removeComplementaries(false, null, null);
        assertEquals("6: 3", clause1.toString(null,0));

        clause = new int[]{7,natl,5,1,1,-2,3,-1,2,2};
        clause1 = new Clause(clause);
        clause1.removeComplementaries(false, null, null);
        assertEquals("7: >=3 1,3,2", clause1.toString(null,0));

        Monitor monitor = new MonitorLife();
        clause = new int[]{8,nint,1,2,3,1,1,-2,3,-1,2,2,-2};
        clause1 = new Clause(clause);
        clause1.removeComplementaries(false, monitor, null);
        assertEquals("8: [0,-1] 3^2,1", clause1.toString(null,0));
    }
}