package Solvers.Normalizer;

import Datastructures.Clauses.Quantifier;
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

    public void testConstructor() {
        System.out.println("Constructor");
        int[] clause = new int[]{5,nor,1,-2,3};
        Clause clause1 = new Clause(clause);
        assertEquals("5: 1,-2,3", clause1.toString(null,0));
        assertFalse(clause1.hasMultiplicities);

        assertEquals("[5, 0, 0, 1, 3, 3, 1, 1, -2, 1, 3, 1]", Arrays.toString(clause1.toIntArray()));

        clause = new int[]{6,natl,2,1,-2,1,-2,-2,3};
        clause1 = new Clause(clause);
        assertEquals("6: >=2 1^2,-2^3,3", clause1.toString(null,0));
        assertTrue(clause1.hasMultiplicities);

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