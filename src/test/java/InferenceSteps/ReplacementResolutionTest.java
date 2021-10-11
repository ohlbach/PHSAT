package InferenceSteps;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseType;
import org.junit.Test;

import static org.junit.Assert.*;

public class ReplacementResolutionTest {

    @Test
    public void rule() {
        System.out.println(ReplacementResolution.rule);
    }

    @Test
    public void testToString() {
        int[] bc1 = new int[]{1, ClauseType.OR.ordinal(),1,2,3,4};
        Clause c1 = new Clause(1,bc1);
        int[] bc2 = new int[]{2, ClauseType.OR.ordinal(),1,2,-3,4,5};
        Clause c2 = new Clause(2,bc2);
        int[] bc3 = new int[]{3, ClauseType.OR.ordinal(),1,2,4,5};
        Clause c3 = new Clause(3,bc3);
        ReplacementResolution rr = new ReplacementResolution(c1,c2, null,3,c3);
        System.out.println(rr.toString(null));
    }
}