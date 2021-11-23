package InferenceSteps;

import Datastructures.Clauses.ClauseOld;
import Datastructures.Clauses.Connective;
import org.junit.Test;

public class ReplacementResolutionTest {

    @Test
    public void rule() {
        System.out.println(ReplacementResolution.rule);
    }

    @Test
    public void testToString() {
        int[] bc1 = new int[]{1, Connective.OR.ordinal(),1,2,3,4};
        ClauseOld c1 = new ClauseOld(1,bc1);
        int[] bc2 = new int[]{2, Connective.OR.ordinal(),1,2,-3,4,5};
        ClauseOld c2 = new ClauseOld(2,bc2);
        int[] bc3 = new int[]{3, Connective.OR.ordinal(),1,2,4,5};
        ClauseOld c3 = new ClauseOld(3,bc3);
        ReplacementResolution rr = new ReplacementResolution(c1,c2, null,3,c3);
        System.out.println(rr.toString(null));
    }
}