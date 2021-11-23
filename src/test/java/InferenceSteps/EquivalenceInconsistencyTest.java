package InferenceSteps;

import Datastructures.Clauses.ClauseOld;
import Datastructures.Clauses.Connective;
import org.junit.Test;

public class EquivalenceInconsistencyTest {

    @Test
    public void rule() {
        System.out.println(EquivalenceInconsistency.rule);
    }

    @Test
    public void testToString() {
        int[] bc = new int[]{1, Connective.EQUIV.ordinal(),1,2,3,-2};
        ClauseOld ec = new ClauseOld(2,bc);
        EquivalenceInconsistency eqi = new EquivalenceInconsistency(ec,2,-2);
        System.out.println(eqi.toString());

    }
}