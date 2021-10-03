package InferenceSteps;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseType;
import org.junit.Test;

import static org.junit.Assert.*;

public class EquivalenceInconsistencyTest {

    @Test
    public void rule() {
        System.out.println(EquivalenceInconsistency.rule);
    }

    @Test
    public void testToString() {
        int[] bc = new int[]{1, ClauseType.EQUIV.ordinal(),1,2,3,-2};
        Clause ec = new Clause(2,bc);
        EquivalenceInconsistency eqi = new EquivalenceInconsistency(ec,2,-2);
        System.out.println(eqi.toString());

    }
}