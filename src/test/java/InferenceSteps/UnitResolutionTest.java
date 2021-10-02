package InferenceSteps;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseType;
import org.junit.Test;

import static org.junit.Assert.*;

public class UnitResolutionTest {

    @Test
    public void rule() {
        System.out.println(UnitResolution.rule);
    }

    @Test
    public void testToString() {
        int[] bc1 = new int[]{1, ClauseType.OR.ordinal(),1,2,3,4};
        Clause c1 = new Clause(2,bc1);

        int[] bc2 = new int[]{3, ClauseType.OR.ordinal(),1,2,4};
        Clause c2 = new Clause(4,bc2);

        UnitResolution inf = new UnitResolution(c1,-3,c2,null);

        System.out.println(inf.toString(null));

    }
}