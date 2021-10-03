package InferenceSteps;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseType;
import org.junit.Test;

import static org.junit.Assert.*;

public class EquivalentTrueLiteralTest {

    @Test
    public void rule() {
        System.out.println(EquivalentTrueLiteral.rule);
    }

    @Test
    public void testToString() {
        int[] bc = new int[]{1, ClauseType.EQUIV.ordinal(),1,2,3,4};
        Clause ec = new Clause(2,bc);
        EquivalentTrueLiteral eq = new EquivalentTrueLiteral(ec,2,3,null);
        System.out.println(eq.toString(null));
    }
}