package InferenceSteps;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.Connective;
import org.junit.Test;

public class EquivalentTrueLiteralTest {

    @Test
    public void rule() {
        System.out.println(EquivalentTrueLiteral.rule);
    }

    @Test
    public void testToString() {
        int[] bc = new int[]{1, Connective.EQUIV.ordinal(),1,2,3,4};
        Clause ec = new Clause(2,bc);
        EquivalentTrueLiteral eq = new EquivalentTrueLiteral(ec,2,3,null);
        System.out.println(eq.toString(null));
    }
}