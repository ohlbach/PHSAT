package InferenceSteps;

import Datastructures.Clauses.ClauseOld;
import Datastructures.Clauses.Connective;
import org.junit.Test;

public class EquivalenceJoiningTest {

    @Test
    public void rule() {
        System.out.println(EquivalenceJoining.rule);
    }

    @Test
    public void testToString() {
        int[] bc1 = new int[]{1, Connective.EQUIV.ordinal(),1,2,3};
        ClauseOld ec1 = new ClauseOld(2,bc1);
        int[] bc2 = new int[]{3, Connective.EQUIV.ordinal(),3,4,5};
        ClauseOld ec2 = new ClauseOld(4,bc2);
        int[] bc3 = new int[]{4, Connective.EQUIV.ordinal(),1,2,3,4,5};
        ClauseOld ec3 = new ClauseOld(5,bc3);
        EquivalenceJoining eqj = new EquivalenceJoining(ec1,ec2,3,ec3);
        System.out.println(eqj.toString());
    }
}