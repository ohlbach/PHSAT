package InferenceSteps;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseType;
import org.junit.Test;

import static org.junit.Assert.*;

public class EquivalenceJoiningTest {

    @Test
    public void rule() {
        System.out.println(EquivalenceJoining.rule);
    }

    @Test
    public void testToString() {
        int[] bc1 = new int[]{1, ClauseType.EQUIV.ordinal(),1,2,3};
        Clause ec1 = new Clause(2,bc1);
        int[] bc2 = new int[]{3, ClauseType.EQUIV.ordinal(),3,4,5};
        Clause ec2 = new Clause(4,bc2);
        int[] bc3 = new int[]{4, ClauseType.EQUIV.ordinal(),1,2,3,4,5};
        Clause ec3 = new Clause(5,bc3);
        EquivalenceJoining eqj = new EquivalenceJoining(ec1,ec2,3,ec3);
        System.out.println(eqj.toString());
    }
}