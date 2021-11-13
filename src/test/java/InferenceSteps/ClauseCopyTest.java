package InferenceSteps;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.Connective;
import org.junit.Test;

public class ClauseCopyTest {

    @Test
    public void rule() {
        System.out.println(ClauseCopy.rule);
    }

    @Test
    public void testToString() {
        System.out.println("basic clause");
        int[] bc = new int[]{1, Connective.OR.ordinal(),1,2,3,4};
        Clause c = new Clause(2,bc);
        ClauseCopy inf = new ClauseCopy(bc,c);
        System.out.println(inf.toString(null));

    }
}