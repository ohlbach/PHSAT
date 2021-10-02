package InferenceSteps;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseType;
import org.junit.Test;

import static org.junit.Assert.*;

public class ClauseCopyTest {

    @Test
    public void rule() {
        System.out.println(ClauseCopy.rule);
    }

    @Test
    public void testToString() {
        System.out.println("basic clause");
        int[] bc = new int[]{1, ClauseType.OR.ordinal(),1,2,3,4};
        Clause c = new Clause(2,bc);
        ClauseCopy inf = new ClauseCopy(bc,c);
        assertEquals(bc,inf.input());
        assertEquals(c,inf.output());
        System.out.println(inf.toString(null));

    }
}