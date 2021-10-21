package InferenceSteps;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseType;
import org.junit.Test;

import static org.junit.Assert.*;

public class DisjointnessClause2ClauseTest {

    @Test
    public void rule() {
        System.out.println(DisjointnessClause2Clause.rule);
    }
/*
    @Test
    public void inference1() {
        System.out.println("basic clause");
        int[] bc = new int[]{1, ClauseType.DISJOINT.ordinal(),1,2,3,4};
        Clause c = new Clause(2,ClauseType.DISJOINT,-2,-4);
        DisjointnessClause2Clause inf = new DisjointnessClause2Clause(bc,c);
        System.out.println(inf.toString(null));
    }

    @Test
    public void inference2() {
        System.out.println("disjointness clause");

        int[] bc = new int[]{1, ClauseType.DISJOINT.ordinal(),1,2,3,4};
        Clause dc = new Clause(2,bc);
        Clause c = new Clause(3,ClauseType.DISJOINT,-2,-4);
        DisjointnessClause2Clause inf = new DisjointnessClause2Clause(dc,c);
        System.out.println(inf.toString(null));
    }
*/
}