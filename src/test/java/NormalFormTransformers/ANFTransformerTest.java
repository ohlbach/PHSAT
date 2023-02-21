package NormalFormTransformers;

import Datastructures.Clauses.Connective;
import InferenceSteps.InferenceStep;
import junit.framework.TestCase;

import java.util.Arrays;

public class ANFTransformerTest extends TestCase {
    private final static int cAtmost = Connective.ATMOST.ordinal();
    private final static int cExactly = Connective.EXACTLY.ordinal();
    private final static int cInterval = Connective.INTERVAL.ordinal();


    public void testAtmost2Atleast() {
        System.out.println("atmost2Atleast");
        int[] ids = new int[]{9};
        ANFTransformer anf = new ANFTransformer(() -> ++ids[0]);
        int[] clause = new int[]{5,cAtmost,2,1,2,3,4,5};
        int[] atleastClause = anf.atmost2Atleast(clause);
        assertEquals("[10, 3, 3, -1, -2, -3, -4, -5]", Arrays.toString(atleastClause));
        InferenceStep step = anf.infAtmost2Atleast(clause,atleastClause);
        //System.out.println(step.toString());
        //System.out.println(step.rule());
        assertEquals("[5]",step.inputClauseIds().toString());
    }


    public void testExactly2Atleast() {
        System.out.println("exactly2Atleast");
        int[] ids = new int[]{9};
        ANFTransformer anf = new ANFTransformer(() -> ++ids[0]);
        int[] clause = new int[]{5,cExactly,2,1,2,3,4,5};
        int[][] exactlyClauses = new int[2][];
        anf.exactly2Atleast(clause,exactlyClauses);
        assertEquals("[10, 3, 2, 1, 2, 3, 4, 5]", Arrays.toString(exactlyClauses[0]));
        assertEquals("[11, 3, 3, -1, -2, -3, -4, -5]", Arrays.toString(exactlyClauses[1]));
        InferenceStep[] steps = new InferenceStep[2];
        anf.infExactly2Atleast(clause,exactlyClauses, steps);
        //System.out.println(steps[0].toString());
        //System.out.println(steps[1].toString());
        //System.out.println(steps[0].rule());
        assertEquals("[5]",steps[0].inputClauseIds().toString());
    }

    public void testInterval2Atleast() {
        System.out.println("interval2Atleast");
        int[] ids = new int[]{9};
        ANFTransformer anf = new ANFTransformer(() -> ++ids[0]);
        int[] clause = new int[]{5,cInterval,2,3,1,2,3,4,5};
        int[][] intervalClauses = new int[2][];
        anf.interval2Atleast(clause,intervalClauses);
        assertEquals("[10, 3, 2, 1, 2, 3, 4, 5]", Arrays.toString(intervalClauses[0]));
        assertEquals("[11, 3, 2, -1, -2, -3, -4, -5]", Arrays.toString(intervalClauses[1]));
        InferenceStep[] steps = new InferenceStep[2];
        anf.infInterval2Atleast(clause,intervalClauses, steps);
        //System.out.println(steps[0].toString());
        //System.out.println(steps[1].toString());
        //System.out.println(steps[0].rule());
        assertEquals("[5]",steps[0].inputClauseIds().toString());
    }

}