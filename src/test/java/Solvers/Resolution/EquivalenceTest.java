package Solvers.Resolution;

import Datastructures.Results.Unsatisfiable;
import InferenceSteps.InfInputClause;
import InferenceSteps.InferenceStep;
import Utilities.IntToByteFunction;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import junit.framework.TestCase;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.function.IntConsumer;

public class EquivalenceTest extends TestCase {

    private boolean monitoring = true;

    public void testConstructor() {
        System.out.println("constructor");
        InferenceStep step = new InfExternal(10);
        Equivalence equivalence = new Equivalence(1,2,step);
        assertEquals("1 == 2", equivalence.toString());
        assertEquals(1,equivalence.representative);
        assertEquals(2,equivalence.literal);
        assertEquals(1,equivalence.getRepresentative(1));
        assertEquals(-1,equivalence.getRepresentative(-1));
        assertEquals(1,equivalence.getRepresentative(2));
        assertEquals(-1,equivalence.getRepresentative(-2));
        assertEquals(3,equivalence.getRepresentative(3));
        assertEquals(-3,equivalence.getRepresentative(-3));

        assertEquals(step,equivalence.inferenceStep);

        equivalence = new Equivalence(-2,1,step);
        assertEquals("1 == -2", equivalence.toString());
        assertEquals(-1,equivalence.getRepresentative(2));
        assertEquals(1,equivalence.getRepresentative(-2));

    }


    public void testApplyTrueLiteral() throws Unsatisfiable {
        System.out.println("applyTrueLiteral");
        IntArrayList trueLiterals = new IntArrayList();
        InferenceStep step = new InfExternal(1);
        ArrayList<InferenceStep> steps = new ArrayList<>();
        Equivalence equivalence = new Equivalence(1,2,step);

        assertFalse(equivalence.applyTrueLiteral((l-> (byte)0),
                step,(l,s1)-> {trueLiterals.add(l); steps.add(s1);}));
        assertTrue(trueLiterals.isEmpty());
        assertTrue(steps.isEmpty());

        assertTrue(equivalence.applyTrueLiteral((l-> (l != 1) ? 0 : (byte)l),
                step,(l,s1)-> {trueLiterals.add(l); steps.add(s1);}));
        assertEquals("[2]",trueLiterals.toString());
        assertEquals(1,steps.size());

        trueLiterals.clear();steps.clear();
        assertTrue(equivalence.applyTrueLiteral((l-> (l != 2) ? 0 : (byte)1),
                step,(l,s1)-> {trueLiterals.add(l); steps.add(s1);}));
        assertEquals("[1]",trueLiterals.toString());
        assertEquals(1,steps.size());

        trueLiterals.clear();steps.clear();
        assertTrue(equivalence.applyTrueLiteral((l-> (l != 2) ? 0 : (byte)-1),
                step,(l,s1)-> {trueLiterals.add(l); steps.add(s1);}));
        assertEquals("[-1]",trueLiterals.toString());
        assertEquals(1,steps.size());

        trueLiterals.clear();steps.clear();
        assertTrue(equivalence.applyTrueLiteral((l-> (byte)-1),
                step,(l,s1)-> {trueLiterals.add(l); steps.add(s1);}));
        assertEquals("[]",trueLiterals.toString());
        assertEquals(0,steps.size());
    }


    public void testCompleteModel() throws Unsatisfiable {
        System.out.println("completeModel");
        InferenceStep step1 = new InfInputClause(10);
        InferenceStep step2 = new InfInputClause(11);
        Equivalence equivalence = new Equivalence(1, 2,  step1);
        byte[] model = new byte[]{0, 0, 0, 0};
        IntToByteFunction modelStatus = (literal -> model[literal]);
        IntConsumer makeTrue = (literal -> {if(literal > 0) model[literal] = 1; else model[-literal] = -1;});

        equivalence.completeModel(modelStatus,makeTrue);
        assertEquals("[0, 1, 1, 0]", Arrays.toString(model));
        model[1] = 0;
        equivalence.completeModel(modelStatus,makeTrue);
        assertEquals("[0, 1, 1, 0]", Arrays.toString(model));

        model[1] = 0; model[2] = -1;
        equivalence.completeModel(modelStatus,makeTrue);
        assertEquals("[0, -1, -1, 0]", Arrays.toString(model));
    }
}