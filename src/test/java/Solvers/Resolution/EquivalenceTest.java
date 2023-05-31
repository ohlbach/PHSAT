package Solvers.Resolution;

import Datastructures.Results.Unsatisfiable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import junit.framework.TestCase;

import java.util.ArrayList;

public class EquivalenceTest extends TestCase {

    private boolean monitoring = true;

    public void testConstructor() {
        System.out.println("constructor");
        InferenceStep step = new InfExternal(10);
        Equivalence equivalence = new Equivalence(1,2,3,step);
        assertEquals("1 -> 2 == 3", equivalence.toString());
        assertEquals(1,equivalence.triggerLiteral);
        assertEquals(2,equivalence.representative);
        assertEquals(step,equivalence.getInferenceStep(3));
        assertEquals(step,equivalence.getInferenceStep(-3));
        assertNull(equivalence.getInferenceStep(2));
        assertNull(equivalence.getInferenceStep(10));

        equivalence = new Equivalence(0,2,3,step);
        assertEquals("2 == 3", equivalence.toString());
        assertEquals(step,equivalence.getInferenceStep(3));
    }
    public void testAdd() throws Unsatisfiable {
        System.out.println("add");
        InferenceStep step3 = new InfExternal(3);
        InferenceStep step4 = new InfExternal(4);
        InferenceStep stepM3 = new InfExternal(-3);
        Equivalence equivalence = new Equivalence(1,2,3,step3);
        equivalence.add(-4,step4);
        assertEquals("1 -> 2 == 3 == -4", equivalence.toString());
        assertEquals(step3,equivalence.getInferenceStep(3));
        assertEquals(step4,equivalence.getInferenceStep(4));
        equivalence.add(3,step3);
        assertEquals("1 -> 2 == 3 == -4", equivalence.toString());
        try{equivalence.add(-3,stepM3);}
        catch(UnsatEquivalence uns) {
            uns.solverId = "EQ";
            uns.problemId ="Test";
            if(monitoring) {
                System.out.println(uns.description(null));
                for(InferenceStep step : uns.inferenceSteps) System.out.println(step);}}
    }

    public void testGetRepresentative() throws Unsatisfiable {
        System.out.println("getRepresentative");
        InferenceStep step3 = new InfExternal(3);
        InferenceStep step4 = new InfExternal(4);
        Equivalence equivalence = new Equivalence(1,2,3,step3);
        equivalence.add(-4,step4);
        assertEquals(2, equivalence.getRepresentative(2));
        assertEquals(-2, equivalence.getRepresentative(-2));
        assertEquals(2, equivalence.getRepresentative(3));
        assertEquals(-2, equivalence.getRepresentative(-3));
        assertEquals(-2, equivalence.getRepresentative(4));
        assertEquals(2, equivalence.getRepresentative(-4));
        assertEquals(1, equivalence.getRepresentative(1));
        assertEquals(-1, equivalence.getRepresentative(-1));
    }


    public void testJoin() throws Unsatisfiable {
        System.out.println("join");
        InferenceStep step3 = new InfExternal(3);
        InferenceStep step4 = new InfExternal(4);
        Equivalence equivalence1 = new Equivalence(1,2,3,step3);
        equivalence1.add(-4,step4);

        InferenceStep step6 = new InfExternal(6);
        InferenceStep step7 = new InfExternal(7);
        InferenceStep stepJ = new InfExternal(10);
        Equivalence equivalence2 = new Equivalence(1,5,6,step6);
        equivalence2.add(-7,step7);
        equivalence1.join(-1,equivalence2,stepJ);
        assertEquals("1 -> 2 == 3 == -4 == -5 == -6 == 7", equivalence1.toString());
        assertEquals(-2, equivalence1.getRepresentative(5));
        assertEquals(2, equivalence1.getRepresentative(7));
// inferences ändern
    }

    public void testApplyTrueLiteral() throws Unsatisfiable {
        System.out.println("applyTrueLiteral");
        IntArrayList trueLiterals = new IntArrayList();
        ArrayList<InferenceStep> steps = new ArrayList<>();
        InferenceStep step3 = new InfExternal(3);
        InferenceStep step4 = new InfExternal(4);
        Equivalence equivalence = new Equivalence(1,2,3,step3);
        equivalence.add(-4,step4);
        assertEquals("1 -> 2 == 3 == -4", equivalence.toString());
        assertFalse(equivalence.applyTrueLiteral(1,(l,s1,s2)-> {trueLiterals.add(l); steps.add(s1); steps.add(s2);}));
        assertTrue(trueLiterals.isEmpty());
        assertTrue(steps.isEmpty());

        assertTrue(equivalence.applyTrueLiteral(2,(l,s1,s2)-> {trueLiterals.add(l); steps.add(s1); steps.add(s2);}));
        assertEquals("[3, -4]",trueLiterals.toString());
        assertEquals(4,steps.size());

        trueLiterals.clear(); steps.clear();
        assertTrue(equivalence.applyTrueLiteral(-2,(l,s1,s2)-> {trueLiterals.add(l); steps.add(s1); steps.add(s2);}));
        assertEquals("[-3, 4]",trueLiterals.toString());
        assertEquals(4,steps.size());

        trueLiterals.clear(); steps.clear();
        assertTrue(equivalence.applyTrueLiteral(3,(l,s1,s2)-> {trueLiterals.add(l); steps.add(s1); steps.add(s2);}));
        assertEquals("[2, -4]",trueLiterals.toString());
        assertEquals(4,steps.size());

        trueLiterals.clear(); steps.clear();
        assertTrue(equivalence.applyTrueLiteral(-3,(l,s1,s2)-> {trueLiterals.add(l); steps.add(s1); steps.add(s2);}));
        assertEquals("[-2, 4]",trueLiterals.toString());
        assertEquals(4,steps.size());
    }

    public void testContains() throws Unsatisfiable {
        System.out.println("contains");
        InferenceStep step3 = new InfExternal(3);
        InferenceStep step4 = new InfExternal(4);
        Equivalence equivalence = new Equivalence(1,2,3,step3);
        equivalence.add(-4,step4);
        assertFalse(equivalence.contains(1));
        assertTrue(equivalence.contains(2));
        assertTrue(equivalence.contains(-2));
        assertTrue(equivalence.contains(4));
        assertTrue(equivalence.contains(-4));
    }
}