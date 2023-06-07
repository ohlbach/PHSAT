package Solvers.Resolution;

import Datastructures.Results.Unsatisfiable;
import InferenceSteps.InfInputClause;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import junit.framework.TestCase;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.function.IntConsumer;
import java.util.function.IntFunction;

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
        InferenceStep step = new InfExternal(1);
        ArrayList<InferenceStep> steps = new ArrayList<>();
        InferenceStep step3 = new InfExternal(3);
        InferenceStep step4 = new InfExternal(4);
        Equivalence equivalence = new Equivalence(1,2,3,step3);
        equivalence.add(-4,step4);
        assertEquals("1 -> 2 == 3 == -4", equivalence.toString());
        assertFalse(equivalence.applyTrueLiteral(1,step,(l,s1)-> {trueLiterals.add(l); steps.add(s1);}));
        assertTrue(trueLiterals.isEmpty());
        assertTrue(steps.isEmpty());

        assertTrue(equivalence.applyTrueLiteral(2,step,(l,s1)-> {trueLiterals.add(l); steps.add(s1);}));
        assertEquals("[3, -4]",trueLiterals.toString());
        assertEquals(2,steps.size());

        trueLiterals.clear(); steps.clear();
        assertTrue(equivalence.applyTrueLiteral(-2,step,(l,s1)-> {trueLiterals.add(l); steps.add(s1);}));
        assertEquals("[-3, 4]",trueLiterals.toString());
        assertEquals(2,steps.size());

        trueLiterals.clear(); steps.clear();
        assertTrue(equivalence.applyTrueLiteral(3,step,(l,s1)-> {trueLiterals.add(l); steps.add(s1);}));
        assertEquals("[2, -4]",trueLiterals.toString());
        assertEquals(2,steps.size());

        trueLiterals.clear(); steps.clear();
        assertTrue(equivalence.applyTrueLiteral(-3,step,(l,s1)-> {trueLiterals.add(l); steps.add(s1);}));
        assertEquals("[-2, 4]",trueLiterals.toString());
        assertEquals(2,steps.size());
        if(monitoring) System.out.println(steps.get(1).toString(null));
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

    public void testFindInconsistency() throws Unsatisfiable{
        System.out.println("findInconsistency");
        InferenceStep step1 = new InfInputClause(10);
        InferenceStep step2 = new InfInputClause(11);
        Equivalence equivalence = new Equivalence(0,1,2,step1);
        equivalence.add(3,step2);
        int[] model = new int[]{0,0,1,0};
        IntFunction<Integer> modelStatus = (literal-> model[literal]);
        try{equivalence.findInconsistency(modelStatus);}
        catch(Unsatisfiable uns) {assertTrue(false);}
        model[3] = -1;
        try{equivalence.findInconsistency(modelStatus);
            assertTrue(false);}
        catch(Unsatisfiable uns) {
            if(monitoring) System.out.println(uns.toString());}
        model[1] = 1; model[2] = 0;
        try{equivalence.findInconsistency(modelStatus);
            assertTrue(false);}
        catch(Unsatisfiable uns) {
            if(monitoring) System.out.println(uns.toString());}}

    public void testLiteralsAreInconsistent() throws Unsatisfiable {
        System.out.println("literalsAreInconsistent");
        InferenceStep step1 = new InfInputClause(10);
        InferenceStep step2 = new InfInputClause(11);
        Equivalence equivalence = new Equivalence(0, 1, 2, step1);
        int[] model = new int[]{0, 0, 0, 0};
        IntFunction<Integer> modelStatus = (literal -> model[literal]);
        assertFalse(equivalence.literalsAreInconsistent(modelStatus));
        model[1] = 1;
        assertFalse(equivalence.literalsAreInconsistent(modelStatus));
        model[2] = 1;
        assertFalse(equivalence.literalsAreInconsistent(modelStatus));
        model[2] = -1;
        assertTrue(equivalence.literalsAreInconsistent(modelStatus));

        equivalence.add(3, step2);
        model[2] = 0;
        model[3] = 0;
        assertFalse(equivalence.literalsAreInconsistent(modelStatus));
        model[3] = 1;
        assertFalse(equivalence.literalsAreInconsistent(modelStatus));
        model[3] = -1;
        assertTrue(equivalence.literalsAreInconsistent(modelStatus));
        model[1] = 0; model[2] = 1;
        assertTrue(equivalence.literalsAreInconsistent(modelStatus));
    }
    public void testCompleteModelForLiterals() throws Unsatisfiable {
        System.out.println("completeModelForLiterals");
        InferenceStep step1 = new InfInputClause(10);
        InferenceStep step2 = new InfInputClause(11);
        Equivalence equivalence = new Equivalence(0, 1, 2, step1);
        int[] model = new int[]{0, 0, 0, 0};
        IntFunction<Integer> modelStatus = (literal -> model[literal]);
        IntConsumer makeTrue = (literal -> {if(literal > 0) model[literal] = 1; else model[-literal] = -1;});

        equivalence.completeModelForLiterals(modelStatus,makeTrue);
        assertEquals("[0, 1, 1, 0]", Arrays.toString(model));
        model[1] = 0;
        equivalence.completeModelForLiterals(modelStatus,makeTrue);
        assertEquals("[0, 1, 1, 0]", Arrays.toString(model));
        equivalence.add(3, step2);
        model[1] = 0; model[2] = -1;
        equivalence.completeModelForLiterals(modelStatus,makeTrue);
        assertEquals("[0, -1, -1, -1]", Arrays.toString(model));
    }
    public void testCompleteModel() throws Unsatisfiable {
        System.out.println("completeModel");
        InferenceStep step1 = new InfInputClause(10);
        InferenceStep step2 = new InfInputClause(11);
        Equivalence equivalence = new Equivalence(1, 2, 3, step1);
        int[] model = new int[]{0, 0, 0, 0, 0};
        IntFunction<Integer> modelStatus = (literal -> (literal > 0) ? model[literal] : -model[-literal]);
        IntConsumer makeTrue = (literal -> {
            if (literal > 0) model[literal] = 1;
            else model[-literal] = -1;});
        equivalence.completeModel(modelStatus, makeTrue);
        assertEquals("[0, 0, 1, 1, 0]", Arrays.toString(model));
        model[3] = -1;
        equivalence.completeModel(modelStatus, makeTrue);
        assertEquals("[0, -1, 1, -1, 0]", Arrays.toString(model));
        model[1] = 1; model[2] = 0;
        equivalence.completeModel(modelStatus, makeTrue);
        assertEquals("[0, 1, -1, -1, 0]", Arrays.toString(model));
        equivalence.add(-4, step2);
        equivalence.completeModel(modelStatus, makeTrue);
        assertEquals("[0, 1, -1, -1, 1]", Arrays.toString(model));

        model[4] = -1;
        try{equivalence.completeModel(modelStatus, makeTrue);
            assertTrue(false);}
        catch(Unsatisfiable uns) {
            if(monitoring) System.out.println(uns.toString());}}
}