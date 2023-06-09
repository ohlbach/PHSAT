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

public class EquivalencesTest extends TestCase {

    private boolean monitoring = true;
    public void testAdd1() throws Unsatisfiable {
        System.out.println("add 1");
        InferenceStep step1 = new InfInputClause(1);
        InferenceStep step2 = new InfInputClause(2);
        Equivalences equivalences = new Equivalences();
        equivalences.add(0,1,2,step1);
        assertEquals("1 == 2", equivalences.toString());
        equivalences.add(0,4,-3,step2);
        assertEquals("1 == 2\n" + "3 == -4", equivalences.toString());

        assertEquals(1,equivalences.getRepresentative(0,1));
        assertEquals(1,equivalences.getRepresentative(1,1));
        assertEquals(1,equivalences.getRepresentative(0,2));
        assertEquals(-1,equivalences.getRepresentative(0,-2));
        assertEquals(-3,equivalences.getRepresentative(0,-3));
        assertEquals(-3,equivalences.getRepresentative(0,4));
        assertEquals(-5,equivalences.getRepresentative(0,-5));

        equivalences.add(5,7,6,null);
        assertEquals(-6,equivalences.getRepresentative(5,-7));
    }
    public void testAdd2() throws Unsatisfiable {
        System.out.println("add 2");
        InferenceStep step1 = new InfInputClause(1);
        InferenceStep step2 = new InfInputClause(2);
        InferenceStep step3 = new InfInputClause(3);
        Equivalences equivalences = new Equivalences();
        equivalences.add(10, 1, 2, step1);
        equivalences.add(10, 3, 2, step2);
        assertEquals("10 -> 1 == 2 == 3",equivalences.toString());
        assertEquals(-1,equivalences.getRepresentative(10,-3));

        equivalences.add(10, 4, -2, step3);
        assertEquals("10 -> 1 == 2 == 3 == -4",equivalences.toString());
        assertEquals(-1,equivalences.getRepresentative(10,4));
        assertEquals("Input Clause Id: 1\n" +
                "Input Clause Id: 3",equivalences.getInferenceStep(10,-4).toString());

    }
    public void testAddJoin() throws Unsatisfiable {
        System.out.println("add Join");
        InferenceStep step1 = new InfInputClause(1);
        InferenceStep step2 = new InfInputClause(2);
        InferenceStep step3 = new InfInputClause(3);
        Equivalences equivalences = new Equivalences();
        equivalences.add(10, 1, 2, step1);

        equivalences.add(10, 5, 4, step2);
        equivalences.add(10, -5, 2, step3);
        assertEquals("10 -> 1 == 2 == -4 == -5",equivalences.toString());
        assertEquals(1, equivalences.getRepresentative(10,-5));
        assertEquals(-1, equivalences.getRepresentative(10,5));
        assertEquals("Input Clause Id: 3\n" +
                "Input Clause Id: 2",equivalences.getInferenceStep(10,-5).toString());
    }

    public void testAddJoin1() throws Unsatisfiable {
        System.out.println("add Join 1");
        InferenceStep step1 = new InfInputClause(1);
        InferenceStep step2 = new InfInputClause(2);
        InferenceStep step3 = new InfInputClause(3);
        InferenceStep step4 = new InfInputClause(4);
        InferenceStep step5 = new InfInputClause(5);
        Equivalences equivalences = new Equivalences();
        equivalences.add(10, 1, 2, step1);
        equivalences.add(10, 3, -2, step1);
        equivalences.add(10, 5, 4, step3);
        equivalences.add(10, 6, -5, step4);
        assertEquals("10 -> 1 == 2 == -3\n" +
                "10 -> 4 == 5 == -6",equivalences.toString());
        equivalences.add(10, 6, 2, step5);
        assertEquals("10 -> 1 == 2 == -3 == -4 == -5 == 6",equivalences.toString());

        assertEquals(1, equivalences.getRepresentative(10,-5));
        assertEquals(-1, equivalences.getRepresentative(10,5));
        assertEquals("Input Clause Id: 5\n" +
                "Input Clause Id: 3",equivalences.getInferenceStep(10,-5).toString());
    }

    public void testApplyTrueLiteral1() throws Unsatisfiable {
        System.out.println("applyTrueLiteral 1");
        IntArrayList trueLiterals = new IntArrayList();
        ArrayList<InferenceStep> inferenceSteps = new ArrayList<>();
        InferenceStep step1 = new InfInputClause(1);
        InferenceStep step2 = new InfInputClause(2);
        InferenceStep step3 = new InfInputClause(3);
        InferenceStep step4 = new InfInputClause(4);
        Equivalences equivalences = new Equivalences();
        equivalences.add(10, 1, 2, step1);
        equivalences.applyTrueLiteral(10,step2,(l,i) -> {trueLiterals.add(l);inferenceSteps.add(i);});
        assertEquals("1 == 2",equivalences.toString());
        assertTrue(trueLiterals.isEmpty());
        assertTrue(inferenceSteps.isEmpty());
        assertEquals("Input Clause Id: 1\n" +
                "Input Clause Id: 2",equivalences.getInferenceStep(0,2).toString());

        equivalences.add(11, 3, 4, step3);
        equivalences.applyTrueLiteral(11,step4,(l,i) -> {trueLiterals.add(l);inferenceSteps.add(i);});
        assertEquals("1 == 2\n" +
                "3 == 4",equivalences.toString());

        equivalences.add(12, 3, 4, step3);
        equivalences.applyTrueLiteral(-12,step4,(l,i) -> {trueLiterals.add(l);inferenceSteps.add(i);});
        assertEquals("1 == 2\n" +
                "3 == 4",equivalences.toString());}

    public void testApplyTrueLiteral2() throws Unsatisfiable {
        System.out.println("applyTrueLiteral 2");
        IntArrayList trueLiterals = new IntArrayList();
        ArrayList<InferenceStep> inferenceSteps = new ArrayList<>();
        InferenceStep step1 = new InfInputClause(1);
        InferenceStep step2 = new InfInputClause(2);
        InferenceStep step3 = new InfInputClause(3);
        Equivalences equivalences = new Equivalences();
        equivalences.add(10, 1, 2, step1);
        equivalences.add(10, 2, 3, step2);

        equivalences.applyTrueLiteral(1, step3, (l, i) -> {
            trueLiterals.add(l);
            inferenceSteps.add(i);});
        assertTrue(equivalences.isEmpty());
        assertEquals("[2, 3]",trueLiterals.toString());
        assertEquals(2,inferenceSteps.size());
        assertEquals("Input Clause Id: 3\n" +
                "Input Clause Id: 1",inferenceSteps.get(0).toString());
        assertEquals("Input Clause Id: 3\n" +
                "Input Clause Id: 1\n" +
                "Input Clause Id: 2",inferenceSteps.get(1).toString());
    }

    public void testApplyTrueLiteral3() throws Unsatisfiable {
        System.out.println("applyTrueLiteral 3");
        IntArrayList trueLiterals = new IntArrayList();
        ArrayList<InferenceStep> inferenceSteps = new ArrayList<>();
        InferenceStep step1 = new InfInputClause(1);
        InferenceStep step2 = new InfInputClause(2);
        InferenceStep step3 = new InfInputClause(3);
        Equivalences equivalences = new Equivalences();
        equivalences.add(10, 1, 2, step1);
        equivalences.add(10, 2, 3, step2);
        equivalences.applyTrueLiteral(2, step3, (l, i) -> {
            trueLiterals.add(l);
            inferenceSteps.add(i);});
        assertEquals("[1, 3]",trueLiterals.toString());
        assertEquals(2,inferenceSteps.size());
        assertEquals("Input Clause Id: 3\n" +
                "Input Clause Id: 1",inferenceSteps.get(0).toString());
        assertEquals("Input Clause Id: 3\n" +
                "Input Clause Id: 1\n" +
                "Input Clause Id: 2",inferenceSteps.get(1).toString());
    }

    public void testApplyTrueLiteral4() throws Unsatisfiable {
        System.out.println("applyTrueLiteral 4");
        IntArrayList trueLiterals = new IntArrayList();
        ArrayList<InferenceStep> inferenceSteps = new ArrayList<>();
        InferenceStep step1 = new InfInputClause(1);
        InferenceStep step2 = new InfInputClause(2);
        InferenceStep step3 = new InfInputClause(3);
        Equivalences equivalences = new Equivalences();
        equivalences.add(10, 1, 2, step1);
        equivalences.add(10, 2, 3, step2);
        equivalences.applyTrueLiteral(-2, step3, (l, i) -> {
            trueLiterals.add(l);
            inferenceSteps.add(i);});
        assertEquals("[-1, -3]",trueLiterals.toString());
        assertEquals(2,inferenceSteps.size());
        assertEquals("Input Clause Id: 3\n" +
                "Input Clause Id: 1",inferenceSteps.get(0).toString());
        assertEquals("Input Clause Id: 3\n" +
                "Input Clause Id: 1\n" +
                "Input Clause Id: 2",inferenceSteps.get(1).toString());
    }

    public void testCompleteModel() throws Unsatisfiable {
        System.out.println("completeModel");
        InferenceStep step1 = new InfInputClause(1);
        InferenceStep step2 = new InfInputClause(2);
        Equivalences equivalences = new Equivalences();
        equivalences.add(1, 2, 3, step1);
        equivalences.add(4, 5, 6, step2);

        int[] model = new int[]{0, 0, 0, 0, 0, 0, -1};
        IntFunction<Integer> modelStatus = (literal -> (literal > 0) ? model[literal] : -model[-literal]);
        IntConsumer makeTrue = (literal -> {
            if (literal > 0) model[literal] = 1;
            else model[-literal] = -1;});
        equivalences.completeModel(modelStatus, makeTrue);
        assertEquals("[0, 0, 1, 1, 0, -1, -1]", Arrays.toString(model));

        model[2] = -1;
        equivalences.completeModel(modelStatus, makeTrue);
        assertEquals("[0, -1, -1, 1, 0, -1, -1]", Arrays.toString(model));
        model[1] = 1;
        Unsatisfiable unsatisfiable = equivalences.completeModel(modelStatus, makeTrue);
        if(monitoring) System.out.println(unsatisfiable);
        }

    }


