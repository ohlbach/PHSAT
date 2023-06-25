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

public class EquivalencesTest extends TestCase {

    private boolean monitoring = true;

    public void testAdd() throws Unsatisfiable {
        System.out.println("add");
        InferenceStep step1 = new InfInputClause(1);
        InferenceStep step2 = new InfInputClause(2);
        Equivalences equivalences = new Equivalences();
        equivalences.add(new Equivalence(1,2,step1));
        assertEquals("Equivalences:\n" +
                "1 == 2", equivalences.toString());
        equivalences.add(new Equivalence(4,-3,step2));
        assertEquals("Equivalences:\n" +
                "1 == 2\n" + "3 == -4", equivalences.toString());
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
        equivalences.add(new Equivalence(1, 2, step1));
        equivalences.applyTrueLiteral((l -> (byte) 0), step2, (l, i) -> {
            trueLiterals.add(l);
            inferenceSteps.add(i);
        });
        assertEquals("Equivalences:\n" +
                "1 == 2", equivalences.toString());
        assertTrue(trueLiterals.isEmpty());
        assertTrue(inferenceSteps.isEmpty());
        equivalences.add(new Equivalence(3, 4, step3));
        assertEquals("Equivalences:\n" +
                "1 == 2\n" +
                "3 == 4", equivalences.toString());
        equivalences.applyTrueLiteral((l -> (l == 3) ? (byte) 1 : 0), step4, (l, i) -> {
            trueLiterals.add(l);
            inferenceSteps.add(i);
        });
        assertEquals("Equivalences:\n1 == 2", equivalences.toString());
        assertEquals("[4]", trueLiterals.toString());
        assertEquals("Input Clause Id: 3\n" +
                "Input Clause Id: 4", inferenceSteps.get(0).toString());
    }


    public void testCompleteModel() throws Unsatisfiable {
        System.out.println("completeModel");
        InferenceStep step1 = new InfInputClause(1);
        InferenceStep step2 = new InfInputClause(2);
        Equivalences equivalences = new Equivalences();
        equivalences.add(new Equivalence(2, 3, step1));
        equivalences.add(new Equivalence(5, 6, step2));

        byte[] model = new byte[]{0, 0, 0, 0, 0, 0, -1};
        IntToByteFunction modelStatus = (literal -> (literal > 0) ? model[literal] : (byte)-model[-literal]);
        IntConsumer makeTrue = (literal -> {
            if (literal > 0) model[literal] = 1;
            else model[-literal] = -1;});
        equivalences.completeModel(modelStatus, makeTrue);
        assertEquals("[0, 0, 1, 1, 0, -1, -1]", Arrays.toString(model));

        model[2] = -1;model[3] = 0;
        equivalences.completeModel(modelStatus, makeTrue);
        assertEquals("[0, 0, -1, -1, 0, -1, -1]", Arrays.toString(model));
        model[2] = 1;
        Unsatisfiable unsatisfiable = equivalences.completeModel(modelStatus, makeTrue);
        if(monitoring) System.out.println(unsatisfiable);
        }

    }


