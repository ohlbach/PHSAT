package Solvers.Resolution;

import Datastructures.Results.Unsatisfiable;
import InferenceSteps.InfInputClause;
import InferenceSteps.InferenceStep;
import junit.framework.TestCase;

public class EquivalencesTest extends TestCase {

    private boolean monitoring = true;
    public void testAdd1() throws Unsatisfiable {
        System.out.println("add1");
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
    public void testAddJoin() throws Unsatisfiable {
        System.out.println("add Join");
        InferenceStep step1 = new InfInputClause(1);
        InferenceStep step2 = new InfInputClause(2);
        Equivalences equivalences = new Equivalences();
        equivalences.add(10, 1, 2, step1);
        equivalences.add(10, 3, 2, step2);
        assertEquals("",equivalences.toString());

    }


    public void testApplyTrueLiteral() {
    }
}