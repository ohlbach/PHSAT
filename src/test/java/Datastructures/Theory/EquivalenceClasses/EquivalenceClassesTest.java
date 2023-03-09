package Datastructures.Theory.EquivalenceClasses;

import Datastructures.Clauses.Connective;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import InferenceSteps.InferenceTest;
import junit.framework.TestCase;

import java.util.ArrayList;

public class EquivalenceClassesTest extends TestCase {
    static Symboltable symboltable= new Symboltable(10);
    static int eqv = Connective.EQUIV.ordinal();
    static {
        symboltable.setName(1,"p");
        symboltable.setName(2,"q");
        symboltable.setName(3,"r");
        symboltable.setName(4,"a");
        symboltable.setName(5,"b");
        symboltable.setName(6,"c");
        symboltable.setName(7,"d");
        symboltable.setName(8,"e");
        symboltable.setName(9,"f");}

    public void testIntegrateEQUIVClauses() throws Unsatisfiable {
        System.out.println("IntegrateEQUIVClauses");
        int[] clause1 = new int[]{10,eqv,1,2,3};
        ArrayList<int[]> clauses = new ArrayList<>(); clauses.add(clause1);
        EquivalenceClasses eqc = new EquivalenceClasses(null,null);
        eqc.integrateEQUIVClauses(clauses);
        assertEquals("Equivalence Classes of Problem TestProblem:\n" +
                "1 = 2 = 3\n",eqc.toString());
        assertEquals("Equivalence Classes of Problem TestProblem:\n" +
                "p = q = r\n",eqc.toString(symboltable));

        eqc = new EquivalenceClasses(null,null);
        int[] clause2 = new int[]{11,eqv,3,4,5};
        clauses.add(clause2);
        eqc.integrateEQUIVClauses(clauses);
        assertEquals("Equivalence Classes of Problem TestProblem:\n" +
                "1 = 2 = 3 = 4 = 5\n",eqc.toString());


        eqc = new EquivalenceClasses(null,null);
        int[] clause3 = new int[]{13,eqv,3,-5,6};
        clauses.add(clause3);
        try{
            eqc.integrateEQUIVClauses(clauses);
            assertTrue(false);}
        catch(Unsatisfiable unsatisfiable) {
           // System.out.println(unsatisfiable.toString());
        }

        eqc = new EquivalenceClasses(null,null);
        int[] clause4 = new int[]{12,eqv,3,3};
        ArrayList<int[]> clauses1 = new ArrayList<>(); clauses1.add(clause4);
        eqc.integrateEQUIVClauses(clauses1);
        assertTrue(eqc.isEmpty());

    }


    public void testGetRepresentative() throws Unsatisfiable{
        System.out.println("getRepresentative");
        int[] clause1 = new int[]{10,eqv,3,2,1};
        int[] clause2 = new int[]{11,eqv,6,-5,-4};
        ArrayList<int[]> clauses = new ArrayList<>(); clauses.add(clause1);clauses.add(clause2);
        EquivalenceClasses eqc = new EquivalenceClasses(null,null);
        eqc.integrateEQUIVClauses(clauses);
        assertEquals(1,eqc.getRepresentative(1));
        assertEquals(-1,eqc.getRepresentative(-1));

        assertEquals(1,eqc.getRepresentative(2));
        assertEquals(-1,eqc.getRepresentative(-2));

        assertEquals(1,eqc.getRepresentative(3));
        assertEquals(-1,eqc.getRepresentative(-3));

        assertEquals(4,eqc.getRepresentative(4));
        assertEquals(-4,eqc.getRepresentative(-4));

        assertEquals(4,eqc.getRepresentative(5));
        assertEquals(-4,eqc.getRepresentative(-5));

        assertEquals(-4,eqc.getRepresentative(6));
        assertEquals(4,eqc.getRepresentative(-6));

    }

    public void testApplyTrueLiteral() throws Unsatisfiable {
        System.out.println("applyTrueLiteral");
        int[] clause1 = new int[]{10,eqv,1,2,3};
        int[] clause2 = new int[]{10,eqv,6,5,4};
        ArrayList<int[]> clauses = new ArrayList<>(); clauses.add(clause1);clauses.add(clause2);
        EquivalenceClasses eqc = new EquivalenceClasses(null,null);
        eqc.integrateEQUIVClauses(clauses);
        eqc.processTrueLiteral(-5,new InferenceTest("Apply 5"));
        assertEquals("-4,-6",eqc.model.toString());
        assertEquals(1,eqc.size());
 }

    public void testApplyEquivalence() throws Unsatisfiable {
        System.out.println("applyEquivalence");
        int[] clause1 = new int[]{10,eqv,1,2,3};
        int[] clause2 = new int[]{11,eqv,6,5,4};
        ArrayList<int[]> clauses = new ArrayList<>(); clauses.add(clause1);clauses.add(clause2);
        EquivalenceClasses eqc = new EquivalenceClasses(null,null);
        eqc.integrateEQUIVClauses(clauses);
        eqc.processEquivalence(3,7,new InferenceTest("Test 37"));
        assertEquals("Equivalence Classes of Problem TestProblem:\n" +
                "1 = 2 = 3 = 7\n" +
                "4 = 6 = 5\n",eqc.toString());

        eqc.processEquivalence(-5,9,new InferenceTest("Test -59"));
        assertEquals("Equivalence Classes of Problem TestProblem:\n" +
                "1 = 2 = 3 = 7\n" +
                "4 = 6 = 5 = -9\n",eqc.toString());

        eqc.processEquivalence(-7,9,new InferenceTest("Test -79"));
        assertEquals("Equivalence Classes of Problem TestProblem:\n" +
                "1 = 2 = 3 = 7 = 4 = 6 = 5 = -9\n",eqc.toString());

        eqc.processEquivalence(12,-11,new InferenceTest("Test 12-11"));
        assertEquals("Equivalence Classes of Problem TestProblem:\n" +
                "1 = 2 = 3 = 7 = 4 = 6 = 5 = -9\n" +
                "11 = -12\n",eqc.toString());
    }

    public void testAddEquivalence() {
        System.out.println("addEquivalence");
    }


    public void testCompleteModel() {
        System.out.println("complete model");
    }


    public void testTestRun() {
    }
}