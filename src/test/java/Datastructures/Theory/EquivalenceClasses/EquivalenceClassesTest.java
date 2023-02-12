package Datastructures.Theory.EquivalenceClasses;

import Datastructures.Clauses.Connective;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
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
        EquivalenceClasses eqc = new EquivalenceClasses(10);
        eqc.integrateEQUIVClauses(clauses);
        assertEquals("Equivalence Classes of Problem TestProblem:\n" +
                "1 = 2 = 3\n",eqc.toString());
        assertEquals("Equivalence Classes of Problem TestProblem:\n" +
                "p = q = r\n",eqc.toString(symboltable));

        eqc = new EquivalenceClasses(10);
        int[] clause2 = new int[]{11,eqv,3,4,5};
        clauses.add(clause2);
        eqc.integrateEQUIVClauses(clauses);
        assertEquals("Equivalence Classes of Problem TestProblem:\n" +
                "1 = 2 = 3 = 4 = 5\n",eqc.toString());




        eqc = new EquivalenceClasses(10);
        int[] clause3 = new int[]{13,eqv,3,-5,6};
        clauses.add(clause3);
        try{
            eqc.integrateEQUIVClauses(clauses);
            assertTrue(false);}
        catch(Unsatisfiable unsatisfiable) {
           // System.out.println(unsatisfiable.toString());
        }

        eqc = new EquivalenceClasses(10);
        int[] clause4 = new int[]{12,eqv,3,3};
        ArrayList<int[]> clauses1 = new ArrayList<>(); clauses1.add(clause4);
        eqc.integrateEQUIVClauses(clauses1);
        assertTrue(eqc.isEmpty());

        eqc = new EquivalenceClasses(10);
        int[] clause5 = new int[]{13,eqv,3,4,-3};
        ArrayList<int[]> clauses2 = new ArrayList<>(); clauses2.add(clause5);
        try{
            eqc.integrateEQUIVClauses(clauses2);
            assertTrue(false);}
        catch(Unsatisfiable unsatisfiable) {
            //System.out.println(unsatisfiable.toString());
        }
    }

    public void testTestRun() {
    }

    public void testGetRepresentative() throws Unsatisfiable{
        System.out.println("getRepresentative");
        int[] clause1 = new int[]{10,eqv,3,2,1};
        int[] clause2 = new int[]{11,eqv,6,-5,-4};
        ArrayList<int[]> clauses = new ArrayList<>(); clauses.add(clause1);clauses.add(clause2);
        EquivalenceClasses eqc = new EquivalenceClasses(10);
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

    public void testApplyTrueLiteral() {
        System.out.println("applyTrueLiteral");
        
    }

    public void testIntegrateEquivalence() {
    }

    public void testAddEquivalence() {
    }


    public void testCompleteModel() {
    }
}