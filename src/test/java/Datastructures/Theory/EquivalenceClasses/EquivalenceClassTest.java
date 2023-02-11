package Datastructures.Theory.EquivalenceClasses;

import Datastructures.Clauses.Connective;
import Datastructures.Results.UnsatInputClause;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Datastructures.Theory.Model;
import InferenceSteps.InferenceTest;
import junit.framework.TestCase;

public class EquivalenceClassTest extends TestCase {
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

    public void testMakeEquivalenceClass() throws Unsatisfiable {
        System.out.println("makeEquivalenceClass");
        Model model = new Model(10);
        int[] clause1 = new int[]{10,eqv,2,-1,3};
        EquivalenceClass eq1 = EquivalenceClass.makeEquivalenceClass(clause1,model,true);
        assertEquals("1 = -2 = -3",eq1.toString());
        assertEquals("p = -q = -r",eq1.toString(symboltable));
        assertEquals(2,eq1.inferenceSteps.size());
        assertTrue(eq1.inferenceSteps.get(0) == eq1.inferenceSteps.get(1));
        assertEquals("Input Clause Id: 10",eq1.inferenceSteps.get(0).toString());

        int[] clause2 = new int[]{11,eqv,2,-1,3,2};
        EquivalenceClass eq2 = EquivalenceClass.makeEquivalenceClass(clause2,model,true);
        assertEquals("1 = -2 = -3",eq2.toString());
        assertEquals(2,eq2.inferenceSteps.size());

        int[] clause3 = new int[]{12,eqv,2,-1,3,-2};
        try{
        EquivalenceClass eq3 = EquivalenceClass.makeEquivalenceClass(clause3,model,true);}
        catch(UnsatInputClause unsat) {
            assertEquals("Contradictory clause E-12: q=-p=r=-q",unsat.description(symboltable));
            //System.out.println(unsat.toString());
        }

        int[] clause4 = new int[]{13,eqv,2,2,2};
        EquivalenceClass eq4 = EquivalenceClass.makeEquivalenceClass(clause4,model,true);
        assertNull(eq4);
    }

    public void testIsAlreadyTrueInModel() throws Unsatisfiable{
        System.out.println("isAlreadyTrueInModel");
        Model model = new Model(10);
        int[] clause1 = new int[]{10,eqv,2,-1,3};
        assertFalse(EquivalenceClass.isAlreadyTrueInModel(clause1,model,true));
        //model.add(-2,new InferenceTest("MyTest"));
        model.addImmediately(-2);
        assertTrue(EquivalenceClass.isAlreadyTrueInModel(clause1,model,true));
        assertEquals("1,-2,-3",model.toString());
        //System.out.println(model.getInferenceStep(1).toString());

        model = new Model(10);
        model.add(2,new InferenceTest("MyTest 2"));
        assertTrue(EquivalenceClass.isAlreadyTrueInModel(clause1,model,true));
        assertEquals("-1,2,3",model.toString());

        model.add(-3,new InferenceTest("MyTest -3"));
        try{
            assertTrue(EquivalenceClass.isAlreadyTrueInModel(clause1,model,true));
            assertTrue(false);}
        catch(Unsatisfiable unsatisfiable) {
            System.out.println(unsatisfiable.toString());
        }

    }

    public void testContainsLiteral() throws Unsatisfiable {
        System.out.println("contains literal");
        Model model = new Model(10);
        int[] clause1 = new int[]{10,eqv,2,-1,3};
        EquivalenceClass eq1 = EquivalenceClass.makeEquivalenceClass(clause1,model,true);
        assertEquals(1,eq1.containsLiteral(-3));
        assertEquals(-1,eq1.containsLiteral(3));
        assertEquals(1,eq1.containsLiteral(1));
        assertEquals(-1,eq1.containsLiteral(-1));
        assertEquals(0,eq1.containsLiteral(4));
    }

    public void testOverlaps() throws Unsatisfiable{
        System.out.println("overlaps");
        Model model = new Model(10);
        int[] clause1 = new int[]{10,eqv,1,2,5};
        int[] clause2 = new int[]{11,eqv,2,4,6};
        int[] clause3 = new int[]{12,eqv,-4,3,-6};
        EquivalenceClass eq1 = EquivalenceClass.makeEquivalenceClass(clause1,model,true);
        EquivalenceClass eq2 = EquivalenceClass.makeEquivalenceClass(clause2,model,true);
        EquivalenceClass eq3 = EquivalenceClass.makeEquivalenceClass(clause3,model,true);
        assertEquals(1,eq1.overlaps(eq2));
        assertEquals(0,eq1.overlaps(eq3));
        assertEquals(-1,eq2.overlaps(eq3));
    }

    public void testAddNewEquivalence() throws Unsatisfiable {
        System.out.println("addNewEquivalence");
        Model model = new Model(10);
        int[] clause1 = new int[]{10,eqv,1,2,3};
        EquivalenceClass eq1 = EquivalenceClass.makeEquivalenceClass(clause1,model,true);
        eq1.addNewEquivalence(2,3,null,null);
        assertEquals("1 = 2 = 3",eq1.toString());
        eq1.addNewEquivalence(2,4,new InferenceTest("Add 4"),null);
        assertEquals("1 = 2 = 3 = 4",eq1.toString());
        assertEquals("1 = 2 = 3\n" +
                "2 = 4\n" +
                "----------------\n" +
                "1 = 2 = 3 = 4",eq1.getInferenceStep(4).toString());
    }


    public void testJoinOverlappingClasses() {
    }

    public void testJoinEquivalenceClass() {
    }

    public void testApplyTrueLiteral() {
    }

    public void testTestToString() {
    }
}