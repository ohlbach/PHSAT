package Datastructures.Theory.EquivalenceClasses;

import Datastructures.Clauses.Connective;
import Datastructures.Results.UnsatInputClause;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Datastructures.Theory.Model;
import InferenceSteps.InferenceStep;
import InferenceSteps.InferenceTest;
import Utilities.TriConsumer;
import junit.framework.TestCase;

import java.util.ArrayList;

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

        model = new Model(10);
        model.add(1,new InferenceTest("MyTest 1"));
        model.add(3,new InferenceTest("MyTest -3"));
        try{
            assertTrue(EquivalenceClass.isAlreadyTrueInModel(clause1,model,true));
            assertTrue(false);}
        catch(Unsatisfiable unsatisfiable) {
            //System.out.println(unsatisfiable.toString());
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

        ArrayList<Object[]> observed = new ArrayList<>();
        ArrayList<TriConsumer<Integer,Integer, InferenceStep>> observers = new ArrayList<>();
        observers.add((reference,literal,inference) -> observed.add(new Object[]{reference,literal,inference}));
        eq1.addNewEquivalence(3,5,new InferenceTest("Add 5"),observers);
        assertEquals(1,observed.get(0)[0]);
        assertEquals(5,observed.get(0)[1]);
        assertEquals("Test Inference:\n" +
                "Add 5",observed.get(0)[2].toString());
        try{
            eq1.addNewEquivalence(2,-3,new InferenceTest("Add -3"),null);
            assertTrue(false);}
        catch(Unsatisfiable unsatisfiable) {
            //System.out.println(unsatisfiable.toString());
        }
    }


    public void testJoinOverlappingClasses() throws Unsatisfiable {
        System.out.println("JoinOverlappingClasses");
        Model model = new Model(10);
        int[] clause1 = new int[]{10,eqv,1,2,3};
        int[] clause2 = new int[]{11,eqv,4,2,3};
        EquivalenceClass eq1 = EquivalenceClass.makeEquivalenceClass(clause1,model,true);
        EquivalenceClass eq2 = EquivalenceClass.makeEquivalenceClass(clause2,model,true);
        EquivalenceClass eq3 = eq1.joinOverlappingClasses(eq2,1);
        assertEquals("1 = 2 = 3 = 4",eq3.toString());
        assertEquals("Input: Clauses 10,11",eq3.getInferenceStep(3).toString());

        int[] clause3 = new int[]{12,eqv,1,3,5};
        int[] clause4 = new int[]{13,eqv,2,4,-5};
        EquivalenceClass eq4 = EquivalenceClass.makeEquivalenceClass(clause3,model,true);
        EquivalenceClass eq5 = EquivalenceClass.makeEquivalenceClass(clause4,model,true);
        EquivalenceClass eq6 = eq4.joinOverlappingClasses(eq5,-1);
        assertEquals("1 = 3 = 5 = -2 = -4",eq6.toString());

        int[] clause5 = new int[]{14,eqv,2,3,-5};
        EquivalenceClass eq7 = EquivalenceClass.makeEquivalenceClass(clause5,model,true);
        try{
            EquivalenceClass eq8 = eq4.joinOverlappingClasses(eq7,-1);
            assertTrue(false);
        }
        catch(Unsatisfiable unsatisfiable) {
           // System.out.println(unsatisfiable.toString());
        }
    }

    public void testJoinEquivalenceClass() throws Unsatisfiable{
        System.out.println("joinEquivalenceClass");
        Model model = new Model(10);
        int[] clause1 = new int[]{10,eqv,1,2,3};
        int[] clause2 = new int[]{11,eqv,5,6,7};
        EquivalenceClass eq1 = EquivalenceClass.makeEquivalenceClass(clause1,model,true);
        EquivalenceClass eq2 = EquivalenceClass.makeEquivalenceClass(clause2,model,true);
        ArrayList<Object[]> observed = new ArrayList<>();
        ArrayList<TriConsumer<Integer,Integer, InferenceStep>> observers = new ArrayList<>();
        observers.add((reference,literal,inference) -> observed.add(new Object[]{reference,literal,inference}));
        EquivalenceClass jeq1 = eq1.joinEquivalenceClass(eq2,3,7,1,new InferenceTest("Mytest 3 7"),observers);
        assertEquals("1 = 2 = 3 = 5 = 6 = 7",jeq1.toString());
        assertEquals(3,observed.size());
        assertEquals(5,observed.get(0)[1]);
        assertEquals("Joining of Connected Equivalences:\n" +
                "1 = 2 = 3\n" +
                "5 = 6 = 7 and 3 = 7\n" +
                "-------------------\n" +
                "1 = 2 = 3 = 5 = 6 = 7",observed.get(1)[2].toString());

        EquivalenceClass jeq2 = eq2.joinEquivalenceClass(eq1,3,-7,-1,new InferenceTest("Mytest 3 -7"),observers);
        assertEquals("1 = 2 = 3 = -5 = -6 = -7",jeq2.toString());
    }



    public void testApplyTrueLiteral() throws Unsatisfiable{
        System.out.println("applyTrueLiteral");
        Model model = new Model(20);
        EquivalenceStatistics statistics = new EquivalenceStatistics("test");
        int[] clause1 = new int[]{10,eqv,1,2,3};
        EquivalenceClass eq1 = EquivalenceClass.makeEquivalenceClass(clause1,model,true);
        eq1.applyTrueLiteral(1,1,new InferenceTest("Test 1"),model,statistics);
        assertEquals("2,3",model.toString());
        assertEquals(2,statistics.derivedTrueLiterals);

        int[] clause2 = new int[]{11,eqv,6,5,4};
        EquivalenceClass eq2 = EquivalenceClass.makeEquivalenceClass(clause2,model,true);
        eq2.applyTrueLiteral(-5,-1,new InferenceTest("Test 2"),model,statistics);
        assertEquals("2,3,-4,-6",model.toString());



        model = new Model(20);
        int[] clause3 = new int[]{12,eqv,7,8,2};
        EquivalenceClass eq3 = EquivalenceClass.makeEquivalenceClass(clause3,model,true);
        model.add(2,new InferenceTest("Model 2"));
        try{
            eq3.applyTrueLiteral(-8,-1,new InferenceTest("Test 3"),model,statistics);
            assertTrue(false);}
        catch(Unsatisfiable unsatisfiable) {
        //System.out.println(unsatisfiable.toString());
        }
    }

}