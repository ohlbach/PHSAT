package Datastructures.Clauses;

import Datastructures.Symboltable;
import Datastructures.Theory.Model;
import junit.framework.TestCase;

import java.util.ArrayList;
import java.util.Arrays;

public class InputClausesTest extends TestCase {
    static Symboltable symboltable = new Symboltable(10);
    static {
        symboltable.setName(1,"p");
        symboltable.setName(2,"q");
        symboltable.setName(3,"r");
        symboltable.setName(4,"s");
        symboltable.setName(5,"t");
        symboltable.setName(6,"u");}
    
    public void testToStringOR() {
        System.out.println("ToString OR");
        InputClauses clauses = new InputClauses("Input",5,null,"test");
        int[] clause = {10,Connective.OR.ordinal(),1,-2,3};
        assertEquals("   10: 1,-2,3",InputClauses.toString(5,clause,null));
        assertEquals("   10: p,-q,r",InputClauses.toString(5,clause,symboltable));
    }
    public void testToStringAND() {
        System.out.println("ToString AND");
        InputClauses clauses = new InputClauses("Input",5,null,"test");
        int[] clause = {10,Connective.AND.ordinal(),1,-2,3};
        assertEquals(" A-10: 1&-2&3",InputClauses.toString(5,clause,null));
        assertEquals(" A-10: p&-q&r",InputClauses.toString(5,clause,symboltable));
    }

    public void testToStringEQUIV() {
        System.out.println("ToString EQUIV");
        InputClauses clauses = new InputClauses("Input",5,null,"test");
        int[] clause = {10,Connective.EQUIV.ordinal(),1,-2,3};
        assertEquals(" E-10: 1=-2=3",InputClauses.toString(5,clause,null));
        assertEquals(" E-10: p=-q=r",InputClauses.toString(5,clause,symboltable));
    }

    public void testToStringATLEAST() {
        System.out.println("ToString ATLEAST");
        InputClauses clauses = new InputClauses("Input",5,null,"test");
        int[] clause = {10,Connective.ATLEAST.ordinal(),2,1,-2,3};
        assertEquals(" L-10: >= 2 1,-2,3",InputClauses.toString(5,clause,null));
        assertEquals(" L-10: >= 2 p,-q,r",InputClauses.toString(5,clause,symboltable));
    }

    public void testToStringATMOST() {
        System.out.println("ToString ATMOST");
        InputClauses clauses = new InputClauses("Input",5,null,"test");
        int[] clause = {10,Connective.ATMOST.ordinal(),2,1,-2,3};
        assertEquals(" M-10: <= 2 1,-2,3",InputClauses.toString(5,clause,null));
        assertEquals(" M-10: <= 2 p,-q,r",InputClauses.toString(5,clause,symboltable));
    }

    public void testToStringEXACTLY() {
        System.out.println("ToString EXACTLY");
        InputClauses clauses = new InputClauses("Input",5,null,"test");
        int[] clause = {10,Connective.EXACTLY.ordinal(),2,1,-2,3};
        assertEquals(" X-10: = 2 1,-2,3",InputClauses.toString(5,clause,null));
        assertEquals(" X-10: = 2 p,-q,r",InputClauses.toString(5,clause,symboltable));
    }

    public void testToStringINTERVAL() {
        System.out.println("ToString INTERVAL");
        InputClauses clauses = new InputClauses("Input",5,null,"test");
        int[] clause = {10,Connective.INTERVAL.ordinal(),2,3,1,-2,3};
        assertEquals(" I-10: 2-3: 1,-2,3",InputClauses.toString(5,clause,null));
        assertEquals(" I-10: 2-3: p,-q,r",InputClauses.toString(5,clause,symboltable));

        int[] clause1 = {11,Connective.INTERVAL.ordinal(),2,2,1,-2,3};
        assertEquals(" I-11: 2-2: 1,-2,3",InputClauses.toString(5,clause1,null));
    }


    public void testCheckSyntaxConnective() {
        System.out.println("checkSyntax connective");
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        int[] clause1 = {10,8,2,3,1,-2,3};
        InputClauses.checkSyntax(clause1,10,"Test ",errors,warnings);
        assertTrue(errors.length() > 0);
        //System.out.println(errors);
        int[] clause2 = {10,0,2,3,1,-2,3};
        errors = new StringBuilder();
        InputClauses.checkSyntax(clause2,8,"Test ",errors,warnings);
        assertTrue(errors.length() == 0);
    }
    public void testCheckSyntaxPredicates() {
        System.out.println("checkSyntax predicates");
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        int[] clause1 = {10, 0, 2, 3, 1, -2, -3};
        InputClauses.checkSyntax(clause1, 2, "Test ", errors, warnings);
        assertTrue(errors.length() > 0);
        //System.out.println(errors);
        errors = new StringBuilder();
        InputClauses.checkSyntax(clause1, 3, "Test ", errors, warnings);
        assertTrue(errors.length() == 0);
    }

    public void testCheckSyntaxBoundaries() {
        System.out.println("checkSyntax boundaries no interval");
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        int[] clause1 = {10, Connective.ATLEAST.ordinal(), -1, 1, -2, -3};
        InputClauses.checkSyntax(clause1, 3, "Test ", errors, warnings);
        assertTrue(errors.length() > 0);
        //System.out.println(errors);
        errors = new StringBuilder();
        warnings = new StringBuilder();
        int[] clause2 = {10, Connective.ATLEAST.ordinal(), 4, 1, -2, -3};
        InputClauses.checkSyntax(clause2, 3, "Test ", errors, warnings);
        assertTrue(errors.length() == 0);
        assertTrue(warnings.length() > 0);
        //System.out.println(warnings);

        errors = new StringBuilder();
        warnings = new StringBuilder();
        int[] clause3 = {10, Connective.ATLEAST.ordinal(), 3, 1, -2, -3};
        InputClauses.checkSyntax(clause3, 3, "Test ", errors, warnings);
        assertTrue(errors.length() == 0);
        assertTrue(warnings.length() == 0);
    }

    public void testCheckSyntaxBoundariesInterval() {
        System.out.println("checkSyntax boundaries for interval");
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        int[] clause1 = {10, Connective.INTERVAL.ordinal(), -1, 1, -2, -3};
        InputClauses.checkSyntax(clause1, 3, "Test ", errors, warnings);
        assertTrue(errors.length() > 0);
        //System.out.println(errors);

        errors = new StringBuilder();
        int[] clause2 = {10, Connective.INTERVAL.ordinal(), 1, -1, -2, -3};
        InputClauses.checkSyntax(clause2, 3, "Test ", errors, warnings);
        assertTrue(errors.length() > 0);
        //System.out.println(errors);

        errors = new StringBuilder();
        int[] clause3 = {10, Connective.INTERVAL.ordinal(), 1, 1, -2, -3};
        InputClauses.checkSyntax(clause3, 3, "Test ", errors, warnings);
        assertTrue(errors.length() == 0);
    }


    public void testContainsComplementaryLiterals() {
        System.out.println("containsComplementaryLiterals");
        int[] clause1 = {10, Connective.EQUIV.ordinal(), 1, -2, -3};
        assertFalse(InputClauses.containsComplementaryLiterals(clause1));
        int[] clause2 = {10, Connective.EQUIV.ordinal(), 1, -2, -1};
        assertTrue(InputClauses.containsComplementaryLiterals(clause2));
        int[] clause3 = {10, Connective.EQUIV.ordinal(), 1, -1, -1};
        assertTrue(InputClauses.containsComplementaryLiterals(clause3));
    }

    public void testNumberOfComplementaryPairs1() {
        System.out.println("number of complementary pairs without model");
        Model model = new Model(10);
        int[] clause1 = {10, Connective.ATLEAST.ordinal(), 1, 1, -2, -3};
        assertEquals(0,InputClauses.numberOfComplementaryPairs(clause1, model));
        assertEquals("[10, 4, 1, 1, -2, -3]", Arrays.toString(clause1));
        int[] clause2 = {10, Connective.ATMOST.ordinal(), 1, 1, -2, -1};
        assertEquals(1,InputClauses.numberOfComplementaryPairs(clause2, model));
        assertEquals("[10, 5, 1, 1, -2, -1]", Arrays.toString(clause2));
        int[] clause3 = {10, Connective.ATMOST.ordinal(), 1, -1, -2, 1};
        assertEquals(1,InputClauses.numberOfComplementaryPairs(clause3, model));
        assertEquals("[10, 5, 1, -1, -2, 1]", Arrays.toString(clause3));

        int[] clause4 = {10, Connective.ATMOST.ordinal(), 1, 1, -2, -1, 1};
        assertEquals(1,InputClauses.numberOfComplementaryPairs(clause4, model));
        assertEquals("[10, 5, 1, 1, -2, -1, 1]", Arrays.toString(clause4));

        int[] clause5 = {10, Connective.ATMOST.ordinal(), 1, 1, -2, -1, 1,-1};
        assertEquals(2,InputClauses.numberOfComplementaryPairs(clause5, model));
        assertEquals("[10, 5, 1, 1, -2, -1, 1, -1]", Arrays.toString(clause5));

        int[] clause6 = {10, Connective.ATMOST.ordinal(), 1, 1, -2, -1, 1,-1,2};
        assertEquals(3,InputClauses.numberOfComplementaryPairs(clause6, model));
        assertEquals("[10, 5, 1, 1, -2, -1, 1, -1, 2]", Arrays.toString(clause6));
    }

    public void testNumberOfComplementaryPairs2() {
        System.out.println("number of complementary pairs with model");
        Model model = new Model(10);
        model.addImmediately(1);
        int[] clause1 = {10, Connective.ATMOST.ordinal(), 1, 1, -2, -1};
        assertEquals(0, InputClauses.numberOfComplementaryPairs(clause1, model));
        assertEquals("[10, 5, 1, 1, -2, -1]", Arrays.toString(clause1));

        int[] clause2 = {10, Connective.ATMOST.ordinal(), 1, 1, -2, -1,2};
        assertEquals(1, InputClauses.numberOfComplementaryPairs(clause2, model));
        assertEquals("[10, 5, 1, 1, -2, -1, 2]", Arrays.toString(clause2));

        model.addImmediately(-2);
        int[] clause3 = {10, Connective.INTERVAL.ordinal(), 1, 2, 1, -2, -1,2};
        assertEquals(0, InputClauses.numberOfComplementaryPairs(clause3, model));
        assertEquals("[10, 3, 1, 2, 1, -2, -1, 2]", Arrays.toString(clause3));
    }

    public void testDisjunctionIsFalse() {
        System.out.println("disjunctionIsFalse");
        Model model = new Model(3);
        int[] clause1 = {10, Connective.OR.ordinal(), 1,2,3};
        assertTrue(InputClauses.disjunctionIsFalse(clause1,model));
        int[] clause2 = {10, Connective.OR.ordinal(), 1,2,-1};
        assertFalse(InputClauses.disjunctionIsFalse(clause2,model));
        model.addImmediately(-3);
        assertTrue(InputClauses.disjunctionIsFalse(clause1,model));
        model.addImmediately(1);
        assertFalse(InputClauses.disjunctionIsFalse(clause1,model));

        model = new Model(3);
        model.addImmediately(1,2,3);
        assertFalse(InputClauses.disjunctionIsFalse(clause1,model));
        assertFalse(InputClauses.disjunctionIsFalse(clause2,model));
    }

    public void testConjunctionIsFalse() {
        System.out.println("conjunctionIsFalse");
        Model model = new Model(3);
        int[] clause1 = {10, Connective.AND.ordinal(), 1,2,3};
        assertTrue(InputClauses.conjunctionIsFalse(clause1,model));
        model.addImmediately(1,2);
        assertTrue(InputClauses.conjunctionIsFalse(clause1,model));
        model.addImmediately(3);
        assertFalse(InputClauses.conjunctionIsFalse(clause1,model));
        int[] clause2 = {10, Connective.AND.ordinal(), 1,2,-2};
        assertTrue(InputClauses.conjunctionIsFalse(clause2,model));

        model = new Model(3);
        model.addImmediately(1);
        assertTrue(InputClauses.conjunctionIsFalse(clause2,model)); // complementary clause
    }

    public void testEquivalenceIsFalse() {
        System.out.println("equivalenceIsFalse");
        Model model = new Model(3);
        int[] clause1 = {10, Connective.EQUIV.ordinal(), 1,2,3};
        assertTrue(InputClauses.equivalenceIsFalse(clause1,model));
        model.addImmediately(1,3);
        assertTrue(InputClauses.equivalenceIsFalse(clause1,model));
        model.addImmediately(2);
        assertFalse(InputClauses.equivalenceIsFalse(clause1,model));
        int[] clause2 = {10, Connective.EQUIV.ordinal(), 1,2,-3};
        assertTrue(InputClauses.equivalenceIsFalse(clause2,model));
        int[] clause3 = {10, Connective.EQUIV.ordinal(), -1,-2,-3};
        assertFalse(InputClauses.equivalenceIsFalse(clause3,model));
    }

    public void testATLEASTIsFalse() {
        System.out.println("atleastIsFalse");
        Model model = new Model(3);
        int[] clause1 = {10, Connective.ATLEAST.ordinal(), 2, 1,2,3};
        assertTrue(InputClauses.quantifiedIsFalse(clause1,model));
        model.addImmediately(2,3);
        assertFalse(InputClauses.quantifiedIsFalse(clause1,model));

        model = new Model(3);
        int[] clause2 = {10, Connective.ATLEAST.ordinal(), 2, 1,2,-1,-2,3};
        assertFalse(InputClauses.quantifiedIsFalse(clause2,model));    }

    public void testATMOSTIsFalse() {
        System.out.println("atmostIsFalse");
        Model model = new Model(3);
        int[] clause1 = {10, Connective.ATMOST.ordinal(), 1, 1,2,3};
        assertFalse(InputClauses.quantifiedIsFalse(clause1,model));
        model.addImmediately(2,3);
        assertTrue(InputClauses.quantifiedIsFalse(clause1,model));

        model = new Model(3);
        int[] clause2 = {10, Connective.ATMOST.ordinal(), 1, 1,2,-1,-2,3};
        assertTrue(InputClauses.quantifiedIsFalse(clause2,model));

        int[] clause3 = {10, Connective.ATMOST.ordinal(), 2, 1,2,-1,-2,3};
        assertFalse(InputClauses.quantifiedIsFalse(clause3,model));    }

    public void testExactlyIsFalse() {
        System.out.println("exactlyIsFalse");
        Model model = new Model(3);
        model.addImmediately(1);
        int[] clause1 = {10, Connective.EXACTLY.ordinal(), 2, 1,2,3};
        assertTrue(InputClauses.quantifiedIsFalse(clause1,model));
        model.addImmediately(2);
        assertFalse(InputClauses.quantifiedIsFalse(clause1,model));

        model = new Model(3);
        int[] clause2 = {10, Connective.EXACTLY.ordinal(), 1, 1,2,-1,-2,3};
        assertTrue(InputClauses.quantifiedIsFalse(clause2,model));

        int[] clause3 = {10, Connective.EXACTLY.ordinal(), 2, 1,2,-1,-2,3};
        assertFalse(InputClauses.quantifiedIsFalse(clause3,model));    }

    public void testIntervalIsFalse() {
        System.out.println("intervalIsFalse");
        Model model = new Model(3);
        model.addImmediately(1);
        int[] clause1 = {10, Connective.INTERVAL.ordinal(), 2,3, 1,2,3};
        assertTrue(InputClauses.intervalIsFalse(clause1,model));
        model.addImmediately(2);
        assertFalse(InputClauses.intervalIsFalse(clause1,model));

        model = new Model(3);
        int[] clause2 = {10, Connective.INTERVAL.ordinal(), 2,3, 1,2,-1,-2,3};
        assertFalse(InputClauses.intervalIsFalse(clause2,model));

        int[] clause3 = {10, Connective.INTERVAL.ordinal(), 1,2, 1,2,-1,-2,3,-3};
        assertTrue(InputClauses.intervalIsFalse(clause3,model));    }

    public void testFalseClausesInModel() {
        System.out.println("falseClausesInModel");
        Model model = new Model(3);
        model.addImmediately(1);
        int[] clause1 = {10, Connective.OR.ordinal(), 1,2,3};
        int[] clause2 = {11, Connective.AND.ordinal(), 1,2,3};
        int[] clause3 = {12, Connective.EQUIV.ordinal(), 1,2,3};
        int[] clause4 = {13, Connective.ATLEAST.ordinal(), 2, 1,2,3};
        int[] clause5 = {14, Connective.ATMOST.ordinal(), 2, 1,2,3};
        int[] clause6 = {15, Connective.EXACTLY.ordinal(), 2, 1,2,3};
        int[] clause7 = {16, Connective.INTERVAL.ordinal(), 2,3, 1,2,3};

        InputClauses clauses = new InputClauses("Input",3,null,"Test");
        clauses.addClause(clause1,clause2,clause3,clause4,clause5,clause6,clause7);
        //System.out.println(clauses.toString());
        ArrayList<int[]> falseClauses = clauses.falseClausesInModel(model);

        assertEquals(" A-11: 1&2&3\n" +
                " E-12: 1=2=3\n" +
                " L-13: >= 2 1,2,3\n" +
                " X-15: = 2 1,2,3\n" +
                " I-16: 2-3: 1,2,3",InputClauses.toString(falseClauses,null));

        model.addImmediately(2);
        falseClauses = clauses.falseClausesInModel(model);
        assertEquals(" A-11: 1&2&3\n" +
                " E-12: 1=2=3",InputClauses.toString(falseClauses,null));

        assertEquals(" A-11: p&q&r\n" +
                " E-12: p=q=r",InputClauses.toString(falseClauses,symboltable));


    }

    }