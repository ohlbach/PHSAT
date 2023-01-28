package Datastructures.Clauses;

import Datastructures.Symboltable;
import Datastructures.Theory.Model;
import junit.framework.TestCase;

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
        InputClauses clauses = new InputClauses(5,null,"test");
        int[] clause = {10,Connective.OR.ordinal(),1,-2,3};
        assertEquals("   10: 1,-2,3",InputClauses.toString(5,clause,null));
        assertEquals("   10: p,-q,r",InputClauses.toString(5,clause,symboltable));
    }
    public void testToStringAND() {
        System.out.println("ToString AND");
        InputClauses clauses = new InputClauses(5,null,"test");
        int[] clause = {10,Connective.AND.ordinal(),1,-2,3};
        assertEquals(" A-10: 1&-2&3",InputClauses.toString(5,clause,null));
        assertEquals(" A-10: p&-q&r",InputClauses.toString(5,clause,symboltable));
    }

    public void testToStringEQUIV() {
        System.out.println("ToString EQUIV");
        InputClauses clauses = new InputClauses(5,null,"test");
        int[] clause = {10,Connective.EQUIV.ordinal(),1,-2,3};
        assertEquals(" E-10: 1=-2=3",InputClauses.toString(5,clause,null));
        assertEquals(" E-10: p=-q=r",InputClauses.toString(5,clause,symboltable));
    }

    public void testToStringATLEAST() {
        System.out.println("ToString ATLEAST");
        InputClauses clauses = new InputClauses(5,null,"test");
        int[] clause = {10,Connective.ATLEAST.ordinal(),2,1,-2,3};
        assertEquals(" L-10: >= 2 1,-2,3",InputClauses.toString(5,clause,null));
        assertEquals(" L-10: >= 2 p,-q,r",InputClauses.toString(5,clause,symboltable));
    }

    public void testToStringATMOST() {
        System.out.println("ToString ATMOST");
        InputClauses clauses = new InputClauses(5,null,"test");
        int[] clause = {10,Connective.ATMOST.ordinal(),2,1,-2,3};
        assertEquals(" M-10: <= 2 1,-2,3",InputClauses.toString(5,clause,null));
        assertEquals(" M-10: <= 2 p,-q,r",InputClauses.toString(5,clause,symboltable));
    }

    public void testToStringEXACTLY() {
        System.out.println("ToString EXACTLY");
        InputClauses clauses = new InputClauses(5,null,"test");
        int[] clause = {10,Connective.EXACTLY.ordinal(),2,1,-2,3};
        assertEquals(" X-10: = 2 1,-2,3",InputClauses.toString(5,clause,null));
        assertEquals(" X-10: = 2 p,-q,r",InputClauses.toString(5,clause,symboltable));
    }

    public void testToStringINTERVAL() {
        System.out.println("ToString INTERVAL");
        InputClauses clauses = new InputClauses(5,null,"test");
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
    }

    public void testConjunctionIsFalse() {
    }

    public void testEquivalenceIsFalse() {
    }

    public void testQuantifiedIsFalse() {
    }

    public void testIntervalIsFalse() {
    }
}