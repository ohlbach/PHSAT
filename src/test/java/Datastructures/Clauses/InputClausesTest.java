package Datastructures.Clauses;

import Datastructures.Symboltable;
import Datastructures.Theory.Model;
import junit.framework.TestCase;

import java.util.ArrayList;
import java.util.Arrays;

public class InputClausesTest extends TestCase {
    private static final int cOr = Quantifier.OR.ordinal();
    private static final int cAnd = Quantifier.AND.ordinal();
    private static final int cEquiv = Quantifier.EQUIV.ordinal();
    private static final int cAtleast = Quantifier.ATLEAST.ordinal();
    private static final int cAtmost = Quantifier.ATMOST.ordinal();
    private static final int cExactly = Quantifier.EXACTLY.ordinal();
    private static final int cInterval = Quantifier.INTERVAL.ordinal();

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
        int[] clause = {10, Quantifier.OR.ordinal(),1,-2,3};
        assertEquals("   10: 1v-2v3",InputClauses.toString(5,clause,null));
        assertEquals("   10: pv-qvr",InputClauses.toString(5,clause,symboltable));
    }
    public void testToStringAND() {
        System.out.println("ToString AND");
        int[] clause = {10, Quantifier.AND.ordinal(),1,-2,3};
        assertEquals("   10: 1&-2&3",InputClauses.toString(5,clause,null));
        assertEquals("   10: p&-q&r",InputClauses.toString(5,clause,symboltable));
    }

    public void testToStringEQUIV() {
        System.out.println("ToString EQUIV");
        int[] clause = {10, Quantifier.EQUIV.ordinal(),1,-2,3};
        assertEquals("   10: 1=-2=3",InputClauses.toString(5,clause,null));
        assertEquals("   10: p=-q=r",InputClauses.toString(5,clause,symboltable));
    }

    public void testToStringATLEAST() {
        System.out.println("ToString ATLEAST");
        int[] clause = {10, Quantifier.ATLEAST.ordinal(),2,1,-2,3};
        assertEquals("   10: >= 2 1,-2,3",InputClauses.toString(5,clause,null));
        assertEquals("   10: >= 2 p,-q,r",InputClauses.toString(5,clause,symboltable));
    }

    public void testToStringATMOST() {
        System.out.println("ToString ATMOST");
        int[] clause = {10, Quantifier.ATMOST.ordinal(),2,1,-2,3};
        assertEquals("   10: <= 2 1,-2,3",InputClauses.toString(5,clause,null));
        assertEquals("   10: <= 2 p,-q,r",InputClauses.toString(5,clause,symboltable));
    }

    public void testToStringEXACTLY() {
        System.out.println("ToString EXACTLY");
        int[] clause = {10, Quantifier.EXACTLY.ordinal(),2,1,-2,3};
        assertEquals("   10: = 2 1,-2,3",InputClauses.toString(5,clause,null));
        assertEquals("   10: = 2 p,-q,r",InputClauses.toString(5,clause,symboltable));
    }

    public void testToStringINTERVAL() {
        System.out.println("ToString INTERVAL");
        int[] clause = {10, Quantifier.INTERVAL.ordinal(),2,3,1,-2,3};
        assertEquals("   10: 2-3: 1,-2,3",InputClauses.toString(5,clause,null));
        assertEquals("   10: 2-3: p,-q,r",InputClauses.toString(5,clause,symboltable));

        int[] clause1 = {11, Quantifier.INTERVAL.ordinal(),2,2,1,-2,3};
        assertEquals("   11: 2-2: 1,-2,3",InputClauses.toString(5,clause1,null));
    }


    public void testCheckSyntaxConnective() {
        System.out.println("checkSyntax connective");
        StringBuilder errors = new StringBuilder();
        int[] clause1 = {10,8,2,3,1,-2,3};
        InputClauses.checkSyntax(clause1,10,"Test ",errors);
        assertTrue(errors.length() > 0);
        //System.out.println(errors);
        int[] clause2 = {10,0,2,3,1,-2,3};
        errors = new StringBuilder();
        InputClauses.checkSyntax(clause2,8,"Test ",errors);
        assertTrue(errors.length() == 0);
    }
    public void testCheckSyntaxPredicates() {
        System.out.println("checkSyntax predicates");
        StringBuilder errors = new StringBuilder();
        int[] clause1 = {10, 0, 2, 3, 1, -2, -3};
        InputClauses.checkSyntax(clause1, 2, "Test ", errors);
        assertTrue(errors.length() > 0);
        //System.out.println(errors);
        errors = new StringBuilder();
        InputClauses.checkSyntax(clause1, 3, "Test ", errors);
        assertEquals(0, errors.length());
    }



    public void testContainsComplementaryLiterals() {
        System.out.println("containsComplementaryLiterals");
        int[] clause1 = {10, Quantifier.EQUIV.ordinal(), 1, -2, -3};
        assertFalse(InputClauses.containsComplementaryLiterals(clause1));
        int[] clause2 = {10, Quantifier.EQUIV.ordinal(), 1, -2, -1};
        assertTrue(InputClauses.containsComplementaryLiterals(clause2));
        int[] clause3 = {10, Quantifier.EQUIV.ordinal(), 1, -1, -1};
        assertTrue(InputClauses.containsComplementaryLiterals(clause3));
    }

    public void testNumberOfComplementaryPairs1() {
        System.out.println("number of complementary pairs without model");
        Model model = new Model(10);
        int[] clause1 = {10, Quantifier.ATLEAST.ordinal(), 1, 1, -2, -3};
        assertEquals(0,InputClauses.numberOfComplementaryPairs(clause1, model));
        assertEquals("[10, 3, 1, 1, -2, -3]", Arrays.toString(clause1));
        int[] clause2 = {10, Quantifier.ATMOST.ordinal(), 1, 1, -2, -1};
        assertEquals(1,InputClauses.numberOfComplementaryPairs(clause2, model));
        assertEquals("[10, 4, 1, 1, -2, -1]", Arrays.toString(clause2));
        int[] clause3 = {10, Quantifier.ATMOST.ordinal(), 1, -1, -2, 1};
        assertEquals(1,InputClauses.numberOfComplementaryPairs(clause3, model));
        assertEquals("[10, 4, 1, -1, -2, 1]", Arrays.toString(clause3));

        int[] clause4 = {10, Quantifier.ATMOST.ordinal(), 1, 1, -2, -1, 1};
        assertEquals(1,InputClauses.numberOfComplementaryPairs(clause4, model));
        assertEquals("[10, 4, 1, 1, -2, -1, 1]", Arrays.toString(clause4));

        int[] clause5 = {10, Quantifier.ATMOST.ordinal(), 1, 1, -2, -1, 1,-1};
        assertEquals(2,InputClauses.numberOfComplementaryPairs(clause5, model));
        assertEquals("[10, 4, 1, 1, -2, -1, 1, -1]", Arrays.toString(clause5));

        int[] clause6 = {10, Quantifier.ATMOST.ordinal(), 1, 1, -2, -1, 1,-1,2};
        assertEquals(3,InputClauses.numberOfComplementaryPairs(clause6, model));
        assertEquals("[10, 4, 1, 1, -2, -1, 1, -1, 2]", Arrays.toString(clause6));
    }

    public void testNumberOfComplementaryPairs2() {
        System.out.println("number of complementary pairs with model");
        Model model = new Model(10);
        model.addImmediately(1);
        int[] clause1 = {10, Quantifier.ATMOST.ordinal(), 1, 1, -2, -1};
        assertEquals(0, InputClauses.numberOfComplementaryPairs(clause1, model));
        assertEquals("[10, 4, 1, 1, -2, -1]", Arrays.toString(clause1));

        int[] clause2 = {10, Quantifier.ATMOST.ordinal(), 1, 1, -2, -1,2};
        assertEquals(1, InputClauses.numberOfComplementaryPairs(clause2, model));
        assertEquals("[10, 4, 1, 1, -2, -1, 2]", Arrays.toString(clause2));

        model.addImmediately(-2);
        int[] clause3 = {10, Quantifier.INTERVAL.ordinal(), 1, 2, 1, -2, -1,2};
        assertEquals(0, InputClauses.numberOfComplementaryPairs(clause3, model));
        assertEquals("[10, 6, 1, 2, 1, -2, -1, 2]", Arrays.toString(clause3));
    }


    public void testDisjunctionStatus() {
        System.out.println("disjunctionStatus");
        Model model = new Model(3);
        int[] clause1 = {10, Quantifier.OR.ordinal(), 1,2,3};
        assertEquals(0, InputClauses.disjunctionStatus(clause1,model));
        int[] clause2 = {10, Quantifier.OR.ordinal(), 1,2,-1};
        assertEquals(1,InputClauses.disjunctionStatus(clause2,model));
        model.addImmediately(-3);
        assertEquals(0, InputClauses.disjunctionStatus(clause1,model));
        model.addImmediately(1);
        assertEquals(1, InputClauses.disjunctionStatus(clause1,model));

        model = new Model(3);
        model.addImmediately(1,2,3);
        clause1 = new int[]{10, Quantifier.OR.ordinal(), 1,2,3,2,3};
        assertEquals(1, InputClauses.disjunctionStatus(clause1,model));
        assertEquals(1, InputClauses.disjunctionStatus(clause2,model));

        clause1 = new int[]{10, Quantifier.OR.ordinal(), -2,-1,-1,-2,-3};
        assertEquals(-1, InputClauses.disjunctionStatus(clause1,model));

    }

    public void testConjunctionStatus() {
        System.out.println("conjunctionStatus");
        Model model = new Model(3);
        int[] clause1 = {10, Quantifier.AND.ordinal(), 1, 2, 3, 2};
        int[] clause2 = {11, Quantifier.AND.ordinal(), 1, 2, -3,2};
        int[] clause3 = {12, Quantifier.AND.ordinal(), 1, 2, -1,2};
        model = new Model(3);
        model.addImmediately(1,2);
        assertEquals(0,InputClauses.conjunctionStatus(clause1,model));
        assertEquals(0,InputClauses.conjunctionStatus(clause2,model));
        assertEquals(-1,InputClauses.conjunctionStatus(clause3,model));
        model.addImmediately(3);
        assertEquals(1,InputClauses.conjunctionStatus(clause1,model));
    }
    public void testEquivalenceStatus() {
        System.out.println("equivalenceStatus");
        Model model = new Model(3);
        int[] clause1 = {10, Quantifier.EQUIV.ordinal(), 1, 2, 3, 2};
        int[] clause2 = {11, Quantifier.EQUIV.ordinal(), 1, 2, -3,2};
        int[] clause3 = {12, Quantifier.EQUIV.ordinal(), 1, 2, -1,2};
        model = new Model(3);
        model.addImmediately(1,2);
        assertEquals(0,InputClauses.equivalenceStatus(clause1,model));
        assertEquals(0,InputClauses.equivalenceStatus(clause2,model));
        assertEquals(-1,InputClauses.equivalenceStatus(clause3,model));
        model.addImmediately(3);
        assertEquals(1,InputClauses.equivalenceStatus(clause1,model));
    }
    public void testQuantifiedStatus() {
        System.out.println("quantifiedStatus");
        Model model = new Model(3);
        int[] clause1 = {1, Quantifier.ATLEAST.ordinal(), 2, 1, 2, 3, 2};
        int[] clause2 = {2, Quantifier.ATMOST.ordinal(),  2, 1, 2, 3, 2};
        int[] clause3 = {3, Quantifier.EXACTLY.ordinal(), 2, 1, 2, 3, 2};
        int[] clause4 = {4, Quantifier.INTERVAL.ordinal(),2, 3, 1, 2, 3, 2};
        model.addImmediately(1);
        assertEquals(0,InputClauses.quantifiedStatus(clause1,model));
        assertEquals(1,InputClauses.quantifiedStatus(clause2,model));
        assertEquals(0,InputClauses.quantifiedStatus(clause3,model));
        assertEquals(0,InputClauses.quantifiedStatus(clause4,model));

        model.addImmediately(3);
        assertEquals(1,InputClauses.quantifiedStatus(clause1,model));
        assertEquals(1,InputClauses.quantifiedStatus(clause2,model));
        assertEquals(1,InputClauses.quantifiedStatus(clause3,model));
        assertEquals(1,InputClauses.quantifiedStatus(clause4,model));

        model.addImmediately(2);
        assertEquals(1,InputClauses.quantifiedStatus(clause1,model));
        assertEquals(-1,InputClauses.quantifiedStatus(clause2,model));
        assertEquals(-1,InputClauses.quantifiedStatus(clause3,model));
        assertEquals(-1,InputClauses.quantifiedStatus(clause4,model));

        model = new Model(3);
        model.addImmediately(-1,-2);
        assertEquals(-1,InputClauses.quantifiedStatus(clause1,model));
        assertEquals( 1,InputClauses.quantifiedStatus(clause2,model));
        assertEquals(-1,InputClauses.quantifiedStatus(clause3,model));
        assertEquals(-1,InputClauses.quantifiedStatus(clause4,model));

        clause1 = new int[]{1, Quantifier.ATLEAST.ordinal(), 2, 1, 2, 3, -2};
        clause2 = new int[]{2, Quantifier.ATMOST.ordinal(),  2, 1, 2, 3, -2};
        clause3 = new int[]{3, Quantifier.EXACTLY.ordinal(), 2, 1, 2, 3, -2};
        clause4 = new int[]{4, Quantifier.INTERVAL.ordinal(),2, 3, 1, 2, 3, -2};


        model = new Model(3);
        assertEquals(0,InputClauses.quantifiedStatus(clause1,model));
        assertEquals(1,InputClauses.quantifiedStatus(clause2,model));
        assertEquals(0,InputClauses.quantifiedStatus(clause3,model));
        assertEquals(0,InputClauses.quantifiedStatus(clause4,model));

        clause1 = new int[]{1, Quantifier.ATLEAST.ordinal(), 2, 1, 2, 3, -2, -1};
        clause2 = new int[]{2, Quantifier.ATMOST.ordinal(),  2, 1, 2, 3, -2,-1};
        clause3 = new int[]{3, Quantifier.EXACTLY.ordinal(), 2, 1, 2, 3, -2,-1};
        clause4 = new int[]{4, Quantifier.INTERVAL.ordinal(),2, 3, 1, 2, 3, -2,-1};


        model = new Model(3);
        assertEquals(1,InputClauses.quantifiedStatus(clause1,model));
        assertEquals(1,InputClauses.quantifiedStatus(clause2,model));
        assertEquals(1,InputClauses.quantifiedStatus(clause3,model));
        assertEquals(1,InputClauses.quantifiedStatus(clause4,model));
    }

        public void testATLEASTIsFalse() {
        System.out.println("atleastIsFalse");
        Model model = new Model(3);
        int[] clause1 = {10, Quantifier.ATLEAST.ordinal(), 2, 1,2,3};
        assertTrue(InputClauses.quantifiedIsFalse(clause1,model));
        model.addImmediately(2,3);
        assertFalse(InputClauses.quantifiedIsFalse(clause1,model));

        model = new Model(3);
        int[] clause2 = {10, Quantifier.ATLEAST.ordinal(), 2, 1,2,-1,-2,3};
        assertFalse(InputClauses.quantifiedIsFalse(clause2,model));    }

    public void testATMOSTIsFalse() {
        System.out.println("atmostIsFalse");
        Model model = new Model(3);
        int[] clause1 = {10, Quantifier.ATMOST.ordinal(), 1, 1,2,3};
        assertFalse(InputClauses.quantifiedIsFalse(clause1,model));
        model.addImmediately(2,3);
        assertTrue(InputClauses.quantifiedIsFalse(clause1,model));

        model = new Model(3);
        int[] clause2 = {10, Quantifier.ATMOST.ordinal(), 1, 1,2,-1,-2,3};
        assertTrue(InputClauses.quantifiedIsFalse(clause2,model));

        int[] clause3 = {10, Quantifier.ATMOST.ordinal(), 2, 1,2,-1,-2,3};
        assertFalse(InputClauses.quantifiedIsFalse(clause3,model));    }

    public void testExactlyIsFalse() {
        System.out.println("exactlyIsFalse");
        Model model = new Model(3);
        model.addImmediately(1);
        int[] clause1 = {10, Quantifier.EXACTLY.ordinal(), 2, 1,2,3};
        assertTrue(InputClauses.quantifiedIsFalse(clause1,model));
        model.addImmediately(2);
        assertFalse(InputClauses.quantifiedIsFalse(clause1,model));

        model = new Model(3);
        int[] clause2 = {10, Quantifier.EXACTLY.ordinal(), 1, 1,2,-1,-2,3};
        assertTrue(InputClauses.quantifiedIsFalse(clause2,model));

        int[] clause3 = {10, Quantifier.EXACTLY.ordinal(), 2, 1,2,-1,-2,3};
        assertFalse(InputClauses.quantifiedIsFalse(clause3,model));    }

    public void testIntervalIsFalse() {
        System.out.println("intervalIsFalse");
        Model model = new Model(3);
        model.addImmediately(1);
        int[] clause1 = {10, Quantifier.INTERVAL.ordinal(), 2,3, 1,2,3};
        assertTrue(InputClauses.intervalIsFalse(clause1,model));
        model.addImmediately(2);
        assertFalse(InputClauses.intervalIsFalse(clause1,model));

        model = new Model(3);
        int[] clause2 = {10, Quantifier.INTERVAL.ordinal(), 2,3, 1,2,-1,-2,3};
        assertFalse(InputClauses.intervalIsFalse(clause2,model));

        int[] clause3 = {10, Quantifier.INTERVAL.ordinal(), 1,2, 1,2,-1,-2,3,-3};
        assertTrue(InputClauses.intervalIsFalse(clause3,model));    }

    public void testFalseClausesInModel() {
        System.out.println("falseClausesInModel");
        Model model = new Model(3);
        model.addImmediately(1);
        int[] clause1 = {10, Quantifier.OR.ordinal(), 1,2,3};
        int[] clause2 = {11, Quantifier.AND.ordinal(), 1,2,3};
        int[] clause3 = {12, Quantifier.EQUIV.ordinal(), 1,2,3};
        int[] clause4 = {13, Quantifier.ATLEAST.ordinal(), 2, 1,2,3};
        int[] clause5 = {14, Quantifier.ATMOST.ordinal(), 2, 1,2,3};
        int[] clause6 = {15, Quantifier.EXACTLY.ordinal(), 2, 1,2,3};
        int[] clause7 = {16, Quantifier.INTERVAL.ordinal(), 2,3, 1,2,3};

        InputClauses clauses = new InputClauses("Input",3,null,"Test");
        clauses.addClause(clause1,clause2,clause3,clause4,clause5,clause6,clause7);
        //System.out.println(clauses.toString());
        ArrayList<int[]> falseClauses = clauses.falseClausesInModel(model);

        assertEquals("   11: 1&2&3\n" +
                "   12: 1=2=3\n" +
                "   13: >= 2 1,2,3\n" +
                "   15: = 2 1,2,3\n" +
                "   16: 2-3: 1,2,3",InputClauses.toString(falseClauses,null));

        model.addImmediately(2);
        falseClauses = clauses.falseClausesInModel(model);
        assertEquals("   11: 1&2&3\n" +
                "   12: 1=2=3",InputClauses.toString(falseClauses,null));

        assertEquals("   11: p&q&r\n" +
                "   12: p=q=r",InputClauses.toString(falseClauses,symboltable));
    }

    public void testAtmostToAtleast() {
        System.out.println("atmostToAtleast");
        int[] clause = {10, cAtmost, 2, 1, 2, 3, 4, 5};
        clause = InputClauses.atmostToAtleast(clause);
        assertEquals("  10: >= 3 -1,-2,-3,-4,-5", InputClauses.toString(clause));

        clause = new int[]{11, cAtmost, 4, 1, 2, 3, 4, 5};
        clause = InputClauses.atmostToAtleast(clause);
        assertEquals("  11: -1v-2v-3v-4v-5", InputClauses.toString(clause));

        clause = new int[]{12, cAtmost, 5, 1, 2, 3, 4, 5};
        clause = InputClauses.atmostToAtleast(clause);
        assertEquals("  12: >= 0 -1,-2,-3,-4,-5", InputClauses.toString(clause));

        clause = new int[]{13, cAtmost, 0, 1, 2, 3, 4, 5};
        clause = InputClauses.atmostToAtleast(clause);
        assertEquals("  13: >= 5 -1,-2,-3,-4,-5", InputClauses.toString(clause));
    }

    public void testExactlyToAtleast() {
        System.out.println("exactlyToAtleast");
        int[] id = {5};
        int[] clause = {10, cExactly, 2, 1, 2, 3, 4, 5};
        int[][]clauses = InputClauses.exactlyToAtleast(clause,()->{return ++id[0];});
        assertEquals("  6: >= 2 1,2,3,4,5", InputClauses.toString(clauses[0]));
        assertEquals("  7: >= 3 -1,-2,-3,-4,-5", InputClauses.toString(clauses[1]));

        clause = new int[]{11, cExactly, 1, 1, 2, 3, 4, 5};
        clauses = InputClauses.exactlyToAtleast(clause,()->{return ++id[0];});
        assertEquals("  8: 1v2v3v4v5", InputClauses.toString(clauses[0]));
        assertEquals("  9: >= 4 -1,-2,-3,-4,-5", InputClauses.toString(clauses[1]));

        clause = new int[]{12, cExactly, 4, 1, 2, 3, 4, 5};
        clauses = InputClauses.exactlyToAtleast(clause,()->{return ++id[0];});
        assertEquals("  10: >= 4 1,2,3,4,5", InputClauses.toString(clauses[0]));
        assertEquals("  11: -1v-2v-3v-4v-5", InputClauses.toString(clauses[1]));

        clause = new int[]{13, cExactly, 0, 1, 2, 3, 4, 5};
        clauses = InputClauses.exactlyToAtleast(clause,()->{return ++id[0];});
        assertEquals("  12: >= 0 1,2,3,4,5", InputClauses.toString(clauses[0]));
        assertEquals("  13: >= 5 -1,-2,-3,-4,-5", InputClauses.toString(clauses[1]));

        clause = new int[]{14, cExactly, 6, 1, 2, 3, 4, 5};
        clauses = InputClauses.exactlyToAtleast(clause,()->{return ++id[0];});
        assertEquals("  14: >= 6 1,2,3,4,5", InputClauses.toString(clauses[0]));
        assertEquals("  15: >= -1 -1,-2,-3,-4,-5", InputClauses.toString(clauses[1]));
 }

    public void testIntervalToAtleast() {
        System.out.println("intervalToAtleast");
        int[] id = {5};
        int[] clause = {10, cInterval, 2, 3, 1, 2, 3, 4, 5};
        int[][] clauses = InputClauses.intervalToAtleast(clause, () -> {return ++id[0];});
        assertEquals("  6: >= 2 1,2,3,4,5", InputClauses.toString(clauses[0]));
        assertEquals("  7: >= 2 -1,-2,-3,-4,-5", InputClauses.toString(clauses[1]));

        clause = new int[]{10, cInterval, 1, 3, 1, 2, 3, 4, 5};
        clauses = InputClauses.intervalToAtleast(clause, () -> {return ++id[0];});
        assertEquals("  8: 1v2v3v4v5", InputClauses.toString(clauses[0]));
        assertEquals("  9: >= 2 -1,-2,-3,-4,-5", InputClauses.toString(clauses[1]));

        clause = new int[]{10, cInterval, 2, 4, 1, 2, 3, 4, 5};
        clauses = InputClauses.intervalToAtleast(clause, () -> {return ++id[0];});
        assertEquals("  10: >= 2 1,2,3,4,5", InputClauses.toString(clauses[0]));
        assertEquals("  11: -1v-2v-3v-4v-5", InputClauses.toString(clauses[1]));

    }



    }