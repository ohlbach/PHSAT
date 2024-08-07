package Datastructures;

import Datastructures.Clauses.InputClauses;
import Datastructures.Clauses.Quantifier;
import Datastructures.Results.Result;
import Datastructures.Results.Satisfiable;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Theory.Model;
import InferenceSteps.InfTrueLiteralInClause;
import InferenceSteps.InferenceStep;
import Management.Monitor.Monitor;
import Management.Monitor.MonitorLife;
import ProblemGenerators.PythagoraenTriples;
import junit.framework.TestCase;

import java.util.function.Consumer;
import java.util.function.Function;

public class ClauseListTest extends TestCase {

    static Monitor monitor = new MonitorLife("Test",System.nanoTime());
    static Consumer<String> mon = (string-> System.out.println(string));
    static Symboltable symboltable = new Symboltable(10);
    static {symboltable.setName(1,"p");
        symboltable.setName(2,"q");
        symboltable.setName(3,"r");
        symboltable.setName(4,"s");
        symboltable.setName(5,"t");
        symboltable.setName(6,"u");}

    static Function<Integer,Literal> litCreator = (literal) -> new Literal(literal,1);
    static int nor = Quantifier.OR.ordinal();
    static int natl = Quantifier.ATLEAST.ordinal();
    static int natm = Quantifier.ATMOST.ordinal();
    static int nint = Quantifier.INTERVAL.ordinal();
    static int nand = Quantifier.AND.ordinal();
    static int neqv = Quantifier.EQUIV.ordinal();
    static int nex = Quantifier.EXACTLY.ordinal();

    static Clause makeClause(int[] inputClause) {
        return new Clause(inputClause,true,(lit -> new Literal(lit,1)));
    }
    static void makeClauses(ClauseList clauseList, int[]... inputClause) {
        for(int[] clause : inputClause) {
            clauseList.addClause(new Clause(clause, true, ((Integer lit) -> new Literal(lit, 1))));
        }}


    public void testAddClause() {
        System.out.println("addClause");
        ClauseList cl = new ClauseList(true,true,monitor);
        Model model = new Model(10);
        cl.initialize("Test",model,symboltable);
        Clause clause1 = makeClause(new int[]{1,nor,1,-2,3});
        Clause clause2 = makeClause(new int[]{2,natl, 2,3,2,1,1});
        Clause clause3 = makeClause(new int[]{3,natm, 2,4,3,3,2});
        Clause clause4 = makeClause(new int[]{4,nex, 2,-3,-3,2});
        Clause clause5 = makeClause(new int[]{5,nint,2,4,-3,-3,2,4,4,5});
        Clause clause6 = makeClause(new int[]{6, nint,2,3,1,-2,1,3,2,2,4});
        cl.addClause(clause1);
        cl.addClause(clause2);
        cl.addClause(clause3);
        cl.addClause(clause4);
        cl.addClause(clause5);
        cl.addClause(clause6);
        System.out.println(clause6.inferenceSteps.get(0).toString(null));
        assertTrue(clause6.inferenceSteps.get(0).verify(mon,null));
        assertEquals("Clauses:\n" +
                "  1: 1v-2v3\n" +
                "  2: >=2 3,2,1^2\n" +
                "  3: <=2 4,3^2,2\n" +
                "  4: =2 -3^2,2\n" +
                "  5: [2,4] -3^2,2,4^2,5\n" +
                "  6: [1,2] 1^2,3,2,4\n" +
                "\n" +
                "\n" +
                "Index:\n" +
                "1:       1: 1v-2v3\n" +
                "         2: >=2 3,2,1^2\n" +
                "         6: [1,2] 1^2,3,2,4\n" +
                "2:       2: >=2 3,2,1^2\n" +
                "         3: <=2 4,3^2,2\n" +
                "         4: =2 -3^2,2\n" +
                "         5: [2,4] -3^2,2,4^2,5\n" +
                "         6: [1,2] 1^2,3,2,4\n" +
                "-2:      1: 1v-2v3\n" +
                "3:       1: 1v-2v3\n" +
                "         2: >=2 3,2,1^2\n" +
                "         3: <=2 4,3^2,2\n" +
                "         6: [1,2] 1^2,3,2,4\n" +
                "-3:      4: =2 -3^2,2\n" +
                "         5: [2,4] -3^2,2,4^2,5\n" +
                "4:       3: <=2 4,3^2,2\n" +
                "         5: [2,4] -3^2,2,4^2,5\n" +
                "         6: [1,2] 1^2,3,2,4\n" +
                "5:       5: [2,4] -3^2,2,4^2,5\n",cl.toString("all",null));
    }

    public void testRemoveClause() {
        System.out.println("removeClause");
        ClauseList cl = new ClauseList(true,true,monitor);
        Model model = new Model(10);
        cl.initialize("Test",model,symboltable);
        Clause clause1 = makeClause(new int[]{1,nor,1,-2,3});
        Clause clause2 = makeClause(new int[]{2,natl, 2,3,2,1,1});
        Clause clause3 = makeClause(new int[]{3,natm, 2,4,3,3,2});
        cl.addClause(clause1);
        cl.addClause(clause2);
        cl.addClause(clause3);
        cl.removeClause(clause2);
        cl.removeClauseFromIndex(clause2);
        assertEquals("Clauses:\n" +
                "  1: 1v-2v3\n" +
                "  3: <=2 4,3^2,2\n" +
                "\n" +
                "\n" +
                "Index:\n" +
                "1:       1: 1v-2v3\n" +
                "2:       3: <=2 4,3^2,2\n" +
                "-2:      1: 1v-2v3\n" +
                "3:       1: 1v-2v3\n" +
                "         3: <=2 4,3^2,2\n" +
                "4:       3: <=2 4,3^2,2\n",cl.toString("all", null));

        cl.removeClause(clause1);
        cl.removeClauseFromIndex(clause1);
        cl.removeClause(clause3);
        cl.removeClauseFromIndex(clause3);
        assertTrue(cl.isEmpty());
        assertEquals("", cl.toString("all", null));

    }


    public void testApplyTrueLiteral() throws Unsatisfiable {
        System.out.println("apply true literal");
        ClauseList cl = new ClauseList(true,true,monitor);
        Model model = new Model(10);
        cl.initialize("Test",model,symboltable);
        cl.addClause(makeClause(new int[]{1,nor,1,2,3,4}));
        cl.addClause(makeClause(new int[]{2,natl,2,1,2,3,4,1}));
        cl.addClause(makeClause(new int[]{3,natm,2,1,2,3,4,1}));
        cl.addClause(makeClause(new int[]{4,nint,2,3,1,2,3,4,5,6}));
        Clause cAnd = makeClause(new int[]{10,nand,1,2});
        InferenceStep step = new InfTrueLiteralInClause(cAnd.simpleClone(),null,1);


        assertEquals ("Clauses:\n" +
                "  1: 1v2v3v4\n" +
                "  2: >=2 1^2,2,3,4\n" +
                "  3: <=2 1^2,2,3,4\n" +
                "  4: [2,3] 1,2,3,4,5,6\n" +
                "\n" +
                "\n" +
                "Index:\n" +
                "1:       1: 1v2v3v4\n" +
                "         2: >=2 1^2,2,3,4\n" +
                "         3: <=2 1^2,2,3,4\n" +
                "         4: [2,3] 1,2,3,4,5,6\n" +
                "2:       1: 1v2v3v4\n" +
                "         2: >=2 1^2,2,3,4\n" +
                "         3: <=2 1^2,2,3,4\n" +
                "         4: [2,3] 1,2,3,4,5,6\n" +
                "3:       1: 1v2v3v4\n" +
                "         2: >=2 1^2,2,3,4\n" +
                "         3: <=2 1^2,2,3,4\n" +
                "         4: [2,3] 1,2,3,4,5,6\n" +
                "4:       1: 1v2v3v4\n" +
                "         2: >=2 1^2,2,3,4\n" +
                "         3: <=2 1^2,2,3,4\n" +
                "         4: [2,3] 1,2,3,4,5,6\n" +
                "5:       4: [2,3] 1,2,3,4,5,6\n" +
                "6:       4: [2,3] 1,2,3,4,5,6\n",cl.toString("all",null));
        cl.applyTrueLiteral(1,step);
        assertEquals("Clauses:\n" +
                "4.1: [1,2] 2,3,4,5,6\n" +
                "\n" +
                "\n" +
                "Queue:\n" +
                "TRUELITERAL: Literal -2\n" +
                "  True Literal in Clause: 3.1: =0 2,3,4 -> -2\n" +
                "TRUELITERAL: Literal -3\n" +
                "  True Literal in Clause: 3.1: =0 2,3,4 -> -3\n" +
                "TRUELITERAL: Literal -4\n" +
                "  True Literal in Clause: 3.1: =0 2,3,4 -> -4\n" +
                "SHORTENED_CLAUSE Clause: 4.1: [1,2] 2,3,4,5,6\n" +
                "\n" +
                "\n" +
                "Index:\n" +
                "2:     4.1: [1,2] 2,3,4,5,6\n" +
                "3:     4.1: [1,2] 2,3,4,5,6\n" +
                "4:     4.1: [1,2] 2,3,4,5,6\n" +
                "5:     4.1: [1,2] 2,3,4,5,6\n" +
                "6:     4.1: [1,2] 2,3,4,5,6\n", cl.toString("all",null));

        Clause c4 = cl.clauses.firstLinkedItem;
        assertTrue(c4.inferenceSteps.get(0).verify(mon,null));
        assertTrue(c4.inferenceSteps.get(1).verify(mon,null));
        System.out.println(c4.inferenceSteps.get(0).toString(null));
        System.out.println(c4.inferenceSteps.get(1).toString(null));

        System.out.println("\nExample 2");
        model = new Model(10);
        cAnd = makeClause(new int[]{11,nand,2,-1});
        step = new InfTrueLiteralInClause(cAnd.simpleClone(),null,-1);

        cl.initialize("Test",model,symboltable);
        cl.addClause(makeClause(new int[]{1,nor,1,2,3,4}));
        cl.addClause(makeClause(new int[]{2,natl,2,1,2,3,4,1}));
        cl.addClause(makeClause(new int[]{3,natm,2,1,2,3,4,1}));
        c4 = makeClause(new int[]{4,nint,2,3,1,2,3,4,5,6});
        cl.addClause(c4);

        cl.applyTrueLiteral(-1,step);
        assertEquals("Clauses:\n" +
                "1.1: 2v3v4\n" +
                "2.1: >=2 2,3,4\n" +
                "3.1: <=2 2,3,4\n" +
                "4.1: [2,3] 2,3,4,5,6\n" +
                "\n" +
                "\n" +
                "Queue:\n" +
                "SHORTENED_CLAUSE Clause: 1.1: 2v3v4\n" +
                "SHORTENED_CLAUSE Clause: 2.1: >=2 2,3,4\n" +
                "SHORTENED_CLAUSE Clause: 3.1: <=2 2,3,4\n" +
                "SHORTENED_CLAUSE Clause: 4.1: [2,3] 2,3,4,5,6\n" +
                "\n" +
                "\n" +
                "Index:\n" +
                "2:     1.1: 2v3v4\n" +
                "       2.1: >=2 2,3,4\n" +
                "       3.1: <=2 2,3,4\n" +
                "       4.1: [2,3] 2,3,4,5,6\n" +
                "3:     1.1: 2v3v4\n" +
                "       2.1: >=2 2,3,4\n" +
                "       3.1: <=2 2,3,4\n" +
                "       4.1: [2,3] 2,3,4,5,6\n" +
                "4:     1.1: 2v3v4\n" +
                "       2.1: >=2 2,3,4\n" +
                "       3.1: <=2 2,3,4\n" +
                "       4.1: [2,3] 2,3,4,5,6\n" +
                "5:     4.1: [2,3] 2,3,4,5,6\n" +
                "6:     4.1: [2,3] 2,3,4,5,6\n", cl.toString("all", null));
        assertTrue(c4.inferenceSteps.get(0).verify(mon,null));
        assertTrue(c4.inferenceSteps.get(1).verify(mon,null));
        System.out.println(c4.inferenceSteps.get(0).toString(null));
        System.out.println(c4.inferenceSteps.get(1).toString(null));
    }

    public void testSubsumesByModels() {
        System.out.println("subsumes by models");
        ClauseList cl = new ClauseList(true, true, monitor);
        Model model = new Model(10);
        cl.initialize("Test", model, symboltable);
        Clause c1 = makeClause(new int[]{1, nor, 1, 2, 3, 4});
        Clause c2 = makeClause(new int[]{2, nor, 1, 3, 2, 5});
        Clause c3 = makeClause(new int[]{3, nor, 3,2,1});

        assertTrue(cl.subsumesByModels(c1,c1));
        assertTrue(cl.subsumesByModels(c3,c1));
        assertFalse(cl.subsumesByModels(c1,c3));
        assertFalse(cl.subsumesByModels(c1,c2));
        assertFalse(cl.subsumesByModels(c2,c1));

        Clause c4 = makeClause(new int[]{4,natl,2,1,2,3});
        Clause c5 = makeClause(new int[] {5,natl,3,1,1,2,2,3,3});
        Clause c6 = makeClause(new int[] {6,natl,2,4,3,2,1});

        assertTrue(cl.subsumesByModels(c4,c5)); // c5 would be simplified to c4
        assertTrue(cl.subsumesByModels(c5, c4));
        assertTrue(cl.subsumesByModels(c4, c6));
        assertFalse(cl.subsumesByModels(c6, c4));
    }

    public void testSubsumes() {
        System.out.println("subsumes");
        ClauseList cl = new ClauseList(true, true, monitor);
        Model model = new Model(10);
        cl.initialize("Test", model, symboltable);
        Clause c1 = makeClause(new int[]{1, nor, 1, 2, 3, 4});
        Clause c2 = makeClause(new int[]{2, nor, 1, 3, 4, 5});
        Clause c3 = makeClause(new int[]{3, nor, 3, 2, 1});
        Clause c4 = makeClause(new int[]{4,natl,2,1,2,3});
        Clause c5 = makeClause(new int[]{5,natl,2,3,1,2,4,4});
        Clause c6 = makeClause(new int[]{6,natl,2,3,1,2,2,4});
        Clause c7 = makeClause(new int[]{7,nint,2,3,1,2,3,4});
        Clause c8 = makeClause(new int[]{8,nint,2,4,1,2,3,4,5,5});
        Clause c9 = makeClause(new int[]{9,nint,2,5,1,2,3,4,5,5});
        assertTrue(cl.subsumes(c3,c1));
        assertTrue(cl.subsumes(c4,c5));
        assertFalse(cl.subsumes(c3,c4));
        assertTrue(cl.subsumes(c4,c6));
        assertFalse(cl.subsumes(c7,c8)); // falsifying model: 1,2,3,-4,5
        assertTrue(cl.subsumes(c7,c9));
    }

    public void testIsSubsumed() {
        System.out.println("isSubsumed");
        ClauseList cl = new ClauseList(true, true, monitor);
        Model model = new Model(10);
        cl.initialize("Test", model, null);
        cl.addClause(makeClause(new int[]{1,nor,1,2,3}));
        cl.addClause(makeClause(new int[]{2,nor,1,4,5}));
        Clause c3 = makeClause(new int[]{3,nor, 1,2,3,4});
        Clause c4 = makeClause(new int[]{4,nor, 2,1,4});
        assertNull(cl.isSubsumed(c4));
        assertEquals(1,cl.isSubsumed(c3).id);
        cl.addClause(makeClause(new int[]{6,natl,3,3,3,2,2,1,1}));
        Clause c7 = makeClause(new int[]{7, natl, 2, 1,2,3});
        assertEquals(6,cl.isSubsumed(c7).id);
    }

    public void testRemoveSubsumedClauses() {
        System.out.println("removeSubsumedClauses");
        ClauseList cl = new ClauseList(true, true, monitor);
        Model model = new Model(10);
        cl.initialize("Test", model, null);
        cl.addClause(makeClause(new int[]{1, nor, 1, 2, 3}));
        cl.addClause(makeClause(new int[]{2, nor, 2, 3, 4}));
        cl.addClause(makeClause(new int[]{3, nor, 3, 4, 5}));
        Clause c4 = makeClause(new int[]{4, nor, 2,3});
        cl.addClause(c4);
        cl.removeSubsumedClauses(c4);
        assertEquals("  3: 3v4v5\n" +
                "  4: 2v3\n", cl.toString("clauses", null));
    }

    public void testResolve() throws Unsatisfiable{
        System.out.println("resolve");
        ClauseList cl = new ClauseList(true, true, monitor);
        Model model = new Model(10);
        cl.initialize("Test", model, null);
        Clause c1 = makeClause(new int[]{1, nor, 1, 2});
        cl.addClause(c1);
        Clause c2 = makeClause(new int[]{2, nor, -1, 2});
        cl.addClause(c2);
        cl.resolve(c1,1, c2);
        assertEquals("Queue:\n" +
                "TRUELITERAL: Literal 2\n" +
                "  Unit Clause 1.1: 2\n",cl.toString("all",null));

        System.out.println("Example 2");
        cl.model.clear();
        cl.initialize("Test",model,null);
        c1 = makeClause(new int[]{1, nor, 1, 2});
        cl.addClause(c1);
        Clause c3 = makeClause(new int[]{3, nor, 2, -1, 3});
        cl.addClause(c3);
        cl.queue.clear();
        cl.resolve(c1,1, c3);
        assertEquals("Clauses:\n" +
                "  1: 1v2\n" +
                "3.1: 2v3\n" +
                "\n" +
                "\n" +
                "Index:\n" +
                "1:       1: 1v2\n" +
                "2:       1: 1v2\n" +
                "       3.1: 2v3\n" +
                "3:     3.1: 2v3\n",cl.toString("all",null));

        System.out.println("Example 3");
        cl.model.clear();
        cl.initialize("Test",model,null);
        c1 = makeClause(new int[]{1, nor, 1, 2, -3});
        cl.addClause(c1);
        c2 = makeClause(new int[]{2, natl, 2, 1, 2,4, 3,3});
        cl.addClause(c2);
        cl.resolve(c1, -3, c2);
        assertEquals("Clauses:\n" +
                "1.1: 1v2\n" +
                "  2: >=2 1,2,4,3^2\n" +
                "\n" +
                "\n" +
                "Index:\n" +
                "1:     1.1: 1v2\n" +
                "         2: >=2 1,2,4,3^2\n" +
                "2:     1.1: 1v2\n" +
                "         2: >=2 1,2,4,3^2\n" +
                "3:       2: >=2 1,2,4,3^2\n" +
                "4:       2: >=2 1,2,4,3^2\n",cl.toString("all",null));

        System.out.println("Example 4");
        cl.model.clear();
        cl.initialize("Test",model,null);
        c1 = makeClause(new int[]{1, nor, 1, 2, -3, 4});
        cl.addClause(c1);
        c2 = makeClause(new int[]{2, natl, 3, 1, 2,4, 3,3,3});
        cl.addClause(c2);
        cl.resolve(c1, -3, c2);
        assertEquals("Clauses:\n" +
                "1.1: 1v2v4\n" +
                "  2: >=3 1,2,4,3^3\n" +
                "\n" +
                "\n" +
                "Index:\n" +
                "1:     1.1: 1v2v4\n" +
                "         2: >=3 1,2,4,3^3\n" +
                "2:     1.1: 1v2v4\n" +
                "         2: >=3 1,2,4,3^3\n" +
                "3:       2: >=3 1,2,4,3^3\n" +
                "4:     1.1: 1v2v4\n" +
                "         2: >=3 1,2,4,3^3\n",cl.toString("all",null));
    }

    public void testMergeResolution() throws Unsatisfiable {
        System.out.println("mergeResolution");
        ClauseList cl = new ClauseList(true, true, monitor);
        Model model = new Model(10);
        cl.initialize("Test", model, null);
        Clause c1 = makeClause(new int[]{1, nor, 1, 2});
        cl.addClause(c1);
        Clause c2 = makeClause(new int[]{2, nor, -1, 2});
        cl.addClause(c2);
        cl.mergeResolution(c1);
        assertEquals("Queue:\n" +
                "TRUELITERAL: Literal 2\n" +
                "  Unit Clause 1.1: 2\n", cl.toString("all", null));
        assertEquals("2",model.toString());

        System.out.println("Example 2");
        model.clear();
        cl.initialize("Test", model, null);
        c1 = makeClause(new int[]{1, nor, 1, 2});
        cl.addClause(c1);
        c2 = makeClause(new int[]{2, nor, -1, 2, 3});
        cl.addClause(c2);
        Clause c3 = makeClause(new int[]{3, nor, 2, 3, -1 , 4});
        cl.addClause(c3);
        Clause c4 = makeClause(new int[]{4, nor, 1, 2, 3});
        cl.addClause(c4);
        cl.mergeResolution(c1);
        assertEquals("Clauses:\n" +
                "  1: 1v2\n" +
                "2.1: 2v3\n" +
                "3.1: 2v3v4\n" +
                "  4: 1v2v3\n" +
                "\n" +
                "\n" +
                "Queue:\n" +
                "SHORTENED_CLAUSE Clause: 2.1: 2v3\n" +
                "SHORTENED_CLAUSE Clause: 3.1: 2v3v4\n" +
                "\n" +
                "\n" +
                "Index:\n" +
                "1:       1: 1v2\n" +
                "         4: 1v2v3\n" +
                "2:       1: 1v2\n" +
                "       2.1: 2v3\n" +
                "       3.1: 2v3v4\n" +
                "         4: 1v2v3\n" +
                "3:     2.1: 2v3\n" +
                "       3.1: 2v3v4\n" +
                "         4: 1v2v3\n" +
                "4:     3.1: 2v3v4\n", cl.toString("all", null));

        System.out.println("Example 3");
        model.clear();
        cl.initialize("Test", model, null);
        c1 = makeClause(new int[]{1, nor, 1, 2, 3, 4});
        cl.addClause(c1);
        c2 = makeClause(new int[]{2, nor, -1, 2, 3, 4});
        cl.addClause(c2);
        c3 = makeClause(new int[]{3, nor, -2, 3, 4});
        cl.addClause(c3);
        c4 = makeClause(new int[]{4, nor,  5, 3, 4, 6});
        cl.addClause(c4);
        cl.mergeResolution(c1);
        assertEquals("Clauses:\n" +
                "1.2: 3v4\n" +
                "\n" +
                "\n" +
                "Index:\n" +
                "3:     1.2: 3v4\n" +
                "4:     1.2: 3v4\n", cl.toString("all", null));

        //System.out.println(cl.statistics.toString());

    }

    public void testResolveLinked() throws Unsatisfiable {
        System.out.println("resolveLinked");
        ClauseList cl = new ClauseList(true, true, monitor);
        Model model = new Model(10);
        cl.initialize("Test", model, null);
        Clause c1 = makeClause(new int[]{1, nor, 1, 3, 4, 5});
        cl.addClause(c1);
        Clause c2 = makeClause(new int[]{2, nor, 2, 3, 4, 5});
        cl.addClause(c2);
        Clause c3 = makeClause(new int[]{3, nor, -1,-2});
        cl.addClause(c3);
        assertEquals(c1, cl.resolveLinked(c3,c1,c2));

        assertEquals("Clauses:\n" +
                "1.1: 3v4v5\n" +
                "  3: -1v-2\n" +
                "\n" +
                "\n" +
                "Index:\n" +
                "-1:      3: -1v-2\n" +
                "-2:      3: -1v-2\n" +
                "3:     1.1: 3v4v5\n" +
                "4:     1.1: 3v4v5\n" +
                "5:     1.1: 3v4v5\n", cl.toString("all", null));

        System.out.println(cl.statistics.toString());
        System.out.println("Example 2");
        model.clear();
        cl.initialize("Test", model, null);

        c1 = makeClause(new int[]{1, nor, 3, 1, 4, 5});
        cl.addClause(c1);
        c2 = makeClause(new int[]{2, nor, 3, 4,2, 5, 6});
        cl.addClause(c2);
        c3 = makeClause(new int[]{3, nor, -1,-2});
        cl.addClause(c3);
        assertEquals(c2,cl.resolveLinked(c3,c1,c2));
        assertTrue(c2.inferenceSteps.get(c2.inferenceSteps.size()-1).verify(mon,null));

        assertEquals("  1: 3v1v4v5\n" +
                "2.1: 3v4v5v6\n" +
                "  3: -1v-2\n", cl.toString("clauses", null));

        System.out.println("Example 3");
        model.clear();
        cl.initialize("Test", model, null);

        c1 = makeClause(new int[]{1, nor, 3, 1, 4, 5, 6});
        cl.addClause(c1);
        c2 = makeClause(new int[]{2, nor, 3, 4,2, 5});
        cl.addClause(c2);
        c3 = makeClause(new int[]{3, nor, -1,-2});
        cl.addClause(c3);
        assertEquals(c1,cl.resolveLinked(c3,c1,c2));

        assertEquals("1.1: 3v4v5v6\n" +
                "  2: 3v4v2v5\n" +
                "  3: -1v-2\n", cl.toString("clauses", null));

        assertTrue(c1.inferenceSteps.get(c1.inferenceSteps.size()-1).verify(mon,null));


        System.out.println("Example 4");
        model.clear();
        cl.initialize("Test", model, null);

        c1 = makeClause(new int[]{1, nor, 3, 1});
        cl.addClause(c1);
        c2 = makeClause(new int[]{2, nor, 3, 2});
        cl.addClause(c2);
        c3 = makeClause(new int[]{3, nor, -1,-2});
        cl.addClause(c3);
        assertEquals(c1,cl.resolveLinked(c3,c1,c2));

        assertEquals("  3: -1v-2\n", cl.toString("clauses", null));
        assertEquals("3",model.toString(null));
        assertTrue(c1.inferenceSteps.get(c1.inferenceSteps.size()-1).verify(mon,null));

    }


    public void testLinkedMergeResolution() throws Unsatisfiable {
        System.out.println("linkedMergeResolution");
        ClauseList cl = new ClauseList(true, true, monitor);
        Model model = new Model(10);
        cl.initialize("Test", model, null);
        Clause c1 = makeClause(new int[]{1, nor, 1, 3, 4, 5});
        cl.addClause(c1);
        Clause c2 = makeClause(new int[]{2, nor, 2, 3, 4, 5});
        cl.addClause(c2);
        Clause c3 = makeClause(new int[]{3, nor, -1, -2});
        cl.addClause(c3);
        cl.linkedMergeResolution(c3);

        assertEquals("1.1: 3v4v5\n" +
                        "  3: -1v-2\n",
                 cl.toString("clauses", null));

        System.out.println("Example 2");

        model = new Model(10);
        cl.initialize("Test", model, null);
        makeClauses(cl,
                new int[]{1,nor, 1,3,4},
                new int[]{2,nor, 3,1,5},
                new int[]{3,nor, 4,1,5,6},
                new int[]{4,nor, 2,4,3},
                new int[]{5,nor, 2,5,3},
                new int[]{6,nor, 6,5,2});
        Clause c = makeClause(new int[]{10, nor, -1, -2});
        cl.addClause(c);
        cl.linkedMergeResolution(c);
        assertEquals(" 1.1: 3v4\n" +
                        " 2.1: 3v5\n" +
                        " 3.1: 4v5v6\n" +
                        "   6: 6v5v2\n" +
                        "  10: -1v-2\n",
                cl.toString("clauses", null));
    }


    public void testIsPositivelyPure() {
        System.out.println("isPositivelyPure");
        ClauseList cl = new ClauseList(true, true, monitor);
        Model model = new Model(10);
        cl.initialize("Test", model, null);
        makeClauses(cl,
                new int[]{1,nor,1,2,3},
                new int[]{2,natl, 2,-1,2,-3,},
                new int[]{3,natm,2,-2,1,-3});
        assertTrue(cl.isPositivelyPure(2));
        assertFalse(cl.isPositivelyPure(1));
        assertFalse(cl.isPositivelyPure(3));
    }

    public void testIsNegativelyPure() {
        System.out.println("isNegativelyPure");
        ClauseList cl = new ClauseList(true, true, monitor);
        Model model = new Model(10);
        cl.initialize("Test", model, null);
        makeClauses(cl,
                new int[]{1,nor,1,-2,3},
                new int[]{2,natl, 2,-1,-2,-3,},
                new int[]{3,natm,2,2,1,-3});
        assertTrue(cl.isNegativelyPure(2));
        assertFalse(cl.isNegativelyPure(1));
        assertFalse(cl.isNegativelyPure(3));
    }

    public void testIsSingletonPure() {
        System.out.println("isSingletonPure");
        ClauseList cl = new ClauseList(true, true, monitor);
        Model model = new Model(10);
        cl.initialize("Test", model, null);
        makeClauses(cl,
                new int[]{1,nor,       1,-2,-4,-6},
                new int[]{2,nint, 1,2, 1,2,3,4},
                new int[]{3,nint,1,2,      3,4,-5,6});
        assertEquals(0, cl.isSingletonPure(1));
        assertEquals(0, cl.isSingletonPure(2));
        assertEquals(0, cl.isSingletonPure(3));
        assertEquals(0, cl.isSingletonPure(4));
        assertEquals(-5, cl.isSingletonPure(5));
        assertEquals(0, cl.isSingletonPure(6));
    }

    public void testRemovePurePredicate() throws Unsatisfiable{
        System.out.println("removePurePredicate");
        ClauseList cl = new ClauseList(true, true, monitor);
        Model model = new Model(10);
        cl.initialize("Test", model, null);
        makeClauses(cl,
                new int[]{1,nor,1,2,3},
                new int[]{2,natl, 2,-1,2,-3,},
                new int[]{3,natm,2,-2,1,-3});
        assertTrue(cl.removePurePredicate(2));
        assertEquals("Clauses:\n" +
                "2.1: -1v-3\n" +
                "\n" +
                "\n" +
                "Queue:\n" +
                "SHORTENED_CLAUSE Clause: 2.1: -1v-3\n" +
                "\n" +
                "\n" +
                "Index:\n" +
                "-1:    2.1: -1v-3\n" +
                "-3:    2.1: -1v-3\n", cl.toString("all", null));

        System.out.println("\nExample 2");
        model.clear();
        cl.initialize("Test", model, null);
        makeClauses(cl,
                new int[]{1,nint,1,2, 1,2,3},
                new int[]{2,nint,1,2,   2,3,4,},
                new int[]{3,nint,1,2,       4,5,6});
        assertFalse(cl.removePurePredicate(2));
        assertTrue(cl.removePurePredicate(1));
        assertEquals("Clauses:\n" +
                "  2: [1,2] 2,3,4\n" +
                "  3: [1,2] 4,5,6\n" +
                "\n" +
                "\n" +
                "Singletons:\n" +
                "Singleton Literals:\n" +
                "1 in clause 1: [1,2] 1,2,3\n" +
                "Index:\n" +
                "2:       2: [1,2] 2,3,4\n" +
                "3:       2: [1,2] 2,3,4\n" +
                "4:       2: [1,2] 2,3,4\n" +
                "         3: [1,2] 4,5,6\n" +
                "5:       3: [1,2] 4,5,6\n" +
                "6:       3: [1,2] 4,5,6\n",cl.toString("all",null));
    }

    public void testRemovePureLiterals() throws Unsatisfiable {
        System.out.println("removePureLiterals");
        ClauseList cl = new ClauseList(true, true, monitor);
        Model model = new Model(10);
        cl.initialize("Test", model, null);
        makeClauses(cl,
                new int[]{1, nor, 1, -2, 3},
                new int[]{2, natl, 2, -1, -2, -3,},
                new int[]{3, natm, 2, 2, 1, -3});
        assertTrue(cl.removePureLiterals());
        assertEquals("Queue:\n" +
                "SHORTENED_CLAUSE Clause: 2.2: >=0 -1\n", cl.toString("all", null));
        System.out.println(cl.statistics.toString());
        ;
    }

    public void testPythagoraenTriples() throws Result {
        System.out.println("Pythagoraen Triples");
        int max = 60;
        StringBuilder errors = new StringBuilder();
        PythagoraenTriples ptr = new PythagoraenTriples(3, max);
        InputClauses inputClauses = ptr.generateProblem(errors);
        ClauseList cl = new ClauseList(true, true, monitor);
        Model model = new Model(max);
        cl.initialize("Test", model, null);

        for (int[] clause : inputClauses.intervals) {
            cl.addClause(makeClause(clause));}

        try{cl.allClausesInserted();}
        catch(Satisfiable sat) {
            assertEquals(0,inputClauses.falseClausesInModel(model).size());
        }
        System.out.println(cl.toString("clauses",null));
        assertEquals("3,4,-5,-6,-7,8,-9,-10,12,-13,-14,-15,16,17,-18,-20,21,24,25,-26,-27,\n" +
                "28,-29,30,-32,33,-34,-35,36,-37,-39,40,-41,-42,44,-45,48,-50,51,-52,-53,\n" +
                "-55,-58,-60", model.toString(null));

    }

}