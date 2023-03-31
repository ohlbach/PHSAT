package Solvers.Walker;

import Datastructures.Clauses.Quantifier;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Theory.Model;
import junit.framework.TestCase;

import java.util.Random;

public class WalkerTest extends TestCase {
    private static int cOr = Quantifier.OR.ordinal();
    private static int cAtleast = Quantifier.ATLEAST.ordinal();
    private static int cAtmost = Quantifier.ATMOST.ordinal();
    private static int cExactly = Quantifier.EXACTLY.ordinal();
    private static int cInterval = Quantifier.INTERVAL.ordinal();

    private static Walker MyWalker(int predicates) {
        Walker walker = new Walker(1,null,0,10,10);
        walker.predicates = predicates;
        walker.model = new Model(predicates);
        walker.literals = new Literals(predicates);
        walker.localModel = new boolean[predicates+1];
        walker.random = new Random(0);
        walker.flipScores = new float[predicates+1];
        walker.predicatesWithPositiveScore = new Predicates(predicates);
        return walker;
    }

    public void testHelp() {
        System.out.println(Walker.help());
    }

    public void testMakeSolvers() {
    }

    public void testSolveProblem() {
    }

    public void testReadInputClauses() {
    }

    public void testInsertClause() throws Unsatisfiable {
        System.out.println("insertClause");
        Walker walker = MyWalker(10);
        int[] clause1 = new int[]{1,cOr,1,2,-3};
        walker.insertClause(clause1);
        assertEquals("1: 1v2v-3\n",walker.toString("clauses"));
        assertEquals("Positive Literals:\n" +
                "1:1,2:1,\n" +
                "Negative Literals:\n" +
                "-3:1,",walker.toString("literals"));
        assertEquals("2@1,",walker.literals.toString(2));
        assertEquals("-3@1,",walker.literals.toString(-3));
        assertEquals("",walker.literals.toString(-2));

        int[] clause2 = new int[]{2,cOr,-1,2,3};
        walker.insertClause(clause2);
        assertEquals("1: 1v2v-3\n" +
                "2: -1v2v3\n",walker.toString("clauses"));
        assertEquals("Positive Literals:\n" +
                "1:1,2:2,3:1,\n" +
                "Negative Literals:\n" +
                "-1:1,-3:1,",walker.toString("literals"));
        assertEquals("2@2,2@1,",walker.literals.toString(2));

        int[] clause3 = new int[]{3,cOr,-1,2,3,1};
        walker.insertClause(clause3);
        assertEquals("1: 1v2v-3\n" +
                "2: -1v2v3\n",walker.toString("clauses"));
    }

    public void testInsertClause2() throws Unsatisfiable {
        System.out.println("insertClause 2");
        Walker walker = MyWalker(10);
        int[] clause1 = new int[]{1, cAtmost, 2, 1, 2, -1, 2,-2,3,-3};
        try{walker.insertClause(clause1);}
        catch(Unsatisfiable unsatisfiable) {
           // System.out.println(unsatisfiable.toString());
        }

        int[] clause2 =  new int[]{2,cInterval,0,2,1,2,3,-2,-3,4};
        walker.insertClause(clause2);
        assertEquals("",walker.toString("clauses"));
        assertEquals("-1,-4",walker.model.toString());

        int[] clause3 =  new int[]{3,cInterval,0,1,5,6,5,6,7};
        walker.insertClause(clause3);
        assertEquals("",walker.toString("clauses"));
        assertEquals("-1,-4,-5,-6",walker.model.toString());

    }

    public void testInitializeModel() throws Unsatisfiable {
        System.out.println("initializeModel");
        Walker walker = MyWalker(10);
        walker.model.add(5);walker.model.add(-6);
        walker.insertClause(new int[]{1, cOr, 1,2,3});
        walker.insertClause(new int[]{2, cOr, 1,-2,3});
        walker.insertClause(new int[]{3, cOr, -1,-2,3});
        walker.insertClause(new int[]{3, cOr, -1,-2,3});
        walker.initializeModel();
        assertTrue(walker.localModel[1]);
        assertFalse(walker.localModel[2]);
        assertTrue(walker.localModel[3]);
        assertTrue(walker.localModel[5]);
        assertFalse(walker.localModel[6]);
        assertEquals(Walker.trueLiteralScore,walker.flipScores[5]);
    }

    public void testFlipPredicate() {
    }

    public void testSetLocalTruth() {
        System.out.println("setLocalTruth");
        Walker walker = MyWalker(10);
        walker.localModel[1] = true;
        Clause clause1 = new Clause(new int[]{1,cOr,1,2,3});
        assertTrue(walker.setLocalTruth(clause1));
        assertTrue(clause1.isLocallyTrue);
        assertEquals(1,clause1.trueLiterals);

        walker.localModel[2] = true; walker.localModel[3] = false;
        Clause clause2 = new Clause(new int[]{2,cInterval,0,2, 1,2,-3,4});
        assertFalse(walker.setLocalTruth(clause2));
        assertFalse(clause2.isLocallyTrue);
        assertEquals(3,clause2.trueLiterals);

    }

    public void testSetInitialFlipScores() {
    }

    public void testUpdateFlipScores() {
    }
}