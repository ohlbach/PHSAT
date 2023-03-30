package Solvers.Walker;

import Datastructures.Clauses.Quantifier;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Theory.Model;
import junit.framework.TestCase;

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
        int[] clause = new int[]{1,cOr,1,2,-3};
        walker.insertClause(clause);
        assertEquals("1: 1v2v-3\n",walker.toString("clauses"));
        assertEquals("Positive Literals:\n" +
                "1:1,2:1,\n" +
                "Negative Literals:\n" +
                "-3:1,",walker.toString("literals"));
        assertEquals("2@1,",walker.literals.toString(2));
        assertEquals("-3@1,",walker.literals.toString(-3));
        assertEquals("",walker.literals.toString(-2));

    }

    public void testInitializeModelAndScores() {
    }

    public void testFlipPredicate() {
    }

    public void testSetLocalTruth() {
    }

    public void testSetInitialFlipScores() {
    }

    public void testUpdateFlipScores() {
    }
}