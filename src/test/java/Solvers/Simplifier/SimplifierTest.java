package Solvers.Simplifier;

import Datastructures.Clauses.Connective;
import Datastructures.Results.Result;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import Management.Monitor.Monitor;
import Management.Monitor.MonitorLife;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import junit.framework.TestCase;

import java.util.function.IntSupplier;

public class SimplifierTest extends TestCase {

    static int cOr = Connective.OR.ordinal();
    static int cAtleast = Connective.ATLEAST.ordinal();

    static boolean monitoring = true;

    static Symboltable symboltable = new Symboltable(10);
    static {symboltable.setName(1,"p");
        symboltable.setName(2,"q");
        symboltable.setName(3,"r");
        symboltable.setName(4,"s");
        symboltable.setName(5,"t");}

    public void testInsertClause() throws Result{
        System.out.println("insertClause");
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = ()->++id[0];
        Simplifier simplifier = new Simplifier(10,monitor,true,nextId);
        Clause clause1 = new Clause(new int[]{1,cOr,1,2,3});
        simplifier.insertClause(clause1);
        Clause clause2 = new Clause(new int[]{2,cAtleast, 2,1,1,2,2,-3,-3});
        simplifier.insertClause(clause2);
        assertEquals("1: 1v2v3\n" +
                "2: >= 2 1^2,2^2,-3^2\n",simplifier.clauses.toString());
        assertEquals("1: pvqvr\n" +
                "2: >= 2 p^2,q^2,-r^2\n",simplifier.clauses.toString(symboltable));
        Clause clause3 = new Clause(new int[]{3,cOr,4,-5});
        simplifier.insertClause(clause3);
        assertEquals("Positive Literals:\n" +
                "4:1,\n" +
                "Negative Literals:\n" +
                "-5:1,",simplifier.literalIndexTwo.toString());
        assertEquals("Positive Literals:\n" +
                "1:2,2:2,3:1,\n" +
                "Negative Literals:\n" +
                "-3:1,",simplifier.literalIndexMore.toString());

        simplifier.removeClause(clause1,false); // no purity check
        assertEquals("2: >= 2 1^2,2^2,-3^2\n" +
                "3: 4v-5\n",simplifier.clauses.toString());

        assertEquals("Positive Literals:\n" +
                "4:1,\n" +
                "Negative Literals:\n" +
                "-5:1,",simplifier.literalIndexTwo.toString());

        assertEquals("Positive Literals:\n" +
                "1:1,2:1,\n" +
                "Negative Literals:\n" +
                "-3:1,",simplifier.literalIndexMore.toString());
    }

    public void testPurityCheck() throws Result {
        System.out.println("purityCheck");
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Simplifier simplifier = new Simplifier(10, monitor, true, nextId);
        Clause clause1 = new Clause(new int[]{1, cOr, 1, 2, 3});
        simplifier.insertClause(clause1);
        Clause clause2 = new Clause(new int[]{2, cOr, -1, -2, 3});
        simplifier.insertClause(clause2);
        simplifier.removeClause(clause2,true);
        assertEquals("1: 1v2v3\n",simplifier.clauses.toString());
        assertEquals("1,2",simplifier.model.toString());
    }
    public void testSimplifyClause() throws Result {
        System.out.println("simplify clause");
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Simplifier simplifier = new Simplifier(10, monitor, true, nextId);
        IntArrayList removedLiterals = new IntArrayList();
        Clause clause = new Clause(new int[]{1, cOr, 1, 2, 3});
        simplifier.simplifyClause(clause, removedLiterals);
        assertEquals("1: 1v2v3", clause.toString());

        clause = new Clause(new int[]{2, cAtleast, 5, 1,1,2,2,3,4});
        simplifier.insertClause(clause);
        assertEquals("Positive Literals:\n" +
                "1:1,2:1,3:1,4:1,\n" +
                "Negative Literals:\n",simplifier.literalIndexMore.toString());
        simplifier.removeClause(clause,false);
        simplifier.simplifyClause(clause,removedLiterals);
        simplifier.insertClause(clause);
        assertEquals("2: 3v4", clause.toString());
        assertEquals("1,2",simplifier.model.toString());
        assertEquals("Positive Literals:\n" +
                "\n" +
                "Negative Literals:\n",simplifier.literalIndexMore.toString());
        assertEquals("Positive Literals:\n" +
                "3:1,4:1,\n" +
                "Negative Literals:\n",simplifier.literalIndexTwo.toString());

        clause = new Clause(new int[]{3, cAtleast, 6, 1,1,1,1,2,2,2,2,3,3,3,3});
        simplifier.simplifyClause(clause,removedLiterals);
        assertEquals("3: >= 3 1^2,2^2,3^2", clause.toString());

        InferenceStep step = simplifier.model.getInferenceStep(1);
        //System.out.println(step.toString(null));
        //System.out.println(step.rule());
        //System.out.println(step.inputClauseIds());
    }
    }