package Solvers.Simplifier;

import Datastructures.Clauses.InputClauses;
import Datastructures.Clauses.Quantifier;
import Datastructures.Results.Result;
import Datastructures.Results.Satisfiable;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import InferenceSteps.InfInputClause;
import InferenceSteps.InferenceStep;
import InferenceSteps.InferenceTest;
import Management.Monitor.Monitor;
import Management.Monitor.MonitorLife;
import junit.framework.TestCase;

import java.util.ArrayList;
import java.util.function.IntSupplier;

public class SimplifierTest extends TestCase {

    static int cOr = Quantifier.OR.ordinal();
    static int cAtleast = Quantifier.ATLEAST.ordinal();
    static int cAtmost = Quantifier.ATMOST.ordinal();
    static int cExactly = Quantifier.EXACTLY.ordinal();
    static int cInterval = Quantifier.INTERVAL.ordinal();


    static boolean monitoring = false;

    static Symboltable symboltable = new Symboltable(10);

    static {
        symboltable.setName(1, "p");
        symboltable.setName(2, "q");
        symboltable.setName(3, "r");
        symboltable.setName(4, "s");
        symboltable.setName(5, "t");
    }

    public void testInsertClause() throws Result {
        System.out.println("insertClause");
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Simplifier simplifier = new Simplifier(10, monitor, true, nextId);
        Clause clause1 = new Clause(new int[]{1, cOr, 1, 2, 3});
        simplifier.insertClause(clause1);
        Clause clause2 = new Clause(new int[]{2, cAtleast, 2, 1, 1, 2, 2, -3, -3});
        simplifier.insertClause(clause2);
        assertEquals("1: 1v2v3\n" +
                "2: >= 2 1^2,2^2,-3^2\n", simplifier.clauses.toString());
        assertEquals("1: pvqvr\n" +
                "2: >= 2 p^2,q^2,-r^2\n", simplifier.clauses.toString(symboltable));
        Clause clause3 = new Clause(new int[]{3, cOr, 4, -5});
        simplifier.insertClause(clause3);
        assertEquals("Positive Literals:\n" +
                "4:1,\n" +
                "Negative Literals:\n" +
                "-5:1,", simplifier.literalIndexTwo.toString());
        assertEquals("Positive Literals:\n" +
                "1:2,2:2,3:1,\n" +
                "Negative Literals:\n" +
                "-3:1,", simplifier.literalIndexMore.toString());

        simplifier.removeClause(clause1, false); // no purity check
        assertEquals("2: >= 2 1^2,2^2,-3^2\n" +
                "3: 4v-5\n", simplifier.clauses.toString());

        assertEquals("Positive Literals:\n" +
                "4:1,\n" +
                "Negative Literals:\n" +
                "-5:1,", simplifier.literalIndexTwo.toString());

        assertEquals("Positive Literals:\n" +
                "1:1,2:1,\n" +
                "Negative Literals:\n" +
                "-3:1,", simplifier.literalIndexMore.toString());
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
        simplifier.removeClause(clause2, true);
        assertEquals("1: 1v2v3\n", simplifier.clauses.toString());
        assertEquals("1,2,3", simplifier.model.toString());
    }

    public void testSimplifyClause() throws Unsatisfiable {
        System.out.println("simplify clause");
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Simplifier simplifier = new Simplifier(10, monitor, true, nextId);
        Clause clause = new Clause(new int[]{1, cOr, 1, 2, 3});
        simplifier.simplifyClause(clause, false);
        assertEquals("1: 1v2v3", clause.toString());

        clause = new Clause(new int[]{2, cAtleast, 5, 1, 1, 2, 2, 3, 4});
        simplifier.insertClause(clause);
        assertEquals("Positive Literals:\n" +
                "1:1,2:1,3:1,4:1,\n" +
                "Negative Literals:\n", simplifier.literalIndexMore.toString());
        simplifier.simplifyClause(clause, false);
        assertEquals("2: 3v4", clause.toString());
        assertEquals("1,2", simplifier.model.toString());
        assertEquals("Positive Literals:\n" +
                "\n" +
                "Negative Literals:\n", simplifier.literalIndexMore.toString());
        assertEquals("Positive Literals:\n" +
                "3:1,4:1,\n" +
                "Negative Literals:\n", simplifier.literalIndexTwo.toString());

        clause = new Clause(new int[]{3, cAtleast, 6, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3});
        simplifier.simplifyClause(clause, false);
        assertEquals("3: >= 3 1^2,2^2,3^2", clause.toString());

        if(monitoring) {
            InferenceStep step = simplifier.model.getInferenceStep(1);
            System.out.println("Inference Step");
            System.out.println(step.toString(null));
            System.out.println(step.rule());
            System.out.println(step.inputClauseIds());}
    }

    public void testCheckAllPurities() throws Result {
        System.out.println("check all purities");
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Simplifier simplifier = new Simplifier(4, monitor, true, nextId);
        Clause clause = new Clause(new int[]{1, cOr, 1, 2, 3});
        simplifier.insertClause(clause);
        clause = new Clause(new int[]{2, cAtleast, 2, -3, -4});
        simplifier.insertClause(clause);
        simplifier.checkAllPurities();
        assertEquals("1,2,-4", simplifier.model.toString());
        InferenceStep step = simplifier.model.getInferenceStep(-4);
        //System.out.println(step.toString());
    }

    public void testInputClausesToAtleastDisjunctions() throws Result {
        System.out.println("inputClausesToAtleast disjunctions");
        int predicates = 3;
        InputClauses inputClauses = new InputClauses("Test", predicates, symboltable, "input clauses test");
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Simplifier simplifier = new Simplifier(predicates, monitor, true, nextId);
        simplifier.inputClauses = inputClauses;
        inputClauses.addClause(new int[]{1, cOr, 1, 2, 3});
        inputClauses.addClause(new int[]{2, cOr, -1, -2, -3});
        simplifier.readInputClauses();
        assertEquals("1: 1v2v3\n" +
                "2: -1v-2v-3\n", simplifier.clauses.toString());

        simplifier.clear();

        inputClauses.addClause(new int[]{1, cOr, 1, 2, 3, -2});
        inputClauses.addClause(new int[]{2, cOr, -1, -2, -3, -2});
        simplifier.readInputClauses();
        assertEquals("2: -1v-3v-2\n", simplifier.clauses.toString());
        assertEquals("-1,-2,-3", simplifier.model.toString());
        //System.out.println(simplifier.statistics.toString());

        simplifier.clear();
        inputClauses.addClause(new int[]{1, cOr, 1, 2, 3});
        inputClauses.addClause(new int[]{2, cOr, -1, -2, -2});
        inputClauses.addClause(new int[]{3, cOr, 3, 3, 3});
        simplifier.readInputClauses();
        assertEquals("1: 1v2v3\n" +
                "2: -1v-2\n", simplifier.clauses.toString());
        assertEquals("3", simplifier.model.toString());

        //System.out.println(simplifier.statistics.toString());
        simplifier.clear();
        inputClauses.addClause(new int[]{1, cOr, 1, 1, 1});
        inputClauses.addClause(new int[]{2, cOr, -1});
        try {
            simplifier.readInputClauses();
        } catch (Unsatisfiable unsatisfiable) {
            //System.out.println(unsatisfiable.toString());
        }
    }

    public void testInputClausesToAtleastAtleasts() throws Result {
        System.out.println("inputClausesToAtleast atleasts");
        int predicates = 4;
        InputClauses inputClauses = new InputClauses("Test", predicates, symboltable, "input clauses test");
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Simplifier simplifier = new Simplifier(predicates, monitor, true, nextId);
        simplifier.inputClauses = inputClauses;
        inputClauses.addClause(new int[]{1, cAtleast, 2, 1, 2, 3});
        simplifier.readInputClauses();
        assertEquals("1: >= 2 1,2,3\n", simplifier.clauses.toString());
        assertEquals("1,2,3,4", simplifier.model.toString());
        //System.out.println(simplifier.statistics.toString());

        //System.out.println("NEXT1");
        simplifier.clear();
        inputClauses.addClause(new int[]{1, cAtleast, 2, 1, 2, -1, 3});
        simplifier.readInputClauses();
        assertEquals("1: 2v3\n", simplifier.clauses.toString());
        //System.out.println(simplifier.statistics.toString());

        //System.out.println("NEXT2");
        simplifier.clear();
        inputClauses.addClause(new int[]{1, cAtleast, 5, 1, 1, 2, 2, 3, 4});
        simplifier.readInputClauses();
        assertEquals("1: 3v4\n", simplifier.clauses.toString());
        //System.out.println(simplifier.statistics.toString());

        //System.out.println("NEXT3");
        simplifier.clear();
        inputClauses.addClause(new int[]{1, cAtleast, 2, 1, 1, 2, 2, 3});
        simplifier.readInputClauses();
        assertEquals("1: 1v2\n", simplifier.clauses.toString());
        //System.out.println(simplifier.statistics.toString());

        //System.out.println("NEXT4");
        simplifier.clear();
        inputClauses.addClause(new int[]{1, cAtleast, 4, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3});
        simplifier.readInputClauses();
        assertEquals("1: 1v2\n", simplifier.clauses.toString());
        //System.out.println(simplifier.statistics.toString());
    }

    public void testInputClausesToAtleastAtmosts() throws Result {
        System.out.println("inputClausesToAtleast atmosts");
        int predicates = 4;
        InputClauses inputClauses = new InputClauses("Test", predicates, symboltable, "input clauses test");
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Simplifier simplifier = new Simplifier(predicates, monitor, true, nextId);
        simplifier.inputClauses = inputClauses;
        inputClauses.addClause(new int[]{1, cAtmost, 1, 1, 2, 3, 4});
        simplifier.readInputClauses();
        assertEquals("1: >= 3 -1,-2,-3,-4\n", simplifier.clauses.toString());
        //System.out.println(simplifier.statistics.toString());

        simplifier.clear();
        inputClauses.addClause(new int[]{1, cAtmost, 4, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3});
        simplifier.readInputClauses();
        assertEquals("1: -1v-3\n", simplifier.clauses.toString());
        assertEquals("-1,-2,-3,4", simplifier.model.toString());
        //System.out.println(simplifier.statistics.toString());
    }

    public void testInputClausesToAtleastExactlys() throws Result {
        System.out.println("inputClausesToAtleast exactlys");
        int predicates = 4;
        InputClauses inputClauses = new InputClauses("Test", predicates, symboltable, "input clauses test");
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Simplifier simplifier = new Simplifier(predicates, monitor, true, nextId);
        simplifier.inputClauses = inputClauses;
        inputClauses.addClause(new int[]{1, cExactly, 1, 1, 2, 3, 4});
        simplifier.readInputClauses();
        assertEquals("11: 1v2v3v4\n" +
                "12: >= 3 -1,-2,-3,-4\n", simplifier.clauses.toString());
        //System.out.println(simplifier.statistics.toString());
        //System.out.println("\nNEW");
        simplifier.clear();
        inputClauses.addClause(new int[]{1, cExactly, 4, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3});
        simplifier.readInputClauses();
        assertEquals("13: 1v2\n" +
                "14: -1v-3\n", simplifier.clauses.toString());
        assertEquals("-2,-3,4", simplifier.model.toString());
        if(monitoring) {
            System.out.println(simplifier.statistics.toString());
            System.out.println(simplifier.clauses.firstClause.nextClause.inferenceStep.toString());}
    }

    public void testInputClausesToAtleastIntervals() throws Result {
        System.out.println("inputClausesToAtleast intervals");
        int predicates = 4;
        InputClauses inputClauses = new InputClauses("Test", predicates, symboltable, "input clauses test");
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Simplifier simplifier = new Simplifier(predicates, monitor, true, nextId);
        simplifier.inputClauses = inputClauses;
        inputClauses.addClause(new int[]{1, cInterval, 1, 3, 1, 2, 3, 4});
        simplifier.readInputClauses();
        assertEquals("11: 1v2v3v4\n" +
                "12: -1v-2v-3v-4\n", simplifier.clauses.toString());
        //System.out.println(simplifier.statistics.toString());
        //System.out.println("\nNEW");
        simplifier.clear();
        inputClauses.addClause(new int[]{1, cInterval, 2, 4, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3});
        simplifier.readInputClauses();
        assertEquals("13: 1v2v3\n" +
                "14: -1v-3\n", simplifier.clauses.toString());
        assertEquals("-2,4", simplifier.model.toString());
        //System.out.println(simplifier.statistics.toString());
        //System.out.println(simplifier.clauses.firstClause.nextClause.inferenceStep.toString());
    }

    public void testProcessTrueLiteralTwo() throws Result {
        System.out.println("processTrueLiteralTwo");
        int predicates = 4;
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Simplifier simplifier = new Simplifier(predicates, monitor, true, nextId);
        simplifier.model.add(2, new InfInputClause(1));
        simplifier.insertClause(new Clause(new int[]{2, cOr, 1, 2}));
        simplifier.insertClause(new Clause(new int[]{3, cOr, -2, 3}));
        simplifier.insertClause(new Clause(new int[]{4, cOr, -1, 4}));
        simplifier.processTrueLiteralTwo(2); // true literal 2
        assertEquals("4: -1v4\n", simplifier.clauses.toString());
        assertEquals("-1,2,3", simplifier.model.toString());
        //System.out.println(simplifier.model.getInferenceStep(3));
        //System.out.println(simplifier.statistics.toString());
    }

    public void testProcessTrueLiteralMore1() throws Result {
        System.out.println("processTrueLiteralMore 1");
        int predicates = 4;
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Simplifier simplifier = new Simplifier(predicates, monitor, true, nextId);
        simplifier.model.add(2, new InfInputClause(1));
        simplifier.insertClause(new Clause(new int[]{2, cOr, 1, 2, 3}));
        simplifier.processTrueLiteralMore(2);
        assertTrue(simplifier.clauses.isEmpty());
        assertEquals("1,2,3", simplifier.model.toString());

        simplifier.clear();
        simplifier.model.add(2, new InfInputClause(1));
        simplifier.insertClause(new Clause(new int[]{2, cOr, -1, -2, -3}));
        simplifier.processTrueLiteralMore(2);
        assertFalse(simplifier.clauses.isEmpty());
        assertEquals("2: -1v-3\n", simplifier.clauses.toString());
        assertEquals("2", simplifier.model.toString());

        simplifier.clear();
        simplifier.model.add(2, new InfInputClause(1));
        simplifier.model.add(4, new InfInputClause(2));
        simplifier.insertClause(new Clause(new int[]{2, cAtleast, 3, 1, 2, 3, 4}));
        simplifier.processTrueLiteralMore(2);
        assertFalse(simplifier.clauses.isEmpty());
        assertEquals("2: 1v3\n", simplifier.clauses.toString());
        assertEquals("2,4", simplifier.model.toString());

        //System.out.println("\nNEW");
        try {
            simplifier.clear();
            simplifier.model.add(-2, new InfInputClause(1));
            simplifier.model.add(-4, new InfInputClause(2));
            simplifier.insertClause(new Clause(new int[]{2, cAtleast, 3, 1, 2, 3, 4}));
            simplifier.processTrueLiteralMore(2);
            assertFalse(simplifier.clauses.isEmpty());
            assertEquals("", simplifier.clauses.toString());
            assertEquals("-2,-4", simplifier.model.toString());}
        catch (Unsatisfiable unsatisfiable) {
          if(monitoring) System.out.println(unsatisfiable.toString());}}

    public void testProcessTrueLiteralMore2() throws Result {
        System.out.println("processTrueLiteralMore 2");
        int predicates = 4;
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Simplifier simplifier = new Simplifier(predicates, monitor, true, nextId);
        simplifier.model.add(-3, new InfInputClause(1));
        simplifier.insertClause(new Clause(new int[]{2, cAtleast, 4, 1,1, 2,2, 3,4}));
        simplifier.processTrueLiteralMore(3);
        assertEquals("", simplifier.clauses.toString());
        assertEquals("1,2,-3,4", simplifier.model.toString());

        if(monitoring) System.out.println("\nNEW");
        simplifier.clear();
        simplifier.model.add(-1, new InfInputClause(1));
        simplifier.model.add(-3, new InfInputClause(2));
        simplifier.model.add(-4, new InfInputClause(3));
        simplifier.insertClause(new Clause(new int[]{2, cOr, 1,2,3,4}));
        simplifier.processTrueLiteralMore(3);
        assertEquals("", simplifier.clauses.toString());
        assertEquals("-1,2,-3,-4", simplifier.model.toString());
        assertEquals("Positive Literals:\n" +
                "\n" +
                "Negative Literals:\n", simplifier.literalIndexMore.toString());
        if(monitoring) {
            System.out.println("\nInference Steps");
            ArrayList<InferenceStep> steps = new ArrayList<>();
            simplifier.model.getInferenceStep(2).inferenceSteps(steps);
            for(InferenceStep step : steps) System.out.println(step.toString());}
    }

    public void testRemoveClausesSubsumedByBinaryClause() throws Result {
        System.out.println("removeClausesSubsumedByBinaryClause");
        int predicates = 6;
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Simplifier simplifier = new Simplifier(predicates, monitor, true, nextId);
        simplifier.insertClause(new Clause(new int[]{2, cOr, 1,2,3,4}));
        simplifier.insertClause(new Clause(new int[]{3, cOr, 3,4,5,6}));
        simplifier.insertClause(new Clause(new int[]{4, cOr, 2,-4,-5,-6}));
        simplifier.insertClause(new Clause(new int[]{5, cOr, -1,2,-3,4}));

        simplifier.insertClause(new Clause(new int[]{6, cAtleast, 2, 1,2,3,4}));

        Clause clause = new Clause(new int[]{7, cOr, 2,4});
        simplifier.insertClause(clause);
        simplifier.removeClausesSubsumedByBinaryClause(clause);
        assertEquals("3: 3v4v5v6\n" +
                "4: 2v-4v-5v-6\n" +
                "6: >= 2 1,2,3,4\n" +
                "7: 2v4\n",simplifier.clauses.toString());
        if(monitoring) System.out.println(simplifier.statistics.toString());

    }
    public void testBinaryMergeResolutionAndEquivalence() throws Result {
        System.out.println("binaryMergeResolutionAndEquivalence");
        int predicates = 6;
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Simplifier simplifier = new Simplifier(predicates, monitor, true, nextId);
        simplifier.insertClause(new Clause(new int[]{2, cOr, 1,2}));
        simplifier.insertClause(new Clause(new int[]{3, cOr, 1,3}));
        Clause clause = new Clause(new int[]{4, cOr, -1,3});
        simplifier.insertClause(clause);
        simplifier.binaryMergeResolutionAndEquivalence(clause,-1,3,false);
        assertEquals("2: 1v2\n",simplifier.clauses.toString());
        assertEquals("1,3",simplifier.model.toString());
        //System.out.println(simplifier.model.getInferenceStep(3).toString());

        simplifier.clear();
        simplifier.insertClause(new Clause(new int[]{2, cOr, 1,2}));
        simplifier.insertClause(new Clause(new int[]{3, cOr, 1,-3}));
        clause = new Clause(new int[]{4, cOr, -1,3});
        simplifier.insertClause(clause);
        simplifier.binaryMergeResolutionAndEquivalence(clause,-1,3,true);
        simplifier.equivalenceClasses.processTasks(true);
        assertEquals("2: 1v2\n",simplifier.clauses.toString());
        assertEquals("",simplifier.model.toString());
        assertEquals("1 = -3\n",simplifier.equivalenceClasses.toString());
    }
    public void testIsSubsumedByBinaryClauses()  {
        System.out.println("binaryClauseIsSubsumedByBinaryClause");
        int predicates = 6;
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Simplifier simplifier = new Simplifier(predicates, monitor, true, nextId);
        simplifier.insertClause(new Clause(new int[]{2, cOr, 1, 2}));
        simplifier.insertClause(new Clause(new int[]{3, cOr, 1, 3}));
        Clause clause = new Clause(new int[]{4, cOr, 1, 3});
        assertNotNull(simplifier.binaryClauseIsSubsumedByBinaryClause(clause));
        clause = new Clause(new int[]{5, cOr, 3, 1});
        assertNotNull(simplifier.binaryClauseIsSubsumedByBinaryClause(clause));
        clause = new Clause(new int[]{6, cOr, 1, 4});
        assertNotNull(simplifier.binaryClauseIsSubsumedByBinaryClause(clause));
    }
    public void testResolveBetweenBinaryClauses() throws Result{
        System.out.println("resolveBetweenBinaryClauses");
        int predicates = 6;
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Simplifier simplifier = new Simplifier(predicates, monitor, true, nextId);
        Clause clause1 = new Clause(new int[]{2, cOr, 1, 2});
        Clause clause2 = new Clause(new int[]{3, cOr, 3, -2});
        Clause resolvent = simplifier.resolveBetweenBinaryClauses(clause1,clause2);
        assertEquals("11: 1v3",resolvent.toString());

        Clause clause3 = new Clause(new int[]{4, cOr, 3, 2});
        assertNull(simplifier.resolveBetweenBinaryClauses(clause3,clause2));
        assertEquals("2,3",simplifier.model.toString());
        //System.out.println(simplifier.model.getInferenceStep(3).toString());

        simplifier.clear();
        simplifier.insertClause(new Clause(new int[]{2, cOr, 1, 2}));
        clause1 = new Clause(new int[]{3, cOr, 1, 3});
        simplifier.insertClause(clause1);
        clause2 = new Clause(new int[]{4, cOr, 2, -3});
        assertNull(simplifier.resolveBetweenBinaryClauses(clause2,clause1));
    }
    public void testBinaryClauseResolutionCompletion() throws Result {
        System.out.println("saturateBinaryClausesWithBinaryCLause");
        int predicates = 6;
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Simplifier simplifier = new Simplifier(predicates, monitor, true, nextId);
        Clause clause1 = new Clause(new int[]{2, cOr, 1, 2});
        simplifier.insertClause(clause1);
        Clause clause2 = new Clause(new int[]{3, cOr, -1,3 });
        simplifier.insertClause(clause2);
        Clause clause3 = new Clause(new int[]{4, cOr, -1,4 });
        simplifier.insertClause(clause3);
        Clause clause4 = new Clause(new int[]{5, cOr, -2,3 });
        simplifier.insertClause(clause4);
        simplifier.saturateBinaryClausesWithBinaryCLause(clause1, 1);
        assertEquals(" 2: 1v2\n" +
                " 3: -1v3\n" +
                " 4: -1v4\n" +
                " 5: -2v3\n" +
                "11: 2v4\n" +
                "12: 2v3\n",simplifier.clauses.toString());
      //  System.out.println(simplifier.statistics.toString());

        simplifier.clear();

        clause1 = new Clause(new int[]{2, cOr, 1, 2});
        simplifier.insertClause(clause1);
        clause2 = new Clause(new int[]{3, cOr, -1,3 });
        simplifier.insertClause(clause2);
        clause3 = new Clause(new int[]{4, cOr, -1,2 });
        simplifier.insertClause(clause3);
        clause4 = new Clause(new int[]{5, cOr, -2,3 });
        simplifier.insertClause(clause4);
        simplifier.saturateBinaryClausesWithBinaryCLause(clause1, 1);
        assertEquals("3: -1v3\n" +
                "5: -2v3\n",simplifier.clauses.toString());
        assertEquals("-1,2",simplifier.model.toString());
      //  System.out.println(simplifier.statistics.toString());
    }
    public void testProcessBinaryClause1() throws Result {
        System.out.println("processBinaryClause 1");
        int predicates = 6;
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Simplifier simplifier = new Simplifier(predicates, monitor, true, nextId);
        Clause clause1 = new Clause(new int[]{1, cOr, 1, 2});
        simplifier.insertClause(clause1);
        Clause clause2 = new Clause(new int[]{2, cOr, -1, 3, 4});
        simplifier.insertClause(clause2);
        Clause clause3 = new Clause(new int[]{3, cOr, 2,4,1});
        simplifier.insertClause(clause3);
        Clause clause4 = new Clause(new int[]{4, cOr, 5,2,1});
        simplifier.insertClause(clause4);
        Clause clause5 = new Clause(new int[]{5, cOr, 4,5,6});
        simplifier.insertClause(clause5);
        simplifier.processBinaryClause(clause1);
        assertEquals("1: 1v2\n" +
                "2: -1v3v4\n" +
                "5: 4v5v6\n",simplifier.clauses.toString());
        assertEquals("2,4,5",simplifier.model.toString());
      //  System.out.println(simplifier.statistics.toString());
    }

    public void testProcessBinaryClause2() throws Result {
        System.out.println("processBinaryClause 2");
        int predicates = 6;
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Simplifier simplifier = new Simplifier(predicates, monitor, true, nextId);
        Clause clause1 = new Clause(new int[]{1, cOr, 1, 5});
        simplifier.insertClause(clause1);
        Clause clause2 = new Clause(new int[]{2, cOr, -1, 3, 4});
        simplifier.insertClause(clause2);
        Clause clause3 = new Clause(new int[]{3, cOr, 2,4,1});
        simplifier.insertClause(clause3);
        Clause clause4 = new Clause(new int[]{4, cOr, 5,2,-1});
        simplifier.insertClause(clause4);
        Clause clause5 = new Clause(new int[]{5, cOr, 4,5,6});
        simplifier.insertClause(clause5);
        Clause clause6 = new Clause(new int[]{6, cOr, -5,2});
        simplifier.insertClause(clause6);

        simplifier.processBinaryClause(clause1);
        simplifier.processTasks(2);


        assertEquals(" 1: 1v5\n" +
                " 2: -1v3v4\n" +
                " 5: 4v5v6\n" +
                "11: 1v2\n",simplifier.clauses.toString());
        assertEquals("2,4,5",simplifier.model.toString());
        //System.out.println(simplifier.statistics.toString());
    }

    public void testProcessBinaryClause3() throws Result {
        System.out.println("processBinaryClause 3");
        int predicates = 6;
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Simplifier simplifier = new Simplifier(predicates, monitor, true, nextId);
        simplifier.model.addObserver((Integer literal, InferenceStep step) -> simplifier.addTrueLiteralTask(literal,step));
        Clause clause1 = new Clause(new int[]{1, cOr, 1, 5});
        simplifier.insertClause(clause1);
        Clause clause2 = new Clause(new int[]{2, cOr, -1, 3, 4});
        simplifier.insertClause(clause2);
        Clause clause3 = new Clause(new int[]{3, cOr, 2,4,1});
        simplifier.insertClause(clause3);
        Clause clause4 = new Clause(new int[]{4, cOr, 5,2,-1});
        simplifier.insertClause(clause4);
        Clause clause5 = new Clause(new int[]{5, cOr, 4,5,6});
        simplifier.insertClause(clause5);
        Clause clause6 = new Clause(new int[]{6, cOr, -5,2});
        simplifier.insertClause(clause6);
        simplifier.processBinaryClause(clause1);

        try{simplifier.processTasks(6);}
        catch(Satisfiable satisfiable) {
            if(monitoring) System.out.println(satisfiable.toString());}

        assertEquals("",simplifier.clauses.toString());
        assertEquals("1,2,3,4,5,6",simplifier.model.toString());
        //System.out.println(simplifier.statistics.toString());
    }
    public void testProcessEquivalence() throws Result {
        System.out.println("processEquivalence");
        int predicates = 6;
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Simplifier simplifier = new Simplifier(predicates, monitor, true, nextId);
        simplifier.insertClause(new Clause(new int[]{1, cOr, 1, 5}));
        simplifier.insertClause(new Clause(new int[]{2, cOr, 5,3}));
        simplifier.processEquivalence(2, 5, new InferenceTest("MyTest 1"));
        assertEquals("1: 1v2\n" +
                "2: 2v3\n", simplifier.clauses.toString());

        if(monitoring) System.out.println("\nNEW 1");
        simplifier.clear();
        simplifier.insertClause(new Clause(new int[]{1, cOr, 1, 5}));
        simplifier.insertClause(new Clause(new int[]{2, cOr, 5,2}));
        simplifier.processEquivalence(2, 5, new InferenceTest("MyTest 2"));
        assertEquals("1: 1v5\n", simplifier.clauses.toString());
        assertEquals("2,5",simplifier.model.toString());

        if(monitoring) System.out.println("\nNEW 2");
        simplifier.clear();
        simplifier.insertClause(new Clause(new int[]{1, cAtleast, 2, 1,3, 5}));
        simplifier.insertClause(new Clause(new int[]{2, cAtleast, 3, 4,4,5,5,5,6,6,2}));
        simplifier.processEquivalence(2, 5, new InferenceTest("MyTest 3"));
        assertEquals("1: >= 2 1,3,2\n" +
                "2: >= 3 4^2,6^2,2^3\n", simplifier.clauses.toString());

        if(monitoring) System.out.println("\nNEW 3");
        simplifier.clear();
        simplifier.insertClause(new Clause(new int[]{1, cAtleast, 2, 1,1,2,3,3}));
        simplifier.processEquivalence(3, 2, new InferenceTest("MyTest 4"));
        assertEquals("1: 1v3\n", simplifier.clauses.toString());

        if(monitoring) System.out.println("\nNEW 4");
        simplifier.clear();
        Clause clause = new Clause(new int[]{1, cAtleast, 3, 1,1,2,3});
        simplifier.insertClause(clause);
        simplifier.processEquivalence(3, 2, new InferenceTest("MyTest 5"));
        assertFalse(clause.exists);
        assertEquals("1,3",simplifier.model.toString());
        assertEquals("", simplifier.clauses.toString());
        if(monitoring) System.out.println(simplifier.statistics.toString());


        if(monitoring) System.out.println("\nNEW 5");
        simplifier.clear();
        simplifier.insertClause(new Clause(new int[]{1, cAtleast, 3, 1,1,2,3}));
        simplifier.insertClause(new Clause(new int[]{2, cAtleast, 3, 1,2,2,3,3,4}));
        simplifier.insertClause(new Clause(new int[]{3, cAtleast, 2, -1,-2,-3,-3,-4}));
        simplifier.insertClause(new Clause(new int[]{4, cOr, -1,-2,-3,-4}));
        simplifier.insertClause(new Clause(new int[]{5, cOr, -1,-3,}));
        simplifier.insertClause(new Clause(new int[]{6, cAtleast, 2, -1,-2,-3,-3,-4,3}));
        simplifier.insertClause(new Clause(new int[]{7, cAtleast, 2, 1,2,3,-4}));
        simplifier.insertClause(new Clause(new int[]{8, cOr, -2,3,}));
        simplifier.processEquivalence(2, 3, new InferenceTest("MyTest 6"));
        assertEquals("3: >= 2 -1,-2^2,-4\n" +
                "4: -1v-2v-4\n" +
                "5: -1v-2\n" +
                "6: -1v-2v-4\n" +
                "7: >= 2 1,2^2,-4\n",simplifier.clauses.toString());

        if(monitoring) System.out.println(simplifier.statistics.toString());

        if(monitoring) System.out.println("\nNEW 6");
        try{
        simplifier.processEquivalence(2, 4, new InferenceTest("MyTest 7"));}
        catch(Result result) {if(monitoring) System.out.println(result.toString());}
    }

    public void testRemoveClausesSubsumedByLongerClause() throws Result {
        System.out.println("removeClausesSubsumedByLongerClause");
        int predicates = 6;
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Simplifier simplifier = new Simplifier(predicates, monitor, true, nextId);
        Clause subsumer = new Clause(new int[]{1, cOr, 1,2,3});
        simplifier.insertClause(subsumer);
        simplifier.insertClause(new Clause(new int[]{2, cOr, 3,2,1}));
        simplifier.insertClause(new Clause(new int[]{3, cOr, 2,4,3,1}));
        simplifier.insertClause(new Clause(new int[]{4, cOr, 2,4,1,5}));
        simplifier.removeClausesSubsumedByLongerClause(subsumer);
        assertEquals("1: 1v2v3\n" +
                "4: 2v4v1v5\n",simplifier.clauses.toString());

        if(monitoring) System.out.println("\nNew");
        simplifier.clear();
        subsumer = new Clause(new int[]{1, cAtleast, 4, 1,1,2,2,3,3,3});
        simplifier.insertClause(subsumer);
        simplifier.insertClause(new Clause(new int[]{2, cAtleast,3 , 1,1,2,2,3,3}));
        simplifier.insertClause(new Clause(new int[]{3, cAtleast,3 , 1,1,1,2,2,3,3,3}));
        simplifier.insertClause(new Clause(new int[]{4, cAtleast,3 , 1,1,2,2,2,3,3}));
        simplifier.insertClause(new Clause(new int[]{5, cAtleast,3 , 1,1,1,2,2,2,3,3,3}));
        simplifier.removeClausesSubsumedByLongerClause(subsumer);
        assertEquals("1: >= 4 1^2,2^2,3^3\n" +
                "2: >= 3 1^2,2^2,3^2\n" +
                "4: >= 3 1^2,2^3,3^2\n",simplifier.clauses.toString());
        }

    public void testmergeResolutionWithLongerClauseDirect1() throws Result {
        System.out.println("mergeResolutionWithLongerClauseDirect 1");
        int predicates = 6;
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Simplifier simplifier = new Simplifier(predicates, monitor, true, nextId);
        Clause clause = new Clause(new int[]{1, cOr, 1, 2, 3});
        simplifier.insertClause(clause);
        Clause clause1 = new Clause(new int[]{2, cOr, 3, -2, 1, 4});
        simplifier.insertClause(clause1);
        assertTrue(simplifier.mergeResolutionWithLongerClauseDirect(clause));
        assertEquals("1: 1v2v3\n" +
                "2: 3v1v4\n", simplifier.clauses.toString());
        if(monitoring) {
            System.out.println(clause1.inferenceStep.toString());
            System.out.println(clause1.inferenceStep.inputClauseIds());}

        if(monitoring) System.out.println("\nNEW 2");
        simplifier.clear();
        clause = new Clause(new int[]{1, cOr, 1, 2, 3});
        simplifier.insertClause(clause);
        simplifier.insertClause(new Clause(new int[]{2, cOr, 3, -2, 1}));
        assertFalse(simplifier.mergeResolutionWithLongerClauseDirect(clause));
        assertEquals("2: 3v1\n", simplifier.clauses.toString());

        if(monitoring) {
            System.out.println(clause1.inferenceStep.toString());
            System.out.println(clause1.inferenceStep.inputClauseIds());
            ArrayList<InferenceStep> steps = new ArrayList<>();
            clause1.inferenceStep.inferenceSteps(steps);
            for(InferenceStep step : steps) System.out.println(step.toString());}

        if(monitoring) System.out.println("\nNEW 3");
        simplifier.clear();
        clause = new Clause(new int[]{1, cOr, 1, 2, 3});
        simplifier.insertClause(clause);
        simplifier.insertClause(new Clause(new int[]{2, cOr, 3, -2, 4}));
        assertTrue(simplifier.mergeResolutionWithLongerClauseDirect(clause));
        assertEquals("1: 1v2v3\n" +
                "2: 3v-2v4\n", simplifier.clauses.toString());


        if(monitoring) System.out.println("\nNEW 4");
        simplifier.clear();
        clause = new Clause(new int[]{1, cAtleast, 2, 1, 2, 3});
        simplifier.insertClause(clause);
        simplifier.insertClause(new Clause(new int[]{2, cAtleast, 2, -1,-1,2,2,3,3}));
        assertTrue(simplifier.mergeResolutionWithLongerClauseDirect(clause));
        assertEquals("1: >= 2 1,2,3\n" +
                "2: 2v3\n", simplifier.clauses.toString());

        if(monitoring) System.out.println("\nNEW 5");
        simplifier.clear();
        clause = new Clause(new int[]{1, cOr, 1, 2, 3});
        simplifier.insertClause(clause);
        simplifier.insertClause(new Clause(new int[]{2, cOr, 3, -2, 4}));
        simplifier.insertClause(new Clause(new int[]{3, cOr, 3, -2, -1}));
        simplifier.insertClause(new Clause(new int[]{4, cOr, 3, -1, 4,2,5}));
        simplifier.insertClause(new Clause(new int[]{5, cOr, -3, 2, 4,1}));
        assertTrue(simplifier.mergeResolutionWithLongerClauseDirect(clause));
        assertEquals("1: 1v2v3\n" +
                "2: 3v-2v4\n" +
                "3: 3v-2v-1\n" +
                "4: 3v4v2v5\n" +
                "5: 2v4v1\n", simplifier.clauses.toString());
        if(monitoring) System.out.println(simplifier.statistics.toString());
    }

    public void testmergeResolutionWithLongerClauseDirect2 () throws Result {
        System.out.println("mergeResolutionWithLongerClauseDirect 2");
        int predicates = 6;
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Simplifier simplifier = new Simplifier(predicates, monitor, true, nextId);
        Clause clause = new Clause(new int[]{1, cAtleast, 3, 1,1, 2, 3,3});
        simplifier.insertClause(clause);
        simplifier.insertClause(new Clause(new int[]{2, cAtleast, 4, 1,1,1,1, -2,-2,-2, 3,3,3,3,4}));
        simplifier.insertClause(new Clause(new int[]{3, cAtleast, 4, 1,1,1,1, -2,-2,-2, 3,3,3,4}));
        simplifier.mergeResolutionWithLongerClauseDirect(clause);
        assertEquals("1: >= 3 1^2,2,3^2\n" +
                "2: 1v3\n" +
                "3: >= 4 1^4,-2^3,3^3,4\n", simplifier.clauses.toString());

        }


    }


