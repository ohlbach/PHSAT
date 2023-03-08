package Solvers.Simplifier;

import Datastructures.Clauses.Connective;
import Datastructures.Clauses.InputClauses;
import Datastructures.Results.Result;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import InferenceSteps.InfInputClause;
import InferenceSteps.InferenceStep;
import Management.Monitor.Monitor;
import Management.Monitor.MonitorLife;
import junit.framework.TestCase;

import java.util.function.IntSupplier;

public class SimplifierTest extends TestCase {

    static int cOr = Connective.OR.ordinal();
    static int cAtleast = Connective.ATLEAST.ordinal();
    static int cAtmost = Connective.ATMOST.ordinal();
    static int cExactly = Connective.EXACTLY.ordinal();
    static int cInterval = Connective.INTERVAL.ordinal();


    static boolean monitoring = true;

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
        assertEquals("1,2", simplifier.model.toString());
    }

    public void testSimplifyClause() throws Result {
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
        simplifier.removeClause(clause, false);
        simplifier.simplifyClause(clause, false);
        simplifier.insertClause(clause);
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

        InferenceStep step = simplifier.model.getInferenceStep(1);
        //System.out.println(step.toString(null));
        //System.out.println(step.rule());
        //System.out.println(step.inputClauseIds());
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
        simplifier.inputClausesToAtleast();
        assertEquals("1: 1v2v3\n" +
                "2: -1v-2v-3\n", simplifier.clauses.toString());

        simplifier.clear();

        inputClauses.addClause(new int[]{1, cOr, 1, 2, 3, -2});
        inputClauses.addClause(new int[]{2, cOr, -1, -2, -3, -2});
        simplifier.inputClausesToAtleast();
        assertEquals("2: -1v-3v-2\n", simplifier.clauses.toString());
        assertEquals("-1,-2,-3", simplifier.model.toString());
        //System.out.println(simplifier.statistics.toString());

        simplifier.clear();
        inputClauses.addClause(new int[]{1, cOr, 1, 2, 3});
        inputClauses.addClause(new int[]{2, cOr, -1, -2, -2});
        inputClauses.addClause(new int[]{3, cOr, 3, 3, 3});
        simplifier.inputClausesToAtleast();
        assertEquals("1: 1v2v3\n" +
                "2: -1v-2\n", simplifier.clauses.toString());
        assertEquals("3", simplifier.model.toString());

        //System.out.println(simplifier.statistics.toString());
        simplifier.clear();
        inputClauses.addClause(new int[]{1, cOr, 1, 1, 1});
        inputClauses.addClause(new int[]{2, cOr, -1});
        try {
            simplifier.inputClausesToAtleast();
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
        simplifier.inputClausesToAtleast();
        assertEquals("1: >= 2 1,2,3\n", simplifier.clauses.toString());
        assertEquals("1,2,3,4", simplifier.model.toString());
        //System.out.println(simplifier.statistics.toString());

        //System.out.println("NEXT1");
        simplifier.clear();
        inputClauses.addClause(new int[]{1, cAtleast, 2, 1, 2, -1, 3});
        simplifier.inputClausesToAtleast();
        assertEquals("1: 2v3\n", simplifier.clauses.toString());
        //System.out.println(simplifier.statistics.toString());

        //System.out.println("NEXT2");
        simplifier.clear();
        inputClauses.addClause(new int[]{1, cAtleast, 5, 1, 1, 2, 2, 3, 4});
        simplifier.inputClausesToAtleast();
        assertEquals("1: 3v4\n", simplifier.clauses.toString());
        //System.out.println(simplifier.statistics.toString());

        //System.out.println("NEXT3");
        simplifier.clear();
        inputClauses.addClause(new int[]{1, cAtleast, 2, 1, 1, 2, 2, 3});
        simplifier.inputClausesToAtleast();
        assertEquals("1: 1v2\n", simplifier.clauses.toString());
        //System.out.println(simplifier.statistics.toString());

        //System.out.println("NEXT4");
        simplifier.clear();
        inputClauses.addClause(new int[]{1, cAtleast, 4, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3});
        simplifier.inputClausesToAtleast();
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
        simplifier.inputClausesToAtleast();
        assertEquals("1: >= 3 -1,-2,-3,-4\n", simplifier.clauses.toString());
        //System.out.println(simplifier.statistics.toString());

        simplifier.clear();
        inputClauses.addClause(new int[]{1, cAtmost, 4, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3});
        simplifier.inputClausesToAtleast();
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
        simplifier.inputClausesToAtleast();
        assertEquals("11: 1v2v3v4\n" +
                "12: >= 3 -1,-2,-3,-4\n", simplifier.clauses.toString());
        //System.out.println(simplifier.statistics.toString());
        //System.out.println("\nNEW");
        simplifier.clear();
        inputClauses.addClause(new int[]{1, cExactly, 4, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3});
        simplifier.inputClausesToAtleast();
        assertEquals("13: 1v2\n" +
                "14: -1v-3\n", simplifier.clauses.toString());
        assertEquals("-2,-3,4", simplifier.model.toString());
        //System.out.println(simplifier.statistics.toString());
        //System.out.println(simplifier.clauses.firstClause.nextClause.inferenceStep.toString());
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
        simplifier.inputClausesToAtleast();
        assertEquals("11: 1v2v3v4\n" +
                "12: -1v-2v-3v-4\n", simplifier.clauses.toString());
        //System.out.println(simplifier.statistics.toString());
        //System.out.println("\nNEW");
        simplifier.clear();
        inputClauses.addClause(new int[]{1, cInterval, 2, 4, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3});
        simplifier.inputClausesToAtleast();
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

        System.out.println("\nNEW");
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
          //  System.out.println(unsatisfiable.toString());
        }}

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

        System.out.println("\nNEW");
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
        /*ArrayList<InferenceStep> steps = new ArrayList<>();
        simplifier.model.getInferenceStep(2).inferenceSteps(steps);
        for(InferenceStep step : steps) System.out.println(step.toString());*/
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
        //System.out.println(simplifier.statistics.toString());

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
        simplifier.equivalenceClasses.run(true);
        assertEquals("2: 1v2\n",simplifier.clauses.toString());
        assertEquals("",simplifier.model.toString());
        assertEquals("1 = -3\n",simplifier.equivalenceClasses.toString());
    }
    public void testIsSubsumedByBinaryClauses()  {
        System.out.println("isSubsumedByBinaryClauses");
        int predicates = 6;
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Simplifier simplifier = new Simplifier(predicates, monitor, true, nextId);
        simplifier.insertClause(new Clause(new int[]{2, cOr, 1, 2}));
        simplifier.insertClause(new Clause(new int[]{3, cOr, 1, 3}));
        Clause clause = new Clause(new int[]{4, cOr, 1, 3});
        assertTrue(simplifier.isSubsumedByBinaryClauses(clause));
        clause = new Clause(new int[]{5, cOr, 3, 1});
        assertTrue(simplifier.isSubsumedByBinaryClauses(clause));
        clause = new Clause(new int[]{6, cOr, 1, 4});
        assertFalse(simplifier.isSubsumedByBinaryClauses(clause));
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
        assertEquals("3",simplifier.model.toString());
        //System.out.println(simplifier.model.getInferenceStep(3).toString());

        simplifier.clear();
        simplifier.insertClause(new Clause(new int[]{2, cOr, 1, 2}));
        clause1 = new Clause(new int[]{3, cOr, 1, 3});
        simplifier.insertClause(clause1);
        clause2 = new Clause(new int[]{4, cOr, 2, -3});
        assertNull(simplifier.resolveBetweenBinaryClauses(clause2,clause1));
    }
    public void testMergeResolutionWithBinaryClause() throws Result {
        System.out.println("mergeResolutionWithBinaryClause");
        int predicates = 6;
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Simplifier simplifier = new Simplifier(predicates, monitor, true, nextId);
        Clause clause1 = new Clause(new int[]{2, cOr, 1, 2});
        simplifier.insertClause(clause1);
        Clause clause2 = new Clause(new int[]{3, cAtleast, 2, -1, 2,2,3,4});
        simplifier.insertClause(clause2);
        simplifier.mergeResolutionWithBinaryClause(clause1,1,2);
        assertEquals("2: 1v2\n" +
                "3: >= 2 2^2,3,4\n",simplifier.clauses.toString());
        //System.out.println("INF " + clause2.inferenceStep.toString());

        simplifier.clear();
        clause1 = new Clause(new int[]{2, cOr, 1, 2});
        simplifier.insertClause(clause1);
        clause2 = new Clause(new int[]{3, cAtleast, 2, -1, 2,2,3});
        simplifier.insertClause(clause2);
        simplifier.mergeResolutionWithBinaryClause(clause1,1,2);
        assertEquals("2: 1v2\n",simplifier.clauses.toString());
        assertEquals("1,2,3",simplifier.model.toString());
        //System.out.println(simplifier.statistics.toString());

        simplifier.clear();
        clause1 = new Clause(new int[]{2, cOr, 1, 2});
        simplifier.insertClause(clause1);
        clause2 = new Clause(new int[]{3, cAtleast, 3, -1, 2,2,3,4});
        simplifier.insertClause(clause2);
        simplifier.mergeResolutionWithBinaryClause(clause1,1,2);
        assertEquals("2: 1v2\n" +
                "3: >= 3 -1,2^2,3,4\n",simplifier.clauses.toString());
        assertEquals("",simplifier.model.toString());

    }

    }
