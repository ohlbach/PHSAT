package Solvers.Resolution;

import Datastructures.Clauses.Quantifier;
import Datastructures.Results.Result;
import Datastructures.Results.Satisfiable;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Datastructures.Theory.InferenceTest;
import InferenceSteps.InfInputClause;
import InferenceSteps.InferenceStep;
import Management.Monitor.Monitor;
import Management.Monitor.MonitorLife;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import junit.framework.TestCase;

import java.util.ArrayList;
import java.util.function.IntSupplier;

public class ResolutionTest extends TestCase {

    static int cOr = Quantifier.OR.ordinal();
    static int cAtleast = Quantifier.ATLEAST.ordinal();
    static int cAtmost = Quantifier.ATMOST.ordinal();
    static int cExactly = Quantifier.EXACTLY.ordinal();
    static int cInterval = Quantifier.INTERVAL.ordinal();


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
        Resolution resolution = new Resolution(10, monitor, true, nextId);
        Clause clause1 = new Clause(new int[]{1, cOr, 1, 2, 3});
        resolution.insertClause(clause1);
        Clause clause2 = new Clause(new int[]{2, cAtleast, 2, 1, 1, 2, 2, -3, -3});
        resolution.insertClause(clause2);
        assertEquals("1: 1v2v3\n" +
                "2: >= 2 1^2,2^2,-3^2\n", resolution.clauses.toString());
        assertEquals("1: pvqvr\n" +
                "2: >= 2 p^2,q^2,-r^2\n", resolution.clauses.toString(symboltable));
        Clause clause3 = new Clause(new int[]{3, cOr, 4, -5});
        resolution.insertClause(clause3);
        assertEquals("Positive Literals:\n" +
                "4:1,\n" +
                "Negative Literals:\n" +
                "-5:1,", resolution.literalIndexTwo.toString());
        assertEquals("Positive Literals:\n" +
                "1:2,2:2,3:1,\n" +
                "Negative Literals:\n" +
                "-3:1,", resolution.literalIndexMore.toString());

        resolution.removeClause(clause1, false); // no purity check
        assertEquals("2: >= 2 1^2,2^2,-3^2\n" +
                "3: 4v-5\n", resolution.clauses.toString());

        assertEquals("Positive Literals:\n" +
                "4:1,\n" +
                "Negative Literals:\n" +
                "-5:1,", resolution.literalIndexTwo.toString());

        assertEquals("Positive Literals:\n" +
                "1:1,2:1,\n" +
                "Negative Literals:\n" +
                "-3:1,", resolution.literalIndexMore.toString());
    }

    public void testPurityCheck() throws Result {
        System.out.println("purityCheck");
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Resolution resolution = new Resolution(10, monitor, true, nextId);
        Clause clause1 = new Clause(new int[]{1, cOr, 1, 2, 3});
        resolution.insertClause(clause1);
        Clause clause2 = new Clause(new int[]{2, cOr, -1, -2, 3});
        resolution.insertClause(clause2);
        resolution.removeClause(clause2, true);
        assertEquals("1: 1v2v3\n", resolution.clauses.toString());
        assertEquals("1,2,3", resolution.localModelString());
    }

    public void testSimplifyClause() throws Unsatisfiable {
        System.out.println("simplify clause");
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Resolution resolution = new Resolution(10, monitor, true, nextId);
        Clause clause = new Clause(new int[]{1, cOr, 1, 2, 3});
        resolution.simplifyClause(clause, false);
        assertEquals("1: 1v2v3", clause.toString());

        clause = new Clause(new int[]{2, cAtleast, 5, 1, 1, 2, 2, 3, 4});
        resolution.insertClause(clause);
        assertEquals("Positive Literals:\n" +
                "1:1,2:1,3:1,4:1,\n" +
                "Negative Literals:\n", resolution.literalIndexMore.toString());
        resolution.simplifyClause(clause, true);
        assertEquals("2: 3v4", clause.toString());
        assertEquals("1,2", resolution.model.toString());
        assertEquals("Positive Literals:\n" +
                "\n" +
                "Negative Literals:\n", resolution.literalIndexMore.toString());
        assertEquals("Positive Literals:\n" +
                "3:1,4:1,\n" +
                "Negative Literals:\n", resolution.literalIndexTwo.toString());

        clause = new Clause(new int[]{3, cAtleast, 6, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3});
        resolution.simplifyClause(clause, false);
        assertEquals("3: >= 3 1^2,2^2,3^2", clause.toString());

        if(monitoring) {
            InferenceStep step = resolution.model.getInferenceStep(1);
            System.out.println("Inference Step");
            System.out.println(step.toString(null));
            System.out.println(step.rule());}
    }


    public void testProcessTrueLiteralTwo() throws Result {
        System.out.println("processTrueLiteralTwo");
        Thread myThread = Thread.currentThread();
        int predicates = 4;
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Resolution resolution = new Resolution(predicates, monitor, true, nextId);
        resolution.model.add(myThread,2, new InfInputClause(1));
        resolution.makeLocallyTrue(2);
        resolution.insertClause(new Clause(new int[]{2, cOr, 1, 2}));
        resolution.insertClause(new Clause(new int[]{3, cOr, -2, 3}));
        resolution.insertClause(new Clause(new int[]{4, cOr, -1, 4}));
        resolution.processTrueLiteralTwo(2,null); // true literal 2
        assertEquals("4: -1v4\n", resolution.clauses.toString());
        assertEquals("2,3", resolution.model.toString());
        assertEquals("-1,2,3", resolution.localModelString());
        //System.out.println(simplifier.model.getInferenceStep(3));
        //System.out.println(simplifier.statistics.toString());
    }

    public void testProcessTrueLiteralMore1() throws Result {
        System.out.println("processTrueLiteralMore 1");
        Thread myThread = Thread.currentThread();
        int predicates = 4;
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Resolution resolution = new Resolution(predicates, monitor, true, nextId);
        resolution.model.add(myThread,2, new InfInputClause(1));
        resolution.makeLocallyTrue(2);
        resolution.insertClause(new Clause(new int[]{2, cOr, 1, 2, 3}));
        resolution.processTrueLiteralMore(2);
        assertTrue(resolution.clauses.isEmpty());
        assertEquals("2", resolution.localModelString());

        resolution.clear();
        resolution.model.add(myThread,2, new InfInputClause(1));
        resolution.makeLocallyTrue(2);
        resolution.insertClause(new Clause(new int[]{2, cOr, -1, -2, -3}));
        resolution.processTrueLiteralMore(2);
        assertFalse(resolution.clauses.isEmpty());
        assertEquals("2: -1v-3\n", resolution.clauses.toString());
        assertEquals("2", resolution.localModelString());

        resolution.clear();
        resolution.model.add(myThread,2, new InfInputClause(1));
        resolution.makeLocallyTrue(2);
        resolution.model.add(myThread,4, new InfInputClause(2));
        resolution.makeLocallyTrue(4);
        resolution.insertClause(new Clause(new int[]{2, cAtleast, 3, 1, 2, 3, 4}));
        resolution.processTrueLiteralMore(2);
        assertFalse(resolution.clauses.isEmpty());
        assertEquals("2: 1v3\n", resolution.clauses.toString());
        assertEquals("2,4", resolution.localModelString());

        //System.out.println("\nNEW");
        try {
            resolution.clear();
            resolution.model.add(myThread,-2, new InfInputClause(1));
            resolution.makeLocallyTrue(-2);
            resolution.model.add(myThread,-4, new InfInputClause(2));
            resolution.makeLocallyTrue(-4);
            resolution.insertClause(new Clause(new int[]{2, cAtleast, 3, 1, 2, 3, 4}));
            resolution.processTrueLiteralMore(2);
            assertFalse(resolution.clauses.isEmpty());
            assertEquals("", resolution.clauses.toString());
            assertEquals("-2,-4", resolution.model.toString());}
        catch (Unsatisfiable unsatisfiable) {
          if(monitoring) System.out.println(unsatisfiable.toString());}}

    public void testProcessTrueLiteralMore2() throws Result {
        System.out.println("processTrueLiteralMore 2");
        Thread myThread = Thread.currentThread();
        int predicates = 4;
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Resolution resolution = new Resolution(predicates, monitor, true, nextId);
        resolution.model.add(myThread,-3, new InfInputClause(1));
        resolution.makeLocallyTrue(-3);
        resolution.insertClause(new Clause(new int[]{2, cAtleast, 4, 1,1, 2,2, 3,4}));
        resolution.processTrueLiteralMore(3);
        assertEquals("", resolution.clauses.toString());
        assertEquals("1,2,-3", resolution.localModelString());

        if(monitoring) System.out.println("\nNEW");
        resolution.clear();
        resolution.model.add(myThread,-1, new InfInputClause(1));
        resolution.makeLocallyTrue(-1);
        resolution.model.add(myThread,-3, new InfInputClause(2));
        resolution.makeLocallyTrue(-3);
        resolution.model.add(myThread,-4, new InfInputClause(3));
        resolution.makeLocallyTrue(-4);
        resolution.insertClause(new Clause(new int[]{2, cOr, 1,2,3,4}));
        resolution.processTrueLiteralMore(3);
        assertEquals("", resolution.clauses.toString());
        assertEquals("-1,2,-3,-4", resolution.localModelString());
        assertEquals("-1,-3,-4", resolution.model.toString());
        assertEquals("Positive Literals:\n" +
                "\n" +
                "Negative Literals:\n", resolution.literalIndexMore.toString());

    }

    public void testRemoveClausesSubsumedByBinaryClause() throws Result {
        System.out.println("removeLongerClausesSubsumedByBinaryClause");
        int predicates = 6;
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Resolution resolution = new Resolution(predicates, monitor, true, nextId);
        resolution.insertClause(new Clause(new int[]{2, cOr, 1,2,3,4}));
        resolution.insertClause(new Clause(new int[]{3, cOr, 3,4,5,6}));
        resolution.insertClause(new Clause(new int[]{4, cOr, 2,-4,-5,-6}));
        resolution.insertClause(new Clause(new int[]{5, cOr, -1,2,-3,4}));

        resolution.insertClause(new Clause(new int[]{6, cAtleast, 2, 1,2,3,4}));

        Clause clause = new Clause(new int[]{7, cOr, 2,4});
        resolution.insertClause(clause);
        resolution.removeLongerClausesSubsumedByBinaryClause(clause);
        assertEquals("3: 3v4v5v6\n" +
                "4: 2v-4v-5v-6\n" +
                "6: >= 2 1,2,3,4\n" +
                "7: 2v4\n", resolution.clauses.toString());
        if(monitoring) System.out.println(resolution.statistics.toString());

    }
    public void testBinaryMergeResolutionAndEquivalence() throws Result {
        System.out.println("binaryMergeResolutionAndEquivalence");
        int predicates = 6;
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Resolution resolution = new Resolution(predicates, monitor, true, nextId);
        resolution.insertClause(new Clause(new int[]{2, cOr, 1,2}));
        resolution.insertClause(new Clause(new int[]{3, cOr, 1,3}));
        Clause clause = new Clause(new int[]{4, cOr, -1,3});
        resolution.insertClause(clause);
        resolution.binaryMergeResolutionAndEquivalence(clause,-1,3,false);
        assertEquals("2: 1v2\n", resolution.clauses.toString());
        assertEquals("3", resolution.model.toString());
        //System.out.println(simplifier.model.getInferenceStep(3).toString());

        System.out.println("NEW");
        resolution.clear();
        resolution.insertClause(new Clause(new int[]{2, cOr, 1,2}));
        resolution.insertClause(new Clause(new int[]{3, cOr, 1,-3}));
        clause = new Clause(new int[]{4, cOr, -1,3});
        resolution.insertClause(clause);
        resolution.binaryMergeResolutionAndEquivalence(clause,-1,3,true);
        //simplifier.equivalenceClasses.processTasks(true);
        assertEquals("2: 1v2\n", resolution.clauses.toString());
        assertEquals("1,3", resolution.localModelString());
        assertEquals("[]",resolution.equivalences.toString());
    }
    public void testIsSubsumedByBinaryClauses()  {
        System.out.println("binaryClauseIsSubsumed");
        int predicates = 6;
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Resolution resolution = new Resolution(predicates, monitor, true, nextId);
        resolution.insertClause(new Clause(new int[]{2, cOr, 1, 2}));
        resolution.insertClause(new Clause(new int[]{3, cOr, 1, 3}));
        Clause clause = new Clause(new int[]{4, cOr, 1, 3});
        assertNotNull(resolution.binaryClauseIsSubsumed(clause));
        clause = new Clause(new int[]{5, cOr, 3, 1});
        assertNotNull(resolution.binaryClauseIsSubsumed(clause));
        clause = new Clause(new int[]{6, cOr, 1, 4});
        assertNull(resolution.binaryClauseIsSubsumed(clause));
    }
    public void testResolveBetweenBinaryClauses() throws Result{
        System.out.println("resolveBetweenBinaryClauses");
        int predicates = 6;
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Resolution resolution = new Resolution(predicates, monitor, true, nextId);
        Clause clause1 = new Clause(new int[]{2, cOr, 1, 2});
        Clause clause2 = new Clause(new int[]{3, cOr, 3, -2});
        Clause resolvent = resolution.resolveBetweenBinaryClauses(clause1,clause2);
        assertEquals("11: 1v3",resolvent.toString());

        Clause clause3 = new Clause(new int[]{4, cOr, 3, 2});
        assertNull(resolution.resolveBetweenBinaryClauses(clause3,clause2));
        assertEquals("",resolution.clauses.toString());
        assertEquals("3", resolution.model.toString());
        assertEquals("3", resolution.localModelString());
        //System.out.println(simplifier.model.getInferenceStep(3).toString());

        resolution.clear();
        resolution.insertClause(new Clause(new int[]{2, cOr, 1, 2}));
        clause1 = new Clause(new int[]{3, cOr, 1, 3});
        resolution.insertClause(clause1);
        clause2 = new Clause(new int[]{4, cOr, 2, -3});
        assertNull(resolution.resolveBetweenBinaryClauses(clause2,clause1));
    }
    public void testBinaryClauseResolutionCompletion() throws Result {
        System.out.println("saturateBinaryClausesWithBinaryClause");
        int predicates = 6;
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Resolution resolution = new Resolution(predicates, monitor, true, nextId);
        Clause clause1 = new Clause(new int[]{2, cOr, 1, 2});
        resolution.insertClause(clause1);
        Clause clause2 = new Clause(new int[]{3, cOr, -1,3 });
        resolution.insertClause(clause2);
        Clause clause3 = new Clause(new int[]{4, cOr, -1,4 });
        resolution.insertClause(clause3);
        Clause clause4 = new Clause(new int[]{5, cOr, -2,3 });
        resolution.insertClause(clause4);
        resolution.saturateBinaryClausesWithBinaryClause(clause1);
        assertEquals(" 2: 1v2\n" +
                " 3: -1v3\n" +
                " 4: -1v4\n" +
                " 5: -2v3\n" +
                "11: 2v4\n" +
                "12: 2v3\n" +
                "13: 1v3\n", resolution.clauses.toString());
      //  System.out.println(simplifier.statistics.toString());

        resolution.clear();

        clause1 = new Clause(new int[]{1, cOr, 1, 2});
        resolution.insertClause(clause1);
        clause2 = new Clause(new int[]{2, cOr, -1,3 });
        resolution.insertClause(clause2);
        clause3 = new Clause(new int[]{3, cOr, -1,2 });
        resolution.insertClause(clause3);
        clause4 = new Clause(new int[]{4, cOr, -2,3 });
        resolution.insertClause(clause4);
        resolution.saturateBinaryClausesWithBinaryClause(clause1);
        assertEquals(" 2: -1v3\n" +
                " 4: -2v3\n" +
                "14: 2v3\n" +
                "15: 1v3\n", resolution.clauses.toString());
        assertEquals("2", resolution.model.toString());
        assertEquals("-1,2", resolution.localModelString());
      //  System.out.println(simplifier.statistics.toString());
    }
    public void testProcessBinaryClause1() throws Result {
        System.out.println("processBinaryClause 1");
        int predicates = 6;
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Resolution resolution = new Resolution(predicates, monitor, true, nextId);
        Clause clause1 = new Clause(new int[]{1, cOr, 1, 2});
        resolution.insertClause(clause1);
        Clause clause2 = new Clause(new int[]{2, cOr, -1, 3, 4});
        resolution.insertClause(clause2);
        Clause clause3 = new Clause(new int[]{3, cOr, 2,4,1});
        resolution.insertClause(clause3);
        Clause clause4 = new Clause(new int[]{4, cOr, 5,2,1});
        resolution.insertClause(clause4);
        Clause clause5 = new Clause(new int[]{5, cOr, 4,5,6});
        resolution.insertClause(clause5);

        resolution.processBinaryClause(clause1);
        assertEquals(" 1: 1v2\n" +
                " 2: -1v3v4\n" +
                " 5: 4v5v6\n" +
                "11: 2v3v4\n", resolution.clauses.toString());
        assertEquals("", resolution.model.toString());
        assertEquals("2,4,5", resolution.localModelString());
      //  System.out.println(simplifier.statistics.toString());
    }

    public void testProcessBinaryClause2() throws Result {
        System.out.println("processBinaryClause 2");
        int predicates = 6;
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Resolution resolution = new Resolution(predicates, monitor, true, nextId);
        Clause clause1 = new Clause(new int[]{1, cOr, 1, 5});
        resolution.insertClause(clause1);
        Clause clause2 = new Clause(new int[]{2, cOr, -1, 3, 4});
        resolution.insertClause(clause2);
        Clause clause3 = new Clause(new int[]{3, cOr, 2,4,1});
        resolution.insertClause(clause3);
        Clause clause4 = new Clause(new int[]{4, cOr, 5,2,-1});
        resolution.insertClause(clause4);
        Clause clause5 = new Clause(new int[]{5, cOr, 4,5,6});
        resolution.insertClause(clause5);
        Clause clause6 = new Clause(new int[]{6, cOr, -5,2});
        resolution.insertClause(clause6);

        resolution.processBinaryClause(clause1);
        assertEquals(" 1: 1v5\n" +
                " 2: -1v3v4\n" +
                " 3: 2v4v1\n" +
                " 4: 5v2v-1\n" +
                " 5: 4v5v6\n" +
                " 6: -5v2\n" +
                "11: 1v2\n" +
                "12: 5v2\n" +
                "13: 5v3v4\n", resolution.clauses.toString());
        resolution.processTasks(3);

        assertEquals("1: 1v5\n" +
                "5: 4v5v6\n", resolution.clauses.toString());
        assertEquals("1,2,3,4,5", resolution.localModelString());
       // System.out.println(resolution.statistics.toString());
    }

    public void testProcessBinaryClause3() throws Result {
        System.out.println("processBinaryClause 3");
        Thread myThread = Thread.currentThread();
        int predicates = 6;
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Resolution resolution = new Resolution(predicates, monitor, true, nextId);
        resolution.model.addObserver(myThread,
                (Integer literal, InferenceStep step) -> {try{
                        resolution.addInternalTrueLiteralTask(literal,true,step);} catch(Unsatisfiable uns) {}});
        Clause clause1 = new Clause(new int[]{1, cOr, 1, 5});
        resolution.insertClause(clause1);
        Clause clause2 = new Clause(new int[]{2, cOr, -1, 3, 4});
        resolution.insertClause(clause2);
        Clause clause3 = new Clause(new int[]{3, cOr, 2,4,1});
        resolution.insertClause(clause3);
        Clause clause4 = new Clause(new int[]{4, cOr, 5,2,-1});
        resolution.insertClause(clause4);
        Clause clause5 = new Clause(new int[]{5, cOr, 4,5,6});
        resolution.insertClause(clause5);
        Clause clause6 = new Clause(new int[]{6, cOr, -5,2});
        resolution.insertClause(clause6);
        resolution.processBinaryClause(clause1);

        try{
            resolution.processTasks(6);}
        catch(Satisfiable satisfiable) {
            if(monitoring) System.out.println(satisfiable.toString());}

        assertEquals("", resolution.clauses.toString());
        assertEquals("1,2,3,4,5", resolution.localModelString());
        //System.out.println(simplifier.statistics.toString());
    }
    public void testProcessEquivalence() throws Result {
        System.out.println("processEquivalence");
        int predicates = 6;
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Resolution resolution = new Resolution(predicates, monitor, true, nextId);
        resolution.insertClause(new Clause(new int[]{1, cOr, 1, 5}));
        resolution.insertClause(new Clause(new int[]{2, cOr, 5,3}));
        resolution.processEquivalence(2, 5, new InferenceTest("MyTest 1"));
        assertEquals("1: 1v2\n" +
                "2: 2v3\n", resolution.clauses.toString());

        if(monitoring) System.out.println("\nNEW 1");
        resolution.clear();
        resolution.insertClause(new Clause(new int[]{1, cOr, 1, 5}));
        resolution.insertClause(new Clause(new int[]{2, cOr, 5,2}));
        resolution.processEquivalence(2, 5, new InferenceTest("MyTest 2"));
        assertEquals("1: 1v5\n", resolution.clauses.toString());
        assertEquals("2,5", resolution.model.toString());

        if(monitoring) System.out.println("\nNEW 2");
        resolution.clear();
        resolution.insertClause(new Clause(new int[]{1, cAtleast, 2, 1,3, 5}));
        resolution.insertClause(new Clause(new int[]{2, cAtleast, 3, 4,4,5,5,5,6,6,2}));
        resolution.processEquivalence(2, 5, new InferenceTest("MyTest 3"));
        assertEquals("1: >= 2 1,3,2\n" +
                "2: >= 3 4^2,6^2,2^3\n", resolution.clauses.toString());

        if(monitoring) System.out.println("\nNEW 3");
        resolution.clear();
        resolution.insertClause(new Clause(new int[]{1, cAtleast, 2, 1,1,2,3,3}));
        resolution.processEquivalence(3, 2, new InferenceTest("MyTest 4"));
        assertEquals("1: 1v3\n", resolution.clauses.toString());

        if(monitoring) System.out.println("\nNEW 4");
        resolution.clear();
        Clause clause = new Clause(new int[]{1, cAtleast, 3, 1,1,2,3});
        resolution.insertClause(clause);
        resolution.processEquivalence(3, 2, new InferenceTest("MyTest 5"));
        assertFalse(clause.exists);
        assertEquals("1,3", resolution.model.toString());
        assertEquals("", resolution.clauses.toString());
        if(monitoring) System.out.println(resolution.statistics.toString());


        if(monitoring) System.out.println("\nNEW 5");
        resolution.clear();
        resolution.insertClause(new Clause(new int[]{1, cAtleast, 3, 1,1,2,3}));
        resolution.insertClause(new Clause(new int[]{2, cAtleast, 3, 1,2,2,3,3,4}));
        resolution.insertClause(new Clause(new int[]{3, cAtleast, 2, -1,-2,-3,-3,-4}));
        resolution.insertClause(new Clause(new int[]{4, cOr, -1,-2,-3,-4}));
        resolution.insertClause(new Clause(new int[]{5, cOr, -1,-3,}));
        resolution.insertClause(new Clause(new int[]{6, cAtleast, 2, -1,-2,-3,-3,-4,3}));
        resolution.insertClause(new Clause(new int[]{7, cAtleast, 2, 1,2,3,-4}));
        resolution.insertClause(new Clause(new int[]{8, cOr, -2,3,}));
        resolution.processEquivalence(2, 3, new InferenceTest("MyTest 6"));
        assertEquals("1,2,-4",resolution.localModelString());
        assertEquals("3: >= 2 -1,-2^2,-4\n" +
                "5: -1v-2\n" +
                "7: >= 2 1,2^2,-4\n", resolution.clauses.toString());

        if(monitoring) System.out.println(resolution.statistics.toString());

        if(monitoring) System.out.println("\nNEW 6");
        try{
        resolution.processEquivalence(2, 4, new InferenceTest("MyTest 7"));}
        catch(Result result) {if(monitoring) System.out.println(result.toString());}
    }

    public void testRemoveClausesSubsumedByLongerClause() throws Result {
        System.out.println("removeClausesSubsumedByLongerClause");
        int predicates = 6;
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Resolution resolution = new Resolution(predicates, monitor, true, nextId);
        Clause subsumer = new Clause(new int[]{1, cOr, 1,2,3});
        resolution.insertClause(subsumer);
        resolution.insertClause(new Clause(new int[]{2, cOr, 3,2,1}));
        resolution.insertClause(new Clause(new int[]{3, cOr, 2,4,3,1}));
        resolution.insertClause(new Clause(new int[]{4, cOr, 2,4,1,5}));
        resolution.removeClausesSubsumedByLongerClause(subsumer);
        assertEquals("1: 1v2v3\n" +
                "4: 2v4v1v5\n", resolution.clauses.toString());

        if(monitoring) System.out.println("\nNew");
        resolution.clear();
        subsumer = new Clause(new int[]{1, cAtleast, 4, 1,1,2,2,3,3,3});
        resolution.insertClause(subsumer);
        resolution.insertClause(new Clause(new int[]{2, cAtleast,3 , 1,1,2,2,3,3}));
        resolution.insertClause(new Clause(new int[]{3, cAtleast,3 , 1,1,1,2,2,3,3,3}));
        resolution.insertClause(new Clause(new int[]{4, cAtleast,3 , 1,1,2,2,2,3,3}));
        resolution.insertClause(new Clause(new int[]{5, cAtleast,3 , 1,1,1,2,2,2,3,3,3}));
        resolution.removeClausesSubsumedByLongerClause(subsumer);
        assertEquals("1: >= 4 1^2,2^2,3^3\n" +
                "2: >= 3 1^2,2^2,3^2\n" +
                "4: >= 3 1^2,2^3,3^2\n", resolution.clauses.toString());
        }

    public void testmergeResolutionWithLongerClauseDirect1() throws Result {
        System.out.println("mergeResolutionWithLongerClause 1");
        int predicates = 6;
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Resolution resolution = new Resolution(predicates, monitor, true, nextId);
        Clause clause = new Clause(new int[]{1, cOr, 1, 2, 3});
        resolution.insertClause(clause);
        Clause clause1 = new Clause(new int[]{2, cOr, 3, -2, 1, 4});
        resolution.insertClause(clause1);
        assertTrue(resolution.mergeResolutionWithLongerClause(clause));
        assertEquals("1: 1v2v3\n" +
                "2: 3v1v4\n", resolution.clauses.toString());
        if(monitoring) {
            System.out.println(clause1.inferenceStep.toString());
            //System.out.println(clause1.inferenceStep.inputClauseIds());
        }

        if(monitoring) System.out.println("\nNEW 2");
        resolution.clear();
        clause = new Clause(new int[]{1, cOr, 1, 2, 3});
        resolution.insertClause(clause);
        resolution.insertClause(new Clause(new int[]{2, cOr, 3, -2, 1}));
        assertFalse(resolution.mergeResolutionWithLongerClause(clause));
        assertEquals("2: 3v1\n", resolution.clauses.toString());

        if(monitoring) {
            System.out.println(clause1.inferenceStep.toString());
            //System.out.println(clause1.inferenceStep.inputClauseIds());
            ArrayList<InferenceStep> steps = new ArrayList<>();
            IntArrayList ids = new IntArrayList();
            clause1.inferenceStep.inferenceSteps(steps,ids);
            for(InferenceStep step : steps) System.out.println(step.toString());}

        if(monitoring) System.out.println("\nNEW 3");
        resolution.clear();
        clause = new Clause(new int[]{1, cOr, 1, 2, 3});
        resolution.insertClause(clause);
        resolution.insertClause(new Clause(new int[]{2, cOr, 3, -2, 4}));
        assertTrue(resolution.mergeResolutionWithLongerClause(clause));
        assertEquals("1: 1v2v3\n" +
                "2: 3v-2v4\n", resolution.clauses.toString());


        if(monitoring) System.out.println("\nNEW 4");
        resolution.clear();
        clause = new Clause(new int[]{1, cAtleast, 2, 1, 2, 3});
        resolution.insertClause(clause);
        resolution.insertClause(new Clause(new int[]{2, cAtleast, 2, -1,-1,2,2,3,3}));
        assertTrue(resolution.mergeResolutionWithLongerClause(clause));
        assertEquals("1: >= 2 1,2,3\n" +
                "2: 2v3\n", resolution.clauses.toString());

        if(monitoring) System.out.println("\nNEW 5");
        resolution.clear();
        clause = new Clause(new int[]{1, cOr, 1, 2, 3});
        resolution.insertClause(clause);
        resolution.insertClause(new Clause(new int[]{2, cOr, 3, -2, 4}));
        resolution.insertClause(new Clause(new int[]{3, cOr, 3, -2, -1}));
        resolution.insertClause(new Clause(new int[]{4, cOr, 3, -1, 4,2,5}));
        resolution.insertClause(new Clause(new int[]{5, cOr, -3, 2, 4,1}));
        assertTrue(resolution.mergeResolutionWithLongerClause(clause));
        assertEquals("1: 1v2v3\n" +
                "2: 3v-2v4\n" +
                "3: 3v-2v-1\n" +
                "4: 3v4v2v5\n" +
                "5: 2v4v1\n", resolution.clauses.toString());
        if(monitoring) System.out.println(resolution.statistics.toString());
    }

    public void testmergeResolutionWithLongerClauseDirect2 () throws Result {
        System.out.println("mergeResolutionWithLongerClause 2");
        int predicates = 6;
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Resolution resolution = new Resolution(predicates, monitor, true, nextId);
        Clause clause = new Clause(new int[]{1, cAtleast, 3, 1,1, 2, 3,3});
        resolution.insertClause(clause);
        resolution.insertClause(new Clause(new int[]{2, cAtleast, 4, 1,1,1,1, -2,-2,-2, 3,3,3,3,4}));
        resolution.insertClause(new Clause(new int[]{3, cAtleast, 4, 1,1,1,1, -2,-2,-2, 3,3,3,4}));
        resolution.mergeResolutionWithLongerClause(clause);
        assertEquals("1: >= 3 1^2,2,3^2\n" +
                "2: 1v3\n" +
                "3: >= 4 1^4,-2^3,3^3,4\n", resolution.clauses.toString());

        }
    public void testbinaryClauseIsSubsumedByBinaryClauses()  {
        System.out.println("binaryClauseIsSubsumed");
        int predicates = 6;
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Resolution resolution = new Resolution(predicates, monitor, true, nextId);

        resolution.insertClause(new Clause(new int[]{1, cOr, 1, 2}));
        resolution.insertClause(new Clause(new int[]{2, cOr, 2, 3}));
        resolution.insertClause(new Clause(new int[]{3, cOr, -2, -1}));

        Clause clause = new Clause(new int[]{4, cOr, 2, -1});
        assertNull(resolution.binaryClauseIsSubsumed(clause));
        clause = new Clause(new int[]{5, cOr, 3, 2});
        assertNotNull(resolution.binaryClauseIsSubsumed(clause));
        clause = new Clause(new int[]{6, cOr, 2,1});
        assertNotNull(resolution.binaryClauseIsSubsumed(clause));
        assertNull(resolution.binaryClauseIsSubsumed(resolution.clauses.firstClause));
    }

    public void testlongerClauseIsSubsumedByBinaryClause()  {
        System.out.println("longerClauseIsSubsumedByBinaryClause");
        int predicates = 6;
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Resolution resolution = new Resolution(predicates, monitor, true, nextId);

        resolution.insertClause(new Clause(new int[]{1, cOr, 1, 2}));
        resolution.insertClause(new Clause(new int[]{2, cOr, 2, 3}));
        resolution.insertClause(new Clause(new int[]{3, cOr, -2, -1}));

        Clause clause = new Clause(new int[]{4, cOr, 2, -1, -3});
        assertNull(resolution.longerClauseIsSubsumedByBinaryClause(clause));
        clause = new Clause(new int[]{5, cOr, 3, 2, 1});
        assertNotNull(resolution.longerClauseIsSubsumedByBinaryClause(clause));
        clause = new Clause(new int[]{6, cOr, 2,1,-3});
        assertNotNull(resolution.longerClauseIsSubsumedByBinaryClause(clause));
    }
    public void testlongerClauseIsSubsumedByLongerClause()  {
        System.out.println("longerClauseIsSubsumedByLongerClause");
        int predicates = 6;
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Resolution resolution = new Resolution(predicates, monitor, false, nextId);

        resolution.insertClause(new Clause(new int[]{1, cOr, 1, 2, 3}));
        resolution.insertClause(new Clause(new int[]{2, cOr, 2, 3, 4}));
        resolution.insertClause(new Clause(new int[]{3, cOr, -2, -1, 3}));

        Clause clause = new Clause(new int[]{4, cOr, 2, -1, -3});
        assertNull(resolution.longerClauseIsSubsumedByLongerClause(clause));
        clause = new Clause(new int[]{5, cOr, 3, 2, 1, 4});
        assertNotNull(resolution.longerClauseIsSubsumedByLongerClause(clause));
        clause = new Clause(new int[]{6, cOr, 4, 2,1,-3});
        assertNull(resolution.longerClauseIsSubsumedByLongerClause(clause));
        clause = new Clause(new int[]{7, cOr, 4, 2,1,3});
        assertNotNull(resolution.longerClauseIsSubsumedByLongerClause(clause));

        resolution.clear();

        resolution.insertClause(new Clause(new int[]{1, cAtleast, 2, 1, 2, 3}));
        clause = new Clause(new int[]{2, cOr, 4, 2,1,3});
        assertNotNull(resolution.longerClauseIsSubsumedByLongerClause(clause));

        clause = new Clause(new int[]{3, cOr, 4, 2,1,3,4});
        assertNotNull(resolution.longerClauseIsSubsumedByLongerClause(clause));

        clause = new Clause(new int[]{4, cOr, 4, 2,-1,3,4});
        assertNull(resolution.longerClauseIsSubsumedByLongerClause(clause));

        clause = new Clause(new int[]{5, cAtleast, 2, 4, 2,1,3});
        assertNotNull(resolution.longerClauseIsSubsumedByLongerClause(clause));

        resolution.insertClause(new Clause(new int[]{6, cAtleast, 3, 1, 2,2, 5}));
        clause = new Clause(new int[]{7, cAtleast, 3, 5, 2,1,3});
        assertNull(resolution.longerClauseIsSubsumedByLongerClause(clause));
        clause = new Clause(new int[]{8, cAtleast, 3, 5, 2,2,2,1,3});
        assertNotNull(resolution.longerClauseIsSubsumedByLongerClause(clause));
    }

    public void testsaturateBinaryClauseWithLongerClauses() throws Result {
        System.out.println("saturateLongerClausesWithBinaryClause");
        int predicates = 6;
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Resolution resolution = new Resolution(predicates, monitor, false, nextId);

        resolution.insertClause(new Clause(new int[]{1, cOr, 1, 2, 3}));
        resolution.insertClause(new Clause(new int[]{2, cOr, 2, 3, 4}));
        resolution.insertClause(new Clause(new int[]{3, cOr, -2, -1, 3}));
        resolution.insertClause(new Clause(new int[]{4, cAtleast, 2, 2, -1, -3}));

        Clause clause = new Clause(new int[]{5, cOr, -2, 5});
        resolution.insertClause(clause);
        resolution.saturateLongerClausesWithBinaryClause(clause);
        assertEquals(" 1: 1v2v3\n" +
                " 2: 2v3v4\n" +
                " 3: -2v-1v3\n" +
                " 4: >= 2 2,-1,-3\n" +
                " 5: -2v5\n" +
                "11: >= 2 5,-1,-3\n" +
                "12: 5v3v4\n" +
                "13: 5v1v3\n", resolution.clauses.toString());

        resolution.clear();

        resolution.insertClause(new Clause(new int[]{1, cOr, 1, 2, -5}));
        resolution.insertClause(new Clause(new int[]{2, cOr, 2, 3, 4, 5}));
        resolution.insertClause(new Clause(new int[]{3, cOr, 2, 3, 5}));

        clause = new Clause(new int[]{4, cOr, -2, 5});
        resolution.insertClause(clause);
        resolution.saturateLongerClausesWithBinaryClause(clause);
        assertEquals(" 1: 1v2v-5\n" +
                " 2: 2v3v4v5\n" +
                " 3: 2v3v5\n" +
                " 4: -2v5\n" +
                "14: 5v3\n", resolution.clauses.toString());
        //System.out.println(simplifier.statistics);
    }

    public void testResolveOld() throws Result {
        System.out.println("resolve1");
        int predicates = 6;
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Resolution resolution = new Resolution(predicates, monitor, false, nextId);

        Clause clause1 = new Clause(new int[]{1, cOr, 1, 2, 3});
        resolution.insertClause(clause1);
        Clause clause2 = new Clause(new int[]{2, cOr, 1, 2, -3});
        resolution.insertClause(clause2);
        Clause clause3 = new Clause(new int[]{3, cOr, 1, -2, 3, 4});
        resolution.insertClause(clause3);
        Clause clause4 = new Clause(new int[]{4, cOr, 1, -2, -3, 4});
        resolution.insertClause(clause4);
        resolution.resolve(clause1.literals.get(2),clause2.literals.get(2));
        resolution.resolve(clause1.literals.get(1),clause3.literals.get(1));
        resolution.resolve(clause1.literals.get(1),clause4.literals.get(1));
        resolution.resolve(clause1.literals.get(1),clause3.literals.get(1));
        assertEquals(" 1: 1v2v3\n" +
                " 2: 1v2v-3\n" +
                " 3: 1v-2v3v4\n" +
                " 4: 1v-2v-3v4\n" +
                "11: 1v2\n" +
                "12: 1v3v4\n", resolution.clauses.toString());
        //System.out.println(simplifier.statistics);
    }

    }


