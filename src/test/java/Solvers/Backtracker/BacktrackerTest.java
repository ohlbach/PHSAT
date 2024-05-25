package Solvers.Backtracker;

import Datastructures.Clauses.Quantifier;
import Datastructures.LinkedItemList;
import Datastructures.LiteralIndex;
import Datastructures.Results.Result;
import Datastructures.Theory.Model;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import junit.framework.TestCase;

import java.util.Arrays;

public class BacktrackerTest extends TestCase {

    static int or = Quantifier.OR.ordinal();
    static int intv = Quantifier.INTERVAL.ordinal();
    static int natl = Quantifier.ATLEAST.ordinal();
    static int natm = Quantifier.ATMOST.ordinal();

    public void testInitializePredicateSequenceRandomly() {
        System.out.println("initializePredicateSequenceRandomly");
        Backtracker backtracker = new Backtracker(1,1,0,1);
        int predicates = 10;
        backtracker.predicates = predicates;
        backtracker.predicateSequence = new int[predicates+1];
        backtracker.predicatePositions = new int[predicates+1];
        backtracker.initializePredicateSequenceRandomly(0);
        assertEquals("[0, 5, 9, 10, 7, 4, 6, 3, 2, 8, 1]", Arrays.toString(backtracker.predicateSequence));
        assertEquals("[0, 10, 8, 7, 5, 1, 6, 4, 9, 2, 3]", Arrays.toString(backtracker.predicatePositions));
        backtracker.predicateSequence = new int[predicates+1];
        backtracker.predicatePositions = new int[predicates+1];
        backtracker.predicates = 5;
        backtracker.initializePredicateSequenceRandomly(0);
        assertEquals("[0, 5, 3, 2, 4, 1, 0, 0, 0, 0, 0]", Arrays.toString(backtracker.predicateSequence));
        assertEquals("[0, 5, 3, 2, 4, 1, 0, 0, 0, 0, 0]", Arrays.toString(backtracker.predicatePositions));
    }

    public void testInitializePredicateSequence() {
        System.out.println("initializePredicateSequence");
        Backtracker backtracker = new Backtracker(1,1,-1,1);
        int predicates = 10;
        backtracker.predicates = predicates;
        backtracker.predicateSequence = new int[predicates+1];
        backtracker.predicatePositions = new int[predicates+1];
        backtracker.initializePredicateSequence(1,-1);
        assertEquals("[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]", Arrays.toString(backtracker.predicateSequence));
        assertEquals("[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]", Arrays.toString(backtracker.predicatePositions));

        backtracker.initializePredicateSequence(2,-1);
        assertEquals("[0, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1]", Arrays.toString(backtracker.predicateSequence));
        assertEquals("[0, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1]", Arrays.toString(backtracker.predicatePositions));

        predicates = 5;
        backtracker.predicates = predicates;
        backtracker.predicateSequence = new int[predicates+1];
        backtracker.predicatePositions = new int[predicates+1];
        backtracker.clauses = new LinkedItemList<>("Clauses");
        backtracker.literalIndex = new LiteralIndex<>(5);

        backtracker.readInputClauses(
                new int[]{1,or,1,2,3},
                new int[]{2,or,1,3,4},
                new int[]{3,or,1,4,5});

        assertEquals("Clauses\n" +
                "    1: 1v2v3\n" +
                "    2: 1v3v4\n" +
                "    3: 1v4v5\n",backtracker.clauses.toString());

        backtracker.initializePredicateSequence(3,-1); // more literals first
        assertEquals("[0, 1, 3, 4, 2, 5]", Arrays.toString(backtracker.predicateSequence));
        assertEquals("[0, 1, 4, 2, 3, 5]", Arrays.toString(backtracker.predicatePositions));

        backtracker.initializePredicateSequence(4,-1); // less literals first
        assertEquals("[0, 2, 5, 3, 4, 1]", Arrays.toString(backtracker.predicateSequence));
        assertEquals("[0, 5, 1, 3, 4, 2]", Arrays.toString(backtracker.predicatePositions));
    }


    public void testLocalModel() throws Result {
        System.out.println("localModel");
        Backtracker backtracker = new Backtracker(1,1,-1,1);
        int predicates = 5;
        backtracker.predicates = predicates;
        backtracker.model = new Model(predicates);
        backtracker.model.add(null,3,-4);
        backtracker.initializeLocalModel();
        assertEquals("1:0,2:0,3:1,4:-1,5:0",backtracker.toStringLocalModel());
        backtracker.setLocalStatus(2); backtracker.setLocalStatus(-5);
        assertEquals("1:0,2:1,3:1,4:-1,5:-1",backtracker.toStringLocalModel());
        assertEquals(1,backtracker.localStatus(2));
        assertEquals(-1,backtracker.localStatus(4));
        backtracker.clearLocalStatus(4);
        assertEquals("1:0,2:1,3:1,4:0,5:-1",backtracker.toStringLocalModel());
  }
    public void testDependencies() throws Result {
        System.out.println("dependencies");
        Backtracker backtracker = new Backtracker(1, 1, -1, 1);
        int predicates = 5;
        backtracker.predicates = predicates;
        backtracker.predicateSequence = new int[predicates + 1];
        backtracker.predicatePositions = new int[predicates + 1];
        backtracker.clauses = new LinkedItemList<>("Clauses");
        backtracker.literalIndex = new LiteralIndex<>(5);

        backtracker.initializePredicateSequence(1, 0);
        assertEquals("[0, 5, 3, 2, 4, 1]", Arrays.toString(backtracker.predicateSequence));
        assertEquals("[0, 5, 3, 2, 4, 1]", Arrays.toString(backtracker.predicatePositions));

        IntArrayList dependencies = IntArrayList.wrap(new int[]{3,1,2});
        assertEquals(1,backtracker.getLastSelection(dependencies));
        assertEquals(1,backtracker.getAndRemoveLastSelection(dependencies));
        assertEquals("[3, 2]",dependencies.toString());

        dependencies = IntArrayList.wrap(new int[]{3});
        assertEquals(3,backtracker.getLastSelection(dependencies));
        assertEquals(3,backtracker.getAndRemoveLastSelection(dependencies));
        assertEquals("[]",dependencies.toString());

        dependencies = IntArrayList.wrap(new int[]{2,3});
        assertEquals(2,backtracker.getLastSelection(dependencies));
        assertEquals(2,backtracker.getAndRemoveLastSelection(dependencies));
        assertEquals("[3]",dependencies.toString());

        backtracker.dependentSelections = new IntArrayList[predicates+1];
        backtracker.dependentSelections[1] = IntArrayList.wrap(new int[]{3, 2});
        backtracker.dependentSelections[2] = IntArrayList.wrap(new int[]{3});
        backtracker.dependentSelections[3] = IntArrayList.wrap(new int[]{1,3});
        Clause clause = new Clause(new int[]{1,or,-3,-1,2});
        assertEquals(1,backtracker.getLastSelection(clause));

        backtracker.model = new Model(predicates);
        backtracker.initializeLocalModel();
        backtracker.setLocalStatus(1);
        backtracker.setLocalStatus(2);

        backtracker.joinDependencies(clause,dependencies);
        assertEquals("[3, 2]",dependencies.toString());


        backtracker.model.add(null,3);
        backtracker.joinDependencies(clause,dependencies);
        assertEquals("[2]",dependencies.toString());

    }

    public void testRemoveLiteral() throws Result {
        System.out.println("removeLiteral");
        long start = System.nanoTime();
        Backtracker backtracker = new Backtracker(1, 1, -1, 1);
        backtracker.monitor = (string) -> System.out.println(string);
        backtracker.monitoring = true;
        int predicates = 5;
        backtracker.predicates = predicates;
        backtracker.clauses = new LinkedItemList<>("Clauses");
        backtracker.literalIndex = new LiteralIndex<>(predicates);
        backtracker.model = new Model(predicates);

        backtracker.readInputClauses(
                new int[]{1,or,1,2,3},
                new int[]{2,or,1,3,4},
                new int[]{3,or,-1,4});

        Clause c = backtracker.clauses.getLinkedItem(1);
        backtracker.removeLiteral(c.literals.get(1));
        assertEquals("Clauses\n" +
                "    1: 1v2v3\n" +
                "    2: 1v4\n" +
                "    3: -1v4\n", backtracker.clauses.toString());

        backtracker.removeLiteral(c.literals.get(1));
        assertEquals("Clauses\n" +
                "    1: 1v2v3\n" +
                "    3: -1v4\n", backtracker.clauses.toString());
        assertEquals("1",backtracker.model.toString());
        try {
            c = backtracker.clauses.getLinkedItem(1);
            backtracker.removeLiteral(c.literals.get(1));
        }
        catch(Result result) {
            result.complete("TestProblem", "Backtracker",start);
            System.out.println("RES "+ result.toString() + "|");
        }}

    public void testRemoveGloballyTrueLiteral() throws Result {
        System.out.println("removeGloballyTrueLiteral");
        long start = System.nanoTime();
        Backtracker backtracker = new Backtracker(1, 1, -1, 1);
        int predicates = 5;
        backtracker.predicates = predicates;
        backtracker.initializeProblemSpecifics();
        backtracker.monitor = (string) -> System.out.println(string);
        backtracker.monitoring = true;
        backtracker.model = new Model(predicates);
        backtracker.initializeLocalModel();

        backtracker.readInputClauses(
                new int[]{1,or,1,2,3},
                new int[]{2,or,1,3,4},
                new int[]{3,or,-1,-4,5});

        backtracker.removeGloballyTrueLiteral(-3);
        assertEquals("Clauses\n" +
                "    1: 1v2\n" +
                "    2: 1v4\n"+
                "    3: -1v-4v5\n", backtracker.clauses.toString());

        backtracker.removeGloballyTrueLiteral(-1);
        assertEquals("Clauses\n", backtracker.clauses.toString());
        assertEquals("2,4",backtracker.model.toString());


        backtracker.initializeProblemSpecifics();
        backtracker.model = new Model(predicates);
        backtracker.initializeLocalModel();

        backtracker.readInputClauses(
                new int[]{1,intv,2,2,1,1,2,2,-3},
                new int[]{2, natl, 2, 1, 1, 2, 2, -3,4},
                new int[]{3, intv,2,2, 1,1,1,2,-3,4,5},
                new int[]{5, intv,2,2, 5,2,-3});

        backtracker.removeGloballyTrueLiteral(3);
        assertEquals("Clauses\n" +
                "  1.1: =1 1,2\n" +
                "  2.2: 1v2\n" +
                "  3.1: =2 2,4,5\n", backtracker.clauses.toString());
        assertEquals("-1,2,5",backtracker.model.toString());
        assertEquals(1,backtracker.literalIndex.size(4));
        assertEquals(1,backtracker.literalIndex.size(5));

        start = System.nanoTime();
        backtracker.initializeProblemSpecifics();
        backtracker.model = new Model(predicates);
        backtracker.initializeLocalModel();

        backtracker.readInputClauses(
                new int[]{1,natl,2,1,2,2});
        try{backtracker.removeGloballyTrueLiteral(-2);}
        catch(Result result) {
            result.complete("TestProblem 1", "Backtracker",start);
            System.out.println(result.toString());}

        start = System.nanoTime();
        backtracker.initializeProblemSpecifics();
        backtracker.model = new Model(predicates);
        backtracker.model.add(null,-1);
        backtracker.initializeLocalModel();

        backtracker.readInputClauses(
                new int[]{1,natl,2,1,2,3});
        try{backtracker.removeGloballyTrueLiteral(-3);}
        catch(Result result) {
            result.complete("TestProblem 2", "Backtracker",start);
            System.out.println(result.toString());}
    }

    public void testRemoveGloballyTrueLiteralWithBacktracking() throws Result {
        System.out.println("removeGloballyTrueLiteral with backtracking");
        long start = System.nanoTime();
        Backtracker backtracker = new Backtracker(1, 1, -1, 1);
        int predicates = 5;
        backtracker.predicates = predicates;
        backtracker.initializeProblemSpecifics();
        backtracker.monitor = (string) -> System.out.println(string);
        backtracker.monitoring = true;
        backtracker.model = new Model(predicates);
        backtracker.initializeLocalModel();

        backtracker.initializePredicateSequence(1,-1);
        assertEquals("[0, 1, 2, 3, 4, 5]", Arrays.toString(backtracker.predicateSequence));
        assertEquals("[0, 1, 2, 3, 4, 5]", Arrays.toString(backtracker.predicatePositions));
        backtracker.dependentSelections[3] = IntArrayList.wrap(new int[]{1});
        backtracker.dependentSelections[4] = IntArrayList.wrap(new int[]{2});
        backtracker.model.add(null,-1);

        backtracker.readInputClauses(
                new int[]{1, or, 1, 2, 3},
                new int[]{2, or, 1, 3, 4},
                new int[]{3, or, -1, -4, 5});
        backtracker.removeGloballyTrueLiteral(-1);
        assertEquals(2,backtracker.backtrackPredicate);
        assertTrue(backtracker.backtrackTryAgain);
    }
    }