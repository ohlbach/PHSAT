package Solvers.Backtracker;

import Datastructures.Clause;
import Datastructures.ClauseList;
import Datastructures.Clauses.Quantifier;
import Datastructures.Literal;
import Datastructures.Results.Result;
import Datastructures.Theory.Model;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import junit.framework.TestCase;

import java.util.ArrayList;
import java.util.Arrays;

public class BacktrackerTest extends TestCase {

    static int or = Quantifier.OR.ordinal();
    static int intv = Quantifier.INTERVAL.ordinal();
    static int natl = Quantifier.ATLEAST.ordinal();
    static int natm = Quantifier.ATMOST.ordinal();

    static ClauseList makeClauses (Model model, int[]... clauses) {
        ClauseList clauseList = new ClauseList(false,false,null);
        clauseList.initialize("test",model,null);
        for(int[] inputClause : clauses) {
            Clause clause = new Clause(inputClause,false, (literal -> new Literal(literal,1)));
            clauseList.addClause(clause);}
        return clauseList;}

    static ClauseList makeClauses (Model model, Clause... clauses) {
        ClauseList clauseList = new ClauseList(false,false,null);
        clauseList.initialize("test",model,null);
        for(Clause clause : clauses) {
            clauseList.addClause(clause);}
        return clauseList;}


    static Clause makeClause (int[] inputClause) {
        return new Clause(inputClause,false, (literal -> new Literal(literal,1)));}

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
        Model model = new Model(predicates);
        backtracker.clauseList = makeClauses(model,
                new int[]{1,or,1,2,3},
                new int[]{2,or,1,3,4},
                new int[]{3,or,1,4,5});

        assertEquals("Clauses\n" +
                "    1: 1v2v3\n" +
                "    2: 1v3v4\n" +
                "    3: 1v4v5\n",backtracker.clauseList.toString("clauses",null));

        backtracker.initializePredicateSequence(3,-1); // more predicates first
        assertEquals("[0, 1, 3, 4, 2, 5]", Arrays.toString(backtracker.predicateSequence));
        assertEquals("[0, 1, 4, 2, 3, 5]", Arrays.toString(backtracker.predicatePositions));

        backtracker.initializePredicateSequence(4,-1); // less predicates first
        assertEquals("[0, 2, 5, 3, 4, 1]", Arrays.toString(backtracker.predicateSequence));
        assertEquals("[0, 5, 1, 3, 4, 2]", Arrays.toString(backtracker.predicatePositions));
    }


    public void testLocalModel() throws Result {
        System.out.println("localModel");
        Backtracker backtracker = new Backtracker(1,1,-1,1);
        int predicates = 5;
        backtracker.predicates = predicates;
        backtracker.trackReasoning = false;
        backtracker.model = new Model(predicates);
        backtracker.model.add(null,3,-4);
        backtracker.initializeLocalModel();
        assertEquals("3,-4",backtracker.toStringLocalModel());
        backtracker.setLocalStatus(2); backtracker.setLocalStatus(-5);
        assertEquals("2,3,-4,-5",backtracker.toStringLocalModel());
        assertEquals(1,backtracker.localStatus(2));
        assertEquals(-1,backtracker.localStatus(4));
        backtracker.clearLocalStatus(4);
        assertEquals("2,3,-5",backtracker.toStringLocalModel());
  }

    public void testFindeNextPredicateIndex() throws Result {
        System.out.println("findNextPrediateIndex");
        Backtracker backtracker = new Backtracker(1,1,-1,1);
        int predicates = 10;
        backtracker.predicates = predicates;
        backtracker.predicateSequence = new int[predicates+1];
        backtracker.predicatePositions = new int[predicates+1];
        backtracker.initializePredicateSequence(1,-1);
        assertEquals("[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]", Arrays.toString(backtracker.predicateSequence));
        backtracker.model = new Model(predicates);
        backtracker.model.add(null,3,-4);
        backtracker.initializeLocalModel();
        backtracker.setLocalStatus(7);
        backtracker.clauseList = makeClauses(backtracker.model,
                new int[]{1,or,1,2,3},
                new int[]{2,or,1,3,4},
                new int[]{3,or,1,4,5});
        assertEquals(1,backtracker.findNextPredicateIndex(1));
        assertEquals(5,backtracker.findNextPredicateIndex(5));
        assertEquals(0,backtracker.findNextPredicateIndex(6));
    }

    public void testJoinUsedClauses() throws Result {
        System.out.println("joinUsedClauses");
        Backtracker backtracker = new Backtracker(1, 1, -1, 1);
        int predicates = 10;
        backtracker.model = new Model(predicates);
        backtracker.model.add(null,3,-4);
        Clause c1 = makeClause(new int[]{1,or,1,2,3});
        Clause c2 = makeClause(new int[]{2,or,2,3,4});
        Clause c3 = makeClause(new int[]{3,or,3,4,5});
        Clause c4 = makeClause(new int[]{4,or,4,5,6});
        Clause c5 = makeClause(new int[]{5,or,-3,-4,-6});
        makeClauses(backtracker.model,c1,c2,c3,c4,c5);
        ArrayList[] usedClauses= new ArrayList[predicates];
        backtracker.usedClausesArray = usedClauses;
        usedClauses[3] = new ArrayList(); usedClauses[3].add(c1);
        usedClauses[4] = new ArrayList(); usedClauses[4].add(c2);usedClauses[4].add(c2);
        usedClauses[5] = new ArrayList(); usedClauses[5].add(c4);usedClauses[5].add(c3);
        backtracker.joinUsedClauses(c5,6);
        assertEquals(3,usedClauses[6].size());
        assertEquals("[5: -3v-4v-6, 1: 1v2v3, 2: 2v3v4]",usedClauses[6].toString());
    }


        public void testDependencies() throws Result {
        System.out.println("dependencies");
        Backtracker backtracker = new Backtracker(1, 1, -1, 1);
        int predicates = 5;
        backtracker.predicates = predicates;
        backtracker.predicateSequence = new int[predicates + 1];
        backtracker.predicatePositions = new int[predicates + 1];
        //backtracker.clauses = new LinkedItemList<>("Clauses");
        //backtracker.literalIndex = new LiteralIndex<>(5);

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
        Clause clause = new Clause(new int[]{1,or,-3,-1,2},false,(lit->new Literal(lit,1)));
        assertEquals(1,backtracker.getLastSelection(clause));

        backtracker.model = new Model(predicates);
        backtracker.initializeLocalModel();
        backtracker.setLocalStatus(1);
        backtracker.setLocalStatus(2);

      //  backtracker.joinDependencies(clause,dependencies);
        assertEquals("[3, 2]",dependencies.toString());


        backtracker.model.add(null,3);
       // backtracker.joinDependencies(clause,dependencies);
        assertEquals("[2]",dependencies.toString());

    }



    }