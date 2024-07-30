package Solvers.Backtracker;

import Datastructures.Clause;
import Datastructures.ClauseList;
import Datastructures.Clauses.Quantifier;
import Datastructures.Literal;
import Datastructures.Results.Result;
import Datastructures.Symboltable;
import Datastructures.Theory.Model;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import junit.framework.TestCase;

import java.util.ArrayList;
import java.util.Arrays;

public class BacktrackerTest extends TestCase {

    static int or = Quantifier.OR.ordinal();
    static int intv = Quantifier.INTERVAL.ordinal();
    static int ex = Quantifier.EXACTLY.ordinal();
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

        assertEquals("Clauses:\n" +
                "  1: 1v2v3\n" +
                "  2: 1v3v4\n" +
                "  3: 1v4v5\n",backtracker.clauseList.toString("clauses",null));

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
        backtracker.makeLocallyTrue(2); backtracker.makeLocallyTrue(-5);
        assertEquals("2,3,-4,-5",backtracker.toStringLocalModel());
        assertEquals(1,backtracker.localStatus(2));
        assertEquals(-1,backtracker.localStatus(4));
        backtracker.localModel[4] = 0;
        assertEquals("2,3,-5",backtracker.toStringLocalModel());
  }

    public void testFindNextPredicateIndex() throws Result {
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
        backtracker.makeLocallyTrue(7);
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

    public void testCompatibleLocally() throws Result {
        System.out.println("compatibleLocally");
        Backtracker backtracker = new Backtracker(1,1,-1,1);
        int predicates = 10;
        backtracker.predicates = predicates;
        backtracker.model = new Model(predicates);
        backtracker.initializeLocalModel();

        IntArrayList preds = IntArrayList.wrap(new int[]{1,2,3});
        int model = 0;
        assertTrue(backtracker.compatibleLocally(model,preds));
        backtracker.makeLocallyTrue(2);
        model = 1;
        assertFalse(backtracker.compatibleLocally(model,preds));
        model = 2;
        assertTrue(backtracker.compatibleLocally(model,preds));
        model = 3;
        assertTrue(backtracker.compatibleLocally(model,preds));
        backtracker.makeLocallyTrue(-1);
        assertFalse(backtracker.compatibleLocally(model,preds));
        }

    public void testVerifyTrueLiteral() throws Result {
        System.out.println("verifyTrueLiteral");
        Backtracker backtracker = new Backtracker(1,1,-1,1);
        int predicates = 10;
        backtracker.predicates = predicates;
        backtracker.model = new Model(predicates);
        backtracker.initializeLocalModel();
        Clause c1 = makeClause(new int[]{1,or,1,2,3});
        backtracker.makeLocallyTrue(-1);
        assertFalse(backtracker.verifyTrueLiteral(c1,3, false));
        backtracker.makeLocallyTrue(-2);
        assertTrue(backtracker.verifyTrueLiteral(c1,3, false));

        Clause c2 = makeClause(new int[]{2,ex,0,4,5,6});
        assertTrue(backtracker.verifyTrueLiteral(c2,-5, false));

        Clause c3 = makeClause(new int[]{3,ex,1,4,5,6});
        assertFalse(backtracker.verifyTrueLiteral(c3, -4, false));
        backtracker.makeLocallyTrue(5);
        assertTrue(backtracker.verifyTrueLiteral(c3, -4, false));

        Clause c5 = makeClause(new int[]{5,intv,1,2,7,8,9});
        assertFalse(backtracker.verifyTrueLiteral(c5, 9, false));
        backtracker.makeLocallyTrue(-7);
        assertFalse(backtracker.verifyTrueLiteral(c5, 9, false));
        backtracker.makeLocallyTrue(-8);
        assertTrue(backtracker.verifyTrueLiteral(c5, 9, false));}

        static class Backtracker1 extends Backtracker{

        /**
         * constructs a new Backtracker.
         *
         * @param solverNumber         for enumerating the solvers.
         * @param predicateArrangement for initializing the sequence of predicates to be selected.<br>
         *                             -  1: just the sequence of natural numbers: 1,2,...<br>
         *                             -  2: just the inverse sequence of natural numbers: n,n-1,...<br>
         *                             -  3: predicates with more literal occurrences first<br>
         *                             -  4: predicates with less literal occurrences first.
         * @param seed                 if seed &gt;= 0 then the predicates are sorted randomly, and predicateArrangement is ignored.
         * @param firstSign            : +1 selected predicates are always true, -1: selected predicates are always false.
         */
        public Backtracker1(int solverNumber, int predicateArrangement, int seed, int firstSign) {
            super(solverNumber, predicateArrangement, seed, firstSign);
            predicates = 20;
            localModel = new byte[predicates+1];
            model = new Model(predicates);
            clauseList = new ClauseList(false,false,null);
            verify = true;}

        ArrayList<Clause> clauses = new ArrayList<>();
        IntArrayList literals = new IntArrayList();

        boolean makeLiteralLocallyTrue(Clause clause, Literal literalObject, int sign) {
            clauses.add(clause);
            int literal = sign * literalObject.literal;
            literals.add(literal);
            if(!verifyTrueLiteral(clause,literal, false)) {
                System.err.println("verifyTrueLiteral failed: " + clause.toString(symboltable,0) +
                        " derived literal: " + Symboltable.toString(literal,symboltable) +
                        "\nLocal Model: " + toStringLocalModel());
                System.exit(1);}
            return false;}

        IntArrayList falseClauseIds = new IntArrayList();

        @Override
        protected synchronized Clause  falseClauseFound(Clause clause) {
             System.out.println("FALSE Clause " + clause.toString(symboltable,0));
             falseClauseIds.add(clause.id);
             return clause;}
        }

    public void testAnalyseClause() throws Result {
        System.out.println("analyseClause");
        Backtracker1 backtracker = new Backtracker1(1, 1, -1, 1);
        Clause c1 = makeClause(new int[]{1,or,1,2,3});
        assertNull(backtracker.analyseClause(c1));
        backtracker.makeLocallyTrue(-3);
        assertNull(backtracker.analyseClause(c1));
        backtracker.makeLocallyTrue(-1);
        assertNull(backtracker.analyseClause(c1));
        assertEquals(1,backtracker.clauses.size());
        assertEquals(1,backtracker.literals.size());
        assertEquals(c1,backtracker.clauses.get(0));
        assertEquals(2,backtracker.literals.getInt(0));
        backtracker.makeLocallyTrue(-2);
        assertEquals(c1,backtracker.analyseClause(c1));

        Clause c2 = makeClause(new int[]{2,intv,1,2, 4,5,6});
        assertNull(backtracker.analyseClause(c2));
        backtracker.makeLocallyTrue(-5);
        assertNull(backtracker.analyseClause(c2));
        backtracker.makeLocallyTrue(-6);
        assertNull(backtracker.analyseClause(c2));
        assertEquals(2,backtracker.clauses.size());
        assertEquals(2,backtracker.literals.size());
        assertEquals(c2,backtracker.clauses.get(1));
        assertEquals(4,backtracker.literals.getInt(1));

        Clause c3 = makeClause(new int[]{3,ex,1,7,8,9});
        assertNull(backtracker.analyseClause(c3));

        backtracker.makeLocallyTrue(7);
        assertNull(backtracker.analyseClause(c3));
        assertEquals("[2, 4, -8, -9]",backtracker.literals.toString());

        Clause c4 = makeClause(new int[]{4,ex,0, 10,11,12});
        assertNull(backtracker.analyseClause(c4));
        assertEquals("[2, 4, -8, -9, -10, -11, -12]",backtracker.literals.toString());
 }

    public void testPropagateLocally() throws Result {
        System.out.println("propagateLocally");
        Backtracker1 backtracker = new Backtracker1(1, 1, -1, 1);
        backtracker.clauseList = makeClauses(backtracker.model,
                new int[]{1,or,1,2,3},
                new int[]{2,ex,1,1,2,4},
                new int[]{3,intv,1,2,-1,-2,-5});
        backtracker.propagateLocally(-1);
        assertEquals(0,backtracker.falseClauseIds.size());
        backtracker.makeLocallyTrue(-2);

        assertNull(backtracker.propagateLocally(-1));
        assertEquals(0,backtracker.falseClauseIds.size());
        assertEquals("[5, 3, 4]",backtracker.literals.toString());

        backtracker.localModel = new byte[backtracker.predicates+1];
        backtracker.makeLocallyTrue(1);
        assertTrue(backtracker.propagateLocally(2) != null);
        assertEquals("[2]",backtracker.falseClauseIds.toString());

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

        backtracker.model = new Model(predicates);
        backtracker.initializePredicateSequence(1, 0);
        assertEquals("[0, 5, 3, 2, 4, 1]", Arrays.toString(backtracker.predicateSequence));
        assertEquals("[0, 5, 3, 2, 4, 1]", Arrays.toString(backtracker.predicatePositions));

        IntArrayList dependencies = IntArrayList.wrap(new int[]{3,1,2});
        assertEquals(1,backtracker.getLastSelectedPredicate(dependencies));

        dependencies = IntArrayList.wrap(new int[]{3});
        assertEquals(3,backtracker.getLastSelectedPredicate(dependencies));

        dependencies = IntArrayList.wrap(new int[]{2,3});
        assertEquals(2,backtracker.getLastSelectedPredicate(dependencies));

        backtracker.dependentSelections = new IntArrayList[predicates+1];
        backtracker.dependentSelections[1] = IntArrayList.wrap(new int[]{3, 2});
        backtracker.dependentSelections[2] = IntArrayList.wrap(new int[]{3});
        backtracker.dependentSelections[3] = IntArrayList.wrap(new int[]{1,3});
        Clause clause = new Clause(new int[]{1,or,-3,-1,2},false,(lit->new Literal(lit,1)));
        assertEquals(1,backtracker.getLastSelectedPredicate(clause));

        backtracker.initializeLocalModel();
        backtracker.makeLocallyTrue(1);
        backtracker.makeLocallyTrue(2);

        predicates = 10;
        backtracker.predicates = predicates;
        backtracker.usedClausesArray = new ArrayList[predicates+1];
        backtracker.dependentSelections = new IntArrayList[predicates+1];

        Clause c1 = makeClause(new int[]{1,or,1,2,3});
        Clause c2 = makeClause(new int[]{2,or,2,3,4});
        Clause c3 = makeClause(new int[]{3,or,3,4,5});
        Clause c4 = makeClause(new int[]{4,or,4,5,6});
        Clause c5 = makeClause(new int[]{5,or,5,6,7});
        ArrayList<Clause> a1 = new ArrayList<>();
        a1.add(c2); a1.add(c3);
        backtracker.usedClausesArray[1] = a1;
        ArrayList<Clause> a2 = new ArrayList<>();
        a2.add(c3); a2.add(c4);
        backtracker.usedClausesArray[2] = a2;

        IntArrayList l1 = new IntArrayList();
        l1.add(5); l1.add(6);
        backtracker.dependentSelections[1] = l1;
        IntArrayList l2 = new IntArrayList();
        l2.add(6);
        l2.add(7);
        backtracker.dependentSelections[2] = l2;
        IntArrayList jdp = backtracker.joinDependencies(c1,3);
        assertEquals("[5, 6, 7]",jdp.toString());
        assertTrue(jdp == backtracker.dependentSelections[3]);

        ArrayList<Clause> ucl = backtracker.usedClausesArray[3];
        assertEquals("[1: 1v2v3, 2: 2v3v4, 3: 3v4v5, 4: 4v5v6]",ucl.toString());

        backtracker.model.addImmediately(1);
        jdp = backtracker.joinDependencies(c1,3);
        assertEquals("[6, 7]",jdp.toString());
        assertTrue(jdp == backtracker.dependentSelections[3]);

        ucl = backtracker.usedClausesArray[3];
        assertEquals("[1: 1v2v3, 3: 3v4v5, 4: 4v5v6]",ucl.toString());

        IntArrayList l3 = new IntArrayList();
        l3.add(7); l3.add(8);
        backtracker.dependentSelections[3] = l3;
        backtracker.localModel[3] = 1;
        jdp = backtracker.joinDependencies(c1,6);
        assertEquals("[6, 7, 8]",jdp.toString());
    }


    public void testRemoveSelectedTrueLiteral() throws Result {
        System.out.println("removeSelectedTrueLiteral");
        Backtracker backtracker = new Backtracker(1, 1, -1, 1);
        backtracker.statistics = new StatisticsBacktracker("Test");
        int predicates = 10;
        backtracker.predicates = predicates;
        backtracker.model = new Model(predicates);
        backtracker.currentlyTrueLiterals = IntArrayList.wrap(new int[]{0,1,2,0,3,4,0,5,6,7,8,9});
        backtracker.model.addImmediately(5);
        backtracker.dependentSelections = new IntArrayList[predicates+1];
        backtracker.dependentSelections[7] = new IntArrayList();
        backtracker.dependentSelections[7].add(5);
        backtracker.dependentSelections[7].add(1);
        Clause c1 = makeClause(new int[]{1,or,1,2,3});
        Clause c2 = makeClause(new int[]{2,or,3,4,5});
        ArrayList<Clause> depClauses = new ArrayList<>();
        depClauses.add(c1); depClauses.add(c2);
        backtracker.usedClausesArray = new ArrayList[predicates+1];
        backtracker.usedClausesArray[2] = depClauses;

        backtracker.incorporateGlobalChanges();
        assertEquals("[0, 1, 2, 0, 3, 4, 6, 7, 8, 9]", backtracker.currentlyTrueLiterals.toString());
        assertEquals("[1]",backtracker.dependentSelections[7].toString());

        backtracker.model.addImmediately(1);

        backtracker.incorporateGlobalChanges();
        assertEquals("[0, 3, 4, 6, 7, 8, 9]", backtracker.currentlyTrueLiterals.toString());
        assertEquals("[]",backtracker.dependentSelections[7].toString());
        assertEquals("1,2,5",backtracker.model.toString());

        System.out.println(backtracker.model.getInferenceStep(2).toString());

        backtracker.currentlyTrueLiterals = IntArrayList.wrap(new int[]{0,1,2,0,3,4,0,5,6,7,8,9});
        backtracker.model = new Model(predicates);
        backtracker.model.addImmediately(1,5);
        backtracker.incorporateGlobalChanges();
        assertEquals("[0, 3, 4, 6, 7, 8, 9]", backtracker.currentlyTrueLiterals.toString());
        assertEquals("1,2,5",backtracker.model.toString());

    }


    public void testRemoveSelectedFalseLiteral() throws Result {
        System.out.println("removeSelectedFalseLiteral");
        Backtracker backtracker = new Backtracker(1, 1, -1, 1);
        backtracker.statistics = new StatisticsBacktracker("Test");
        int predicates = 10;
        backtracker.predicates = predicates;
        backtracker.initializePredicateSequence(1, -1);
        backtracker.selectedPredicatePosition = 5;
        backtracker.model = new Model(predicates);
        backtracker.localModel = new byte[predicates+1];
        backtracker.currentlyTrueLiterals = IntArrayList.wrap(new int[]{0,1,2,0,3,4,0,5,6,7,8,9});
        backtracker.model.addImmediately(-5);
        backtracker.makeLocallyTrue(8);

        backtracker.incorporateGlobalChanges();
        assertEquals("[0, 1, 2, 0, 3, 4]", backtracker.currentlyTrueLiterals.toString());
        assertEquals(3,backtracker.selectedPredicatePosition);
        assertEquals("",backtracker.toStringLocalModel());


        backtracker.model.addImmediately(-1);
        backtracker.incorporateGlobalChanges();
        assertEquals("[]", backtracker.currentlyTrueLiterals.toString());
        assertEquals(1,backtracker.selectedPredicatePosition);
    }

    public void testRemoveDerivedTrueLiteral() throws Result {
        System.out.println("removeDerivedTrue");
        Backtracker backtracker = new Backtracker(1, 1, -1, 1);
        backtracker.statistics = new StatisticsBacktracker("Test");
        int predicates = 10;
        backtracker.predicates = predicates;
        backtracker.initializePredicateSequence(1, -1);
        backtracker.dependentSelections = new IntArrayList[predicates+1];
        backtracker.selectedPredicatePosition = 5;
        backtracker.model = new Model(predicates);
        backtracker.localModel = new byte[predicates + 1];
        backtracker.currentlyTrueLiterals = IntArrayList.wrap(new int[]{0, 1, 2, 0, 3, 4, 0, 5, 6, 7, 8, 9});
        backtracker.model.addImmediately(6);

        backtracker.incorporateGlobalChanges();
        assertEquals("[0, 1, 2, 0, 3, 4, 0, 5, 7, 8, 9]", backtracker.currentlyTrueLiterals.toString());

    }

    public void testRemoveDerivedFalseLiteral() throws Result {
        System.out.println("removeDerivedFalse");
        Backtracker backtracker = new Backtracker(1, 1, -1, 1);
        backtracker.statistics = new StatisticsBacktracker("Test");
        int predicates = 10;
        backtracker.predicates = predicates;
        backtracker.initializePredicateSequence(1, -1);
        backtracker.monitoring = true;
        backtracker.monitor = (string -> System.out.println(string));
        backtracker.dependentSelections = new IntArrayList[predicates+1];
        backtracker.selectedPredicatePosition = 5;
        backtracker.model = new Model(predicates);
        backtracker.localModel = new byte[predicates + 1];
        backtracker.dependentSelections[6] = IntArrayList.wrap(new int[]{1,3});
        backtracker.currentlyTrueLiterals = IntArrayList.wrap(new int[]{0, 1, 2, 0, 3, 4, 0, 5, 6, 7, 8, 9});
        backtracker.model.addImmediately(-6);

        Clause c1 = makeClause(new int[]{1,or,1,2,3});
        Clause c2 = makeClause(new int[]{2,or,3,4,5});
        ArrayList<Clause> depClauses = new ArrayList<>();
        depClauses.add(c1); depClauses.add(c2);
        backtracker.usedClausesArray = new ArrayList[predicates+1];
        backtracker.usedClausesArray[2] = depClauses;

        backtracker.incorporateGlobalChanges();
        assertEquals("-3",backtracker.toStringLocalModel());
        assertEquals("[0, 1, 2]", backtracker.currentlyTrueLiterals.toString());
        assertEquals(3,backtracker.selectedPredicatePosition);

        backtracker.model.addImmediately(-2);
        backtracker.dependentSelections[2] = IntArrayList.wrap(new int[]{1});
        backtracker.incorporateGlobalChanges();
        assertEquals("-1,-2,-6",backtracker.model.toString());
        System.out.println(backtracker.model.getInferenceStep(1).toString(null));


    }
    }