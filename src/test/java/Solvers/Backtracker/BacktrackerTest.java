package Solvers.Backtracker;

import Datastructures.Clauses.Quantifier;
import Datastructures.Theory.Model;
import Solvers.Normalizer.Normalizer;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import junit.framework.TestCase;

import java.util.Arrays;

public class BacktrackerTest extends TestCase {

    boolean monitoring = true;

    private static final int cOr = Quantifier.OR.ordinal();
    private static final int cAnd = Quantifier.AND.ordinal();
    private static final int cEquiv = Quantifier.EQUIV.ordinal();
    private static final int cAtleast = Quantifier.ATLEAST.ordinal();
    private static final int cAtmost = Quantifier.ATMOST.ordinal();
    private static final int cExactly = Quantifier.EXACTLY.ordinal();
    private static final int cInterval = Quantifier.INTERVAL.ordinal();

    Backtracker makeBacktracker(int predicates, int seed) {
        Backtracker backtracker = new Backtracker(1,null,seed,true);
        backtracker.predicates = predicates;
        backtracker.model = new Model(predicates);
        backtracker.localModel = new byte[predicates+1];
        backtracker.trueLiteralIndex = new int[predicates+1];
        backtracker.derivedTrueLiterals = new IntArrayList();
        backtracker.predicateIndex = new int[predicates+1];
        backtracker.derivedTrueLiteralArray = new IntArrayList[predicates+1];
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            backtracker.predicateIndex[predicate] = predicate;
            backtracker.derivedTrueLiteralArray[predicate] = new IntArrayList();}
        backtracker.literalIndex = new Literals();
        backtracker.literalIndex.reset(predicates);
        backtracker.clauses = new Clauses();
        backtracker.statistics = new BacktrackerStatistics("test");
        return backtracker;
    }

    public void testInitializePredicateIndex() {
        System.out.println("initializePredicateIndex");
        Backtracker backtracker = makeBacktracker(5,0);
        backtracker.initializePredicateIndex();
        assertEquals("[0, 1, 2, 3, 4, 5]", Arrays.toString(backtracker.predicateIndex));
        backtracker = makeBacktracker(5,2);
        backtracker.initializePredicateIndex();
        assertEquals("[0, 3, 2, 5, 1, 4]", Arrays.toString(backtracker.predicateIndex));
    }

    public void testInitializeLocalModel() {
        System.out.println("initializeModel");
        Backtracker backtracker = makeBacktracker(5,0);
        backtracker.model.addImmediately(2);
        backtracker.model.addImmediately(-4);
        backtracker.initializeLocalModel();
        assertEquals("[0, 0, 1, 0, -1, 0]",Arrays.toString(backtracker.localModel));
    }

    public void testDeriveTrueLiteralsOr() {
        System.out.println("deriveTrueLiteralsOr");
        Backtracker backtracker = makeBacktracker(5,0);
        int[] inputClause = new int[]{1,cOr,1,2,3};
        IntArrayList listClause = Normalizer.makeClause(inputClause);
        Clause clause = new Clause(listClause);
        assertEquals(-1,backtracker.deriveTrueLiteralsOr(clause));
        backtracker.localModel[2] = -1;
        assertEquals(-1,backtracker.deriveTrueLiteralsOr(clause));
        backtracker.localModel[1] = -1;
        backtracker.trueLiteralIndex[1] = 5;
        backtracker.trueLiteralIndex[2] = 6;
        assertEquals(-1,backtracker.deriveTrueLiteralsOr(clause));
        assertEquals("[3]",backtracker.derivedTrueLiterals.toString());
        assertEquals(6,backtracker.trueLiteralIndex[3]);

        backtracker = makeBacktracker(5,0);
        inputClause = new int[]{2,cOr,-1,-2,-3};
        listClause = Normalizer.makeClause(inputClause);
        clause = new Clause(listClause);
        backtracker.localModel[1]= 1;
        backtracker.localModel[2]= 1;
        backtracker.localModel[3]= 1;
        backtracker.trueLiteralIndex[1] = 7;
        backtracker.trueLiteralIndex[2] = 6;
        backtracker.trueLiteralIndex[3] = 5;
        assertEquals(7,backtracker.deriveTrueLiteralsOr(clause));
    }
    public void testDeriveTrueLiteralsAtleast() {
        System.out.println("deriveTrueLiteralsAtleast");
        Backtracker backtracker = makeBacktracker(5, 0);
        int[] inputClause = new int[]{1, cAtleast, 2, 1, 2, 3};
        IntArrayList listClause = Normalizer.makeClause(inputClause);
        Clause clause = new Clause(listClause);
        assertEquals(-1, backtracker.deriveTrueLiteralsAtleast(clause));
        assertEquals("[0, 0, 0, 0, 0, 0]",Arrays.toString(backtracker.localModel));
        backtracker.localModel[2] = -1;
        backtracker.trueLiteralIndex[2] = 6;
        assertEquals(-1, backtracker.deriveTrueLiteralsAtleast(clause));
        assertEquals("[0, 1, -1, 1, 0, 0]",Arrays.toString(backtracker.localModel));
        assertEquals("[0, 6, 6, 6, 0, 0]",Arrays.toString(backtracker.trueLiteralIndex));

        backtracker = makeBacktracker(5, 0);
        inputClause = new int[]{2, cAtleast, 2, 1,2,3};
        listClause = Normalizer.makeClause(inputClause);
        clause = new Clause(listClause);
        backtracker.localModel[1] = -1;
        backtracker.localModel[2] = -1;
        backtracker.trueLiteralIndex[1] = 6;
        backtracker.trueLiteralIndex[2] = 5;
        assertEquals(6, backtracker.deriveTrueLiteralsAtleast(clause));

        backtracker = makeBacktracker(5, 0);
        inputClause = new int[]{3, cAtleast, 2, 1,2,3,4};
        listClause = Normalizer.makeClause(inputClause);
        clause = new Clause(listClause);
        backtracker.localModel[1] = 1;
        backtracker.trueLiteralIndex[1] = 6;
        assertEquals(-1, backtracker.deriveTrueLiteralsAtleast(clause));
        backtracker.localModel[2] = 1;
        backtracker.trueLiteralIndex[2] = 6;
        assertEquals(-1, backtracker.deriveTrueLiteralsAtleast(clause));

        backtracker = makeBacktracker(5, 0);
        inputClause = new int[]{3, cAtleast, 2, 1,2,3,4};
        listClause = Normalizer.makeClause(inputClause);
        clause = new Clause(listClause);
        backtracker.localModel[1] = -1;
        backtracker.trueLiteralIndex[1] = 6;
        assertEquals(-1, backtracker.deriveTrueLiteralsAtleast(clause));
        backtracker.localModel[2] = -1;
        backtracker.trueLiteralIndex[2] = 4;
        assertEquals(-1, backtracker.deriveTrueLiteralsAtleast(clause));
        assertEquals("[0, -1, -1, 1, 1, 0]",Arrays.toString(backtracker.localModel));
        backtracker.localModel[3] = -1;
        backtracker.trueLiteralIndex[3] = 3;
        assertEquals(6, backtracker.deriveTrueLiteralsAtleast(clause));

        backtracker = makeBacktracker(5, 0);
        inputClause = new int[]{3, cAtleast, 2, 1,1,2,2,3,3};
        listClause = Normalizer.makeClause(inputClause);
        clause = new Clause(listClause);
        backtracker.localModel[1] = -1;
        backtracker.trueLiteralIndex[1] = 6;
        assertEquals(-1, backtracker.deriveTrueLiteralsAtleast(clause));
        backtracker.localModel[2] = -1;
        backtracker.trueLiteralIndex[2] = 5;
        assertEquals(-1, backtracker.deriveTrueLiteralsAtleast(clause));
        assertEquals("[3]",backtracker.derivedTrueLiterals.toString());

    }

    public void testDeriveTrueLiteralsAtmost() {
        System.out.println("deriveTrueLiteralsAtmost");
        Backtracker backtracker = makeBacktracker(5, 0);
        int[] inputClause = new int[]{1, cAtmost, 2, 1, 2, 3};
        IntArrayList listClause = Normalizer.makeClause(inputClause);
        Clause clause = new Clause(listClause);
        assertEquals(-1, backtracker.deriveTrueLiteralsAtmost(clause));
        assertEquals("[0, 0, 0, 0, 0, 0]", Arrays.toString(backtracker.localModel));
        backtracker.localModel[2] = -1;
        backtracker.trueLiteralIndex[2] = 6;
        assertEquals(-1, backtracker.deriveTrueLiteralsAtmost(clause));
        assertEquals("[0, 0, -1, 0, 0, 0]", Arrays.toString(backtracker.localModel));
        assertEquals("[0, 0, 6, 0, 0, 0]", Arrays.toString(backtracker.trueLiteralIndex));

        backtracker.localModel[1] = -1;
        backtracker.trueLiteralIndex[1] = 5;
        assertEquals(-1, backtracker.deriveTrueLiteralsAtmost(clause)); // contradiction

        backtracker = makeBacktracker(5, 0);
        backtracker.localModel[2] = 1;
        backtracker.trueLiteralIndex[2] = 6;
        assertEquals(-1, backtracker.deriveTrueLiteralsAtmost(clause));
        assertEquals("[0, 0, 1, 0, 0, 0]", Arrays.toString(backtracker.localModel));backtracker.localModel[2] = 1;
        backtracker.localModel[1] = 1;
        backtracker.trueLiteralIndex[1] = 5;
        assertEquals(-1, backtracker.deriveTrueLiteralsAtmost(clause));
        assertEquals("[0, 1, 1, -1, 0, 0]",Arrays.toString(backtracker.localModel));
        assertEquals("[0, 5, 6, 6, 0, 0]", Arrays.toString(backtracker.trueLiteralIndex));
    }

    public void testDeriveTrueLiteralsInterval() {
        System.out.println("deriveTrueLiteralsInterval");
        Backtracker backtracker = makeBacktracker(5, 0);
        int[] inputClause = new int[]{1, cInterval, 2, 4, 1, 2, 3, 4, 5};
        IntArrayList listClause = Normalizer.makeClause(inputClause);
        Clause clause = new Clause(listClause);
        assertEquals(-1, backtracker.deriveTrueLiteralsAtmost(clause));
        assertEquals("[0, 0, 0, 0, 0, 0]", Arrays.toString(backtracker.localModel));

        backtracker.localModel[1] = 1;
        backtracker.trueLiteralIndex[1] = 6;
        assertEquals(-1, backtracker.deriveTrueLiteralsAtmost(clause));
        assertEquals("[0, 1, 0, 0, 0, 0]", Arrays.toString(backtracker.localModel));

        backtracker.localModel[2] = 1;
        backtracker.trueLiteralIndex[2] = 5;
        assertEquals(-1, backtracker.deriveTrueLiteralsAtmost(clause));
        assertEquals("[0, 1, 1, 0, 0, 0]", Arrays.toString(backtracker.localModel));

        backtracker.localModel[3] = 1;
        backtracker.trueLiteralIndex[3] = 4;
        backtracker.localModel[4] = 1;
        backtracker.trueLiteralIndex[4] = 3;
        assertEquals(-1, backtracker.deriveTrueLiteralsAtmost(clause));
        assertEquals("[0, 1, 1, 1, 1, -1]", Arrays.toString(backtracker.localModel));
        assertEquals(-1, backtracker.deriveTrueLiteralsAtmost(clause));
        assertEquals("[0, 6, 5, 4, 3, 6]", Arrays.toString(backtracker.trueLiteralIndex));

        backtracker = makeBacktracker(5, 0);
        for(int i = 1; i <= 5; ++i) {
            backtracker.localModel[i] = 1;
            backtracker.trueLiteralIndex[i] = i;}
        assertEquals(5, backtracker.deriveTrueLiteralsInterval(clause));

        backtracker = makeBacktracker(5, 0);
        for(int i = 1; i <= 3; ++i) {
            backtracker.localModel[i] = -1;
            backtracker.trueLiteralIndex[i] = i;}
        assertEquals(-1, backtracker.deriveTrueLiteralsInterval(clause));
        assertEquals("[0, -1, -1, -1, 1, 1]", Arrays.toString(backtracker.localModel));
        assertEquals("[0, 1, 2, 3, 3, 3]", Arrays.toString(backtracker.trueLiteralIndex));
    }

    public void testMergeResolution() {
        System.out.println("deriveTrueLiteralsInterval");
        Backtracker backtracker = makeBacktracker(5, 0);
        int[] inputClause = new int[]{1, cOr, 1,2,3};
        IntArrayList listClause = Normalizer.makeClause(inputClause);
        Clause clause = new Clause(listClause);
        backtracker.insertClause(clause);
        backtracker.localModel[3] = -1;
        backtracker.trueLiteralIndex[3] = 5;
        assertTrue(backtracker.mergeResolution(-1,2,1));
        assertEquals("[0, 0, 1, -1, 0, 0]",Arrays.toString(backtracker.localModel));
        assertEquals("[0, 0, 5, 5, 0, 0]", Arrays.toString(backtracker.trueLiteralIndex));
    }

    public void testSearchModel() {
        System.out.println("searchModel");
        Backtracker backtracker = makeBacktracker(5, 0);
        backtracker.insertClause(new Clause(Normalizer.makeClause(new int[]{1, cOr, 1, 2, 3})));
        backtracker.insertClause(new Clause(Normalizer.makeClause(new int[]{2, cOr, 1, 2, -3})));
        backtracker.insertClause(new Clause(Normalizer.makeClause(new int[]{3, cOr, 1, -2, 3})));
        backtracker.insertClause(new Clause(Normalizer.makeClause(new int[]{4, cOr, 1, -2, -3})));
        backtracker.insertClause(new Clause(Normalizer.makeClause(new int[]{5, cOr, -1, 2, 3})));
        backtracker.insertClause(new Clause(Normalizer.makeClause(new int[]{6, cOr, -1, 2, -3})));
        backtracker.insertClause(new Clause(Normalizer.makeClause(new int[]{7, cOr, -1, -2, 3})));
        assertTrue(backtracker.searchModel());
        assertEquals("[0, 1, 1, 1, 1, 1]",Arrays.toString(backtracker.localModel));
        System.out.println(backtracker.statistics);

        backtracker = makeBacktracker(5, 0);
        backtracker.insertClause(new Clause(Normalizer.makeClause(new int[]{1, cOr, 1, 2, 3})));
        backtracker.insertClause(new Clause(Normalizer.makeClause(new int[]{2, cOr, 1, 2, -3})));
        backtracker.insertClause(new Clause(Normalizer.makeClause(new int[]{3, cOr, 1, -2, 3})));
        backtracker.insertClause(new Clause(Normalizer.makeClause(new int[]{4, cOr, 1, -2, -3})));
        backtracker.insertClause(new Clause(Normalizer.makeClause(new int[]{5, cOr, -1, 2, 3})));
        backtracker.insertClause(new Clause(Normalizer.makeClause(new int[]{6, cOr, -1, 2, -3})));
        backtracker.insertClause(new Clause(Normalizer.makeClause(new int[]{7, cOr, -1, -2, 3})));
        backtracker.insertClause(new Clause(Normalizer.makeClause(new int[]{8, cOr, -1, -2, -3})));
        assertFalse(backtracker.searchModel());
        assertEquals("[0, 1, 1, 1, 1, 1]",Arrays.toString(backtracker.localModel));
        System.out.println(backtracker.statistics);


    }
    }