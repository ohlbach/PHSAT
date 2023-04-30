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
        Backtracker backtracker = new Backtracker(1,null,seed);
        backtracker.predicates = predicates;
        backtracker.model = new Model(predicates);
        backtracker.localModel = new short[predicates+1];
        backtracker.trueLiteralIndex = new int[predicates+1];
        backtracker.derivedTrueLiterals = new IntArrayList();
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
        assertEquals(5,backtracker.minTruthIndex(clause));
        assertEquals(-1,backtracker.deriveTrueLiteralsOr(clause));
        assertEquals("[3]",backtracker.derivedTrueLiterals.toString());
        assertEquals(5,backtracker.trueLiteralIndex[3]);

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
        assertEquals(5,backtracker.deriveTrueLiteralsOr(clause));
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
        assertEquals(5, backtracker.deriveTrueLiteralsAtleast(clause));

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
        assertEquals(3, backtracker.deriveTrueLiteralsAtleast(clause));

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
        assertEquals(5, backtracker.deriveTrueLiteralsAtmost(clause)); // contradiction

        backtracker = makeBacktracker(5, 0);
        backtracker.localModel[2] = 1;
        backtracker.trueLiteralIndex[2] = 6;
        assertEquals(-1, backtracker.deriveTrueLiteralsAtmost(clause));
        assertEquals("[0, 0, 1, 0, 0, 0]", Arrays.toString(backtracker.localModel));backtracker.localModel[2] = 1;
        backtracker.localModel[1] = 1;
        backtracker.trueLiteralIndex[1] = 5;
        assertEquals(-1, backtracker.deriveTrueLiteralsAtmost(clause));
        assertEquals("[0, 1, 1, -1, 0, 0]",Arrays.toString(backtracker.localModel));
        assertEquals("[0, 5, 6, 5, 0, 0]", Arrays.toString(backtracker.trueLiteralIndex));
    }
    }