package Solvers.Walker;

import Datastructures.Clauses.InputClauses;
import Datastructures.Clauses.Quantifier;
import Datastructures.Results.Result;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Theory.Model;
import ProblemGenerators.ProblemGenerator;
import ProblemGenerators.RandomClauseSetGenerator;
import junit.framework.TestCase;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Random;

public class WalkerTest extends TestCase {
    private static int cOr = Quantifier.OR.ordinal();
    private static int cAtleast = Quantifier.ATLEAST.ordinal();
    private static int cAtmost = Quantifier.ATMOST.ordinal();
    private static int cExactly = Quantifier.EXACTLY.ordinal();
    private static int cInterval = Quantifier.INTERVAL.ordinal();

    private static Walker MyWalker(int predicates, int maxFlips) {
        Walker walker = new Walker(1,null,0,maxFlips,10);
        walker.predicates = predicates;
        walker.model = new Model(predicates);
        walker.literals = new Literals(predicates);
        walker.localModel = new boolean[predicates+1];
        walker.random = new Random(0);
        walker.flipScores = new int[predicates+1];
        walker.predicatesWithPositiveScore = new Predicates(predicates);
        walker.statistics = new WalkerStatistics("Walker");
        walker.jumpFrequency = 2;
        walker.myThread = Thread.currentThread();
        return walker;
    }

    private Solvers.Normalizer.Clause makeClause(int[] inputClause) {
       return new Solvers.Normalizer.Clause(inputClause,false,null,null);}

    private Clause makClause(int[] inputClause) {
        return new Clause(makeClause(inputClause));}

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
        Walker walker = MyWalker(10,10);
        int[] clause1 = new int[]{1,cOr,1,2,-3};
        walker.insertClause(makeClause(clause1));
        assertEquals("1: 1v2v-3\n",walker.toString("clauses"));
        assertEquals("Positive Literals:\n" +
                "1:1,2:1,\n" +
                "Negative Literals:\n" +
                "-3:1,",walker.toString("literals"));
        assertEquals("2@1,",walker.literals.toString(2));
        assertEquals("-3@1,",walker.literals.toString(-3));
        assertEquals("",walker.literals.toString(-2));

        int[] clause2 = new int[]{2,cOr,-1,2,3};
        walker.insertClause(makeClause(clause2));
        assertEquals("1: 1v2v-3\n" +
                "2: -1v2v3\n",walker.toString("clauses"));
        assertEquals("Positive Literals:\n" +
                "1:1,2:2,3:1,\n" +
                "Negative Literals:\n" +
                "-1:1,-3:1,",walker.toString("literals"));
        assertEquals("2@2,2@1,",walker.literals.toString(2));

    }

    public void testInitializeModel()  {
        System.out.println("initializeModel");
        Walker walker = MyWalker(10,10);
        walker.insertClause(makeClause(new int[]{1, cOr, 1,2,3}));
        walker.insertClause(makeClause(new int[]{2, cOr, 1,-2,3}));
        walker.insertClause(makeClause(new int[]{3, cOr, -1,-2,3}));
        walker.model.addImmediately(4,-5);
        walker.initializeModel();
        assertTrue(walker.localModel[1]);
        assertFalse(walker.localModel[2]);
        assertTrue(walker.localModel[3]);
        assertTrue(walker.localModel[4]);
        assertFalse(walker.localModel[5]);
    }

    public void testFlipPredicate() {
    }

    public void testInitializeLocalTruthForClause() {
        System.out.println("initializeLocalTruthForClause");
        Walker walker = MyWalker(10,10);
        walker.localModel[1] = true;
        Clause clause1 = makClause(new int[]{1,cOr,1,2,3});
        assertTrue(walker.initializeLocalTruthForClause(clause1));
        assertTrue(clause1.isLocallyTrue);
        assertEquals(1,clause1.trueLiterals);

        walker.localModel[2] = true; walker.localModel[3] = false;
        Clause clause2 = makClause(new int[]{2,cInterval,0,2, 1,2,-3,4});
        assertFalse(walker.initializeLocalTruthForClause(clause2));
        assertFalse(clause2.isLocallyTrue);
        assertEquals(3,clause2.trueLiterals);
    }

    public void testInitializeFlipScores1() {
        System.out.println("initializeFlipScores: true clauses");
        Walker walker = MyWalker(10,10);
        Clause clause1 = makClause(new int[]{1,cInterval,2,4, 1,2,3,4,5,6});
        walker.localModel[1] = true; walker.localModel[2] = true;
        walker.initializeLocalTruthForClause(clause1);
        walker.initializeFlipScores(clause1);
        assertEquals("1:-1,2:-1,",walker.toString("flipscores"));

        walker = MyWalker(10,10);
        clause1 = makClause(new int[]{1,cInterval,2,4, 1,2,3,4,5,6});
        walker.localModel[1] = true; walker.localModel[2] = true; walker.localModel[3] = true;
        walker.initializeLocalTruthForClause(clause1);
        walker.initializeFlipScores(clause1);
        assertEquals("",walker.toString("flipscores"));

        walker = MyWalker(10,10);
        clause1 = makClause(new int[]{1,cInterval,2,4, 1,2,3,4,5,6});
        walker.localModel[1] = true; walker.localModel[2] = true; walker.localModel[3] = true; walker.localModel[4] = true;
        walker.initializeLocalTruthForClause(clause1);
        walker.initializeFlipScores(clause1);
        assertEquals("5:-1,6:-1,",walker.toString("flipscores"));

        walker = MyWalker(10,10);
        clause1 = makClause(new int[]{1,cInterval,2,4, 1,1,2,3,4,5,6});
        walker.localModel[1] = true;
        walker.initializeLocalTruthForClause(clause1);
        walker.initializeFlipScores(clause1);
        assertEquals("1:-1,",walker.toString("flipscores"));

        walker = MyWalker(10,10);
        clause1 = makClause(new int[]{1,cInterval,2,4, 1,1,2,2,3,4,5,6});
        walker.localModel[1] = true; walker.localModel[2] = true;
        walker.initializeLocalTruthForClause(clause1);
        walker.initializeFlipScores(clause1);
        assertEquals("3:-1,4:-1,5:-1,6:-1,",walker.toString("flipscores"));
    }

    public void testInitializeFlipScores2() {
        System.out.println("initializeFlipScores: false clauses");
        Walker walker = MyWalker(10,10);
        Clause clause1 = makClause(new int[]{1, cInterval, 2, 4, 1, 2, 3, 4, 5});
        walker.initializeLocalTruthForClause(clause1);
        walker.initializeFlipScores(clause1);
        assertEquals("1:1,2:1,3:1,4:1,5:1,", walker.toString("flipscores"));

        walker = MyWalker(10,10);
        clause1 = makClause(new int[]{1, cInterval, 3, 5, 1, 2, 3, 4, 5, 6, 7, 8});
        walker.localModel[1] = true;
        walker.initializeLocalTruthForClause(clause1);
        walker.initializeFlipScores(clause1);
        assertEquals("1:-1,2:1,3:1,4:1,5:1,6:1,7:1,8:1,", walker.toString("flipscores"));

        walker = MyWalker(10,10);
        clause1 = makClause(new int[]{1, cInterval, 3, 5, 1, 2, 3, 4, 5, 6, 7, 8});
        walker.localModel[1] = true; walker.localModel[2] = true; // one true literal not enough
        walker.initializeLocalTruthForClause(clause1);
        walker.initializeFlipScores(clause1);
        assertEquals("1:-1,2:-1,3:1,4:1,5:1,6:1,7:1,8:1,", walker.toString("flipscores"));

        walker = MyWalker(10,10);
        clause1 = makClause(new int[]{1, cInterval, 3, 5, 1, 2, 3, 4, 5, 6, 7, 8});
        walker.localModel[1] = true; walker.localModel[2] = true;
        walker.localModel[3] = true; walker.localModel[4] = true;
        walker.localModel[5] = true; walker.localModel[6] = true; // one true literal too much
        walker.initializeLocalTruthForClause(clause1);
        walker.initializeFlipScores(clause1);
        assertEquals("1:1,2:1,3:1,4:1,5:1,6:1,7:-1,8:-1,", walker.toString("flipscores"));

        walker = MyWalker(10,10);
        clause1 = makClause(new int[]{1, cInterval, 3, 5, 1, 2, 3, 4, 5, 6, 6, 7, 8});
        walker.localModel[1] = true; walker.localModel[2] = true;
        walker.localModel[3] = true; walker.localModel[4] = true;
        walker.localModel[5] = true; walker.localModel[6] = true; // two true literal too much
        walker.initializeLocalTruthForClause(clause1);
        walker.initializeFlipScores(clause1);
        assertEquals("1:1,2:1,3:1,4:1,5:1,6:1,7:-1,8:-1,", walker.toString("flipscores"));

        walker = MyWalker(10,10);
        clause1 = makClause(new int[]{1, cInterval, 3, 5, 1, 2, 3, 4, 5, 5, 6, 6, 6, 7, 8});
        walker.localModel[1] = true; walker.localModel[2] = true;
        walker.localModel[3] = true; walker.localModel[4] = true;
        walker.localModel[5] = true; walker.localModel[6] = true; // two true literal too much
        walker.initializeLocalTruthForClause(clause1);
        walker.initializeFlipScores(clause1);
        assertEquals("1:1,2:1,3:1,4:1,5:1,6:1,7:-1,8:-1,", walker.toString("flipscores"));

        walker = MyWalker(10,10);
        clause1 = makClause(new int[]{1, cInterval, 3, 4, 1, 2, 3, 3, 3, 4, 5, 6});
        walker.localModel[1] = true; walker.localModel[2] = true; // don't flip 3
        walker.initializeLocalTruthForClause(clause1);
        walker.initializeFlipScores(clause1);
        assertEquals("1:-1,2:-1,3:-1,4:1,5:1,6:1,", walker.toString("flipscores"));

        walker = MyWalker(10,10);
        clause1 = makClause(new int[]{1, cInterval, 2, 3, 1, 2, 3, 4, 4, 5, 6});
        walker.localModel[1] = true; walker.localModel[2] = true;
        walker.localModel[3] = true; walker.localModel[4] = true;
        walker.initializeLocalTruthForClause(clause1);
        walker.initializeFlipScores(clause1);
        assertEquals("1:1,2:1,3:1,4:1,5:-1,6:-1,", walker.toString("flipscores"));

        walker = MyWalker(10,10);
        clause1 = makClause(new int[]{1, cInterval, 3, 4, 1, 1, 1, 2, 3, 4});
        walker.localModel[1] = true;walker.localModel[2] = true;walker.localModel[3] = true;
        walker.initializeLocalTruthForClause(clause1);
        walker.initializeFlipScores(clause1);
        assertEquals("1:-1,2:1,3:1,4:-1,", walker.toString("flipscores"));
    }
    public void testInitializeFlipScores3() {
        System.out.println("initializeFlipScores: exactlys");
        Walker walker = MyWalker(10,10);
        Clause clause1 = makClause(new int[]{1, cExactly, 2, 1, 2, 3, 4, 5});
        walker.initializeLocalTruthForClause(clause1);
        walker.initializeFlipScores(clause1);
        assertEquals("1:1,2:1,3:1,4:1,5:1,", walker.toString("flipscores"));

        walker = MyWalker(10,10);
        clause1 = makClause(new int[]{1, cExactly, 2, 1, 2, 3, 4, 5});
        walker.localModel[1] = true;
        walker.initializeLocalTruthForClause(clause1);
        walker.initializeFlipScores(clause1);
        assertEquals("1:-1,2:1,3:1,4:1,5:1,", walker.toString("flipscores"));

        walker = MyWalker(10,10);
        clause1 = makClause(new int[]{1, cExactly, 2, 1, 2, 3, 4, 5});
        walker.localModel[1] = true; walker.localModel[2] = true;
        walker.initializeLocalTruthForClause(clause1);
        walker.initializeFlipScores(clause1);
        assertEquals("1:-1,2:-1,3:-1,4:-1,5:-1,", walker.toString("flipscores"));

        walker = MyWalker(10,10);
        clause1 = makClause(new int[]{1, cExactly, 2, 1, 2, 3, 4, 5});
        walker.localModel[1] = true; walker.localModel[2] = true; walker.localModel[3] = true;
        walker.initializeLocalTruthForClause(clause1);
        walker.initializeFlipScores(clause1);
        assertEquals("1:1,2:1,3:1,4:-1,5:-1,", walker.toString("flipscores"));

        walker = MyWalker(10,10);
        clause1 = makClause(new int[]{1, cExactly, 2, 1, 2, 3, 4, 5});
        walker.localModel[1] = true; walker.localModel[2] = true; walker.localModel[3] = true; walker.localModel[4] = true;
        walker.initializeLocalTruthForClause(clause1);
        walker.initializeFlipScores(clause1);
        assertEquals("1:1,2:1,3:1,4:1,5:-1,", walker.toString("flipscores"));
    }

    public void testInitializeFlipScores4() {
        System.out.println("initializeFlipScores 4");
        Walker walker = MyWalker(10,10);
        Clause clause1 = makClause(new int[]{1, cInterval, 1, 2, 1, 2, 3, 4});
        walker.initializeLocalTruthForClause(clause1);
        walker.initializeFlipScores(clause1);
        assertEquals("1:1,2:1,3:1,4:1,", walker.toString("flipscores"));

        walker = MyWalker(10,10);
        clause1 = makClause(new int[]{2, cInterval, 2, 3, 1, 2, 3, 4});
        walker.initializeLocalTruthForClause(clause1); // clause is false, two literals should be flipped.
        walker.initializeFlipScores(clause1);
        assertEquals("1:1,2:1,3:1,4:1,", walker.toString("flipscores"));

        walker = MyWalker(10,10);
        clause1 = makClause(new int[]{3, cInterval, 2, 3, 1, 2, 3, 4});
        walker.localModel[2] = true; // clause is false
        walker.initializeLocalTruthForClause(clause1);
        walker.initializeFlipScores(clause1);
        assertEquals("1:1,2:-1,3:1,4:1,", walker.toString("flipscores"));

        walker = MyWalker(10,10);
        clause1 = makClause(new int[]{3, cInterval, 2, 3, 1, 2, 3, 4});
        walker.localModel[2] = true; walker.localModel[3] = true; // clause is true
        walker.initializeLocalTruthForClause(clause1);
        walker.initializeFlipScores(clause1);
        assertEquals("2:-1,3:-1,", walker.toString("flipscores"));

        walker = MyWalker(10,10);
        clause1 = makClause(new int[]{3, cInterval, 2, 3, 1, 2, 3, 4,5,6});
        walker.localModel[2] = true; walker.localModel[3] = true; walker.localModel[4] = true; walker.localModel[5] = true;
        walker.initializeLocalTruthForClause(clause1); // clause is false. One true literal too much
        walker.initializeFlipScores(clause1);
        assertEquals("1:-1,2:1,3:1,4:1,5:1,6:-1,", walker.toString("flipscores"));

        walker = MyWalker(10,10);
        clause1 = makClause(new int[]{3, cInterval, 2, 3, 1, 2, 3, 4,5,6});
        walker.localModel[1] = true; walker.localModel[2] = true; walker.localModel[3] = true; walker.localModel[4] = true; walker.localModel[5] = true;
        walker.initializeLocalTruthForClause(clause1); // clause is false. Two true literal too much
        walker.initializeFlipScores(clause1);
        assertEquals("1:1,2:1,3:1,4:1,5:1,6:-1,", walker.toString("flipscores"));
    }

    public void testInitializePredicatesWithPositiveScores() {
        System.out.println("initializePredicatesWithPositiveScores");

        Walker walker = MyWalker(10,10);
        Clause clause1 = makClause(new int[]{1, cInterval, 1, 2, 1, 2, 3, 4});
        walker.initializeLocalTruthForClause(clause1);
        walker.initializeFlipScores(clause1);
        assertEquals("1:1,2:1,3:1,4:1,", walker.toString("flipscores"));
        walker.initializePredicatesWithPositiveScores();
        assertEquals("1,2,3,4,",walker.toString("predicates"));
    }

    public void testSelectPredicateInFalseClause() {
        System.out.println("selectPredicateInFalseClause");
        Walker walker = MyWalker(10,10);
        Clause clause = makClause(new int[]{1, cInterval, 2, 3, 1, 2, 3, 4});
        walker.initializeLocalTruthForClause(clause);
        assertEquals(3, walker.selectPredicateInFalseClause(clause));

        walker = MyWalker(10,10);
        clause = makClause(new int[]{1, cInterval, 2, 3, 1, 2, 3, 4});
        walker.localModel[1] = true;
        walker.initializeLocalTruthForClause(clause);
        assertEquals(2, walker.selectPredicateInFalseClause(clause));

        walker = MyWalker(10,10);
        clause = makClause(new int[]{1, cInterval, 2, 3, 1, 2, 3, 4});
        walker.localModel[1] = true; walker.localModel[2] = true; walker.localModel[3] = true; walker.localModel[4] = true;
        walker.initializeLocalTruthForClause(clause);
        assertEquals(3, walker.selectPredicateInFalseClause(clause));

    }

    public void testSelectFlipPredicate() {
        System.out.println("selectFlipPredicate");
        Walker walker = MyWalker(10,10);
        Clause clause = makClause(new int[]{1, cOr, 1, 2, 3});
        walker.initializeLocalTruthForClause(clause);
        walker.initializeFlipScores(clause);
        walker.initializePredicatesWithPositiveScores();
        assertEquals("1:1,2:1,3:1,", walker.toString("flipscores"));
        assertEquals("1,2,3,", walker.toString("predicates"));
        assertEquals(1, walker.selectFlipPredicate());

        walker = MyWalker(10,10);
        Clause clause2 = makClause(new int[]{2, cOr, 1, 2});
        Clause clause3 = makClause(new int[]{3, cOr, 3, 4});
        Clause clause4 = makClause(new int[]{4, cOr, -1,-3});
        Clause clause5 = makClause(new int[]{5, cOr, -4,-3});
        walker.localModel[1] = true;
        walker.localModel[3] = true;
        walker.localModel[4] = true;

        walker.initializeLocalTruthForClause(clause2);
        walker.initializeLocalTruthForClause(clause3);
        walker.initializeLocalTruthForClause(clause4);
        walker.initializeLocalTruthForClause(clause5);


        walker.initializeFlipScores(clause2);
        walker.initializeFlipScores(clause3);
        walker.initializeFlipScores(clause4);
        walker.initializeFlipScores(clause5);
        walker.initializePredicatesWithPositiveScores();

        assertEquals("3:2,4:1,", walker.toString("flipscores"));
        assertEquals("3,4,", walker.toString("predicates"));
        assertEquals(3, walker.selectFlipPredicate());
        assertEquals(4, walker.selectFlipPredicate());
        assertEquals(3, walker.selectFlipPredicate());
    }

    public void testUpdateFlipScores1() {
        System.out.println("updateFlipScores ");
        Walker walker = MyWalker(10,10);
        int[] clause1 = new int[]{1, cOr, 1, 2, 3};
        walker.insertClause(makeClause(clause1));
        Clause clause = walker.clauses.get(0);
        walker.localModel[1] = true;
        walker.initializeLocalTruthForClause(clause);
        walker.initializeFlipScores(clause);
        assertEquals("1:-1,", walker.toString("flipscores"));
        walker.initializePredicatesWithPositiveScores();
        assertEquals("",walker.toString("predicates"));
        assertEquals("false clauses\n",walker.toString("falseClauses"));
        assertEquals(0,walker.falseClauseList.size());
        walker.localModel[2] = true;
        walker.updateFlipScores(2);
        assertEquals("", walker.toString("flipscores"));
        walker.localModel[2] = false;
        walker.updateFlipScores(2);
        walker.flipPredicate(1);
        assertEquals("1:1,2:1,3:1,", walker.toString("flipscores"));
        assertEquals("1,2,3,",walker.toString("predicates"));
        assertEquals("false clauses\n" +
                "    1: 1v2v3\n",walker.toString("falseClauses"));
        assertEquals(1,walker.falseClauseList.size());
    }
    public void testAddGloballyTrueLiteral() throws Unsatisfiable {
        System.out.println("addGloballyTrueLiteral ");
        Walker walker = MyWalker(10,10);
        int[] clause1 = new int[]{1, cOr, 1, 2, 3};
        walker.insertClause(makeClause(clause1));
        Clause clause = walker.clauses.get(0);
        walker.initializeLocalTruthForClause(clause);
        walker.initializeFlipScores(clause);
        walker.initializePredicatesWithPositiveScores();
        assertEquals("1:1,2:1,3:1,", walker.toString("flipscores"));
        assertEquals("1,2,3,",walker.toString("predicates"));
        assertEquals("false clauses\n" +
                "    1: 1v2v3\n",walker.toString("falseClauses"));
        assertEquals(1,walker.falseClauseList.size());

        walker.model.add(null,1,null);
        walker.addGloballyTrueLiteral(1, null);
        assertTrue(walker.myThread.isInterrupted());
        assertTrue(walker.trueLiteralInterrupt);
        walker.integrateGloballyTrueLiterals();
        assertFalse(walker.trueLiteralInterrupt);
        assertEquals("1:-1073741826,", walker.toString("flipscores"));
        assertEquals("",walker.toString("predicates"));
        assertEquals("false clauses\n",walker.toString("falseClauses"));
        assertEquals(0,walker.falseClauseList.size());
    }

    public InputClauses makeRandom(int predicates, int seed, int ors, String length) {
        HashMap<String, String> parameters = new HashMap<>();
        parameters.put("predicates",""+predicates); // illegal key, missing predicates
        parameters.put("seed",""+seed);
        parameters.put("length",length);
        //parameters.put("ands", "1");
        parameters.put("equivs", "0");
        parameters.put("ors", ""+ors);
        parameters.put("intervals", "0");
        parameters.put("atleasts", "0");
        parameters.put("atmosts", "0");
        parameters.put("redundant","false");
        ArrayList<ProblemGenerator> generators = new ArrayList<>();
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        RandomClauseSetGenerator.makeProblemGenerator(parameters, generators, errors, warnings);
        System.out.println(errors);
        System.out.println(warnings);
        ProblemGenerator generator = generators.get(0);
        //System.out.println(generator.toString());

        InputClauses inputClauses = generator.generateProblem(errors);
        if(!errors.isEmpty()) System.out.println(errors.toString());
        //System.out.println(inputClauses.toString());
        return inputClauses;}


public void testWalk() throws Unsatisfiable {
    System.out.println("walk ");
    int predicates = 1000;
    InputClauses inputClauses = makeRandom(predicates,3,350,"3");
    Walker walker = MyWalker(predicates,1000);
    for(int[] inputClause : inputClauses.disjunctions) {
        walker.insertClause(makeClause(inputClause));}
    try{
        walker.initializeModel();
        for(Clause clause : walker.clauses) {
            walker.initializeLocalTruthForClause(clause);
            walker.initializeFlipScores(clause);}
        if(walker.falseClauseList.size == 0) {throw walker.localToGlobalModel();}
        walker.initializePredicatesWithPositiveScores();
        walker.walk();}
    catch(Result result) {
        System.out.println("RESULT " + result.toString(null));
        for(int[] clause : inputClauses.falseClausesInModel(walker.model)) {
            System.out.println(Arrays.toString(clause));}
        System.out.println(walker.statistics);
    }}

}

