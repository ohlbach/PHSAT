package Solvers.Normalizer;

import Datastructures.ClauseList;
import Datastructures.Clauses.InputClauses;
import Datastructures.Clauses.Quantifier;
import Datastructures.Results.Result;
import Datastructures.Results.Satisfiable;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Theory.Model;
import ProblemGenerators.PythagoraenTriples;
import ProblemGenerators.RandomClauseSetGenerator;
import junit.framework.TestCase;

import java.util.function.Consumer;

public class NormalizerTest extends TestCase {

    int nand = Quantifier.AND.ordinal();
    int nor = Quantifier.OR.ordinal();
    int natl = Quantifier.ATLEAST.ordinal();
    int natm = Quantifier.ATMOST.ordinal();
    int nex = Quantifier.EXACTLY.ordinal();
    int nint = Quantifier.INTERVAL.ordinal();
    int neq = Quantifier.EQUIV.ordinal();

    static Consumer<String> monitor = (string -> System.out.println(string));


    static void makeClauses (InputClauses inputClauses, int[]... clauses) {
        for(int[] clause : clauses) inputClauses.addClause(clause);
    }

    public void testTransformConjunction() throws Unsatisfiable {
        Normalizer nom = new Normalizer(true, true, monitor);
        int predicates = 5;
        InputClauses inputClauses = new InputClauses("Test", predicates, null, "Test");
        Model model = new Model(predicates);
        nom.initialize(inputClauses, model);
        int[] c1 = {1, nand, 1, 2, -3};
        nom.transformConjunction(c1);
        assertEquals("1,2,-3", model.toString(null));
        int[] c2 = {2, nand, 1, -2, 3};
        try{nom.transformConjunction(c2);}
        catch(Unsatisfiable uns) {
            System.out.println(uns.toString(null));}}

    public void testTransformEquivalence() throws Unsatisfiable {
        Normalizer nom = new Normalizer(true, true, monitor);
        int predicates = 5;
        InputClauses inputClauses = new InputClauses("Test", predicates, null, "Test");
        Model model = new Model(predicates);
        nom.initialize(inputClauses, model);
        int[] c1 = {1, neq, 1, 2, -3};
        nom.transformEquivalence(c1);
        assertEquals("1: 1 = 2 = -3",nom.equivalences.get(0).toString(null));

        int[] c2 = {2, neq, 4,4};
        nom.transformEquivalence(c2);
        assertEquals(1,nom.equivalences.size());

        int[] c3 = {3, neq, 2,3,-2};
        try{
            nom.transformEquivalence(c3);
            assertFalse(true);}
        catch(Unsatisfiable unsat) {
            System.out.println(unsat.toString(null));
        }}

    public void testTransformAndSimplify() throws Unsatisfiable {
        Normalizer nom = new Normalizer(true, true, monitor);
        int predicates = 10;
        InputClauses inputClauses = new InputClauses("Test", predicates, null, "Test");
        Model model = new Model(predicates);
        nom.initialize(inputClauses, model);
        int[] c1 = {1, nor, 1, 2, -3};
        nom.transformAndSimplify(c1);
        assertEquals("  1: 1v2v-3\n",nom.clauseList.toString("clauses", null));

        int[] c2 = {2, natl, 3, 1,1,2,2,3,3};
        nom.transformAndSimplify(c2);
        assertEquals("  1: 1v2v-3\n" +
                "2.2: >=2 1,2,3\n",nom.clauseList.toString("clauses", null));

        int[] c3 = {3, natm, 0, 3,4,5};
        nom.transformAndSimplify(c3);
        assertEquals("  1: 1v2v-3\n" +
                "2.2: >=2 1,2,3\n",nom.clauseList.toString("clauses", null));
        assertEquals("-3,-4,-5",model.toString(null));

        int[] c4 = {4, natl, 2,5,5,6,6,7,7,8};
        nom.transformAndSimplify(c4);
        assertEquals("  1: 1v2v-3\n" +
                "2.2: >=2 1,2,3\n" +
                "4.2: 5v6v7\n",nom.clauseList.toString("clauses", null));
    }

    public void testPythagoraenTriples() {
        System.out.println("Pythagoraen Triples");
        Normalizer nom = new Normalizer(true, true, monitor);
        int max = 60;
        StringBuilder errors = new StringBuilder();
        PythagoraenTriples ptr = new PythagoraenTriples(3, max);
        InputClauses inputClauses = ptr.generateProblem(errors);
        Model model = new Model(max);
        nom.initialize(inputClauses, model);

        try{nom.normalizeClauses();}
        catch(Result result) {assertTrue(result instanceof Satisfiable);}
        //System.out.println(result.toString(null));
        assertEquals(0,inputClauses.falseClausesInModel(model).size());

        assertEquals("3,4,-5,-6,-7,8,-9,-10,12,-13,-14,-15,16,17,-18,-20,21,24,25,-26,-27,\n" +
                "28,-29,30,-32,33,-34,-35,36,-37,-39,40,-41,-42,44,-45,48,-50,51,-52,-53,\n" +
                "-55,-58,-60", model.toString(null));
    }

    public void testRandomMixed()  {
        System.out.println("Random mixed");
        Normalizer nom = new Normalizer(true, true, monitor);
        StringBuilder errors = new StringBuilder();
        int seed = 2;
        int predicates = 50;
        int[] length = {3};
        boolean redundant = false;
        int ors = 300;
        int ands = 0;
        int equivs = 0;
        int  atleasts = 0;
        int atmosts = 0;
        int exactlies = 0;
        int intervals = 0;
        RandomClauseSetGenerator rpg = new RandomClauseSetGenerator(seed, predicates, length, redundant, ors, ands, equivs, atleasts, atmosts, exactlies, intervals);
        InputClauses inputClauses = rpg.generateProblem(errors);
        System.out.println(inputClauses);
        Model model = new Model(predicates);
        nom.initialize(inputClauses, model);

        try{nom.normalizeClauses();}
        catch(Result result) {System.out.println(result);}
        System.out.println(nom.statistics.toString());
        System.out.println(nom.clauseList.statistics.toString());
    }

    public void testRandom3SAT()  {
        System.out.println("Random 3SAT");
        Normalizer nom = new Normalizer(true, true, monitor);
        StringBuilder errors = new StringBuilder();
        int seed = 3;
        int predicates = 50;
        int[] length = {2,10};
        boolean redundant = true;
        int ors = 150;
        int ands = 0;
        int equivs = 0;
        int  atleasts = 10;
        int atmosts = 10;
        int exactlies = 0;
        int intervals = 0;
        RandomClauseSetGenerator rpg = new RandomClauseSetGenerator(seed, predicates, length, redundant, ors, ands, equivs, atleasts, atmosts, exactlies, intervals);
        InputClauses inputClauses = rpg.generateProblem(errors);
        System.out.println(inputClauses);
        Model model = new Model(predicates);
        nom.initialize(inputClauses, model);

        ClauseList clauseList = null;
        try{clauseList = nom.normalizeClauses();}
        catch(Result result) {System.out.println(result);}

        System.out.println(nom.clauseList.toString("clauses",null));
        System.out.println("Model: " + model.toString(null));
        System.out.println(nom.statistics.toString());
        System.out.println(nom.clauseList.statistics.toString());
    }

    }