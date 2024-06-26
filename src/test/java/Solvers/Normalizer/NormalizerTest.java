package Solvers.Normalizer;

import Datastructures.Clauses.InputClauses;
import Datastructures.Clauses.Quantifier;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Datastructures.Theory.Model;
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

    static Symboltable symboltable = new Symboltable(10);
    static Consumer<String> monitor = (string -> System.out.println(string));

    static StatisticsNormalizer statistics = new StatisticsNormalizer(null);

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
        }

    }

    /*


    public void testTransformAndSimplif1() throws Unsatisfiable {
        System.out.println("transform and simplify: no simplifications ");
        Normalizer nom = new Normalizer("Test","monitor",true,null,7);

        int[] clause1 = new int[]{1, nor, 1, -2, 3};
        nom.transformAndSimplify(clause1);
        assertEquals("  1: 1v-2v3", nom.toString(null));

        int[] clause2 = new int[]{2, natl, 2, 3, 1, -2, 3};
        nom.transformAndSimplify(clause2);
        assertEquals("  1: 1v-2v3\n" +
                "  2: >=2 3^2,1,-2", nom.toString(null));

        int[] clause3 = new int[]{3, nex, 2, 4,5,5,6};
        nom.transformAndSimplify(clause3);
        assertEquals("  1: 1v-2v3\n" +
                "  2: >=2 3^2,1,-2\n" +
                "  3: =2 4,5^2,6", nom.toString(null));

        int[] clause4 = new int[]{4, nint, 2,3, 4,5,5,6};
        nom.transformAndSimplify(clause3);
        assertEquals("  1: 1v-2v3\n" +
                "  2: >=2 3^2,1,-2\n" +
                "  3: =2 4,5^2,6\n" +
                "  3: =2 4,5^2,6", nom.toString(null));
    }

    public void testTransformAndSimplif2() throws Unsatisfiable {
        Normalizer nom = new Normalizer("Test","monitor",true,null,7);
        int[] clause1 = new int[]{1, nor, 1, -2, 3, 2};
        nom.transformAndSimplify(clause1);
        assertEquals("", nom.toString(null));

        int[] clause2 = new int[]{2, nor, 1, -2, 3, -2};
        nom.transformAndSimplify(clause2);
        assertEquals("  2: 1v-2v3", nom.toString(null));


        int[] clause3 = new int[]{3, natl, 2, -2, 3, -2, -2, 3, 3, 4};
        nom.transformAndSimplify(clause3);
        assertEquals("  2: 1v-2v3\n" +
                "3.2: -2v3", nom.toString(null));
        ArrayList<InferenceStep> steps = (ArrayList<InferenceStep>)nom.clauses.getLinkedItem(1).inferenceSteps;
        assertEquals(2,steps.size());
        assertTrue(steps.get(0).verify(monitor,null));
        assertTrue(steps.get(1).verify(monitor,null));


        int[] clause4 = new int[]{4, natm, 2, -2, 3, -2, -2, 3, 3, 4};
        nom.transformAndSimplify(clause4);
        assertEquals("  2: 1v-2v3\n" +
                "3.2: -2v3", nom.toString(null));
        assertEquals("2,-3",nom.model.toString());
        assertEquals("true(2)\n" +
                "true(-3)\n",nom.queueToString(null));
        InferenceStep step = nom.model.getInferenceStep(2);
        assertTrue(step.verify(monitor,null));
        step = nom.model.getInferenceStep(-3);
        assertTrue(step.verify(monitor,null));
    }


    public void testApplyTrueLiteral() throws Unsatisfiable{
        System.out.println("apply true literal");
        StringBuilder errors = new StringBuilder();
        String clauses = "p cnf 15\n" +
                "1,2 3, 4\n"+
                "<= 2 1,2,3,4,1\n"+
                ">= 2 1,2,3,4\n"+
                "[2,3] 1,2,3,4,5,6";
        StringClauseSetGenerator scg = new StringClauseSetGenerator("Test",clauses);
        Normalizer nom = new Normalizer("Test","monitor",true,null,15);
        nom.inputClauses = scg.generateProblem(errors);
        nom.normalizeClauses();
        assertEquals ("  1: 1v2v3v4\n" +
                "  3: >=2 1,2,3,4\n" +
                "  2: <=2 1^2,2,3,4\n" +
                "4.2: [2,3] 1,2,3,4\n" +
                "\n" +
                "Singleton Literals:\n" +
                "5 in clause 4: [2,3] 1,2,3,4,5,6\n" +
                "6 in clause 4.1: [2,3] 1,2,3,4,6\n",nom.toString(null));
        nom.applyTrueLiteral(1);
        assertEquals("3.1: 2v3v4\n" +
                "4.3: [1,2] 2,3,4\n" +
                "\n" +
                "Singleton Literals:\n" +
                "5 in clause 4: [2,3] 1,2,3,4,5,6\n" +
                "6 in clause 4.1: [2,3] 1,2,3,4,6\n", nom.toString(null));
        assertEquals("-2,-3,-4",nom.model.toString(null));

        System.out.println("Example 2");
        clauses = "p cnf 15\n" +
                "1,2 3, 4\n"+
                "<= 2 1,2,3,4,1\n"+
                ">= 2 1,2,3,4\n"+
                "[2,3] 1,2,3,4,5,6";
        scg = new StringClauseSetGenerator("Test",clauses);
        nom = new Normalizer("Test","monitor",true,null,15);
        nom.inputClauses = scg.generateProblem(errors);
        nom.normalizeClauses();
        nom.applyTrueLiteral(-1);
        assertEquals("1.1: 2v3v4\n" +
                "3.1: >=2 2,3,4\n" +
                "2.1: <=2 2,3,4\n" +
                "4.3: >=2 2,3,4\n" +
                "\n" +
                "Singleton Literals:\n" +
                "5 in clause 4: [2,3] 1,2,3,4,5,6\n" +
                "6 in clause 4.1: [2,3] 1,2,3,4,6\n", nom.toString(null));
    }

    public void testApplyEquivalence() throws Unsatisfiable {
        System.out.println("applyEquivalence");
        StringBuilder errors = new StringBuilder();
        String clauses = "p cnf 15\n"+
                "1,2 3, 4\n"+
                "1,-2 3, 4\n"+
                "1,5 3, 4\n"+
                "<= 2 1,2,3,4,1\n"+
                "<= 2 1,-2,3,4,1\n"+
                ">= 2 1,2,3,4\n"+
                "[2,3] 1,2,3,4,5,6\n"+
                "[2,3] 1,-2,3,4,5,6";
        StringClauseSetGenerator scg = new StringClauseSetGenerator("Test",clauses);
        Normalizer nom = new Normalizer("Test","monitor",true,null,15);
        nom.inputClauses = scg.generateProblem(errors);
        nom.normalizeClauses();
        assertEquals("  1: 1v2v3v4\n" +
                "  2: 1v-2v3v4\n" +
                "  3: 1v5v3v4\n" +
                "  6: >=2 1,2,3,4\n" +
                "  4: <=2 1^2,2,3,4\n" +
                "  5: <=2 1^2,-2,3,4\n" +
                "  7: [2,3] 1,2,3,4,5,6\n" +
                "  8: [2,3] 1,-2,3,4,5,6",nom.toString(null));
        nom.applyEquivalence(2,3,null);
        assertEquals("1.1: 1v2v4\n" +
                "3.1: 1v5v2v4\n" +
                "6.1: >=2 1,2^2,4\n" +
                "4.1: <=2 1^2,2^2,4\n" +
                "7.1: [2,3] 1,2^2,4,5,6\n" +
                "8.1: [1,2] 1,4,5,6",nom.toString(null));
        assertEquals("-1",nom.model.toString(null));
    }

    public void testApplyEquivalenceToModel() throws Result {
        System.out.println("applyEquivalenceToModel");
        StringBuilder errors = new StringBuilder();
        String clauses = "p cnf 5\n"+
                "& 1,2 3, 4";
        StringClauseSetGenerator scg = new StringClauseSetGenerator("Test",clauses);
        Normalizer nom = new Normalizer("Test","monitor",true,null,15);
        nom.inputClauses = scg.generateProblem(errors);
        nom.normalizeClauses();
        assertEquals("1,2,3,4",nom.model.toString());
        nom.applyEquivalenceToModel(2,5,null);
        assertEquals("1,2,3,4,5",nom.model.toString());
        nom.applyEquivalenceToModel(-6,3,null);
        assertEquals("1,2,3,4,5,-6",nom.model.toString());
        nom.applyEquivalenceToModel(6,-4,null);
        assertEquals("1,2,3,4,5,-6",nom.model.toString());
        try{nom.applyEquivalenceToModel(6,4,null);}
        catch(Unsatisfiable uns){
            System.out.println(uns.description(null));
        }

    }

    public void testApplyEquivalence2() throws Result {
        System.out.println("applyEquivalence2");
        StringBuilder errors = new StringBuilder();
        String clauses = "p cnf 15\n"+
                "& 1,2 3, 4\n"+
                "e 5,3\n" +
                "-5,6,7\n" +
                "-6,-7";
        StringClauseSetGenerator scg = new StringClauseSetGenerator("Test",clauses);
        Normalizer nom = new Normalizer("Test","monitor",true,null,15);
        nom.inputClauses = scg.generateProblem(errors);
        nom.normalizeClauses();
        assertEquals("1,2,3,4,5",nom.model.toString());
        assertEquals("3.1: 6v7\n" +
                "  4: -6v-7",nom.toString(null));

    }

    public ProblemSupervisor makePythogoraenTriples(int maximum) {
        Parameters parameters = new Parameters("PTripels");
        //parameters.put("maximum", ""+maximum);
        //parameters.put("name", "MyProblem");
        ArrayList<ProblemGenerator> generators = new ArrayList<>();
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        PythagoraenTriples.makeProblemGenerators(parameters, generators);
        //System.out.println(errors);
        //System.out.println(warnings);
        ProblemGenerator generator = generators.get(0);
        //System.out.println(generator.description());

        InputClauses inputClauses = generator.generateProblem(errors);
        if(!errors.isEmpty()) System.out.println(errors.toString());
        System.out.println(inputClauses.toString());
        ArrayList<HashMap<String,String>> pars = new ArrayList<>();
        //pars.add(parameters);
        GlobalParameters globalParameters = null; //new GlobalParameters(pars,errors,warnings);
        ProblemSupervisor supervisor = new ProblemSupervisor(null,globalParameters,generator,null);
        supervisor.inputClauses = inputClauses;
        supervisor.model = new Model(inputClauses.predicates);
        supervisor.monitor = new MonitorLife("Test",System.nanoTime());
        return supervisor;}

    public void testPythagoraenTriples() throws Unsatisfiable {
        System.out.println("pythagoraen triples");
        StringBuilder errors = new StringBuilder();
        int predicates = 200;
        PythagoraenTriples phtr = new PythagoraenTriples(3, predicates);
        Normalizer nom = new Normalizer("Test","monitor",true,null,predicates);
        nom.inputClauses = phtr.generateProblem(errors);
        System.out.println(nom.inputClauses.toString(null,false));
        nom.normalizeClauses();
        System.out.println("\nModel  " + nom.model.toString(null));
        System.out.println(nom.clauses.toString(null));
        System.out.println("");
        System.out.println(nom.singletonsToString(null));
        nom.extendModel();
        System.out.println("\n"+nom.model.toString(null));
        System.out.println(nom.statistics.toString());
        assertTrue(nom.inputClauses.falseClausesInModel(nom.model).isEmpty());
    }

    public void testExtendModel() throws Unsatisfiable {
        System.out.println("extendModel");
        StringBuilder errors = new StringBuilder();
        String clauses = "p cnf 15\n"+
                "-1,-4,-5,-6\n" +
                "[2,3] 1,2,3,4,5,6\n";
        StringClauseSetGenerator scg = new StringClauseSetGenerator("Test",clauses);
        Normalizer nom = new Normalizer("Test","monitor",true,null,15);
        nom.inputClauses = scg.generateProblem(errors);
        nom.normalizeClauses();
        System.out.println(nom.clauses.toString(null));
        System.out.println("");
        System.out.println(nom.singletonsToString(null));
        nom.model.addImmediately(6);
        nom.extendModel();
        assertEquals("-1,2,-3,-4,-5,6",nom.model.toString(null));
        nom.model = new Model(15);
        nom.model.addImmediately(4,5,6);
        nom.extendModel();
        assertEquals("-1,-2,-3,4,5,6",nom.model.toString(null));

        System.out.println(nom.statistics.toString());
    }

    public ProblemSupervisor makeRandom() {
        HashMap<String, String> parameters = new HashMap<>();
        parameters.put("predicates","200"); // illegal key, missing predicates
        parameters.put("seed","1");
        parameters.put("length","2-5");
        //parameters.put("ands", "1");
        parameters.put("equivs", "1");
        parameters.put("ors", "20");
        parameters.put("intervals", "30");
        parameters.put("atleasts", "10");
        parameters.put("atmosts", "20");
        parameters.put("redundant","true");
        ArrayList<ProblemGenerator> generators = new ArrayList<>();
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        //RandomClauseSetGenerator.makeProblemGenerator(parameters, generators, errors, warnings);
        System.out.println(errors);
        System.out.println(warnings);
        ProblemGenerator generator = generators.get(0);
        //System.out.println(generator.description());

        InputClauses inputClauses = generator.generateProblem(errors);
        if(!errors.isEmpty()) System.out.println(errors.toString());
        System.out.println(inputClauses.toString());
        ArrayList<HashMap<String,String>> pars = new ArrayList<>();
        pars.add(parameters);
        GlobalParameters globalParameters = null; //new GlobalParameters(pars,errors,warnings);
        ProblemSupervisor supervisor = new ProblemSupervisor(null,globalParameters,generator,null);
        supervisor.inputClauses = inputClauses;
        supervisor.model = new Model(inputClauses.predicates);
        supervisor.monitor = new MonitorLife("Test",System.nanoTime());
        return supervisor;}

    public void testRandom() throws Unsatisfiable {
        System.out.println("random Clauses");
        ProblemSupervisor supervisor = makeRandom();
        Normalizer nom = new Normalizer(supervisor);
        Result res = nom.normalizeClauses();
        if(res != null) System.out.println(res.toString(null,false));
        System.out.println(nom.clauses.toString(null));
        System.out.println("");
        System.out.println(nom.singletonsToString(null));
        System.out.println("Model: " + nom.model.toString(null));
    }
*/
    }