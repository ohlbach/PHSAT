package Solvers.Normalizer;

import Datastructures.Clauses.InputClauses;
import Datastructures.Clauses.Quantifier;
import Datastructures.Results.Result;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Datastructures.Theory.Model;
import Management.GlobalParameters;
import Management.Monitor.Monitor;
import Management.Monitor.MonitorLife;
import Management.Parameters;
import Management.ProblemSupervisor;
import ProblemGenerators.ProblemGenerator;
import ProblemGenerators.PythagoraenTriples;
import ProblemGenerators.StringClauseSetGenerator;
import Solvers.Normalizer.NMInferenceSteps.NMISTrueLiteralToEquivalence;
import junit.framework.TestCase;

import java.util.ArrayList;
import java.util.HashMap;

public class NormalizerTest extends TestCase {

    int nand = Quantifier.AND.ordinal();
    int nor = Quantifier.OR.ordinal();
    int natl = Quantifier.ATLEAST.ordinal();
    int natm = Quantifier.ATMOST.ordinal();
    int nex = Quantifier.EXACTLY.ordinal();
    int nint = Quantifier.INTERVAL.ordinal();
    int neq = Quantifier.EQUIV.ordinal();

    static Symboltable symboltable = new Symboltable(10);
    static Monitor monitor = new MonitorLife();
    static {
        symboltable.setName(1,"p");
        symboltable.setName(2,"q");
        symboltable.setName(3,"r");
        symboltable.setName(4,"s");

    }
    static NormalizerStatistics statistics = new NormalizerStatistics(null);

    static Clause makeClause(int[] inputClause) {
        return new Clause(inputClause,false,null);
    }


    public ProblemSupervisor makeProblemSupervisor(String clauses) {
        HashMap<String, String> parameters = new HashMap<>();
        parameters.put("clauses", clauses);
        parameters.put("name", "MyProblem");
        ArrayList<ProblemGenerator> generators = new ArrayList<>();
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        StringClauseSetGenerator.makeProblemGenerator(parameters, generators, errors, warnings);
        //System.out.println(errors);
        //System.out.println(warnings);
        ProblemGenerator generator = generators.get(0);
        //System.out.println(generator.description());

        InputClauses inputClauses = generator.generateProblem(errors);
        if(!errors.isEmpty()) System.out.println(errors.toString());
        //System.out.println(inputClauses.description());
        ArrayList<HashMap<String,String>> pars = new ArrayList<>();
        pars.add(parameters);
        GlobalParameters globalParameters = null; //new GlobalParameters(pars,errors,warnings);
        ProblemSupervisor supervisor = new ProblemSupervisor(null,globalParameters,generator,null);
        supervisor.inputClauses = inputClauses;
        supervisor.model = new Model(inputClauses.predicates);
        supervisor.monitor = new MonitorLife("Test",System.nanoTime());
        return supervisor;}

    public void testAddClauseToIndex() {
        System.out.println("addClauseToIndex");
        String clauses = "p cnf 5\n";
        ProblemSupervisor supervisor = makeProblemSupervisor(clauses);
        Normalizer nom = new Normalizer(supervisor);

        Clause clause1 = makeClause(new int[]{5,nor,1,-2,3});
        Clause clause2 = makeClause(new int[]{6,natl, 2,3,2,1,1});
        Clause clause3 = makeClause(new int[]{7,natm, 2,4,3,3,2});
        Clause clause4 = makeClause(new int[]{8,nex, 2,-3,-3,2});
        Clause clause5 = makeClause(new int[]{9,nint,2,4,-3,-3,2,4,4,5});
        nom.addClauseToIndex(clause1);
        nom.addClauseToIndex(clause2);
        nom.addClauseToIndex(clause3);
        nom.addClauseToIndex(clause4);
        nom.addClauseToIndex(clause5);
        System.out.println(nom.indexToString(null));
        assertEquals(clause2,nom.positiveOccAtleast[3].get(1));
        assertEquals(clause5,nom.negativeOccInterval[3].get(1));
    }

    public void testRemoveClauseFromIndex() {
        System.out.println("removeClauseFromIndex");
        String clauses = "p cnf 5\n";
        ProblemSupervisor supervisor = makeProblemSupervisor(clauses);
        Normalizer nom = new Normalizer(supervisor);

        Clause clause1 = makeClause(new int[]{1,nor,1,-2,3});
        nom.addClauseToIndex(clause1);
        System.out.println(nom.indexToString(null));
        nom.removeClauseFromIndex(clause1);
        System.out.println(nom.indexToString(null));
        assertNull(nom.positiveOccAtleast[3]);

        nom.addClauseToIndex(clause1);
        Clause clause2 = makeClause(new int[]{2,natl,2, 3,1,-2,3});
        nom.addClauseToIndex(clause2);
        System.out.println(nom.indexToString(null));
        nom.removeClauseFromIndex(clause1);
        System.out.println(nom.indexToString(null));
        assertEquals(clause2,nom.positiveOccAtleast[3].get(0));

    }

    public void testTransformAndSimplif1() throws Unsatisfiable {
        System.out.println("transform and simplify: no simplifications ");
        String clauses = "p cnf 15\n";
        ProblemSupervisor supervisor = makeProblemSupervisor(clauses);
        Normalizer nom = new Normalizer(supervisor);
        int[] clause1 = new int[]{1, nor, 1, -2, 3};
        nom.transformAndSimplify(clause1);
        assertEquals("  1: 1,-2,3", nom.toString(null));

        int[] clause2 = new int[]{2, natl, 2, 3, 1, -2, 3};
        nom.transformAndSimplify(clause2);
        assertEquals("  1: 1,-2,3\n" +
                "  2: >=2 3^2,1,-2", nom.toString(null));

        int[] clause3 = new int[]{3, nex, 2, 4,5,5,6};
        nom.transformAndSimplify(clause3);
        assertEquals(" 1: 1,-2,3\n" +
                "  2: >=2 3^2,1,-2\n" +
                "  3: =2 4,5^2,6", nom.toString(null));

        int[] clause4 = new int[]{4, nint, 2,3, 4,5,5,6};
        nom.transformAndSimplify(clause3);
        assertEquals("  1: 1,-2,3\n" +
                "  2: >=2 3^2,1,-2\n" +
                "  3: =2 4,5^2,6", nom.toString(null));
    }

    public void testTransformAndSimplif2() throws Unsatisfiable {
        System.out.println("transform and simplify: with simplifications ");
        String clauses = "p cnf 15\n";
        ProblemSupervisor supervisor = makeProblemSupervisor(clauses);
        Normalizer nom = new Normalizer(supervisor);
        int[] clause1 = new int[]{1, nor, 1, -2, 3, 2};
        nom.transformAndSimplify(clause1);
        assertEquals("", nom.toString(null));

        int[] clause2 = new int[]{2, nor, 1, -2, 3, -2};
        nom.transformAndSimplify(clause2);
        assertEquals("2.1: 1,-2,3", nom.toString(null));


        int[] clause3 = new int[]{3, natl, 2, -2, 3, -2, -2, 3, 3, 4};
        nom.transformAndSimplify(clause3);
        assertEquals("2.1: 1,-2,3\n" +
                "3.2: -2,3", nom.toString(null));

        int[] clause4 = new int[]{4, natm, 2, -2, 3, -2, -2, 3, 3, 4};
        nom.transformAndSimplify(clause4);
        assertEquals("2.1: 1,-2,3\n" +
                "3.2: -2,3", nom.toString(null));
        assertEquals("2,-3",supervisor.model.toString());
        assertEquals("true(2)\n" +
                "true(-3)\n",nom.queueToString(null));
    }

    public void testApplyTrueLiteralToEquivalence() throws Unsatisfiable {
        System.out.println("apply true literal to equivalence");
        StringBuilder errors = new StringBuilder();
        String clauses = "p cnf 15\n"+
                "e 1=2 3, 4";
        ProblemSupervisor supervisor = makeProblemSupervisor(clauses);

        Normalizer nom = new Normalizer(supervisor);
        nom.applyTrueLiteralToEquivalences(1);
        assertEquals("1,2,3,4", nom.model.toString());

        clauses = "p cnf 15\n"+
                "e 1=2 3, 4\n"+
                "e 4=-5\n"+
                "e -5=6 7\"";
        supervisor = makeProblemSupervisor(clauses);
        nom = new Normalizer(supervisor);
        //System.out.println(nom.problemSupervisor.inputClauses.description());
        nom.trackReasoning = true;
        Monitor mon = new MonitorLife("MON",System.nanoTime());
        nom.monitor = (string) ->  mon.println("TEST",string);
        nom.monitoring = true;
        nom.applyTrueLiteralToEquivalences(-2);
        assertEquals("-1,-2,-3,-4,5,-6,-7", nom.model.toString());
        System.out.println(nom.model.getInferenceStep(4).toString(null));
        System.out.println(nom.model.getInferenceStep(-7).toString(null));
        assertTrue(((NMISTrueLiteralToEquivalence)(nom.model.getInferenceStep(-7))).verify(null,errors));
    }
    public void testApplyTrueLiteralToEquivalenceUnsat() throws Unsatisfiable {
        System.out.println("apply true literal to equivalence Unsatisfiable");
        StringBuilder errors = new StringBuilder();
        String clauses = "p cnf 15\n"+
                "e 1=2 3, 4";
        ProblemSupervisor supervisor = makeProblemSupervisor(clauses);

        Normalizer nom = new Normalizer(supervisor);
        nom.applyTrueLiteralToEquivalences(1);
        assertEquals("1,2,3,4", nom.model.toString());

        clauses = "p cnf 15\n"+
                "e 1=2 3, 4\n"+
                "e 4=-5\n"+
                "e -5=6 7\"";
        supervisor = makeProblemSupervisor(clauses);
        nom = new Normalizer(supervisor);
        //System.out.println(nom.problemSupervisor.inputClauses.description());
        nom.trackReasoning = true;
        Monitor mon = new MonitorLife("MON",System.nanoTime());
        nom.monitor = (string) ->  mon.println("TEST",string);
        nom.monitoring = true;
        nom.model.addImmediately(-7);
        try{
            nom.applyTrueLiteralToEquivalences(1);}
        catch(Unsatisfiable unsat) {
            System.out.println(unsat.description(null));
        }}

    public void testApplyTrueLiteral() throws Unsatisfiable{
        System.out.println("apply true literal");
        StringBuilder errors = new StringBuilder();
        String clauses = "p cnf 15\n" +
                "1,2 3, 4\n"+
                "<= 2 1,2,3,4,1\n"+
                ">= 2 1,2,3,4\n"+
                "[2,3] 1,2,3,4,5,6";
        ProblemSupervisor supervisor = makeProblemSupervisor(clauses);

        Normalizer nom = new Normalizer(supervisor);
        nom.normalizeClauses(0);
        assertEquals ("  1: 1,2,3,4\n" +
                "  3: >=2 1,2,3,4\n" +
                "  2: <=2 1^2,2,3,4\n" +
                "4.2: <=3 1,2,3,4",nom.toString(null));
        nom.applyTrueLiteral(1);
        assertEquals("3.1: 2,3,4\n" +
                "4.3: <=2 2,3,4", nom.toString(null));

        clauses = "p cnf 15\n" +
                "1,2 3, 4\n"+
                "<= 2 1,2,3,4,1\n"+
                ">= 2 1,2,3,4\n"+
                "[2,3] 1,2,3,4,5,6";
        supervisor = makeProblemSupervisor(clauses);

        nom = new Normalizer(supervisor);
        nom.normalizeClauses(0);
        nom.applyTrueLiteral(-1);
        assertEquals("1.1: 2,3,4\n" +
                "3.1: >=2 2,3,4\n" +
                "2.1: <=2 2,3,4", nom.toString(null));
    }

    public void testApplyEquivalence() throws Unsatisfiable {
        System.out.println("applyEquivalence");
        String clauses = "p cnf 15\n"+
                "1,2 3, 4\n"+
                "1,-2 3, 4\n"+
                "1,5 3, 4\n"+
                "<= 2 1,2,3,4,1\n"+
                "<= 2 1,-2,3,4,1\n"+
                ">= 2 1,2,3,4\n"+
                "[2,3] 1,2,3,4,5,6\n"+
                "[2,3] 1,-2,3,4,5,6";
        ProblemSupervisor supervisor = makeProblemSupervisor(clauses);
        Normalizer nom = new Normalizer(supervisor);
        nom.normalizeClauses(0);
        assertEquals("  1: 1,2,3,4\n" +
                "  2: 1,-2,3,4\n" +
                "  3: 1,5,3,4\n" +
                "  6: >=2 1,2,3,4\n" +
                "  4: <=2 1^2,2,3,4\n" +
                "  5: <=2 1^2,-2,3,4\n" +
                "  7: [2,3] 1,2,3,4,5,6\n" +
                "  8: [2,3] 1,-2,3,4,5,6",nom.toString(null));
        nom.applyEquivalence(2,3,null);
        assertEquals("1.2: 1,2,4\n" +
                "3.1: 1,5,2,4\n" +
                "6.1: >=2 1,2^2,4\n" +
                "4.1: <=2 1^2,2^2,4\n" +
                "7.1: [2,3] 1,2^2,4,5,6\n" +
                "8.2: [1,2] 1,4,5,6",nom.toString(null));
        assertEquals("-1",nom.model.toString(null));

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
        ProblemSupervisor supervisor = makePythogoraenTriples(100);
        Normalizer nom = new Normalizer(supervisor);
        nom.normalizeClauses(0);
        System.out.println(nom.clauses.toString(null));
        System.out.println("");
        System.out.println(nom.singletonsToString(null));
        System.out.println(nom.statistics.toString());
    }

    public void testExtendModel() throws Unsatisfiable {
        System.out.println("extendModel");
        String clauses = "p cnf 15\n"+
                "-1,-4,-5,-6\n" +
                "[2,3] 1,2,3,4,5,6\n";
        ProblemSupervisor supervisor = makeProblemSupervisor(clauses);
        Normalizer nom = new Normalizer(supervisor);
        nom.normalizeClauses(0);
        System.out.println(nom.clauses.toString(null));
        System.out.println("");
        System.out.println(nom.singletonsToString(null));
        nom.model.addImmediately(6);
        nom.extendModel();
        assertEquals("2,3,6",nom.model.toString(null));
        nom.model = new Model(15);
        nom.model.addImmediately(4,5,6);
        nom.extendModel();
        assertEquals("-2,-3,4,5,6",nom.model.toString(null));

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
        /*RandomClauseSetGenerator.makeProblemGenerator(parameters, generators, errors, warnings);*/
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
        Result res = nom.normalizeClauses(0);
        if(res != null) System.out.println(res.toString(null,false));
        System.out.println(nom.clauses.toString(null));
        System.out.println("");
        System.out.println(nom.singletonsToString(null));
        System.out.println("Model: " + nom.model.toString(null));
    }

    }