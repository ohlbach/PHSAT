package Datastructures;

import Datastructures.Clauses.Quantifier;
import Datastructures.Theory.Model;
import junit.framework.TestCase;

import java.util.function.Consumer;
import java.util.function.Function;

public class ClauseListTest extends TestCase {

    static Consumer<String> monitor = (string -> System.out.println(string));
    static Symboltable symboltable = new Symboltable(10);
    static {symboltable.setName(1,"p");
        symboltable.setName(2,"q");
        symboltable.setName(3,"r");
        symboltable.setName(4,"s");}

    static Function<Integer,Literal> litCreator = (literal) -> new Literal(literal,1);
    static int nor = Quantifier.OR.ordinal();
    static int natl = Quantifier.ATLEAST.ordinal();
    static int natm = Quantifier.ATMOST.ordinal();
    static int nint = Quantifier.INTERVAL.ordinal();
    static int nand = Quantifier.AND.ordinal();
    static int neqv = Quantifier.EQUIV.ordinal();
    static int nex = Quantifier.EXACTLY.ordinal();

    static Clause makeClause(int[] inputClause) {
        return new Clause(inputClause,true,(lit -> new Literal(lit,1)), null);
    }

    public void testAddClause() {
        System.out.println("addClause");
        ClauseList cl = new ClauseList(true,true,monitor);
        Model model = new Model(10);
        cl.initialize("Test",model,symboltable);
        Clause clause1 = makeClause(new int[]{1,nor,1,-2,3});
        Clause clause2 = makeClause(new int[]{2,natl, 2,3,2,1,1});
        Clause clause3 = makeClause(new int[]{3,natm, 2,4,3,3,2});
        Clause clause4 = makeClause(new int[]{4,nex, 2,-3,-3,2});
        Clause clause5 = makeClause(new int[]{5,nint,2,4,-3,-3,2,4,4,5});
        Clause clause6 = makeClause(new int[]{6, nint,2,3,1,-2,1,3,2,2,4});
        cl.addClause(clause1);
        cl.addClause(clause2);
        cl.addClause(clause3);
        cl.addClause(clause4);
        cl.addClause(clause5);
        cl.addClause(clause6);
        System.out.println(clause6.inferenceSteps.get(0).toString(null));
        assertTrue(clause6.inferenceSteps.get(0).verify(monitor,null));
        assertEquals("Clauses:\n" +
                "  1: 1v-2v3\n" +
                "  2: >=2 3,2,1^2\n" +
                "  3: <=2 4,3^2,2\n" +
                "  4: =2 -3^2,2\n" +
                "  5: [2,4] -3^2,2,4^2,5\n" +
                "  6: [1,2] 1^2,3,2,4\n" +
                "\n" +
                "\n" +
                "Singletons:\n" +
                "Singleton Literals:\n" +
                "\n" +
                "\n" +
                "Index:\n" +
                "1:       1: 1v-2v3\n" +
                "         2: >=2 3,2,1^2\n" +
                "         6: [1,2] 1^2,3,2,4\n" +
                "2:       2: >=2 3,2,1^2\n" +
                "         3: <=2 4,3^2,2\n" +
                "         4: =2 -3^2,2\n" +
                "         5: [2,4] -3^2,2,4^2,5\n" +
                "         6: [1,2] 1^2,3,2,4\n" +
                "-2:      1: 1v-2v3\n" +
                "3:       1: 1v-2v3\n" +
                "         2: >=2 3,2,1^2\n" +
                "         3: <=2 4,3^2,2\n" +
                "         6: [1,2] 1^2,3,2,4\n" +
                "-3:      4: =2 -3^2,2\n" +
                "         5: [2,4] -3^2,2,4^2,5\n" +
                "4:       3: <=2 4,3^2,2\n" +
                "         5: [2,4] -3^2,2,4^2,5\n" +
                "         6: [1,2] 1^2,3,2,4\n" +
                "5:       5: [2,4] -3^2,2,4^2,5\n",cl.toString("all",null));
    }

    public void testRemoveClause() {
        System.out.println("removeClause");
        ClauseList cl = new ClauseList(true,true,monitor);
        Model model = new Model(10);
        cl.initialize("Test",model,symboltable);
        Clause clause1 = makeClause(new int[]{1,nor,1,-2,3});
        Clause clause2 = makeClause(new int[]{2,natl, 2,3,2,1,1});
        Clause clause3 = makeClause(new int[]{3,natm, 2,4,3,3,2});
        cl.addClause(clause1);
        cl.addClause(clause2);
        cl.addClause(clause3);
        cl.removeClause(clause2);
        cl.removeClauseFromIndex(clause2);
        assertEquals("Clauses:\n" +
                "  1: 1v-2v3\n" +
                "  3: <=2 4,3^2,2\n" +
                "\n" +
                "\n" +
                "Singletons:\n" +
                "Singleton Literals:\n" +
                "\n" +
                "\n" +
                "Index:\n" +
                "1:       1: 1v-2v3\n" +
                "2:       3: <=2 4,3^2,2\n" +
                "-2:      1: 1v-2v3\n" +
                "3:       1: 1v-2v3\n" +
                "         3: <=2 4,3^2,2\n" +
                "4:       3: <=2 4,3^2,2\n",cl.toString("all", null));

        cl.removeClause(clause1);
        cl.removeClauseFromIndex(clause1);
        cl.removeClause(clause3);
        cl.removeClauseFromIndex(clause3);
        assertTrue(cl.isEmpty());
        assertEquals("Clauses:\n" +
                "\n" +
                "\n" +
                "Singletons:\n" +
                "Singleton Literals:\n" +
                "\n" +
                "\n" +
                "Index:\n", cl.toString("all", null));

    }

    /*
    public void testRemoveClauseFromIndex() {
        System.out.println("removeClauseFromIndex");
        Normalizer nom = new Normalizer("Test","monitor",true,null,5);

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
        //*RandomClauseSetGenerator.makeProblemGenerator(parameters, generators, errors, warnings);
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