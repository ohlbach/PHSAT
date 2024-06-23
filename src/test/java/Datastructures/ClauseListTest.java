package Datastructures;

import Datastructures.Clauses.Quantifier;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Theory.Model;
import InferenceSteps.InfTrueLiteralInClause;
import InferenceSteps.InferenceStep;
import junit.framework.TestCase;

import java.util.function.Consumer;
import java.util.function.Function;

public class ClauseListTest extends TestCase {

    static Consumer<String> monitor = (string -> System.out.println(string));
    static Symboltable symboltable = new Symboltable(10);
    static {symboltable.setName(1,"p");
        symboltable.setName(2,"q");
        symboltable.setName(3,"r");
        symboltable.setName(4,"s");
        symboltable.setName(5,"t");
        symboltable.setName(6,"u");}

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
        assertEquals("", cl.toString("all", null));

    }


    public void testApplyTrueLiteral() throws Unsatisfiable {
        System.out.println("apply true literal");
        ClauseList cl = new ClauseList(true,true,monitor);
        Model model = new Model(10);
        cl.initialize("Test",model,symboltable);
        cl.addClause(makeClause(new int[]{1,nor,1,2,3,4}));
        cl.addClause(makeClause(new int[]{2,natl,2,1,2,3,4,1}));
        cl.addClause(makeClause(new int[]{3,natm,2,1,2,3,4,1}));
        cl.addClause(makeClause(new int[]{4,nint,2,3,1,2,3,4,5,6}));
        Clause cAnd = makeClause(new int[]{10,nand,1,2});
        InferenceStep step = new InfTrueLiteralInClause(cAnd.simpleClone(),1);


        assertEquals ("Clauses:\n" +
                "  1: 1v2v3v4\n" +
                "  2: >=2 1^2,2,3,4\n" +
                "  3: <=2 1^2,2,3,4\n" +
                "  4: [2,3] 1,2,3,4,5,6\n" +
                "\n" +
                "\n" +
                "Index:\n" +
                "1:       1: 1v2v3v4\n" +
                "         2: >=2 1^2,2,3,4\n" +
                "         3: <=2 1^2,2,3,4\n" +
                "         4: [2,3] 1,2,3,4,5,6\n" +
                "2:       1: 1v2v3v4\n" +
                "         2: >=2 1^2,2,3,4\n" +
                "         3: <=2 1^2,2,3,4\n" +
                "         4: [2,3] 1,2,3,4,5,6\n" +
                "3:       1: 1v2v3v4\n" +
                "         2: >=2 1^2,2,3,4\n" +
                "         3: <=2 1^2,2,3,4\n" +
                "         4: [2,3] 1,2,3,4,5,6\n" +
                "4:       1: 1v2v3v4\n" +
                "         2: >=2 1^2,2,3,4\n" +
                "         3: <=2 1^2,2,3,4\n" +
                "         4: [2,3] 1,2,3,4,5,6\n" +
                "5:       4: [2,3] 1,2,3,4,5,6\n" +
                "6:       4: [2,3] 1,2,3,4,5,6\n",cl.toString("all",null));
        cl.applyTrueLiteral(1,step);
        assertEquals("Clauses:\n" +
                "4.1: [1,2] 2,3,4,5,6\n" +
                "\n" +
                "\n" +
                "Queue:\n" +
                "TRUELITERAL: Literal -2\n" +
                "  True Literal in Clause: 3.1: =0 2,3,4 -> -2\n" +
                "TRUELITERAL: Literal -3\n" +
                "  True Literal in Clause: 3.1: =0 2,3,4 -> -3\n" +
                "TRUELITERAL: Literal -4\n" +
                "  True Literal in Clause: 3.1: =0 2,3,4 -> -4\n" +
                "SHORTENED_CLAUSE Clause: 4.1: [1,2] 2,3,4,5,6\n" +
                "\n" +
                "\n" +
                "Index:\n" +
                "2:     4.1: [1,2] 2,3,4,5,6\n" +
                "3:     4.1: [1,2] 2,3,4,5,6\n" +
                "4:     4.1: [1,2] 2,3,4,5,6\n" +
                "5:     4.1: [1,2] 2,3,4,5,6\n" +
                "6:     4.1: [1,2] 2,3,4,5,6\n", cl.toString("all",null));

        Clause c4 = cl.clauses.firstLinkedItem;
        assertTrue(c4.inferenceSteps.get(0).verify(monitor,null));
        assertTrue(c4.inferenceSteps.get(1).verify(monitor,null));
        System.out.println(c4.inferenceSteps.get(0).toString(null));
        System.out.println(c4.inferenceSteps.get(1).toString(null));

        System.out.println("\nExample 2");
        model = new Model(10);
        cAnd = makeClause(new int[]{11,nand,2,-1});
        step = new InfTrueLiteralInClause(cAnd.simpleClone(),-1);

        cl.initialize("Test",model,symboltable);
        cl.addClause(makeClause(new int[]{1,nor,1,2,3,4}));
        cl.addClause(makeClause(new int[]{2,natl,2,1,2,3,4,1}));
        cl.addClause(makeClause(new int[]{3,natm,2,1,2,3,4,1}));
        c4 = makeClause(new int[]{4,nint,2,3,1,2,3,4,5,6});
        cl.addClause(c4);

        cl.applyTrueLiteral(-1,step);
        assertEquals("Clauses:\n" +
                "1.1: 2v3v4\n" +
                "2.1: >=2 2,3,4\n" +
                "3.1: <=2 2,3,4\n" +
                "4.1: [2,3] 2,3,4,5,6\n" +
                "\n" +
                "\n" +
                "Queue:\n" +
                "TRUELITERAL: Literal -2\n" +
                "  True Literal in Clause: 3.1: =0 2,3,4 -> -2\n" +
                "TRUELITERAL: Literal -3\n" +
                "  True Literal in Clause: 3.1: =0 2,3,4 -> -3\n" +
                "TRUELITERAL: Literal -4\n" +
                "  True Literal in Clause: 3.1: =0 2,3,4 -> -4\n" +
                "SHORTENED_CLAUSE Clause: 4.1: [1,2] 2,3,4,5,6\n" +
                "SHORTENED_CLAUSE Clause: 1.1: 2v3v4\n" +
                "SHORTENED_CLAUSE Clause: 2.1: >=2 2,3,4\n" +
                "SHORTENED_CLAUSE Clause: 3.1: <=2 2,3,4\n" +
                "SHORTENED_CLAUSE Clause: 4.1: [2,3] 2,3,4,5,6\n" +
                "\n" +
                "\n" +
                "Index:\n" +
                "2:     4.1: [1,2] 2,3,4,5,6\n" +
                "       1.1: 2v3v4\n" +
                "       2.1: >=2 2,3,4\n" +
                "       3.1: <=2 2,3,4\n" +
                "       4.1: [2,3] 2,3,4,5,6\n" +
                "3:     4.1: [1,2] 2,3,4,5,6\n" +
                "       1.1: 2v3v4\n" +
                "       2.1: >=2 2,3,4\n" +
                "       3.1: <=2 2,3,4\n" +
                "       4.1: [2,3] 2,3,4,5,6\n" +
                "4:     4.1: [1,2] 2,3,4,5,6\n" +
                "       1.1: 2v3v4\n" +
                "       2.1: >=2 2,3,4\n" +
                "       3.1: <=2 2,3,4\n" +
                "       4.1: [2,3] 2,3,4,5,6\n" +
                "5:     4.1: [1,2] 2,3,4,5,6\n" +
                "       4.1: [2,3] 2,3,4,5,6\n" +
                "6:     4.1: [1,2] 2,3,4,5,6\n" +
                "       4.1: [2,3] 2,3,4,5,6\n", cl.toString("all", null));
        assertTrue(c4.inferenceSteps.get(0).verify(monitor,null));
        assertTrue(c4.inferenceSteps.get(1).verify(monitor,null));
        System.out.println(c4.inferenceSteps.get(0).toString(null));
        System.out.println(c4.inferenceSteps.get(1).toString(null));
    }

    public void testSubsumesByModels() throws Unsatisfiable {
        System.out.println("subsumes by models");
        ClauseList cl = new ClauseList(true, true, monitor);
        Model model = new Model(10);
        cl.initialize("Test", model, symboltable);
        Clause c1 = makeClause(new int[]{1, nor, 1, 2, 3, 4});
        Clause c2 = makeClause(new int[]{2, nor, 1, 3, 2, 5});
        Clause c3 = makeClause(new int[]{3, nor, 3,2,1});

        assertTrue(cl.subsumesByModels(c1,c1));
        assertTrue(cl.subsumesByModels(c3,c1));
        assertFalse(cl.subsumesByModels(c1,c3));
        assertFalse(cl.subsumesByModels(c1,c2));
        assertFalse(cl.subsumesByModels(c2,c1));

        Clause c4 = makeClause(new int[]{4,natl,2,1,2,3});
        Clause c5 = makeClause(new int[] {5,natl,3,1,1,2,2,3,3});
        Clause c6 = makeClause(new int[] {6,natl,2,4,3,2,1});

        assertTrue(cl.subsumesByModels(c4,c5)); // c5 would be simplified to c4
        assertTrue(cl.subsumesByModels(c5, c4));
        assertTrue(cl.subsumesByModels(c4, c6));
        assertFalse(cl.subsumesByModels(c6, c4));
    }

/*


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