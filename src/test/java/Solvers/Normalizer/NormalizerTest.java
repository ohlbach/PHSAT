package Solvers.Normalizer;

import Datastructures.Clauses.InputClauses;
import Datastructures.Clauses.Quantifier;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Datastructures.Theory.Model;
import Management.GlobalParameters;
import Management.Monitor.Monitor;
import Management.Monitor.MonitorLife;
import Management.ProblemSupervisor;
import ProblemGenerators.ProblemGenerator;
import ProblemGenerators.StringClauseSetGenerator;
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

    static Symboltable symboltable = new Symboltable(10);
    static Monitor monitor = new MonitorLife();
    static {
        symboltable.setName(1,"p");
        symboltable.setName(2,"q");
        symboltable.setName(3,"r");
        symboltable.setName(4,"s");

    }

    static Clause makeClause(int[] inputClause) {
        return new Clause(inputClause,false,null,null);
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
        //System.out.println(generator.toString());

        InputClauses inputClauses = generator.generateProblem(null);
        //System.out.println(inputClauses.toString());
        ArrayList<HashMap<String,String>> pars = new ArrayList<>();
        pars.add(parameters);
        GlobalParameters globalParameters = new GlobalParameters(pars,errors,warnings);
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

    }