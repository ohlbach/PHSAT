package Solvers.Normalizer;

import Datastructures.Clauses.InputClauses;
import Management.GlobalParameters;
import Management.ProblemSupervisor;
import ProblemGenerators.ProblemGenerator;
import ProblemGenerators.StringClauseSetGenerator;
import junit.framework.TestCase;

import java.util.ArrayList;
import java.util.HashMap;

public class NormalizerTest extends TestCase {
    public ProblemSupervisor makeProblemSupervisor(String clauses) {
        HashMap<String, String> parameters = new HashMap<>();
        parameters.put("clauses", clauses);
        parameters.put("name", "MyProblem");
        ArrayList<ProblemGenerator> generators = new ArrayList<>();
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        StringClauseSetGenerator.makeProblemGenerator(parameters, generators, errors, warnings);
        System.out.println(errors);
        System.out.println(warnings);
        ProblemGenerator generator = generators.get(0);
        System.out.println(generator.toString());

        InputClauses inputClauses = generator.generateProblem(null);
        System.out.println(inputClauses.toString());
        ArrayList<HashMap<String,String>> pars = new ArrayList<>();
        pars.add(parameters);
        GlobalParameters globalParameters = new GlobalParameters(pars,errors,warnings);
        ProblemSupervisor supervisor = new ProblemSupervisor(null,globalParameters,generator,null);
        supervisor.inputClauses = inputClauses;
        return supervisor;}

    public void testAddClauseToIndex() {
        System.out.println("addClauseToIndex");
        String clauses = "p cnf 5\n"+
                "1,2,3";
        ProblemSupervisor supervisor = makeProblemSupervisor(clauses);
        Normalizer nom = new Normalizer(supervisor);
        System.out.println("INPUT");
        System.out.println(nom.problemSupervisor.inputClauses.toString());
    }

    public void testRemoveClauseFromIndex() {
    }
}