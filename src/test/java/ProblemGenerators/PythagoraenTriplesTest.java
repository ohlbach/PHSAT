package ProblemGenerators;


import Datastructures.Clauses.InputClauses;
import org.junit.Test;

import java.util.ArrayList;
import java.util.HashMap;

public class PythagoraenTriplesTest {
    @Test
    public void testHelp() {System.out.println(PythagoraenTriples.help());}

    @Test
    public void testMakeProblemGenerator() {
        System.out.println("makeProblemGenerator");
        int maximum = 2000;
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        HashMap<String,String> params = new HashMap<>();
        ArrayList<ProblemGenerator> generators = new ArrayList<>();
        params.put("maximum",""+maximum);
        PythagoraenTriples.makeProblemGenerator(params,generators, errors,warnings);
        System.out.println("Errors:\n" + errors);
        System.out.println("Warnings:\n" + warnings);
        ProblemGenerator generator = generators.get(0);
        System.out.println(generator.toString());
        InputClauses inputClauses = generator.generateProblem(null);
        System.out.println(inputClauses.toString());
    }

}
