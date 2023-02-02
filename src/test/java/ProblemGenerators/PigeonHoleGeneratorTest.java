package ProblemGenerators;

import Datastructures.Clauses.InputClauses;
import Datastructures.Symboltable;
import org.junit.Test;

import java.util.ArrayList;
import java.util.HashMap;

import static org.junit.Assert.assertEquals;

/**
 * Created by ohlbach on 09.09.2018.
 */
public class PigeonHoleGeneratorTest {

    @Test
    public void help(){
        System.out.println(PigeonHoleGenerator.help());
    }

    @Test
    public void makeProblemGenerator()  {
        System.out.println("makeProblemGenerator with errors");
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        HashMap<String,String> params = new HashMap<>();
        ArrayList<ProblemGenerator> generators = new ArrayList<>();
        params.put("holes","4");
        params.put("pigeons", "5");
        params.put("capacities", "[1,2], >= 2");
        PigeonHoleGenerator.makeProblemGenerator(params,generators, errors,warnings);
        System.out.println("Errors:\n" + errors);
        System.out.println("Warnings:\n" + warnings);
        assertEquals(2, generators.size());
        for(ProblemGenerator generator: generators) {
            System.out.println(generator.toString());
            System.out.println("\n\n");
            InputClauses inputClauses = generator.generateProblem(null);
            Symboltable symboltable = null;
            System.out.println(inputClauses.toString());
            System.out.println("\n\n");}
        }

}