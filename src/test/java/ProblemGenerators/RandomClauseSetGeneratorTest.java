package ProblemGenerators;

import org.junit.Test;

/**
 * Created by ohlbach on 06.09.2018.
 */
public class RandomClauseSetGeneratorTest {

    @Test
    public void help() {
        //System.out.println(RandomClauseSetGenerator.help());
    }
/*
    @Test
    public void keys()  {
        System.out.println("keys");
        assertEquals("[seeds, generator, redundant, precises, atleasts, ors, ands, predicates, intervals, lengths, equivs, exactlies, cpRatios, atmosts]",
                RandomClauseSetGenerator.keys.toString());}


    @Test
    public void makeProblemGeneratorErrors1() {
        System.out.println("makeProblemGenerator Errors 1");
        HashMap<String,String> parameters = new HashMap<>();
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        parameters.put("predicates","10"); // illegal key, missing predicates
        parameters.put("lengths","3");
        parameters.put("ors", "5");
        parameters.put("intervals", "4");
        parameters.put("atleasts", "5");
        parameters.put("precises","true,false");
        //parameters.put("cpRatios", "4");
        //parameters.put("seeds", "seed");
        ArrayList<ProblemGenerator> generators = new ArrayList();
        RandomClauseSetGenerator.makeProblemGenerator(parameters,generators,errors,warnings);
        System.out.println("Errors\n"+errors.toString());
        System.out.println("Warnings\n"+warnings.toString());
        for(ProblemGenerator generator : generators) {
            System.out.println(generator.toString());
            System.out.println("\n");
            InputClauses inputClauses = generator.generateProblem(null);
            System.out.println(inputClauses.toString());
            System.out.println("\n");
        }
    }
*/
}