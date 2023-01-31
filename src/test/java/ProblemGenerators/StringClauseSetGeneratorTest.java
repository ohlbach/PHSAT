package ProblemGenerators;

import Datastructures.Clauses.InputClauses;
import org.junit.Test;

import java.util.ArrayList;
import java.util.HashMap;

/**
 * Created by ohlbach on 27.08.2018.
 */
public class StringClauseSetGeneratorTest {
    String errorPrefix = "Error: ";

     @Test
     public void help() {
         System.out.println(StringClauseSetGenerator.help());
     }

     @Test
    public void testMakeProblemGenerator() throws Exception {
        System.out.println("makeProblemGenerator");
        String clauses = "Info1\n" +
                "Info2\n" +
                "% test\n" +
                "c comment\n" +
                "\n" +
                "p cnf 20\n" +
                "p,q,r\n" +
                "c Info3 \n"+
                "% testttt\n"+
                "& a,b,-c\n" +
                "e r s t\n" +
                "<= 2 -p -q\n" +
                ">= 3 -a -b -c -d\n" +
                "= 2 x y\n" +
                "[2,3] a -b -c -d";
        HashMap<String,String> parameters = new HashMap<>();
        parameters.put("clauses",clauses);
        parameters.put("name","MyProblem");
        ArrayList<ProblemGenerator> generators = new ArrayList<>();
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        StringClauseSetGenerator.makeProblemGenerator(parameters,null,generators,errors,warnings);
        System.out.println(errors);
        System.out.println(warnings);
        ProblemGenerator generator = generators.get(0);
        System.out.println(generator.toString());

        InputClauses inputClauses = generator.generateProblem(null);
        System.out.println(inputClauses.toString());
 }

}