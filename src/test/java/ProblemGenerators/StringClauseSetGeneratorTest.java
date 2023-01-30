package ProblemGenerators;

import org.junit.Test;

/**
 * Created by ohlbach on 27.08.2018.
 */
public class StringClauseSetGeneratorTest {
    String errorPrefix = "Error: ";

     @Test
     public void help() {
         System.out.println(StringClauseSetGenerator.help());
     }



/*
    @Test
    public void generate() throws Exception {
        System.out.println("generate");
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        String clauses = "p 5\n" +
                        "p,q\n" +
                "<= 2 -p,q,r\n" +
                ">= 3 q,-r,s";
        HashMap<String,String> map = new HashMap<>();
        map.put("clauses",clauses);
        ArrayList<HashMap<String,Object>> params = StringClauseSetGenerator.parseParameters(map,errors,warnings);
        BasicClauseList bcl = StringClauseSetGenerator.generate(params.get(0), problemSupervisor, errors,warnings);
        assertEquals("String Generator\n" +
                "Disjunctions:\n" +
                "  1: p,q\n" +
                "Quantifieds:\n" +
                "M-2: 2 -p,q,r\n" +
                "L-3: 3 q,-r,s\n", bcl.toString());
    }
*/
}