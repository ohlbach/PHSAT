package ProblemGenerators;

/**
 * Created by ohlbach on 06.09.2018.
 */
public class RandomClauseSetGeneratorTest {
    /*
    @Test
    public void help() {
        System.out.println(RandomClauseSetGenerator.help());
    }

    @Test
    public void keys()  {
        System.out.println("keys");
        assertEquals("[precises, atleasts, ors, ands, predicates, intervals, lengths, equivs, exactlys, seeds, cpRatios, atmosts]",
                RandomClauseSetGenerator.keys.toString());}

    @Test
    public void parseProblemParametersErrors1() {
        System.out.println("parseProblemParameters Errors 1");
        HashMap<String,String> parameters = new HashMap<>();
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        parameters.put("predicate","10"); // illegal key, missing predicates
        parameters.put("lengths","3-4a");
        parameters.put("seeds", "seed");
        assertNull(RandomClauseSetGenerator.parseParameters(parameters,errors,warnings));
        System.out.println("Errors\n"+errors.toString());
        System.out.println("Warnings\n"+warnings.toString());
    }

    @Test
    public void parseProblemParametersErrors2() {
        System.out.println("parseProblemParameters Errors no cpRatio");
        HashMap<String,String> parameters = new HashMap<>();
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        parameters.put("predicates","5"); // illegal key, missing predicates
        parameters.put("lengths","3");
        parameters.put("seeds", "-1");
        parameters.put("ors", "-1");
        parameters.put("ands", "-1");
        parameters.put("atleasts", "-1");
        parameters.put("exactlys", "6");
        parameters.put("atmosts", "7");
        assertNull(RandomClauseSetGenerator.parseParameters(parameters,errors,warnings));
        System.out.println("Errors\n"+errors.toString());
        System.out.println("Warnings\n"+warnings.toString());
    }

    @Test
    public void parseProblemParametersErrors3() {
        System.out.println("parseProblemParameters Errors with cpRatio");
        HashMap<String,String> parameters = new HashMap<>();
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        parameters.put("predicates","-5"); // illegal key, missing predicates
        parameters.put("lengths","0");
        parameters.put("cpRatios", "-1");
        parameters.put("seeds", "-1");
        assertNull(RandomClauseSetGenerator.parseParameters(parameters,errors,warnings));
        System.out.println("Errors\n"+errors.toString());
        System.out.println("Warnings\n"+warnings.toString());
    }

    @Test
    public void parseProblemParameters1() {
        System.out.println("parseProblemParameters 1 with cpRatio");
        HashMap<String, String> parameters = new HashMap<>();
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        parameters.put("predicates","5"); // illegal key, missing predicates
        parameters.put("lengths","2 to 4");
        parameters.put("cpRatios", "2,3");
        ArrayList<HashMap<String,Object>> result = RandomClauseSetGenerator.parseParameters(parameters,errors,warnings);
        //System.out.println(errors.toString());
        assertNotNull(result);
        assertEquals("[{ors=10, predicates=5, seed=0, length=2, name=RD052, precise=true}, "+
                "{ors=10, predicates=5, seed=0, length=3, name=RD053, precise=true}, "+
                "{ors=10, predicates=5, seed=0, length=4, name=RD054, precise=true}, "+
                "{ors=15, predicates=5, seed=0, length=2, name=RD052, precise=true}, "+
                "{ors=15, predicates=5, seed=0, length=3, name=RD053, precise=true}, "+
                "{ors=15, predicates=5, seed=0, length=4, name=RD054, precise=true}]",result.toString());
    }

    @Test
    public void parseProblemParameters2() {
        System.out.println("parseProblemParameters 1 no cpRatio");
        HashMap<String, String> parameters = new HashMap<>();
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        parameters.put("predicates","5"); // illegal key, missing predicates
        parameters.put("lengths","2");
        parameters.put("seeds","4");
        parameters.put("precises","false,true");
        parameters.put("ors","10");
        parameters.put("ands","11");
        parameters.put("equivs","12");
        parameters.put("atleasts","13");
        parameters.put("atmosts","14");
        parameters.put("exactlys","15");
        parameters.put("intervals","20");
        ArrayList<HashMap<String,Object>> result = RandomClauseSetGenerator.parseParameters(parameters,errors,warnings);
        System.out.println(errors.toString());
        assertNotNull(result);
        assertEquals("[{atleasts=13, ors=10, ands=11, predicates=5, intervals=20, seed=4, equivs=12, exactlys=15, length=2, name=RD452, precise=false, atmosts=14}, " +
                "{atleasts=13, ors=10, ands=11, predicates=5, intervals=20, seed=4, equivs=12, exactlys=15, length=2, name=RD452, precise=true, atmosts=14}]",result.toString());
    }


    private ProblemSupervisor prepare(HashMap<String,Object> problemParameters) {
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        Controller controller = new Controller(null,null,null);
        GlobalParameters globalParameters=new GlobalParameters();
        globalParameters.trackReasoning = true;
        problemParameters.put("name","test");
        return new ProblemSupervisor(controller,globalParameters,problemParameters,null);}

    @Test
    public void generate1() {
        System.out.println("generate 1");
        HashMap<String,String> parameters = new HashMap<>();
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        parameters.put("predicates","10");
        parameters.put("ors","5");
        parameters.put("lengths","3");
        parameters.put("precises", "true");
        HashMap<String,Object> problemParameters = RandomClauseSetGenerator.parseParameters(parameters,errors,warnings).get(0);
        ProblemSupervisor problemSupervisor = prepare(problemParameters);
        BasicClauseList bcl = RandomClauseSetGenerator.generate(problemParameters,problemSupervisor,errors,warnings);
        assertEquals("Randomly generated clauses with parameters:\n" +
                "{ors=5, predicates=10, seed=0, length=3, name=test, precise=true}\n" +
                "Disjunctions:\n" +
                "  1: 9,-8,4\n" +
                "  2: 2,5,-8\n" +
                "  3: -3,5,6\n" +
                "  4: 1,-9,-8\n" +
                "  5: -1,-3,4\n",bcl.toString());
    }

    @Test
    public void generate2()  {
        System.out.println("generate 2");
        HashMap<String,String> parameters = new HashMap<>();
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        parameters.put("predicates","10");
        parameters.put("ors","7");
        parameters.put("ands","6");
        parameters.put("equivs","5");
        parameters.put("atleasts","4");
        parameters.put("atmosts","3");
        parameters.put("exactlys","2");
        parameters.put("intervals","4");
        parameters.put("lengths","3");
        parameters.put("precises", "false");
        HashMap<String,Object> problemParameters = RandomClauseSetGenerator.parseParameters(parameters,errors,warnings).get(0);
        ProblemSupervisor problemSupervisor = prepare(problemParameters);
        BasicClauseList bcl = RandomClauseSetGenerator.generate(problemParameters,problemSupervisor,errors,warnings);
        assertEquals("Randomly generated clauses with parameters:\n" +
                "{atleasts=4, ors=7, ands=6, predicates=10, intervals=4, seed=0, equivs=5, exactlys=2, length=3, name=test, precise=false, atmosts=3}\n" +
                "Disjunctions:\n" +
                "   1: 10\n" +
                "   2: 4,2,5\n" +
                "   3: -4,6,5\n" +
                "   4: 1\n" +
                "   5: -5,-3\n" +
                "   6: -3,4\n" +
                "   7: 8,-3,-6\n" +
                "Conjunctions:\n" +
                " A-8: 6&-1&2\n" +
                " A-9: 3&-10\n" +
                "A-10: 7\n" +
                "A-11: 9\n" +
                "A-12: -8&9&-3\n" +
                "A-13: 7&8&6\n" +
                "Equivalences:\n" +
                "E-14: 4=10\n" +
                "E-15: -10=-2\n" +
                "E-16: 1=-5\n" +
                "E-17: -8=6=7\n" +
                "E-18: 4=8\n" +
                "Quantifieds:\n" +
                "L-19: 1 -4\n" +
                "L-20: 1 2,8\n" +
                "L-21: 1 -2\n" +
                "L-22: 1 6\n" +
                "M-23: 1 3,-1\n" +
                "M-24: 1 6\n" +
                "M-25: 2 3,10\n" +
                "X-26: 1 -4,-9\n" +
                "X-27: 1 -2\n" +
                "Intervals:\n" +
                "I-28: 1-2: -9,7\n" +
                "I-29: 1-3: -6,10,-6\n" +
                "I-30: 2-3: 9,9,8\n" +
                "I-31: 1-2: 2,2\n",bcl.toString());
    }
*/
}