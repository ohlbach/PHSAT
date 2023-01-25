package Generators;

/**
 * Created by ohlbach on 09.09.2018.
 */
public class PigeonHoleGeneratorTest {
    /*
    @Test
    public void help(){
        System.out.println(PigeonHoleGenerator.help());
    }


    private ProblemSupervisor prepare(HashMap<String,Object> problemParameters) {
        Controller controller = new Controller(null,null,null);
        GlobalParameters globalParameters=new GlobalParameters();
        globalParameters.trackReasoning = true;
        problemParameters.put("name","test");
        return new ProblemSupervisor(controller,globalParameters,problemParameters,null);}

    @Test
    public void parseProblemParametersErrors()  {
        System.out.println("parseProblemParameters with errors");
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        HashMap<String,String> params = new HashMap<>();
        params.put("holes","2 - 3");
        params.put("pidgeons", "3 - 4");
        params.put("abc","5");
        params.put("capacity", "3a");
        params.put("quantifier","all");
        assertNull(PigeonHoleGenerator.parseParameters(params,errors,warnings));
        System.out.println("Errors:\n" + errors);
        System.out.println("Warnings:\n" + warnings);
        }
    @Test
    public void parseProblemParameters()  {
        System.out.println("parseProblemParameters");
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        HashMap<String,String> params = new HashMap<>();
        params.put("holes","2 to 3");
        params.put("pigeons", "3,4");
        params.put("capacity", "2");
        params.put("quantifier","atleast");
        ArrayList<HashMap<String,Object>> values = PigeonHoleGenerator.parseParameters(params,errors,warnings);
        //System.out.println("Errors:\n" + errors.toString());
        //System.out.println("Warnings:\n" + warnings.toString());
        assertEquals("[{name=PH32, pigeons=3, quantifier=atleast, holes=2, capacity=2}, "+
                "{name=PH42, pigeons=4, quantifier=atleast, holes=2, capacity=2}, "+
                "{name=PH33, pigeons=3, quantifier=atleast, holes=3, capacity=2}, "+
                "{name=PH43, pigeons=4, quantifier=atleast, holes=3, capacity=2}]",
                values.toString());
    }

    @Test
    public void parseProblemParameters1()  {
        System.out.println("parseProblemParameters interval");
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        HashMap<String,String> params = new HashMap<>();
        params.put("holes","2 to 3");
        params.put("pigeons", "3,4");
        params.put("capacity", "[2,3]");
        params.put("quantifier","interval");
        ArrayList<HashMap<String,Object>> values = PigeonHoleGenerator.parseParameters(params,errors,warnings);
        //System.out.println("Errors:\n" + errors.toString());
        //System.out.println("Warnings:\n" + warnings.toString());
        assertEquals("[{name=PH32, pigeons=3, quantifier=interval, holes=2, capacity=[2,3]}, {name=PH42, pigeons=4, quantifier=interval, holes=2, capacity=[2,3]}, {name=PH33, pigeons=3, quantifier=interval, holes=3, capacity=[2,3]}, {name=PH43, pigeons=4, quantifier=interval, holes=3, capacity=[2,3]}]",
                values.toString());
    }

    @Test
    public void generate1() {
        System.out.println("generate 1");
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        HashMap<String,Object> params = new HashMap<>();
        params.put("pigeons",5);
        params.put("holes",4);
        params.put("capacity",2);
        params.put("quantifier","atleast");
        ProblemSupervisor problemSupervisor = prepare(params);
        BasicClauseList bcl = PigeonHoleGenerator.generate(params,problemSupervisor, errors,warnings);
        assertEquals("Pigeon Hole example with 5 pigeons in 4 holes\n" +
                "capacity: atleast 2 pigeons in a hole.\n" +
                "Quantifieds:\n" +
                "X-1: 1 P1H1,P1H2,P1H3,P1H4\n" +
                "X-2: 1 P2H1,P2H2,P2H3,P2H4\n" +
                "X-3: 1 P3H1,P3H2,P3H3,P3H4\n" +
                "X-4: 1 P4H1,P4H2,P4H3,P4H4\n" +
                "X-5: 1 P5H1,P5H2,P5H3,P5H4\n" +
                "L-6: 2 P1H1,P2H1,P3H1,P4H1,P5H1\n" +
                "L-7: 2 P1H2,P2H2,P3H2,P4H2,P5H2\n" +
                "L-8: 2 P1H3,P2H3,P3H3,P4H3,P5H3\n" +
                "L-9: 2 P1H4,P2H4,P3H4,P4H4,P5H4\n",bcl.toString());

    }
    @Test
    public void generate2() {
        System.out.println("generate interval");
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        HashMap<String,Object> params = new HashMap<>();
        params.put("pigeons",5);
        params.put("holes",4);
        params.put("capacity",Interval.parseInterval("[2,3]"));
        params.put("quantifier","interval");
        ProblemSupervisor problemSupervisor = prepare(params);
        BasicClauseList bcl = PigeonHoleGenerator.generate(params,problemSupervisor, errors,warnings);
        assertEquals("Pigeon Hole example with 5 pigeons in 4 holes\n" +
                "capacity: interval [2,3] pigeons in a hole.\n" +
                "Quantifieds:\n" +
                "X-1: 1 P1H1,P1H2,P1H3,P1H4\n" +
                "X-2: 1 P2H1,P2H2,P2H3,P2H4\n" +
                "X-3: 1 P3H1,P3H2,P3H3,P3H4\n" +
                "X-4: 1 P4H1,P4H2,P4H3,P4H4\n" +
                "X-5: 1 P5H1,P5H2,P5H3,P5H4\n" +
                "Intervals:\n" +
                "I-6: 2-3: P1H1,P2H1,P3H1,P4H1,P5H1\n" +
                "I-7: 2-3: P1H2,P2H2,P3H2,P4H2,P5H2\n" +
                "I-8: 2-3: P1H3,P2H3,P3H3,P4H3,P5H3\n" +
                "I-9: 2-3: P1H4,P2H4,P3H4,P4H4,P5H4\n",bcl.toString());
    }
*/
}