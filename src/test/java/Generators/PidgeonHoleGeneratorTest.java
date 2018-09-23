package Generators;

import org.junit.Test;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Objects;

import static org.junit.Assert.*;

/**
 * Created by ohlbach on 09.09.2018.
 */
public class PidgeonHoleGeneratorTest {
    @Test
    public void help(){
        System.out.println(PidgeonHoleGenerator.help());
    }

    @Test
    public void parseProblemParameters() throws Exception {
        System.out.println("parseProblemParameters");
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
        HashMap<String,String> params = new HashMap<>();
        params.put("holes","2 to 3");
        params.put("pidgeons", "3 to 4");
        ArrayList<HashMap<String,Object>> parameters = PidgeonHoleGenerator.parseParameters(params,errors,warnings);
        System.out.println(parameters);
        assertEquals("[{pidgeons=3, holes=2}, {pidgeons=4, holes=2}, {pidgeons=3, holes=3}, {pidgeons=4, holes=3}]",parameters.toString());
    }

    @Test
    public void generate() throws Exception {
        System.out.println("generate");
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
        HashMap<String,Object> params = new HashMap<>();
        params.put("pidgeons",5);
        params.put("holes",4);
        PidgeonHoleGenerator.generate(params,errors,warnings);
        //System.out.println(params.get("disjunctions"));
        assertEquals("Pidgeon Hole example with 5 pidgeons in 4 holes.\n" +
                "Disjunctions:\n" +
                "1 o: P1H1,P1H2,P1H3,P1H4\n" +
                "2 o: P2H1,P2H2,P2H3,P2H4\n" +
                "3 o: P3H1,P3H2,P3H3,P3H4\n" +
                "4 o: P4H1,P4H2,P4H3,P4H4\n" +
                "5 o: P5H1,P5H2,P5H3,P5H4\n" +
                "Disjoints:\n" +
                "6 d: P1H1,P2H1,P3H1,P4H1,P5H1\n" +
                "7 d: P1H2,P2H2,P3H2,P4H2,P5H2\n" +
                "8 d: P1H3,P2H3,P3H3,P4H3,P5H3\n" +
                "9 d: P1H4,P2H4,P3H4,P4H4,P5H4\n",params.get("clauses").toString());

    }

}