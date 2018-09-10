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
        //System.out.println(params.get("clauses"));
        assertEquals("[0, 1, 2, 3, 4]\n" +
                "[0, 5, 6, 7, 8]\n" +
                "[0, 9, 10, 11, 12]\n" +
                "[0, 13, 14, 15, 16]\n" +
                "[0, 17, 18, 19, 20]\n" +
                "[1, 1, 5, 9, 13, 17]\n" +
                "[1, 2, 6, 10, 14, 18]\n" +
                "[1, 3, 7, 11, 15, 19]\n" +
                "[1, 4, 8, 12, 16, 20]\n",params.get("clauses").toString());

        //System.out.println(PidgeonHoleGenerator.help());

    }

}