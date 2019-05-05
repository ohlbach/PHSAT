package Generators;

import Datastructures.Clauses.BasicClauseList;
import org.junit.Test;

import java.util.ArrayList;
import java.util.HashMap;

import static org.junit.Assert.*;

/**
 * Created by ohlbach on 05.05.2019.
 */
public class SingleModelGeneratorTest {
    @Test
    public void parseParameters() throws Exception {
        System.out.println("parseProblemParameters 1");
        HashMap<String,String> parameters = new HashMap<>();
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
        parameters.put("predicates","10");
        parameters.put("clauses","30");
        parameters.put("length","3");
        parameters.put("precise","true,false");
        parameters.put("cpRatio","4.2");
        ArrayList<HashMap<String,Object>> map = SingleModelGenerator.parseParameters(parameters,errors,warnings);
        System.out.println("E\n"+errors.toString());
        System.out.println("W\n"+warnings.toString());
        System.out.println(map);

    }

    @Test
    public void help() throws Exception {
        System.out.println(SingleModelGenerator.help());

    }

    @Test
    public void generate1() throws Exception {
        System.out.println("generate 1");
        HashMap<String,String> parameters = new HashMap<>();
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
        parameters.put("predicates","10");
        parameters.put("clauses","40");
        parameters.put("length","5");
        parameters.put("precise", "false");
        HashMap<String,Object> map = SingleModelGenerator.parseParameters(parameters,errors,warnings).get(0);

        BasicClauseList bcl = SingleModelGenerator.generate(map,errors,warnings);
        System.out.println(bcl.toString());
    }



}