package Generators;

import org.junit.Test;

import java.util.HashMap;

import static org.junit.Assert.*;

/**
 * Created by ohlbach on 09.10.2018.
 */
public class SourceTypeTest {
    @Test
    public void getGeneratorClass() throws Exception {

    }

    @Test
    public void help() throws Exception {
        System.out.println("help(name)");
        System.out.println(SourceType.help("random"));

    }

    @Test
    public void help1() throws Exception {
        System.out.println("help()");
        System.out.println(SourceType.help());

    }

    @Test
    public void parseParameters() throws Exception {
        System.out.println("parseParameters");
        HashMap<String,String> parameters = new HashMap<>();
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
        parameters.put("predicates","10");
        parameters.put("disjunctions","30");
        parameters.put("length","3");
        System.out.println(SourceType.parseParameters("random",parameters,errors,warnings));
    }

    @Test
    public void generate() throws Exception {
        System.out.println("generate");
        HashMap<String,String> parameters = new HashMap<>();
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
        parameters.put("predicates","10");
        parameters.put("disjunctions","30");
        parameters.put("length","3");
        parameters.put("precise", "true");
        HashMap<String,Object> map = SourceType.parseParameters("random",parameters,errors,warnings).get(0);
        System.out.println(SourceType.generate("random",map,errors,warnings));
    }

}