package Generators;

import org.junit.Test;

import java.util.HashMap;

/**
 * Created by ohlbach on 09.10.2018.
 */
public class GeneratorTest {
    @Test
    public void getGeneratorClass() throws Exception {

    }

    @Test
    public void help() throws Exception {
        System.out.println("help(name)");
        System.out.println(Generator.help("random"));

    }

    @Test
    public void help1() throws Exception {
        System.out.println("help()");
        System.out.println(Generator.help());

    }

    @Test
    public void parseParameters() throws Exception {
        System.out.println("parseParameters");
        HashMap<String,String> parameters = new HashMap<>();
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        parameters.put("predicates","10");
        parameters.put("disjunctions","30");
        parameters.put("length","3");
        System.out.println(Generator.parseParameters("random",parameters,errors,warnings));
    }

    @Test
    public void generate() throws Exception {
        System.out.println("generate");
        HashMap<String,String> parameters = new HashMap<>();
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        parameters.put("predicates","10");
        parameters.put("disjunctions","30");
        parameters.put("length","3");
        parameters.put("precise", "true");
        HashMap<String,Object> map = Generator.parseParameters("random",parameters,errors,warnings).get(0);
        System.out.println(Generator.generate("random",map,null,errors,warnings));
    }

}