package Management;

import Generators.StringClauseSetGenerator;
import jdk.nashorn.internal.objects.Global;
import org.junit.Test;

import java.nio.file.Paths;
import java.util.HashMap;

import static org.junit.Assert.*;

/**
 * Created by ohlbach on 02.11.2018.
 */
public class GlobalParametersTest {
    @Test
    public void help() throws Exception {
        System.out.println(GlobalParameters.help());
    }

    @Test
    public void constructor() {
        System.out.println("constructor");
        HashMap<String, String> pars = new HashMap<>();
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        pars.put("directory","test");
        pars.put("parallel","3");
        pars.put("logging", "logging.log");
        pars.put("monitor", "mixed "+ "monitor");
        pars.put("results","results.res");
        pars.put("statistics","stat.sta");

        GlobalParameters glb = new GlobalParameters(pars,errors,warnings);
        System.out.println("Errors\n" + errors);
        System.out.println("Warnings\n" + warnings);
        System.out.println(glb.toString());

    }

}