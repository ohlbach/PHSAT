package Management;

import org.junit.Test;

import java.util.ArrayList;
import java.util.HashMap;

/**
 * Created by ohlbach on 02.11.2018.
 */
public class GlobalParametersTest {

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
        ArrayList<HashMap<String,String>> parameters = new ArrayList<>();
        parameters.add(pars);
        GlobalParameters glb = null; //new GlobalParameters(parameters,errors,warnings);
        System.out.println("Errors\n" + errors);
        System.out.println("Warnings\n" + warnings);
        System.out.println(glb.toString());

    }

}