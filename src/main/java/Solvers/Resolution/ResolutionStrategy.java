package Solvers.Resolution;

import java.util.ArrayList;
import java.util.Arrays;

/**
 * Created by ohlbach on 08.06.2019.
 */
public enum ResolutionStrategy {
    INPUT, SOS, POSITIVE, NEGATIVE;

    public static String help() {
        return "  strategy:\n" +
                "   INPUT:    resolvents with one parent from the input clauses.\n" +
                "   SOS:      resolvents with one parent from the sos, resolvent into sos.\n" +
                "   POSITIVE: resolvents with one parent from the positive clauses.\n" +
                "   NEGATIVE: resolvents with one parent from the negative clauses.\n";}

    public static ArrayList parseStrategies(String strategies, String place, StringBuffer errors, StringBuffer warnings) {
        ArrayList<ResolutionStrategy> list = new ArrayList<>();
        for(String name : strategies.trim().split("(\\s+|\\s*,\\s*)")) {
            ResolutionStrategy strategy = null;
            try{strategy = ResolutionStrategy.valueOf(name.toUpperCase());}
            catch(IllegalArgumentException ex) {
                warnings.append(place + ": unknown resolution strategy " + name  + ", is ignored\n");
                continue;}
            list.add(strategy);}
        if(list.isEmpty()) {
            errors.append(place + ": no known resolution strategy specified. Use one of " + Arrays.toString(ResolutionStrategy.values()));}
        return list;}
}
