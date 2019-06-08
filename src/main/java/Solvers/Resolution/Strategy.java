package Solvers.Resolution;

import Generators.StringClauseSetGenerator;

import java.util.ArrayList;
import java.util.Arrays;

/**
 * Created by ohlbach on 08.06.2019.
 */
public enum Strategy {
    INPUT, SOS, POSITIVE, NEGATIVE;

    public static String help() {
        return "  Resolution Strategies:\n" +
                "   INPUT:    resolvents with one parent from the input clauses.\n" +
                "   SOS:      resolvents with one parent from the sos, resolvent into sos.\n" +
                "   POSITIVE: resolvents with one parent from the positive clauses.\n" +
                "   NEGATIVE: resolvents with one parent from the negative clauses.\n";}

    public static ArrayList parseStrategies(String strategies, String place, StringBuffer errors, StringBuffer warnings) {
        ArrayList<Strategy> list = new ArrayList<>();
        for(String name : strategies.trim().split("\\s*")) {
            Strategy strategy = Strategy.valueOf(name.toUpperCase());
            if(strategy == null) {
                warnings.append(place + ": unknown resolution strategy " + name  + ", use one of " +
                    Arrays.toString(Strategy.values())+"\n");
                continue;}}
        if(list.isEmpty()) {
            errors.append(place + ": no known resolution strategy specified. Use one of " + Arrays.toString(Strategy.values()));
            list.add(INPUT);}
        return list;}
}
