package Generators;

import Datastructures.Clauses.BasicClauseList;
import Utilities.Utilities;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;

/**
 * Created by ohlbach on 09.09.2018.<br/>
 *
 * This class is for generating pidgeon hole problems of arbitrary size.
 *
 */
public class PidgeonHoleGenerator {

    private static HashSet<String> keys = new HashSet<>();
    static { // these are the allowed keys in the specification.
        for(String key : new String[]{"type", "pidgeons","holes"}) {
            keys.add(key);}

    }


    /** generates for a range of pidgeons and a range of holes a sequence of pidgeon hole specifications.
     * The pidgeons and holes may be ranges like '4,5,6' or '5 to 10' or '5 to 11 step 2'.
     *
     * @param parameters a HashMap with keys "pidgeons" and "holes"
     * @param errors    for error messages
     * @param warnings  for warnings
     * @return a list of HashMaps with "pidgeons" and "holes
     */
    public static ArrayList<HashMap<String,Object>> parseParameters(HashMap<String,String> parameters,
                                                                           StringBuffer errors, StringBuffer warnings){
        for(String key : parameters.keySet()) {
            if(!keys.contains(key)) {warnings.append("PidgeonHoleGenerator: unknown key in parameters: " + key + "\n");}}

        HashMap<String,Object> map = new HashMap<>();

        String hole      = parameters.get("holes");
        String pidgeon = parameters.get("pidgeons");

        String place =  "PidgeonHoleGenerator: ";

        ArrayList holes = null;
        if(hole == null) {errors.append(place).append("no holes specified.\n");}
        else {holes = Utilities.parseIntRange(place+"holes: ",hole,errors);}

        ArrayList pidgeons = null;
        if(pidgeon == null) {errors.append(place).append("no pidgeons specified.\n");}
        else {pidgeons = Utilities.parseIntRange(place+"pidgeons: ",pidgeon,errors);}

        if(holes == null || pidgeons == null) {return null;}

        ArrayList<HashMap<String,Object>> params = new ArrayList<>();
        ArrayList<ArrayList> list = Utilities.crossProduct(holes,pidgeons);
        for(ArrayList<Object> p : list ) {
            HashMap<String,Object> spec = new HashMap<>();
            spec.put("holes",p.get(0));
            spec.put("pidgeons",p.get(1));
            params.add(spec);}
        return params;}

    public static String help() {
        return "Pidgeon Hole disjunctions with keys:\n" +
                "pidgeons:  range of pidgeons\n" +
                "holes:     range of holes.\n" +
                "The pidgeons and holes may be ranges like '4,5,6' or '5 to 10' or '5 to 11 step 2'.";
    }


    /** genereartes for a given number of pidgeons and a number of holes the corresponding pidgeon hole disjunctions.
     *
     * @param parameters a HashMap with keys "holes" and "pidgeons"
     * @param errors    no effect
     * @param warnings  no effect
     * @return parameters with an extra entry: "clauses".
     */
    public static HashMap<String,Object> generate(HashMap<String,Object> parameters,
                                                  StringBuffer errors, StringBuffer warnings){
        int holes = (Integer)parameters.get("holes");
        int pidgeons = (Integer)parameters.get("pidgeons");
        StringBuilder st = new StringBuilder();
        for(int pidgeon = 1; pidgeon <= pidgeons; ++pidgeon) {
            for(int hole = 1; hole <= holes; ++hole) {
                st.append("P"+pidgeon+"H"+hole);
                if(hole < holes) {st.append(" ");}}
            st.append("\n");}
        for(int hole = 1; hole <= holes; ++hole) {
            st.append("$d ");
            for(int pidgeon = 1; pidgeon <= pidgeons; ++pidgeon) {
                st.append("P"+pidgeon+"H"+hole);
                if(pidgeon < pidgeons) {st.append(" ");}}
            st.append("\n");}

        parameters.put("clauseString",st.toString());
        StringClauseSetGenerator.generate(parameters,errors,warnings);
        ((BasicClauseList)parameters.get("clauses")).info = "Pidgeon Hole example with " + pidgeons + " pidgeons in " + holes + " holes.";
        return parameters;
    }


    }
