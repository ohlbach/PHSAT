package Generators;

import Datastructures.Clauses.BasicClauseList;
import Datastructures.Clauses.ClauseType;
import Datastructures.Symboltable;
import Management.ProblemSupervisor;
import Utilities.Utilities;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;

/**
 * Created by ohlbach on 09.09.2018.<br>
 *
 * This class is for generating variations of pigeon hole problems of arbitrary size.<br>
 * There are n holes and m pigeons <br>
 * Each pigeon must be placed into exactly one hole. <br>
 * Each hole, however, may have different capacities: <br>
 *   atleast n pigeons may be put into a hole <br>
 *   atmost  n pigeons may be put into a hole <br>
 *   exactly n pigeons may be put into a hole.
 */
public final class PigeonHoleGenerator {

    private static final HashSet<String> keys = new HashSet<>();
    static { // these are the allowed keys in the specification.
        Collections.addAll(keys, "pigeons", "holes", "capacity", "quantifier");

    }
    public static String help() {
        return "Pigeon Hole Generator for putting a number of pigeons into a number or holes\n" +
                "Each pigeon must be put into exactly one hole\n"+
                "Each hole has some capacity for taking pigeons\n"+
                "The keys are:\n"+
                "pigeons:    range of pigeons\n" +
                "holes:      range of holes.\n" +
                "capacity:   range of hole capacities (default 1)\n" +
                "quantifier: atleast, atmost, exactly\n"+
                "The numbers may be ranges like '4,5,6' or '5 to 10' or '5 to 11 step 2'.";
    }

    /** generates for a range of pigeons and a range of holes a sequence of pigeon hole specifications.
     * The pigeons and holes may be ranges like '4,5,6' or '5 to 10' or '5 to 11 step 2'.
     *
     * @param parameters a HashMap with keys "pigeons" and "holes" and "capacity" and "quantifier"
     * @param errors    for error messages
     * @param warnings  for warnings
     * @return a list of HashMaps with "pigeons" and "holes", "capacity", "quantifier"
     */
    public static ArrayList<HashMap<String,Object>> parseParameters(HashMap<String,String> parameters,
                                                                           StringBuilder errors, StringBuilder warnings){
        String prefix = "Pigeon Hole Generator: ";
        for(String key : parameters.keySet()) {
            if(key.equals("type") || key.equals("name")) continue;
            if(!keys.contains(key)) {
                warnings.append(prefix+"Unknown key in parameters: " + key + "\n").
                        append("Allowed keys: ").append(keys.toString()).append("\n");}}


        String hole       = parameters.get("holes");
        String pigeon     = parameters.get("pigeons");
        String capacity   = parameters.get("capacity");
        String quantifier = parameters.get("quantifier");

        boolean erroneous = false;

        ArrayList holes = null;
        if(hole == null) {errors.append(prefix+"No holes specified.\n"); erroneous = true;}
        else {holes = Utilities.parseIntRange(prefix + "holes: ",hole,errors);
            if(holes == null) {errors.append("\n"); erroneous = true;}}

        ArrayList pigeons = null;
        if(pigeon == null) {errors.append(prefix+"No pigeons specified.\n");}
        else {pigeons = Utilities.parseIntRange(prefix+"pigeons: ",pigeon,errors);
                if(pigeons == null) {errors.append("\n"); erroneous = true;}}

        ArrayList capacities = null;
        if(capacity ==null) {ArrayList list = new ArrayList(); list.add(1);}
        else {capacities = Utilities.parseIntRange(prefix+"capacity: ",capacity,errors);
            if(capacities == null) {errors.append("\n"); erroneous = true;}}

        ArrayList quantifiers = null;
        if(quantifier == null) {quantifiers = new ArrayList(); quantifiers.add("exactly");}
        else {String[] parts = ((String)quantifier).split("\\s*( |,)\\s*");
            quantifiers = new ArrayList();
            for(String part : parts) quantifiers.add(part);}

        if(erroneous) {return null;}

        ArrayList<HashMap<String,Object>> params = new ArrayList<>();
        ArrayList<ArrayList> list = Utilities.crossProduct(holes,pigeons,capacities,quantifiers);
        for(ArrayList<Object> p : list ) {
            int holesv         = (int)p.get(0);
            int pigeonsv       = (int)p.get(1);
            int capacityv      = (int)p.get(2);
            String quantifierv = (String)p.get(3);
            if(holesv <= 0) {
                errors.append(prefix+"holes is not positive " + holesv); erroneous = true;}
            if(pigeonsv <= 0) {
                errors.append(prefix+"pigeons is not positive " + pigeonsv); erroneous = true;}
            if(capacityv <= 0) {
                errors.append(prefix+"capacity is not positive " + capacityv); erroneous = true;}
            if(!(quantifierv.equals("atleast") || quantifierv.equals("atmost") ||quantifierv.equals("exactly"))) {
                errors.append(prefix + "quantifier ist not atleast, atmost or exactly: " + quantifierv);
                erroneous = true;}
            if(erroneous) return null;

            HashMap<String,Object> spec = new HashMap<>();
            spec.put("holes",holesv);
            spec.put("pigeons",pigeonsv);
            spec.put("capacity", capacityv);
            spec.put("quantifier",quantifierv);
            spec.put("name","PH"+pigeonsv+""+holesv);
            params.add(spec);}
        return params;}




    /** generates the clauses for the problem.
     *  Example: 3 pigeons, 3 holes, capacity atmost 2<br>
     *  exactly 1 P1H1, P1H2, P1H3<br>
     *  exactly 1 P2H1, P2H2, P2H3<br>
     *  exactly 1 P3H1, P3H2, P3H3<br>
     *  <br>
     *  atmost 2 P1H1, P2H1, P3H1<br>
     *  atmost 2 P1H2, P2H2, P3H2<br>
     *  atmost 2 P1H3, P2H3, P3H3<br>
     *
     * @param parameters a HashMap with keys "holes" and "pidgeons"
     * @param errors    no effect
     * @param warnings  no effect
     * @return the new clauses
     */
    public static BasicClauseList generate(HashMap<String,Object> parameters,ProblemSupervisor problemSupervisor,
                                                  StringBuilder errors, StringBuilder warnings){
        String prefix = "Pigeon Hole Generator: ";
        int holes         = (Integer)parameters.get("holes");
        int pigeons       = (Integer)parameters.get("pigeons");
        int capacity      = (Integer)parameters.get("capacity");
        String quantifier = (String)parameters.get("quantifier");
        int clauseType = 3;
        if(quantifier.equals("atmost"))  clauseType = 4;
        if(quantifier.equals("exactly")) clauseType = 5;

        Symboltable symboltable = new Symboltable(pigeons*holes);
        int predicates = 0;
        for(int pigeon = 1; pigeon <= pigeons; ++pigeon) {
            for(int hole = 1; hole <= holes; ++hole) {
                symboltable.setName(++predicates,"P"+pigeon+"H"+hole);}}

        BasicClauseList bcl = new BasicClauseList();
        bcl.predicates = predicates;
        bcl.info = "Pigeon Hole example with " + pigeons + " pigeons in " + holes + " holes\n"+
                    "capacity: " + quantifier + " " + capacity + " pigeons in a hole.";
        bcl.symboltable = symboltable;

        for(int pigeon = 1; pigeon <= pigeons; ++pigeon) {
            int[] clause = new int[holes+3];
            clause[0] = problemSupervisor.nextClauseId();
            clause[1] = ClauseType.EXACTLY.ordinal();
            clause[2] = 1;
            for(int hole = 1; hole <= holes; ++hole) {
                clause[hole+2] = symboltable.getPredicate("P"+pigeon+"H"+hole);}
            bcl.addClause(clause,prefix,errors,warnings);}

        for(int hole = 1; hole <= holes; ++hole) {
            int[] clause = new int[pigeons+3];
            clause[0] = problemSupervisor.nextClauseId();
            clause[1] = clauseType;
            clause[2] = capacity;
            for(int pigeon = 1; pigeon <= pigeons; ++pigeon) {
                clause[pigeon+2] = symboltable.getPredicate("P"+pigeon+"H"+hole);}
            bcl.addClause(clause,prefix,errors,warnings);}

        return bcl;
    }


    }
