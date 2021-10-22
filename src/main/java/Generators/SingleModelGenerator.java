package Generators;

import Datastructures.Clauses.BasicClauseList;
import Datastructures.Clauses.ClauseType;
import Utilities.Utilities;

import java.lang.reflect.Array;
import java.util.*;

/** The generator generates clause sets with exactly one model.
 * Created by ohlbach on 05.05.2019.
 */
public final class SingleModelGenerator {
    private static HashSet<String> keys = new HashSet<>();
    static { // these are the allowed keys in the specification.
        for(String key : new String[]{"problem","type", "seed","predicates","cpRatio", "clauses","length","precise"}) {
            keys.add(key);}}

    /**
     *  *
     * @param parameters the input parameters
     * @param errors    for reporting syntax errors
     * @param warnings  for reporting warnings
     * @return an ArrayList of HashMaps with the translated parameters.
     */
    public static ArrayList<HashMap<String,Object>> parseParameters(HashMap<String,String> parameters, StringBuilder errors, StringBuilder warnings) {
        for (String key : parameters.keySet()) {
            if (!keys.contains(key)) {
                warnings.append("SingleModelGenerator: unknown key in parameters: " + key + "\n");}}

        String seed      = parameters.get("seed");
        String predicate = parameters.get("predicates");
        String cpRatio   = parameters.get("cpRatio");
        String clause    = parameters.get("clauses");
        String length    = parameters.get("length");
        String precise   = parameters.get("precise");

        ArrayList seeds = null;
        if(seed == null) {seeds = new ArrayList<>(); seeds.add(0);}
        else {seeds = Utilities.parseIntRange("SingleModelGenerator seed",seed,errors);}

        ArrayList predicates = null;
        if(predicate == null) {errors.append("SingleModelGenerator: no number of predicates defined.\n");}
        else {predicates = Utilities.parseIntRange("SingleModelGenerator predicate",predicate,errors);}

        ArrayList cpRatios = null;
        if(cpRatio != null) {
            cpRatios = Utilities.parseFloatRange("SingleModelGenerator cpRatio",cpRatio,errors);}

        ArrayList clauses = null;
        if(clause == null && cpRatios == null) {errors.append("SingleModelGenerator: no number of disjunctions defined.\n");}
        else {clauses = Utilities.parseIntRange("SingleModelGenerator predicate",clause,errors);}

        ArrayList lengths = null;
        if(length == null) {lengths = new ArrayList<>(); lengths.add(3);}
        else {lengths = Utilities.parseIntRange("SingleModelGenerator length",length,errors);}

        ArrayList precises = null;
        if(precise == null) {precises = new ArrayList<>(); precises.add(true);}
        else {precises = Utilities.parseBoolean("SingleModelGenerator precise", precise,errors);}

        ArrayList<HashMap<String,Object>> control = new ArrayList<>();
        if(cpRatios == null) {
            ArrayList<ArrayList> list = Utilities.crossProduct(seeds,predicates,clauses,lengths, precises);
            for(ArrayList values : list) {
                HashMap<String,Object> cntr = new HashMap<>();
                cntr.put("seed",values.get(0));
                Integer pred = (Integer)values.get(1);
                if(pred < 1) {errors.append("SingleModelGenerator: number of predicates must be > 0, not " + pred + ".\n");}
                cntr.put("predicates",pred);
                Integer cl = (Integer)values.get(2);
                if(cl < 1) {errors.append("SingleModelGenerator: number of clauses must be > 0, not " + cl + ".\n");}
                cntr.put("clauses",  cl);
                Integer ln =  (Integer)values.get(3);
                if(ln < 1) {errors.append("SingleModelGenerator: clause length must be > 0, not " + ln + ".\n");}
                cntr.put("length",   ln);
                cntr.put("precise",  values.get(4));
                control.add(cntr);}}
            else {
            ArrayList<ArrayList> list = Utilities.crossProduct(seeds,predicates,cpRatios,lengths, precises);
            for(ArrayList values : list) {
                HashMap<String,Object> cntr = new HashMap<>();
                cntr.put("seed",values.get(0));
                Integer pred = (Integer)values.get(1);
                if(pred < 1) {errors.append("SingleModelGenerator: number of predicates must be > 0, not " + pred + ".\n");}
                cntr.put("predicates",pred);
                Float cp = (Float)values.get(2);
                if(cp <= 0) {errors.append("SingleModelGenerator: cpRatio must be positive, not " + cp + ".\n");}
                int nClauses = Math.round(pred*cp);
                cntr.put("clauses",  nClauses);
                Integer ln =  (Integer)values.get(3);
                if(ln < 1) {errors.append("SingleModelGenerator: clause length must be > 0, not " + ln + ".\n");}
                cntr.put("length",   ln);
                cntr.put("precise",  values.get(4));
                control.add(cntr);}}
            return control;}

    /** yields a help string.
     *
     * @return a help string.
     */
    public static String help() {
        return "Single Model Generator:\n" +
                "It generates a clause set (disjunctions only) which has a single model.\n" +
                "seed:       for the random number generator (default 0).\n" +
                "predicates: number of predicates.\n" +
                "length:     clause length (default 3).\n" +
                "precise:    if true then the clauses have exactly the specified length (default)\n" +
                "            if false then the clauses have varying length between 1 and the specified length.\n" +
                "clauses:    number of clauses, or alternatively\n" +
                "cpRatio:    determines the number of clauses by: cpRatio * predicates.\n";}


    /** generates the clause set
     *
     * @param parameters for controlling the generator.
     * @param errors for error messages
     * @param warnings for warnings
     * @return  the generated BasicClauseList
     */
    public static BasicClauseList generate(HashMap<String,Object> parameters, StringBuilder errors, StringBuilder warnings) {
        int seed = (Integer) parameters.get("seed");
        int predicates = (Integer) parameters.get("predicates");
        int numberClauses = (Integer) parameters.get("clauses");
        int maxClauseLength = (Integer) parameters.get("length");
        boolean precise = (Boolean) parameters.get("precise");

        if(predicates < maxClauseLength) {
            errors.append("SingleModelGenerator: More literals in a clause than predicates.\n").
                    append("predicates: " + predicates + " literals " + maxClauseLength +"\n");
            return null;}

        short[] signs = new short[predicates+1];
        Random rnd = new Random(seed);
        for(int pred = 1; pred <= predicates; ++pred) {signs[pred] = (short)rnd.nextInt(2);}
        System.out.println("P " + Arrays.toString(signs));
        BasicClauseList clauseList = new BasicClauseList();
        clauseList.predicates = predicates;

        ArrayList<Integer> literals = new ArrayList();
        clauseList.info = "Single model clause set with seed " + seed + " and model\n";
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            if(signs[predicate] == 0) {clauseList.info += "-";}
            clauseList.info += ""+predicate+",";}

        int type = ClauseType.OR.ordinal();
        int clauseCounter = 0;
        while(clauseCounter < numberClauses) {
            literals.clear();
            int clauseLength = precise ? maxClauseLength : rnd.nextInt(maxClauseLength)+1;
            int truePredicate =  rnd.nextInt(predicates)+1;
            int trueLiteral = truePredicate;
            if(signs[truePredicate] == 0) {trueLiteral = -trueLiteral;}
            int position = rnd.nextInt(clauseLength);
            int[] lits = new int[clauseLength+2];
            lits[0] = ++clauseCounter;
            lits[1] = type;
            for(int i = 0; i < clauseLength; ++i) {
                if(i == position) {lits[i+2] = trueLiteral; continue;}
                boolean again = true;
                while(again) {
                    again = false;
                    int lit =  rnd.nextInt(predicates)+1;
                    if(lit == truePredicate) {again = true; continue;}
                    for(int j = 0; j < i; ++j) {if(Math.abs(lits[j+2]) == lit) {again = true; break;}}
                    if(again) {continue;}
                    if(signs[lit] == 1) {lit = -lit;}
                    lits[i+2] = lit;}}
            clauseList.addClause(lits,"",errors,warnings);}

        return clauseList;}

    }
