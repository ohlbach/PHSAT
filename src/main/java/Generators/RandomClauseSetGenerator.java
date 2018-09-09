package Generators;

import Datastructures.Clauses.BasicClauseList;
import Datastructures.Model;
import Datastructures.Status;
import Utilities.Utilities;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Random;

/**
 * Created by ohlbach on 26.08.2018.
 *
 * This generator generates clause sets with randomly generated literals.<br/>
 * It can generate two types of clauses, ordinary clauses and disjointness clauses <br/>
 * A clause p,q,r as a disjointness clause  means that at most one of the predicates can be true in a model.
 * If disjointness clauses are generated then the representation of a clause in BasicClauseList starts with
 * 0 for ordinary clause and 1 for disjointness clause.
 */
public class RandomClauseSetGenerator {

    private static HashSet<String> keys = new HashSet<>();
    static { // these are the allowed keys in the specification.
        for(String key : new String[]{"type", "seed","predicates","clauses","cpRatio","length","precise","dBlocks","dLength"}) {
            keys.add(key);}}

    /** The method translates the string-valued parameters in the HashMap to objects for controlling the generator.
     * The allowed parameters are: <br/>
     * predicates: an integer > 0, specifies the number of predicates in the clause set.<br/>
     * clauses:    an integer > 0, specifies the number of normal clauses to be generated.<br/>
     * cpRatio:    a float > 0, specifies the clause/predicate ratio.<br/>
     *             cpRatio = 4.3 means: for 100 predicates 430 clauses.<br/>
     * length:     an integer > 0, specifies the maximum number of literals per clause.<br/>
     * precise:    a boolean, if true then the clauses have exactly the specified length (default true).<br/>
     * seed:       an integer >= 0 for starting the random number generator (default 0).<br/>
     * dBlocks (optional): an integer > 0, the number of dijointness blocks.<br/>
     * dLength (optional): an integer > 0, the number of predicates in each disjointness block.<br/>
     *
     * The integer values can be specified as \'ranges\', with the following syntactic possibilities:<br/>
     *   List:       3,6,7\n<br/>
     *   Range:      3 to 10\n<br/>
     *   With steps: 3 to 10 step 2\n<br/>
     * Float values can be specified;\n<br/>
     *   List:       4.6,7.8<br/>
     *  With steps: 3.5 to 5.6 step 0.1<br/>
     * Boolean values are for example \'true\', \'false\' of both \'true,false\'.<br/>
     *
     *
     * @param parameters the input parameters
     * @param errors    for reporting syntax errors
     * @param warnings  for reporting warnings
     * @return an ArrayList of HashMaps with the translated parameters.
     */
    public static ArrayList<HashMap<String,Object>> parseProblemParameters(HashMap<String,String> parameters, StringBuffer errors, StringBuffer warnings){
        for(String key : parameters.keySet()) {
            if(!keys.contains(key)) {warnings.append("RandomClauseSetGenerator: unknown key in parameters: " + key + "\n");}}

        String seed      = parameters.get("seed");
        String predicate = parameters.get("predicates");
        String clause    = parameters.get("clauses");
        String cpRatio   = parameters.get("cpRatio");
        String length    = parameters.get("length");
        String precise   = parameters.get("precise");
        String dBlocks   = parameters.get("dBlocks");
        String dLength   = parameters.get("dLength");
        if(dLength != null && dBlocks == null) {dBlocks = "1";}

        ArrayList seeds = null;
        if(seed == null) {seeds = new ArrayList<>(); seeds.add(0);}
        else {seeds = Utilities.parseIntRange("RandomClauseSetGenerator seed",seed,errors);}

        ArrayList predicates = null;
        if(predicate == null) {errors.append("RandomClauseSetGenerator: no number of predicates defined.\n");}
        else {predicates = Utilities.parseIntRange("RandomClauseSetGenerator predicate",predicate,errors);}

        ArrayList cpRatios = null;
        if(cpRatio != null) {
            cpRatios = Utilities.parseFloatRange("RandomClauseSetGenerator cpRatio",cpRatio,errors);}

        ArrayList clauses = null;
        if(clause == null && cpRatios == null) {errors.append("RandomClauseSetGenerator: no number of clauses defined.\n");}
        else {clauses = Utilities.parseIntRange("RandomClauseSetGenerator predicate",clause,errors);}

        ArrayList lengths = null;
        if(length == null) {errors.append("RandomClauseSetGenerator: no clause length defined.\n");}
        else {lengths = Utilities.parseIntRange("RandomClauseSetGenerator length",length,errors);}

        ArrayList precises = Utilities.parseBoolean("RandomClauseSetGenerator precise", precise,errors);

        ArrayList dBlockss = null;
        if(dBlocks != null) { dBlockss = Utilities.parseIntRange("RandomClauseSetGenerator dBlocks",dBlocks,errors);}

        ArrayList dLengths = null;
        if(dLength != null) {
            dLengths = Utilities.parseIntRange("RandomClauseSetGenerator dLength",dLength,errors);}

        if(seeds == null || predicates == null || (clauses == null && cpRatio == null) || lengths == null || precises == null) {return null;}

        ArrayList<HashMap<String,Object>> control = new ArrayList<>();
        if(cpRatios == null) {
            ArrayList<ArrayList> list = Utilities.crossProduct(seeds,predicates,clauses,lengths, precises,dBlockss,dLengths);
            for(ArrayList values : list) {
                Integer pred = (Integer)values.get(1);
                Integer dbl = (Integer)values.get(5);
                Integer dle = (Integer)values.get(6);
                if(dbl != null && dle != null && dbl * dle > pred) {
                    errors.append("RandomClauseSetGenerator: number of disjoint predicates exceeds number of predicates: " +
                            dbl + "*" + dle + " > " + pred +".\n ");
                    continue;}
                HashMap<String,Object> cntr = new HashMap<>();
                cntr.put("seed",values.get(0));
                cntr.put("predicates",pred);
                cntr.put("clauses",  values.get(2));
                cntr.put("length",   values.get(3));
                cntr.put("precise",  values.get(4));
                cntr.put("dBlocks",  dbl);
                cntr.put("dLengths", dle);
                control.add(cntr);}
            return control;}

        ArrayList<ArrayList> list = Utilities.crossProduct(seeds,predicates,cpRatios,lengths, precises,dBlockss,dLengths);
        for(ArrayList values : list) {
            HashMap<String,Object> cntr = new HashMap<>();
            cntr.put("seed",values.get(0));
            cntr.put("predicates",values.get(1));
            cntr.put("clauses",Math.round((Integer)values.get(1)*(Float)values.get(2)));
            cntr.put("length",   values.get(3));
            cntr.put("precise",  values.get(4));
            cntr.put("dBlocks",  values.get(5));
            cntr.put("dLengths", values.get(6));
            control.add(cntr);}
        return control;}


    /** yields a help string.
     *
     * @return a help string.
     */
    public static String help() {
        StringBuilder st = new StringBuilder();
        st.append("Random Clause Set Generator\n");
        st.append("It can generate normal propositional clauses as well as \"disjointness clauses\"\n");
        st.append("A disjointenss clause \'p,q,r\' means that at most one of the literals can be true in a model.\n");
        st.append("The parameters are:\n");
        st.append("predicates: an integer > 0, specifies the number of predicates in the clause set.\n");
        st.append("clauses:    an integer > 0, specifies the number of normal clauses to be generated.\n");
        st.append("cpRatio:    a float > 0, specifies the clause/predicate ratio.\n");
        st.append("            cpRatio = 4.3 means: for 100 predicates 430 clauses.");
        st.append("length:     an integer > 0, specifies the maximum number of literals per clause.\n");
        st.append("precise:    a boolean, if true then the clauses have exactly the specified length (default true).\n");
        st.append("seed:       an integer >= 0 for starting the random number generator (default 0).\n");
        st.append("dBlocks (optional): an integer > 0, the number of dijointness blocks.\n" );
        st.append("dLength (optional): an integer > 0, the number of predicates in each disjointness block.\n");
        st.append("\n");
        st.append("The integer values can be specified as \'ranges\', with the following syntactic possibilities:\n");
        st.append("  List:       3,6,7\n");
        st.append("  Range:      3 to 10\n");
        st.append("  With steps: 3 to 10 step 2\n");
        st.append("Float values can be specified;\n");
        st.append("  List:       4.6,7.8\n");
        st.append("  With steps: 3.5 to 5.6 step 0.1\n");
        st.append("Boolean values are for example \'true\', \'false\' of both \'true,false\'.\n");
        st.append("");
        st.append("The specification of ranges causes the generation of a sequence of clause lists.\n");
        return st.toString();}



    /** generates the clause set
     *
     * @param parameters for controlling the generator.
     * @return  the parameters  with an additional key "clauses" with the generatied BasicClauseList
     */
    public static HashMap<String,Object> generate(HashMap<String,Object> parameters, StringBuffer errors, StringBuffer warnings) {
        int seed            = (Integer)parameters.get("seed");
        int predicates      = (Integer)parameters.get("predicates");
        int numberClauses   = (Integer)parameters.get("clauses");
        int maxClauseLength = (Integer)parameters.get("length");
        boolean precise     = (Boolean)parameters.get("precise");
        Integer dBlocks     = (Integer)parameters.get("dBlocks");
        Integer dLength     = (Integer)parameters.get("dLengths");
        boolean withDisjointness = dBlocks != null;

        BasicClauseList clauseList = new BasicClauseList(withDisjointness);

        ArrayList<Integer> literals = new ArrayList();
        Status stat = new Status();
        stat.seed = seed;
        Model model = new Model(predicates);
        clauseList.info = "Randomly generated clauses with seed " + seed;
        Random rnd = new Random(seed);
        int clauseCounter = 1;
        while(clauseList.clauses.size() < numberClauses) {
            literals.clear();
            int clauseLength = precise ? maxClauseLength : rnd.nextInt(maxClauseLength)+1;
            while(literals.size() < clauseLength) {
                int literal = rnd.nextInt(predicates)+1;
                if(rnd.nextBoolean()) {literal = -literal;}
                if(literals.contains(-literal)) {literals.clear(); continue;}
                if(literals.contains(literal)) {continue;}
                literals.add(literal);}
            int[] lits = new int[withDisjointness ? clauseLength+1 : clauseLength];
            int start = 0;
            if(withDisjointness) {lits[0]=0; start=1;}
            for(int i = start; i < lits.length; ++i) {lits[i] = literals.get(withDisjointness ? i-1 : i);}
            clauseList.clauses.add(lits);}

        if(withDisjointness) {
            HashSet<Integer> preds = new HashSet();
            for(int block = 0; block < dBlocks; ++block) {
                literals.clear(); literals.add(1);
                while(literals.size() < dLength) {
                    int predicate = rnd.nextInt(predicates)+1;
                    if(preds.contains(predicate)) {continue;}
                    preds.add(predicate);
                    literals.add(predicate);}
                int lits[] = new int[literals.size()];
                for(int i = 0; i < lits.length; ++i) {lits[i] = literals.get(i);}
                clauseList.clauses.add(lits);}
            }
        parameters.put("clauses",clauseList);
        return parameters;}}




