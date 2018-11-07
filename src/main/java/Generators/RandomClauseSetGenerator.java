package Generators;

import Datastructures.Clauses.BasicClauseList;
import Datastructures.Clauses.ClauseType;
import Datastructures.Status;
import Utilities.Utilities;

import java.util.*;

/**
 * Created by ohlbach on 26.08.2018.
 *
 * This generator generates clause sets with randomly generated literals.<br>
 * It can generate the following types of clauses:<br>
 * - disjunctions <br>
 * - Xor clauses (exactly one of the literals must be true)<br>
 * - disjointness clauses (at most one of the literals may be true<br>
 * - equivalence clauses (either all of its literals are true or all are false) <br>
 */
public final class RandomClauseSetGenerator {

    private static HashSet<String> keys = new HashSet<>();
    static { // these are the allowed keys in the specification.
        for(String key : new String[]{"problem","type", "seed","predicates","disjunctions","cpRatio","length","precise",
                "dBlocks","dLength","xBlocks", "xLength","eBlocks","eLength"}) {
            keys.add(key);}}

    /** The method translates the string-valued parameters in the HashMap to objects for controlling the generator.
     * The allowed parameters are: <br>
     * predicates: an integer &gt; 0, specifies the number of predicates in the clause set.<br>
     * disjunctions:    an integer &gt; 0, specifies the number of normal disjunctions to be generated.<br>
     * cpRatio:    a float &gt; 0, specifies the clause/predicate ratio.<br>
     *             cpRatio = 4.3 means: for 100 predicates 430 disjunctions.<br>
     * length:     an integer &gt; 0, specifies the maximum number of literals per clause.<br>
     * precise:    a boolean, if true then the disjunctions have exactly the specified length (default true).<br>
     * seed:       an integer &ge; 0 for starting the random number generator (default 0).<br>
     * dBlocks (optional): an integer &gt; 0, the number of disjointness blocks.<br>
     * dLength (optional): an integer &gt;  0, the number of predicates in each disjointness block.<br>
     * xBlocks (optional): an integer &gt;  0, the number of xors blocks.<br>
     * xLength (optional): an integer &gt;  0, the number of predicates in each xors block.<br>
     * eBlocks (optional): an integer &gt;  0, the number of equivalence blocks.<br>
     * eLength (optional): an integer &gt;  0, the number of predicates in each equivalence block.<br>
     *
     * The integer values can be specified as \'ranges\', with the following syntactic possibilities:<br>
     *   List:       3,6,7\n<br>
     *   Range:      3 to 10\n<br>
     *   With steps: 3 to 10 step 2\n<br>
     * Float values can be specified;\n<br>
     *   List:       4.6,7.8<br>
     *  With steps: 3.5 to 5.6 step 0.1<br>
     * Boolean values are for example \'true\', \'false\' of both \'true,false\'.<br>
     *
     *
     * @param parameters the input parameters
     * @param errors    for reporting syntax errors
     * @param warnings  for reporting warnings
     * @return an ArrayList of HashMaps with the translated parameters.
     */
    public static ArrayList<HashMap<String,Object>> parseParameters(HashMap<String,String> parameters, StringBuffer errors, StringBuffer warnings){
        for(String key : parameters.keySet()) {
            if(!keys.contains(key)) {warnings.append("RandomClauseSetGenerator: unknown key in parameters: " + key + "\n");}}

        String seed      = parameters.get("seed");
        String predicate = parameters.get("predicates");
        String clause    = parameters.get("disjunctions");
        String cpRatio   = parameters.get("cpRatio");
        String length    = parameters.get("length");
        String precise   = parameters.get("precise");
        String dBlocks   = parameters.get("dBlocks");
        String dLength   = parameters.get("dLength");
        String xBlocks   = parameters.get("xBlocks");
        String xLength   = parameters.get("xLength");
        String eBlocks   = parameters.get("eBlocks");
        String eLength   = parameters.get("eLength");
        if(dLength != null && dBlocks == null) {dBlocks = "1";}
        if(xLength != null && xBlocks == null) {xBlocks = "1";}
        if(eLength != null && eBlocks == null) {eBlocks = "1";}

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
        if(clause == null && cpRatios == null) {errors.append("RandomClauseSetGenerator: no number of disjunctions defined.\n");}
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

        ArrayList xBlockss = null;
        if(xBlocks != null) { xBlockss = Utilities.parseIntRange("RandomClauseSetGenerator xBlocks",xBlocks,errors);}

        ArrayList xLengths = null;
        if(xLength != null) {
            xLengths = Utilities.parseIntRange("RandomClauseSetGenerator xLength",xLength,errors);}

        ArrayList eBlockss = null;
        if(eBlocks != null) { eBlockss = Utilities.parseIntRange("RandomClauseSetGenerator eBlocks",eBlocks,errors);}

        ArrayList eLengths = null;
        if(eLength != null) {
            eLengths = Utilities.parseIntRange("RandomClauseSetGenerator eLength",eLength,errors);}


        if(seeds == null || predicates == null || (clauses == null && cpRatio == null) || lengths == null || precises == null) {return null;}

        ArrayList<HashMap<String,Object>> control = new ArrayList<>();
        if(cpRatios == null) {
            ArrayList<ArrayList> list = Utilities.crossProduct(seeds,predicates,clauses,lengths, precises,
                    dBlockss,dLengths,xBlockss,xLengths,eBlockss,eLengths);
            for(ArrayList values : list) {
                Integer pred = (Integer)values.get(1);
                Integer dbl = (Integer)values.get(5);
                Integer dle = (Integer)values.get(6);
                Integer xbl = (Integer)values.get(7);
                Integer xle = (Integer)values.get(8);
                Integer ebl = (Integer)values.get(9);
                Integer ele = (Integer)values.get(10);
                if(dbl != null && dle != null && dbl * dle > pred) {
                    errors.append("RandomClauseSetGenerator: number of disjoint predicates exceeds number of predicates: " +
                            dbl + "*" + dle + " > " + pred +".\n ");
                    continue;}
                if(xbl != null && xle != null && xbl * xle > pred) {
                    errors.append("RandomClauseSetGenerator: number of xors predicates exceeds number of predicates: " +
                            xbl + "*" + xle + " > " + pred +".\n ");
                    continue;}
                if(ebl != null && ele != null && ebl * ele > pred) {
                    errors.append("RandomClauseSetGenerator: number of equivalence predicates exceeds number of predicates: " +
                            ebl + "*" + ele + " > " + pred +".\n ");
                    continue;}

                if(pred < (Integer)values.get(3)){
                    errors.append("RandomClauseSetGenerator: there can't be less predicates than literals in a clause:\n").
                            append("predicates: " + predicates + " clause length: " + values.get(3)+"\n");}

                HashMap<String,Object> cntr = new HashMap<>();
                cntr.put("seed",values.get(0));
                cntr.put("predicates",pred);
                cntr.put("disjunctions",  values.get(2));
                cntr.put("length",   values.get(3));
                cntr.put("precise",  values.get(4));
                cntr.put("dBlocks",  dbl);
                cntr.put("dLengths", dle);
                cntr.put("xBlocks",  xbl);
                cntr.put("xLengths", xle);
                cntr.put("eBlocks",  ebl);
                cntr.put("eLengths", ele);
                cntr.put("name","RD"+values.get(0)+values.get(1)+values.get(1));
                control.add(cntr);}
            return control;}

        ArrayList<ArrayList> list = Utilities.crossProduct(seeds,predicates,cpRatios,lengths, precises,
                dBlockss,dLengths,xBlockss,xLengths,eBlockss,eLengths);
        for(ArrayList values : list) {
            if((Integer)values.get(1) < (Integer)values.get(3)){
                errors.append("RandomClauseSetGenerator: there can't be less predicates than literals in a clause:\n").
                        append("predicates: " + values.get(1) + " clause length: " + values.get(3)+"\n");}
            HashMap<String,Object> cntr = new HashMap<>();
            int nClauses = Math.round((Integer)values.get(1)*(Float)values.get(2));
            cntr.put("seed",values.get(0));
            cntr.put("predicates",values.get(1));
            cntr.put("disjunctions",nClauses);
            cntr.put("length",   values.get(3));
            cntr.put("precise",  values.get(4));
            cntr.put("dBlocks",  values.get(5));
            cntr.put("dLengths", values.get(6));
            cntr.put("xBlocks",  values.get(7));
            cntr.put("xLengths", values.get(8));
            cntr.put("eBlocks",  values.get(9));
            cntr.put("eLengths", values.get(10));
            cntr.put("name","RD"+values.get(0)+values.get(1)+nClauses);
            control.add(cntr);}
        return control;}


    /** yields a help string.
     *
     * @return a help string.
     */
    public static String help() {
        StringBuilder st = new StringBuilder();
        st.append("Random Clause Set Generator\n");
        st.append("It can generate normal propositional disjunctions as well as \"disjointness disjunctions\"\n");
        st.append("A disjointenss clause \'p,q,r\' means that at most one of the literals can be true in a model.\n");
        st.append("The parameters are:\n");
        st.append("predicates: an integer > 0, specifies the number of predicates in the clause set.\n");
        st.append("disjunctions:    an integer > 0, specifies the number of normal disjunctions to be generated.\n");
        st.append("cpRatio:    a float > 0, specifies the clause/predicate ratio.\n");
        st.append("            cpRatio = 4.3 means: for 100 predicates 430 disjunctions.\n");
        st.append("length:     an integer > 0, specifies the maximum number of literals per clause.\n");
        st.append("precise:    a boolean, if true then the disjunctions have exactly the specified length (default true).\n");
        st.append("seed:       an integer >= 0 for starting the random number generator (default 0).\n");
        st.append("dBlocks (optional): an integer > 0, the number of disjointness blocks.\n" );
        st.append("dLength (optional): an integer > 0, the number of predicates in each disjointness block.\n");
        st.append("xBlocks (optional): an integer > 0, the number of xors blocks.\n" );
        st.append("xLength (optional): an integer > 0, the number of predicates in each xors block.\n");
        st.append("eBlocks (optional): an integer > 0, the number of equivalence blocks.\n" );
        st.append("eLength (optional): an integer > 0, the number of predicates in each equivalence block.\n");
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
     * @param errors for error messages
     * @param warnings for warnings
     * @return  the generated BasicClauseList
     */
    public static BasicClauseList generate(HashMap<String,Object> parameters, StringBuffer errors, StringBuffer warnings) {
        int seed            = (Integer)parameters.get("seed");
        int predicates      = (Integer)parameters.get("predicates");
        int numberClauses   = (Integer)parameters.get("disjunctions");
        int maxClauseLength = (Integer)parameters.get("length");
        boolean precise     = (Boolean)parameters.get("precise");
        Integer dBlocks     = (Integer)parameters.get("dBlocks");
        Integer dLength     = (Integer)parameters.get("dLengths");
        Integer xBlocks     = (Integer)parameters.get("xBlocks");
        Integer xLength     = (Integer)parameters.get("xLengths");
        Integer eBlocks     = (Integer)parameters.get("eBlocks");
        Integer eLength     = (Integer)parameters.get("eLengths");

        if(predicates < maxClauseLength) {
            errors.append("RandomClauseSetGenerator: More literals in a clause than predicates.\n").
                    append("predicates: " + predicates + " literals " + maxClauseLength +"\n");
            return null;}

        BasicClauseList clauseList = new BasicClauseList();
        clauseList.predicates = predicates;

        ArrayList<Integer> literals = new ArrayList();
        Status stat = new Status();
        stat.seed = seed;
        clauseList.info = "Randomly generated clauses with seed " + seed;
        Random rnd = new Random(seed);
        int clauseCounter = 0;
        while(clauseList.disjunctions.size() < numberClauses) {
            literals.clear();
            int clauseLength = precise ? maxClauseLength : rnd.nextInt(maxClauseLength)+1;
            while(literals.size() < clauseLength) {
                int literal = rnd.nextInt(predicates)+1;
                if(rnd.nextBoolean()) {literal = -literal;}
                if(literals.contains(-literal)) {literals.clear(); continue;}
                if(literals.contains(literal)) {continue;}
                literals.add(literal);}

            int[] lits = new int[literals.size()+2];
            lits[0] = ++clauseCounter;
            lits[1] = ClauseType.OR.ordinal();
            for(int i = 0; i < literals.size(); ++i) {lits[i+2] = literals.get(i);}
            clauseList.addClause(lits);}

        if(dBlocks != null) {
            clauseCounter = addClauses(predicates,ClauseType.DISJOINT,     clauseList,clauseCounter,dBlocks,dLength,rnd);}
        if(xBlocks != null) {
            clauseCounter = addClauses(predicates,ClauseType.XOR,clauseList,clauseCounter,xBlocks,xLength,rnd);}
        if(eBlocks != null) {
            clauseCounter = addClauses(predicates,ClauseType.EQUIV,   clauseList,clauseCounter,eBlocks,eLength,rnd);}

        return clauseList;}

    private static int addClauses(int predicates, ClauseType type, BasicClauseList clauseList,
                                  int clauseCounter, int blocks, int length, Random rnd) {
        HashSet<Integer> preds = new HashSet();
        ArrayList<Integer> literals = new ArrayList<>();
        for(int block = 0; block < blocks; ++block) {
            int[] lits = new int[length+2];
            lits[0]= ++clauseCounter; lits[1]=type.ordinal();
            for(int i = 0; i < length; ++i) {
                int literal = rnd.nextInt(predicates)+1;
                if(preds.contains(literal)) {--i;continue;}
                preds.add(literal);
                if(rnd.nextBoolean()) {literal = -literal;}
                lits[i+2] = literal;}
            clauseList.addClause(lits);}
        return clauseCounter;
    }}



