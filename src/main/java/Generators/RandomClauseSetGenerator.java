package Generators;

import Datastructures.Clauses.BasicClauseList;
import Datastructures.Clauses.ClauseType;
import Management.ProblemSupervisor;
import Utilities.Utilities;

import java.util.*;

/**
 * Created by ohlbach on 26.08.2018.
 *
 * This generator generates clause sets with randomly generated literals.<br>
 * It can generate the following types of clauses:<br>
 * - OR      (disjunctions) <br>
 * - AND     (conjunctions) <br>
 * - EQUIV   (equivalences) <br>
 * - ATLEAST (numeric: atleast 2, p,q,r: atleast two of them are true)<br>
 * - ATMOST  (numeric: atmost 2, p,q,r: atmost two of them are true)<br>
 * - EXACTLY (numeric: exactly 2, p,q,r: exactly two of them are true)
 */
public final class RandomClauseSetGenerator {

    protected static final HashSet<String> keys = new HashSet<>();
    static { // these are the allowed keys in the specification.
        Collections.addAll(keys,"seeds", "predicates", "cpRatios", "lengths", "precises",
                "ors", "ands", "equivs", "atleasts", "atmosts", "exactlys");
    }

    /** yields a help string.
     *
     * @return a help string.
     */
    public static String help() {
        return "Random Clause Set Generator\n" +
                "It can generate clauses of the following types:\n" +
                "OR      (disjunctions)\n" +
                "AND     (conjunctions)\n" +
                "EQUIV   (equivalences)\n" +
                "ATLEAST (atleast 2 p,q,r means atleast two of p,q,r must be true)\n" +
                "ATMOST  (atmost  2 p,q,r means atmost two of p,q,r must be true)\n" +
                "EXACTLY (exactly 2 p,q,r means exactly two of p,q,r must be true)\n\n" +
                "The parameters are:\n" +
                "predicates: an integer > 0, specifies the number of predicates in the clause set.\n" +
                "lengths:    an integer > 0, specifies the maximum number of literals per clause.\n" +
                "precises:   a boolean, if true then the clauses have exactly the specified length (default true).\n" +
                "seeds:      an integer >= 0 for starting the random number generator (default 0).\n" +
                "\n" +
                "ors:        an integer >= 0, specifies the number of disjunctions to be generated.\n" +
                "ands:       an integer >= 0, specifies the number of conjunctions to be generated.\n" +
                "equivs:     an integer >= 0, specifies the number of equivalences to be generated.\n" +
                "atleasts:   an integer >= 0, specifies the number of atleast clauses to be generated.\n" +
                "atmosts:    an integer >= 0, specifies the number of atmost clauses  to be generated.\n" +
                "exactlys:   an integer >= 0, specifies the number of exactly clauses to be generated.\n" +
                "\n" +
                "cpRatio:    a float > 0, specifies the clause/predicate ratio.\n" +
                "            cpRatio = 4.3 means: for 100 predicates 430 disjunctions.\n" +
                "if cpRatio is specified then only disjunctions are generated. The other values are ignored.\n" +
                "\n" +
                "The integer values can be specified as \'ranges\', with the following syntactic possibilities:\n" +
                "  List:       3,6,7\n" +
                "  Range:      3 to 10\n" +
                "  With steps: 3 to 10 step 2\n" +
                "Float values can be specified;\n" +
                "  List:       4.6,7.8\n" +
                "  With steps: 3.5 to 5.6 step 0.1\n" +
                "Boolean values are for example \'true\', \'false\' of both \'true,false\'.\n" +
                "\n" +
                "The specification of ranges causes the generation of a sequence of clause lists.\n";}



    /** The method translates the string-valued parameters in the HashMap to objects for controlling the generator.
     * The allowed parameters are: <br>
     * predicates: an integer > 0, specifies the number of predicates in the clause set.<br>
     * lengths:    an integer > 0, specifies the maximum number of literals per clause.<br>
     * precises:   a boolean, if true then the clauses have exactly the specified length (default true).<br>
     * seeds:      an integer >= 0 for starting the random number generator (default 0).<br>
     * <br>
     * ors:        an integer >= 0, specifies the number of disjunctions to be generated.<br>
     * ands:       an integer >= 0, specifies the number of conjunctions to be generated.<br>
     * equivs:     an integer >= 0, specifies the number of equivalences to be generated.<br>
     * atleasts:   an integer >= 0, specifies the number of atleast clauses to be generated.<br>
     * atmosts:    an integer >= 0, specifies the number of atmost clauses  to be generated.<br>
     * exactlys:   an integer >= 0, specifies the number of exactly clauses to be generated.<br>
     *<br>
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
    public static ArrayList<HashMap<String,Object>> parseParameters(HashMap<String,String> parameters, StringBuilder errors, StringBuilder warnings){
        String prefix = "RandomClauseSetGenerator: ";
        boolean erraneous = false;
        for(String key : parameters.keySet()) {
            if(key.equals("type") || key.equals("problem")) continue;
            if(!keys.contains(key)) {
                warnings.append(prefix+"unknown key in parameters: " + key + "\n");
                warnings.append("  The allowed keys are\n  " + keys.toString() + "\n");}}

        String seed      = parameters.get("seeds");
        String predicate = parameters.get("predicates");
        String cpRatio   = parameters.get("cpRatios");
        String length    = parameters.get("lengths");
        String precise   = parameters.get("precises");
        String ors       = parameters.get("ors");
        String ands      = parameters.get("ands");
        String equivs    = parameters.get("equivs");
        String atleasts  = parameters.get("atleasts");
        String atmosts   = parameters.get("atmosts");
        String exactlys  = parameters.get("exactlys");

        ArrayList predicates = null;
        if(predicate == null) {errors.append(prefix+"no number of predicates defined.");}
        else {predicates = Utilities.parseIntRange(prefix+"predicate",predicate,errors);}
        if(predicates == null) {erraneous = true; errors.append("\n");}

        ArrayList lengths = null;
        if(length == null) {errors.append(prefix+"no clause length defined.");}
        else {lengths = Utilities.parseIntRange(prefix+"length: ",length,errors);}
        if(lengths == null) {erraneous = true; errors.append("\n");}

        ArrayList seeds = null;
        if(seed == null) {seeds = new ArrayList<>(); seeds.add(0);}
        else {seeds = Utilities.parseIntRange(prefix+"seed: ",seed,errors);
             if(seeds == null) {
                 errors.append("\n");
                 seeds = new ArrayList<>();
                 seeds.add(0);
                 warnings.append(prefix + "assuming seeds = 0");}}

        ArrayList cpRatios = null;
        if(cpRatio != null) {
            cpRatios = Utilities.parseFloatRange(prefix+"cpRatio: ",cpRatio,errors);
            if(cpRatios == null) {errors.append("\n"); erraneous = true;}}

        ArrayList precises = null;
        if(precise == null) {precises = new ArrayList(); precises.add(true);}
        else {precises = Utilities.parseBoolean(prefix+"precise: ", precise,errors);
             if(precises == null) {errors.append("\n"); erraneous = true;}}

        ArrayList orss = null;
        if(ors != null) {
            orss = Utilities.parseIntRange(prefix+"ors",ors,errors);
            if(orss == null) {errors.append("\n"); erraneous = true;}}

        ArrayList andss = null;
        if(ands != null) {
            andss = Utilities.parseIntRange(prefix+"ands: ",ands,errors);
            if(andss == null) {errors.append("\n"); erraneous = true;}}

        ArrayList equivss = null;
        if(equivs != null) {
            equivss = Utilities.parseIntRange(prefix+"equivs: ",equivs,errors);
            if(equivss == null) {errors.append("\n"); erraneous = true;}}

        ArrayList atleastss = null;
        if(atleasts != null) {
            atleastss = Utilities.parseIntRange(prefix+"atleasts: ",atleasts,errors);
            if(atleastss == null) {errors.append("\n"); erraneous = true;}}

        ArrayList atmostss = null;
        if(atmosts != null) {
            atmostss = Utilities.parseIntRange(prefix+"atmosts: ",atmosts,errors);
            if(atmostss == null) {errors.append("\n"); erraneous = true;}}

        ArrayList exactlyss = null;
        if(exactlys != null) {
            exactlyss = Utilities.parseIntRange(prefix+"exactlys: ",exactlys,errors);
            if(exactlyss == null) {errors.append("\n"); erraneous = true;}}

        if(erraneous) return null;
        ArrayList<HashMap<String,Object>> control = new ArrayList<>();
        if(cpRatios == null) {
            ArrayList<ArrayList<Object>> list = Utilities.crossProduct(seeds,predicates,lengths, precises,
                    orss,andss,equivss,atleastss,atmostss,exactlyss);

            for(ArrayList<Object> values : list) {
                Integer seedv       = (Integer)values.get(0);
                Integer predicatesv = (Integer)values.get(1);
                Integer lengthv     = (Integer)values.get(2);
                Boolean precisev    = (Boolean)values.get(3);
                Integer orv         = (Integer)values.get(4);
                Integer andv        = (Integer)values.get(5);
                Integer equivv      = (Integer)values.get(6);
                Integer atleastv    = (Integer)values.get(7);
                Integer atmostv     = (Integer)values.get(8);
                Integer exactlyv    = (Integer)values.get(9);

                if(seedv < 0) {
                    errors.append(prefix+"Negative seed specified: " + seedv + "\n");
                    erraneous = true;}
                if(lengthv <= 0) {
                    errors.append(prefix+"No positive clause length specified: " + lengthv + "\n");
                    erraneous = true;}
                if(predicatesv <= 0) {
                    errors.append(prefix+"Wrong number of predicates specified: " + predicatesv + "\n");
                    erraneous = true;}
                else {if(lengthv > predicatesv) {
                        errors.append(prefix+"Clause Length : " + lengthv + "> number of predicates " + predicatesv + "\n");
                        erraneous = true;}}
                if(orv == null && andv == null && equivv == null &&
                        atleastv == null && atmostv == null && exactlyv == null) {
                    errors.append(prefix+"No number of clauses specified\n");
                    erraneous = true;}
                if(orv != null && orv < 0) {
                    errors.append(prefix+"negative number of ors specified: " + orv + "\n");
                    erraneous = true;}
                if(andv != null && andv < 0) {
                    errors.append(prefix+"negative number of ands specified: " + andv + "\n");
                    erraneous = true;}
                if(equivv != null && equivv < 0) {
                    errors.append(prefix+"negative number of equivs specified: " + equivv + "\n");
                    erraneous = true;}
                if(atleastv != null && atleastv < 0) {
                    errors.append(prefix+"negative number of atleasts specified: " + atleastv + "\n");
                    erraneous = true;}
                if(atmostv != null && atmostv < 0) {
                    errors.append(prefix+"negative number of atmosts specified: " + atmostv + "\n");
                    erraneous = true;}
                if(exactlyv != null && exactlyv < 0) {
                    errors.append(prefix+"negative number of exactlys specified: " + exactlyv + "\n");
                    erraneous = true;}

                if(erraneous) return null;

                HashMap<String,Object> cntr = new HashMap<>();
                cntr.put("seed",seedv);
                cntr.put("predicates",predicatesv);
                cntr.put("length",   lengthv);
                cntr.put("precise",  precisev);
                if(orv != null)       cntr.put("ors", orv);
                if(andv != null)      cntr.put("ands", andv);
                if(equivv != null)    cntr.put("equivs", equivv);
                if(atleastv != null)  cntr.put("atleasts", atleastv);
                if(atmostv != null)   cntr.put("atmosts", atmostv);
                if(exactlyv != null)  cntr.put("exactlys", exactlyv);

                cntr.put("name","RD"+seedv+predicatesv+lengthv);
                control.add(cntr);}
            return control;}

        // with cpRation > 0
        ArrayList<ArrayList<Object>> list = Utilities.crossProduct(seeds,predicates,lengths, precises,cpRatios);
        for(ArrayList<Object> values : list) {
            Integer seedv = (Integer) values.get(0);
            if (seedv == null) seedv = 0;
            Integer predicatesv = (Integer) values.get(1);
            Integer lengthv = (Integer) values.get(2);
            Boolean precisev = (Boolean) values.get(3);
            if (precisev == null) precisev = true;
            Float cpRatiov = (Float)values.get(4);
            if(seedv < 0) {
                errors.append(prefix+"Negative seed specified: " + seedv + "\n");
                erraneous = true;}
            if(lengthv == null) {
                errors.append(prefix+"No clause length specified\n");
                erraneous = true;}
            else {
                if(lengthv <= 0) {
                    errors.append(prefix+"No positive clause length specified " + lengthv + "\n");}}
            if(predicatesv <= 0) {
                errors.append(prefix+"Wrong number of predicates specified: " + predicatesv + "\n");
                erraneous = true;}
            else {if(lengthv != null &&  lengthv > predicatesv) {
                errors.append(prefix+"Clause Length : " + lengthv + "> number of predicates " + predicatesv + "\n");
                erraneous = true;}}
            if(cpRatiov < 0) {
                errors.append(prefix+"Negative cpRatio: " + cpRatio + "\n");
                erraneous = true;}
            if(erraneous) return null;

            HashMap<String,Object> cntr = new HashMap<>();
            cntr.put("seed",seedv);
            cntr.put("predicates",predicatesv);
            cntr.put("length",   lengthv);
            cntr.put("precise",  precisev);
            cntr.put("ors", Math.round(predicatesv*cpRatiov));
            cntr.put("name","RD"+seedv+predicatesv+lengthv);
            control.add(cntr);}
        return control;
        }


    /** generates the clause set
     *
     * @param parameters for controlling the generator.
     * @param errors for error messages
     * @param warnings for warnings
     * @return  the generated BasicClauseList
     */
    public static BasicClauseList generate(HashMap<String,Object> parameters,
                                           ProblemSupervisor problemSupervisor,
                                           StringBuilder errors, StringBuilder warnings) {
        int seed            = (Integer)parameters.get("seed");
        int predicates      = (Integer)parameters.get("predicates");
        int maxClauseLength = (Integer)parameters.get("length");
        boolean precise     = (Boolean)parameters.get("precise");
        Integer ors         = (Integer)parameters.get("ors");
        Integer ands        = (Integer)parameters.get("ands");
        Integer equivs      = (Integer)parameters.get("equivs");
        Integer atleasts    = (Integer)parameters.get("atleasts");
        Integer atmosts     = (Integer)parameters.get("atmosts");
        Integer exactlys    = (Integer)parameters.get("exactlys");

        String prefix = "RandomClauseSetGenerator: ";
        BasicClauseList clauseList = new BasicClauseList();
        clauseList.predicates = predicates;
        Random rnd = new Random(seed);
        if(ors != null)
            generateClauses(problemSupervisor,clauseList,predicates,0,ors,maxClauseLength,precise,rnd,
                    prefix,errors,warnings);
        if(ands != null)
            generateClauses(problemSupervisor,clauseList,predicates,1,ands,maxClauseLength,precise,rnd,
                    prefix,errors,warnings);
        if(equivs != null)
            generateClauses(problemSupervisor,clauseList,predicates,2,equivs,maxClauseLength,precise,rnd,
                    prefix,errors,warnings);
        if(atleasts != null)
            generateClauses(problemSupervisor,clauseList,predicates,3,atleasts,maxClauseLength,precise,rnd,
                    prefix,errors,warnings);
        if(atmosts != null)
            generateClauses(problemSupervisor,clauseList,predicates,4,atmosts,maxClauseLength,precise,rnd,
                    prefix,errors,warnings);
        if(exactlys != null)
            generateClauses(problemSupervisor,clauseList,predicates,5,exactlys,maxClauseLength,precise,rnd,
                    prefix,errors,warnings);
        return clauseList;}



    /** generates random clauses of the give type
     * Double literals and complementary literals are avoided.
     *
     * @param problemSupervisor   for generating next clause id
     * @param clauseList          for inserting the clause
     * @param predicates          largest predicate number
     * @param typeNumber          0 (OR), 1 (AND), 2 (EQUIV), 3 (ATLEAST), 4 (ATMOST) 5 (EXACTLY)
     * @param numberOfClauses     number of clauses to be generated
     * @param maxClauseLength     largest clause length
     * @param preciseClauseLength if true then clause will be exactly this length
     * @param rnd                 random number generator
     * @param errorPrefix         for error messages (should never be used)
     * @param errors              for errors (should never be used)
     * @param warnings            for warnings (should never be used)
     */
    protected static void generateClauses(ProblemSupervisor problemSupervisor, BasicClauseList clauseList,
                                           int predicates, int typeNumber,
                                           int numberOfClauses, int maxClauseLength, boolean preciseClauseLength,
                                           Random rnd, String errorPrefix, StringBuilder errors, StringBuilder warnings) {
        boolean numeric = ClauseType.isNumeric(typeNumber);
        int start = numeric ? 3 : 2;
        int counter = -1;
        while(++counter < numberOfClauses) {
            int clauseLength = preciseClauseLength ? maxClauseLength : rnd.nextInt(maxClauseLength)+1;
            if(ClauseType.EQUIV.ordinal() == typeNumber && clauseLength == 1) {++clauseLength;}
            int[] clause = new int[clauseLength+start];
            clause[0] = problemSupervisor.nextClauseId();
            clause[1] = typeNumber;
            if(numeric) {clause[2] = rnd.nextInt(clauseLength)+1;}

            for(int i = start; i < clauseLength+start; ++i) {
                int sign = rnd.nextBoolean() ? +1 : -1;
                int literal = sign*(rnd.nextInt(predicates)+1);
                boolean found = false;
                for(int j = start; j < i; ++j) {if(literal == clause[j] || -literal == clause[j]) {found = true; break;}}
                if(found) {--i; continue;}
                clause[i] = literal;}
            clauseList.addClause(clause,errorPrefix,errors,warnings);}
    }
}


