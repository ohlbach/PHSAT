package ProblemGenerators;

import Datastructures.Clauses.Connective;
import Datastructures.Clauses.InputClauses;
import Management.Monitor.Monitor;
import Utilities.Utilities;

import java.util.*;

/**
 * Created by ohlbach on 26.08.2018.
 * This generator generates clause sets with randomly generated literals.<br>
 * It can generate the following types of clauses:<br>
 * - OR      (disjunctions). <br>
 * - AND     (conjunctions). <br>
 * - EQUIV   (equivalences). <br>
 * - ATLEAST (numeric: atleast 2, p,q,r: atleast two of them are true).<br>
 * - ATMOST  (numeric: atmost 2, p,q,r: atmost two of them are true).<br>
 * - EXACTLY (numeric: exactly 2, p,q,r: exactly two of them are true).<br>
 * - INTERVAL (numeric: [2,4] p,q,r,s: between 2 and 4 of them are true).
 */
public class RandomClauseSetGenerator extends ProblemGenerator {

    protected static final HashSet<String> keys = new HashSet<>();
    static { // these are the allowed keys in the specification.
        Collections.addAll(keys,"generator", "seeds", "predicates", "cpRatios", "lengths", "precises",
                "redundant", "ors", "ands", "equivs", "atleasts", "atmosts", "exactlys", "intervals");
    }

    private final int seed;
    private final int predicates;
    private final int length;
    private final boolean precise;
    private final boolean redundant;
    private final int ors;
    private final int ands;
    private final int equivs;
    private final int atleasts;
    private final int atmosts;
    private final int exactly;
    private final int intervals;

    public RandomClauseSetGenerator(int seed, int predicates, int length, boolean precise, boolean redundant,
                                    int ors, int ands, int equivs, int  atleasts, int atmosts, int exactly,
                                    int intervals) {
        this.seed       = seed;
        this.predicates = predicates;
        this.length     = length;
        this.precise    = precise;
        this.redundant  = redundant;
        this.ors        = ors;
        this.ands       = ands;
        this.equivs     = equivs;
        this.atleasts   = atleasts;
        this.atmosts    = atmosts;
        this.exactly    = exactly;
        this.intervals  = intervals;
    }



    /** yields a help string.
     *
     * @return a help string.
     */
    public static String help() {
        return "Random Clause Set Generator\n" +
                "It can generate clauses of the following types:\n" +
                "OR       (disjunctions)\n" +
                "AND      (conjunctions)\n" +
                "EQUIV    (equivalences)\n" +
                "ATLEAST  (atleast 2 p,q,r means atleast two of p,q,r must be true)\n" +
                "ATMOST   (atmost  2 p,q,r means atmost two of p,q,r must be true)\n" +
                "EXACTLY  (exactly 2 p,q,r means exactly two of p,q,r must be true)\n" +
                "INTERVAL ([2,4] p,q,r,s: between 2 and 4 of them are true)\n\n"+
                "The parameters are:\n" +
                "predicates: an integer > 0, specifies the number of predicates in the clause set.\n" +
                "length:     an integer > 0, specifies the maximum number of literals per clause.\n" +
                "precise:    a boolean, if true then the clauses have exactly the specified length (default true).\n" +
                "redundant:  a boolean, if false then tautologies and doubble literals are avoided (default false).\n"+
                "seed:       an integer >= 0 for starting the random number generator (default 0).\n" +
                "\n" +
                "ors:        an integer >= 0, specifies the number of disjunctions to be generated.\n" +
                "ands:       an integer >= 0, specifies the number of conjunctions to be generated.\n" +
                "equivs:     an integer >= 0, specifies the number of equivalences to be generated.\n" +
                "atleasts:   an integer >= 0, specifies the number of atleast clauses to be generated.\n" +
                "atmosts:    an integer >= 0, specifies the number of atmost clauses  to be generated.\n" +
                "exactlys:   an integer >= 0, specifies the number of exactly clauses to be generated.\n" +
                "intervals:  an integer >= 0, specifies the number of interval clauses to be generated.\n" +
                "\n" +
                "cpRatio:    a float > 0, specifies the clause/predicate ratio.\n" +
                "            cpRatio = 4.3 means: for 100 predicates 430 disjunctions.\n" +
                "if cpRatio is specified then only disjunctions are generated. The other values are ignored.\n" +
                "\n" +
                "The integer values can be specified as 'ranges', with the following syntactic possibilities:\n" +
                "  List:       3,6,7\n" +
                "  Range:      3 to 10\n" +
                "  With steps: 3 to 10 step 2\n" +
                "Float values can be specified;\n" +
                "  List:       4.6,7.8\n" +
                "  With steps: 3.5 to 5.6 step 0.1\n" +
                "Boolean values are for example 'true', 'false' of both 'true,false'.\n" +
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
     * intervals:  an integer >= 0, specifies the number of interval clauses to be generated.<br>
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
     */
    public static void makeProblemGenerator(HashMap<String,String> parameters,
                                            ArrayList<ProblemGenerator> generators,
                                            StringBuilder errors, StringBuilder warnings){
        String prefix = "RandomClauseSetGenerator: ";
        boolean erraneous = false;
        for(String key : parameters.keySet()) {
            if(!keys.contains(key)) {
                warnings.append(prefix).append("unknown key in parameters: ").append(key).append("\n");
                warnings.append("  The allowed keys are\n  ").append(keys).append("\n");}}

        String seedS      = parameters.get("seed");
        String predicateS = parameters.get("predicates");
        String cpRatioS   = parameters.get("cpRatio");
        String lengthS    = parameters.get("length");
        String preciseS   = parameters.get("precise");
        String redundantS  = parameters.get("redundant");
        String orsS       = parameters.get("ors");
        String andsS      = parameters.get("ands");
        String equivsS    = parameters.get("equivs");
        String atleastsS  = parameters.get("atleasts");
        String atmostsS   = parameters.get("atmosts");
        String exactlysS  = parameters.get("exactlys");
        String intervalsS = parameters.get("intervals");

        ArrayList predicatesA = null;
        if(predicateS == null) {
            errors.append(prefix).append("no number of predicates defined.");}
        else {predicatesA = Utilities.parseIntRange(prefix+"predicate",predicateS,errors);}
        if(predicatesA == null) {erraneous = true; errors.append("\n");}

        ArrayList lengthA = null;
        if(lengthS == null) {
            errors.append(prefix).append("no clause length defined.");}
        else {lengthA = Utilities.parseIntRange(prefix+"length: ",lengthS,errors);}
        if(lengthA == null) {erraneous = true; errors.append("\n");}

        ArrayList seedA = null;
        if(seedS == null) {seedA = new ArrayList<>(); seedA.add(0);}
        else {seedA = Utilities.parseIntRange(prefix+"seed: ",seedS,errors);
             if(seedA == null) {
                 errors.append("\n");
                 seedA = new ArrayList<>();
                 seedA.add(0);
                 warnings.append(prefix).append("assuming seeds = 0");}}

        ArrayList cpRatioA = null;
        if(cpRatioS != null) {
            cpRatioA = Utilities.parseFloatRange(prefix+"cpRatio: ",cpRatioS,errors);
            if(cpRatioA == null) {errors.append("\n"); erraneous = true;}}

        ArrayList preciseA = null;
        if(preciseS == null) {preciseA = new ArrayList(); preciseA.add(true);}
        else {preciseA = Utilities.parseBoolean(prefix+"precise: ", preciseS,errors);
             if(preciseA == null) {errors.append("\n"); erraneous = true;}}

        ArrayList redundantA = null;
        if(redundantS == null) {redundantA = new ArrayList(); redundantA.add(false);}
        else {redundantA = Utilities.parseBoolean(prefix+"redundant: ", redundantS,errors);
            if(redundantA == null) {errors.append("\n"); erraneous = true;}}

        ArrayList orsA = null;
        if(orsS != null) {
            orsA = Utilities.parseIntRange(prefix+"ors",orsS,errors);
            if(orsA == null) {errors.append("\n"); erraneous = true;}}

        ArrayList andsA = null;
        if(andsS != null) {
            andsA = Utilities.parseIntRange(prefix+"ands: ",andsS,errors);
            if(andsA == null) {errors.append("\n"); erraneous = true;}}

        ArrayList equivsA = null;
        if(equivsS != null) {
            equivsA = Utilities.parseIntRange(prefix+"equivs: ",equivsS,errors);
            if(equivsA == null) {errors.append("\n"); erraneous = true;}}

        ArrayList atleastsA = null;
        if(atleastsS != null) {
            atleastsA = Utilities.parseIntRange(prefix+"atleasts: ",atleastsS,errors);
            if(atleastsA == null) {errors.append("\n"); erraneous = true;}}

        ArrayList atmostsA = null;
        if(atmostsS != null) {
            atmostsA = Utilities.parseIntRange(prefix+"atmosts: ",atmostsS,errors);
            if(atmostsA == null) {errors.append("\n"); erraneous = true;}}

        ArrayList exactlysA = null;
        if(exactlysS != null) {
            exactlysA = Utilities.parseIntRange(prefix+"exactlys: ",exactlysS,errors);
            if(exactlysA == null) {errors.append("\n"); erraneous = true;}}

        ArrayList intervalsA = null;
        if(intervalsS != null) {
            intervalsA = Utilities.parseIntRange(prefix+"intervals: ",intervalsS,errors);
            if(intervalsA == null) {errors.append("\n"); erraneous = true;}}

        if(erraneous) return ;
        if(cpRatioS == null) {
            ArrayList<ArrayList<Object>> list = Utilities.crossProduct(seedA,predicatesA,lengthA, preciseA,redundantA,
                    cpRatioA, orsA,andsA,equivsA,atleastsA,atmostsA,exactlysA,intervalsA);

            for(ArrayList<Object> values : list) {
                Integer seedv       = (Integer)values.get(0);
                Integer predicatesv = (Integer)values.get(1);
                Integer lengthv     = (Integer)values.get(2);
                Boolean precisev    = (Boolean)values.get(3);
                Boolean redundantv    = (Boolean)values.get(4);
                Float   cpRatiov    = (Float)values.get(5);
                Integer orsv         = (Integer)values.get(6);
                Integer andsv        = (Integer)values.get(7);
                Integer equivsv      = (Integer)values.get(8);
                Integer atleastsv    = (Integer)values.get(9);
                Integer atmostsv     = (Integer)values.get(10);
                Integer exactlysv    = (Integer)values.get(11);
                Integer intervalsv   = (Integer)values.get(12);

                if(seedv < 0) {
                    errors.append(prefix).append("Negative seed specified: ").append(seedv).append("\n");
                    erraneous = true;}
                if(lengthv <= 0) {
                    errors.append(prefix).append("No positive clause length specified: ").append(lengthv).append("\n");
                    erraneous = true;}
                if(predicatesv <= 0) {
                    errors.append(prefix).append("Wrong number of predicates specified: ").append(predicatesv).append("\n");
                    erraneous = true;}
                else {if(lengthv > predicatesv) {
                        errors.append(prefix).append("Clause Length : ").append(lengthv).append("> number of predicates ").append(predicatesv).append("\n");
                        erraneous = true;}}
                if(cpRatiov == null && orsv == null && andsv == null && equivsv == null &&
                        atleastsv == null && atmostsv == null && exactlysv == null) {
                    errors.append(prefix).append("No number of clauses specified\n");
                    erraneous = true;}
                if(orsv != null && orsv < 0) {
                    errors.append(prefix).append("negative number of ors specified: ").append(orsv).append("\n");
                    erraneous = true;}
                if(andsv != null && andsv < 0) {
                    errors.append(prefix).append("negative number of ands specified: ").append(andsv).append("\n");
                    erraneous = true;}
                if(equivsv != null && equivsv < 0) {
                    errors.append(prefix).append("negative number of equivs specified: ").append(equivsv).append("\n");
                    erraneous = true;}
                if(atleastsv != null && atleastsv < 0) {
                    errors.append(prefix).append("negative number of atleasts specified: ").append(atleastsv).append("\n");
                    erraneous = true;}
                if(atmostsv != null && atmostsv < 0) {
                    errors.append(prefix).append("negative number of atmosts specified: ").append(atmostsv).append("\n");
                    erraneous = true;}
                if(exactlysv != null && exactlysv < 0) {
                    errors.append(prefix).append("negative number of exactlys specified: ").append(exactlysv).append("\n");
                    erraneous = true;}
                if(intervalsv != null && intervalsv < 0) {
                    errors.append(prefix).append("negative number of intervals specified: ").append(intervalsv).append("\n");
                    erraneous = true;}

                if(erraneous) return;
                generators.add(new RandomClauseSetGenerator(seedv,predicatesv,lengthv,precisev,redundantv,orsv,
                        andsv,equivsv,atleastsv,atmostsv,exactlysv,intervalsv));}
            return;}

        // with cpRation > 0
        ArrayList<ArrayList<Object>> list = Utilities.crossProduct(seedA,predicatesA,lengthA, preciseA,redundantA,cpRatioA);
        for(ArrayList<Object> values : list) {
            Integer seedv = (Integer) values.get(0);
            if (seedv == null) seedv = 0;
            Integer predicatesv = (Integer) values.get(1);
            Integer lengthv = (Integer) values.get(2);
            Boolean precisev = (Boolean) values.get(3);
            Boolean redundantv = (Boolean)values.get(4);
            if (precisev == null) precisev = true;
            if(redundantv = null) redundantv = false;
            Float cpRatiov = (Float)values.get(4);
            if(seedv < 0) {
                errors.append(prefix).append("Negative seed specified: ").append(seedv).append("\n");
                erraneous = true;}
            if(lengthv == null) {
                errors.append(prefix).append("No clause length specified\n");
                erraneous = true;}
            else {
                if(lengthv <= 0) {
                    errors.append(prefix).append("No positive clause length specified ").append(lengthv).append("\n");}}
            if(predicatesv <= 0) {
                errors.append(prefix).append("Wrong number of predicates specified: ").append(predicatesv).append("\n");
                erraneous = true;}
            else {if(lengthv != null &&  lengthv > predicatesv) {
                errors.append(prefix).append("Clause Length : ").append(lengthv).append("> number of predicates ").append(predicatesv).append("\n");
                erraneous = true;}}
            if(cpRatiov < 0) {
                errors.append(prefix).append("Negative cpRatio: ").append(cpRatiov).append("\n");
                erraneous = true;}
            if(erraneous) return;

            generators.add(new RandomClauseSetGenerator(seedv,predicatesv,lengthv,precisev,redundantv,Math.round(predicatesv*cpRatiov),
                    0,0,0,0,0,0));}
        }


    /** generates the clause set
     *
     * @param errors for error messages
     * @return  true
     */
    public InputClauses generateProblem(Monitor errors) {
        String info = "Randomly generated clauses";
        InputClauses inputClauses = new InputClauses("",predicates,null,info);
        Random rnd = new Random(seed);
        int[] id = {0};
        if(ors       != 0) generateClauses(inputClauses,id,Connective.OR,ors,rnd);
        if(ands      != 0) generateClauses(inputClauses,id,Connective.AND,ands,rnd);
        if(equivs    != 0) generateClauses(inputClauses,id,Connective.EQUIV,equivs,rnd);
        if(atleasts  != 0) generateClauses(inputClauses,id,Connective.ATLEAST,atleasts,rnd);
        if(atmosts   != 0) generateClauses(inputClauses,id,Connective.ATMOST,atmosts,rnd);
        if(exactly   != 0) generateClauses(inputClauses,id,Connective.EXACTLY,exactly,rnd);
        if(intervals != 0) generateClauses(inputClauses,id,Connective.INTERVAL,intervals,rnd);

        inputClauses.info = "Randomly generated clauses:\n";
        inputClauses.nextId = id[0]+1;
        return inputClauses;}



    /** generates random clauses of the give type
     * Double literals and complementary literals are avoided.
     *
     * @param inputClauses for adding the new clauses
     * @param id   for generating next clause id
     * @param connective          AND etc.
     * @param numberOfClauses     number of clauses to be generated
     * @param rnd                 random number generator
     */
    protected void generateClauses(InputClauses inputClauses, int[] id,Connective connective, int numberOfClauses,
                                   Random rnd) {
        boolean numeric = connective.isQuantifier();
        int start = numeric ? 3 : 2;
        if(connective == Connective.INTERVAL) start = 4;
        int counter = -1;
        while(++counter < numberOfClauses) {
            int clauseLength = precise ? length : rnd.nextInt(length)+1;
            if(Connective.EQUIV == connective && clauseLength == 1) {++clauseLength;}
            int[] clause = new int[clauseLength+start];
            clause[0] = ++id[0];
            clause[1] = connective.ordinal();
            if(connective == Connective.INTERVAL) {
                int min = rnd.nextInt(clauseLength)+1;
                int max = rnd.nextInt(clauseLength)+1;
                if(min <= max) {clause[2] = min; clause[3] = max;}
                else {clause[2] = max; clause[3] = min;}}
            else {if(numeric) {clause[2] = rnd.nextInt(clauseLength)+1;}}

            for(int i = start; i < clauseLength+start; ++i) {
                int sign = rnd.nextBoolean() ? +1 : -1;
                int literal = sign*(rnd.nextInt(predicates)+1);
                if(numeric) {clause[i] = literal; continue;}
                if(redundant) clause[i] = literal;
                else {
                    boolean found = false;
                    for(int j = start; j < i; ++j) {if(literal == clause[j] || -literal == clause[j]) {found = true; break;}}
                    if(found) {--i; continue;}}}
            inputClauses.addClause(clause);}
    }
}


