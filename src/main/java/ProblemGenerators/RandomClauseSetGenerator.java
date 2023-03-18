package ProblemGenerators;

import Datastructures.Clauses.Quantifier;
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

    /** all allowed keys in the parameters */
    protected static final HashSet<String> keys = new HashSet<>();
    static { // these are the allowed keys in the specification.
        Collections.addAll(keys,"problem", "seed", "predicates", "cpRatio", "length", "precise",
                "redundant", "ors", "ands", "equivs", "atleasts", "atmosts", "exactlies", "intervals");
    }

    /** the original parameters (for documentation only. */
    private final HashMap<String,String> parameters;
    /** the seed for the random number generator */
    private final int seed;
    /** the number of predicates */
    private final int predicates;
    /** the clause length */
    private final int length;
    /** if false then the clause length may be smaller */
    private final boolean precise;
    /** if true then complementary and double literals are allowed */
    private final boolean redundant;

    /** number of disjumctions (or) */
    private final int ors;
    /** number of conjunctions (and) */
    private final int ands;
    /** number of equivalences */
    private final int equivs;
    /** number of atleast clauses */
    private final int atleasts;
    /** number of atmost clauses */
    private final int atmosts;
    /** number of exactly clauses */
    private final int exactly;
    /** number of interval clauses */
    private final int intervals;

    /** constructs a random clause set generator.
     *
     * @param parameters   the original specification (only for documentation).
     * @param seed         the seed for the random number generator.
     * @param predicates   the number of predicates.
     * @param length       the clause length.
     * @param precise      if false then the clause length may be less than 'length'.
     * @param redundant    if true then complementary and double literals are allowed.
     * @param ors          the number of disjunctions (or).
     * @param ands         the number of conjunctions (and).
     * @param equivs       the number of equivalences.
     * @param atleasts     the number of atleast clauses.
     * @param atmosts      the number of atmost clauses.
     * @param exactlies    the number of exactly clauses.
     * @param intervals    the number of interval clauses.
     */
    public RandomClauseSetGenerator(HashMap<String,String> parameters, int seed, int predicates, int length, boolean precise, boolean redundant,
                                    int ors, int ands, int equivs, int  atleasts, int atmosts, int exactlies,
                                    int intervals) {
        this.parameters = parameters;
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
        this.exactly    = exactlies;
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
                "exactlies:  an integer >= 0, specifies the number of exactly clauses to be generated.\n" +
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



    /** The method translates the string-valued parameters in the HashMap to generators.
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
     * exactlies:  an integer >= 0, specifies the number of exactly clauses to be generated.<br>
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
     * @param parameters the input parameters.
     * @param generators for adding new generators.
     * @param errors     for reporting syntax errors.
     * @param warnings   for reporting warnings.
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
        String redundantS = parameters.get("redundant");
        String orsS       = parameters.get("ors");
        String andsS      = parameters.get("ands");
        String equivsS    = parameters.get("equivs");
        String atleastsS  = parameters.get("atleasts");
        String atmostsS   = parameters.get("atmosts");
        String exactlysS  = parameters.get("exactlies");
        String intervalsS = parameters.get("intervals");

        ArrayList predicatesA = null;
        if(predicateS == null) {
            errors.append(prefix).append("no number of predicates defined.");}
        else {predicatesA = Utilities.parseIntRange(prefix+"predicate",predicateS,errors);}
        if(predicatesA == null) {erraneous = true; errors.append("\n");}

        ArrayList lengthA = null;
        if(lengthS == null) {
            errors.append(prefix).append("no clause length defined.");}
        else {lengthA = Utilities.parseIntRange(prefix+"lengths: ",lengthS,errors);}
        if(lengthA == null) {erraneous = true; errors.append("\n");}

        ArrayList seedA;
        if(seedS == null) {seedA = new ArrayList<>(); seedA.add(0);}
        else {seedA = Utilities.parseIntRange(prefix+"seeds: ",seedS,errors);
             if(seedA == null) errors.append("\n");}

        ArrayList cpRatioA = null;
        if(cpRatioS != null) {
            cpRatioA = Utilities.parseFloatRange(prefix+"cpRatios: ",cpRatioS,errors);
            if(cpRatioA == null) {errors.append("\n"); erraneous = true;}}

        ArrayList preciseA;
        if(preciseS == null) {preciseA = new ArrayList(); preciseA.add(true);}
        else {preciseA = Utilities.parseBoolean(prefix+"precise: ", preciseS,errors);
             if(preciseA == null) {errors.append("\n"); erraneous = true;}}

        ArrayList redundantA;
        if(redundantS == null) {redundantA = new ArrayList(); redundantA.add(false);}
        else {redundantA = Utilities.parseBoolean(prefix+"redundants: ", redundantS,errors);
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
            exactlysA = Utilities.parseIntRange(prefix+"exactlies: ",exactlysS,errors);
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
                Integer seedv        = (Integer)values.get(0);  if(seedv       == null) seedv       = 0;
                Integer predicatesv  = (Integer)values.get(1);  if(predicatesv == null) predicatesv = 0;
                Integer lengthv      = (Integer)values.get(2);  if(lengthv     == null) lengthv     = 0;
                Boolean precisev     = (Boolean)values.get(3);  if(precisev    == null) precisev    = true;
                Boolean redundantv   = (Boolean)values.get(4);  if(redundantv  == null) redundantv  = false;
                Float   cpRatiov     = (Float)values.get(5);    if(cpRatiov    == null) cpRatiov    = 0.0f;
                Integer orsv         = (Integer)values.get(6);  if(orsv        == null) orsv        = 0;
                Integer andsv        = (Integer)values.get(7);  if(andsv       == null) andsv       = 0;
                Integer equivsv      = (Integer)values.get(8);  if(equivsv     == null) equivsv     = 0;
                Integer atleastsv    = (Integer)values.get(9);  if(atleastsv   == null) atleastsv   = 0;
                Integer atmostsv     = (Integer)values.get(10); if(atmostsv    == null) atmostsv    = 0;
                Integer exactlysv    = (Integer)values.get(11); if(exactlysv   == null) exactlysv   = 0;
                Integer intervalsv   = (Integer)values.get(12); if(intervalsv  == null) intervalsv  = 0;

                if(seedv < 0) {
                    errors.append(prefix).append("negative seed specified: ").append(seedv).append("\n");
                    erraneous = true;}
                if(lengthv <= 0) {
                    errors.append(prefix).append("length of clauses is not positive: ").append(lengthv).append("\n");
                    erraneous = true;}
                if(predicatesv <= 0) {
                    errors.append(prefix).append("number of predicates is not positive: ").append(predicatesv).append("\n");
                    erraneous = true;}
                else {if(lengthv > predicatesv) {
                        errors.append(prefix).append("clause Length : ").append(lengthv).append("> number of predicates ").append(predicatesv).append("\n");
                        erraneous = true;}}
                if(cpRatiov == 0.0f && orsv == 0 && andsv == 0 && equivsv == 0 &&
                        atleastsv == 0 && atmostsv == 0 && exactlysv == 0) {
                    errors.append(prefix).append("no number of clauses specified\n");
                    erraneous = true;}
                if(orsv < 0) {
                    errors.append(prefix).append("negative number of ors specified: ").append(orsv).append("\n");
                    erraneous = true;}
                if(andsv < 0) {
                    errors.append(prefix).append("negative number of ands specified: ").append(andsv).append("\n");
                    erraneous = true;}
                if(equivsv < 0) {
                    errors.append(prefix).append("negative number of equivs specified: ").append(equivsv).append("\n");
                    erraneous = true;}
                if(atleastsv < 0) {
                    errors.append(prefix).append("negative number of atleasts specified: ").append(atleastsv).append("\n");
                    erraneous = true;}
                if(atmostsv < 0) {
                    errors.append(prefix).append("negative number of atmosts specified: ").append(atmostsv).append("\n");
                    erraneous = true;}
                if(exactlysv < 0) {
                    errors.append(prefix).append("negative number of exactlies specified: ").append(exactlysv).append("\n");
                    erraneous = true;}
                if(intervalsv < 0) {
                    errors.append(prefix).append("negative number of intervals specified: ").append(intervalsv).append("\n");
                    erraneous = true;}

                if(erraneous) return;
                generators.add(new RandomClauseSetGenerator(parameters,(int)seedv,(int)predicatesv,(int)lengthv,(boolean)precisev,
                        (boolean)redundantv,(int)orsv,(int)andsv,(int)equivsv,(int)atleastsv,(int)atmostsv,
                        (int)exactlysv,(int)intervalsv));}
            return;}

        // with cpRation > 0
        ArrayList<ArrayList<Object>> list = Utilities.crossProduct(seedA,predicatesA,lengthA, preciseA,redundantA,cpRatioA);
        for(ArrayList<Object> values : list) {
            Integer seedv       = (Integer)values.get(0); if(seedv       == null) seedv       = 0;
            Integer predicatesv = (Integer)values.get(1); if(predicatesv == null) predicatesv = 0;
            Integer lengthv     = (Integer)values.get(2); if(lengthv     == null) lengthv     = 0;
            Boolean precisev    = (Boolean)values.get(3); if(precisev    == null) precisev    = true;
            Boolean redundantv  = (Boolean)values.get(4); if(redundantv  == null) redundantv  = false;
            Float   cpRatiov    = (Float)values.get(5);  if(cpRatiov     == null) cpRatiov    = 0.0f;
            if(seedv < 0) {
                errors.append(prefix).append("negative seed specified: ").append(seedv).append("\n");
                erraneous = true;}
            if(lengthv <= 0) {
                errors.append(prefix).append("Clause Length is not positive\n").append(lengthv).append("\n");}
            if(predicatesv <= 0) {
                errors.append(prefix).append("number of predicates is not positive: ").append(predicatesv).append("\n");
                erraneous = true;}
            else {if(lengthv > predicatesv) {
                errors.append(prefix).append("clause Length : ").append(lengthv).append("> number of predicates ").append(predicatesv).append("\n");
                erraneous = true;}}
            if(cpRatiov < 0.0f) {
                errors.append(prefix).append("negative cpRatio: ").append(cpRatiov).append("\n");
                erraneous = true;}
            if(erraneous) return;

            generators.add(new RandomClauseSetGenerator(parameters,seedv,predicatesv,lengthv,precisev,redundantv,
                    Math.round(predicatesv*cpRatiov),0,0,0,0,0,0));}
        }


    /** generates the clause set
     *
     * @param errors for error messages
     * @return  true
     */
    public InputClauses generateProblem(Monitor errors) {
        String problemName = "RND:";
        String info = "Randomly generated clauses with parameters:" + parameters.toString()+":\n";
        InputClauses inputClauses = new InputClauses("",predicates,null,info);
        Random rnd = new Random(seed);
        int[] id = {0};
        if(ors       != 0) {generateClauses(inputClauses,id, Quantifier.OR,ors,rnd);
            problemName += " o:" +ors;
            info += "\ndisjunctions:" + ors;}
        if(ands      != 0) {generateClauses(inputClauses,id, Quantifier.AND,ands,rnd);
            problemName += " &:" +ands;
            info += "\nconjunctions:" + ands;}
        if(equivs    != 0) {generateClauses(inputClauses,id, Quantifier.EQUIV,equivs,rnd);
            problemName += " e:" +equivs;
            info += "\nequivlences:" + equivs;}
        if(atleasts  != 0) {generateClauses(inputClauses,id, Quantifier.ATLEAST,atleasts,rnd);
            problemName += " >=:"+atleasts;
            info += "\natleasts:" + atleasts;}
        if(atmosts   != 0) {generateClauses(inputClauses,id, Quantifier.ATMOST,atmosts,rnd);
            problemName += " <=:"+atmosts;
            info += "\natmosts:" + atmosts;}
        if(exactly   != 0) {generateClauses(inputClauses,id, Quantifier.EXACTLY,exactly,rnd);
            problemName += " =:" +exactly;
            info += "\nexactlies:" + exactly;}
        if(intervals != 0) {generateClauses(inputClauses,id, Quantifier.INTERVAL,intervals,rnd);
            problemName += " []:"+intervals;
            info += "\nintervals:" + intervals;}

        inputClauses.problemName = problemName;
        inputClauses.info        = info;
        inputClauses.nextId      = id[0]+1;
        return inputClauses;}



    /** generates random clauses of the give type.
     *
     * @param inputClauses        for adding the new clauses
     * @param id                  for generating next clause id
     * @param quantifier          AND etc.
     * @param numberOfClauses     number of clauses to be generated
     * @param rnd                 random number generator
     */
    protected void generateClauses(InputClauses inputClauses, int[] id, Quantifier quantifier,
                                   int numberOfClauses,Random rnd) {
        int start = quantifier.isQuantifier() ? 3 : 2;
        if(quantifier == Quantifier.INTERVAL) start = 4;
        int counter = -1;
        while(++counter < numberOfClauses) {
            int clauseLength = precise ? length : rnd.nextInt(length)+1;
            if(Quantifier.EQUIV == quantifier && clauseLength == 1) {++clauseLength;}
            int[] clause = new int[clauseLength+start];
            clause[0] = ++id[0];
            clause[1] = quantifier.ordinal();
            if(quantifier.isQuantifier()) clause[2] = rnd.nextInt(clauseLength)+1;
            if(quantifier == Quantifier.INTERVAL) {
                int min = clause[2];
                int max = rnd.nextInt(clauseLength)+1;
                if(min <= max) {clause[3] = max;}
                else {clause[2] = max; clause[3] = min;}}

            for(int i = start; i < clauseLength+start; ++i) {
                int sign = rnd.nextBoolean() ? +1 : -1;
                int literal = sign*(rnd.nextInt(predicates)+1);
                if(redundant) clause[i] = literal; // tautologies and double literals allowed
                else {
                    boolean found = false;
                    for(int j = start; j < i; ++j) {if(literal == clause[j] || -literal == clause[j]) {found = true; break;}}
                    if(found) {--i; continue;} else clause[i] = literal;}}
            inputClauses.addClause(clause);}
    }

    /** generates a description of the generator.
     *
     * @return a description of the generator.
     */
    public String toString() {
        StringBuilder st = new StringBuilder();
        st.append("Random Clause Set Generator\n");
        st.append("Parameters:    ").append(parameters.toString()).append("\n");
        st.append("predicates:    ").append(predicates).append("\n");
        st.append("seed:          ").append(seed).append("\n");
        st.append("clause length: ").append(length);
        if(precise) st.append(" precisely");
        st.append("\n");
        if(redundant) st.append("complementary and double literals allowed\n");
        if(ors       != 0) st.append("disjunctions:  ").append(ors).append("\n");
        if(ands      != 0) st.append("conjunctions:  ").append(ands).append("\n");
        if(equivs    != 0) st.append("equivalences:  ").append(equivs).append("\n");
        if(atleasts  != 0) st.append("atleasts:      ").append(atleasts).append("\n");
        if(atmosts   != 0) st.append("atmosts:       ").append(atmosts).append("\n");
        if(exactly   != 0) st.append("exactlies:     ").append(exactly).append("\n");
        if(intervals != 0) st.append("intervals:     ").append(intervals).append("\n");
        if(ors != 0) st.append("cp-ratio for disjunctions: ").append((float)ors/(float)predicates);
        return st.toString();
    }
}


