package ProblemGenerators;

import Datastructures.Clauses.InputClauses;
import Datastructures.Clauses.Quantifier;
import Management.Parameter;
import Management.Parameters;
import Utilities.Utilities;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.*;

import static Utilities.Utilities.toArrayList;

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

    /** for enumerating the problems. */
    private static int problemCounter = 0;
    /** the original parameters (for documentation only.) */
    private HashMap<String,String> parameters = null;
    /** the seed for the random number generator */
    private final int seed;
    /** the number of predicates */
    private final int predicates;
    /** the clause length */
    private final int[] length;
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

    /**
     * Creates a Parameters object for opening a file.
     *
     * @return the created Parameters object
     */
    public static Parameters makeParameter() {
        Parameters parameters = new Parameters("Randomly Generated Clauses");
        Parameter predicates = new Parameter("Predicates",Parameter.Type.String, "10",
                IntArrayList.wrap(new int[]{10}),
                "Number of predicates (atleast 1)");
        predicates.setParser((String rangeString, StringBuilder errors) -> Utilities.parseIntRange(rangeString,1,errors));
        parameters.add(predicates);

        Parameter length = new Parameter("Length", Parameter.Type.String, "3",
                IntArrayList.wrap(new int[]{3}),
                "Length of clauses: length or min-max (atleast 1)\n"+
                "Examples: 3 (3 literals in a clause), 2-3 (2 or 3 literals in a clause). ");
        length.setParser((String lengthString, StringBuilder errors) -> {
                if(lengthString == null || lengthString.length() == 0) {
                    errors.append("empty length string\n");
                    return null;}
                String[] parts = lengthString.trim().split("\\s*-\\s*");
                try {
                    switch (parts.length) {
                        case 1: int n = Integer.parseInt(parts[0]); return IntArrayList.wrap(new int[]{n,n});
                        case 2: return IntArrayList.wrap(new int[]{Integer.parseInt(parts[0]), Integer.parseInt(parts[1])});
                        default: errors.append("Illegal length format: " + lengthString + "\n"); return null;}}
                catch(Exception ex) {errors.append(ex);}
            return null;});
        parameters.add(length);

        Parameter redundant = new Parameter("Redundant", Parameter.Type.Boolean, "false",
                false,
                "Generate redundant clauses\n"+
                "If false then multiple and complementary occurrences of literals are avoided.");
        parameters.add(redundant);

        Parameter ors = new Parameter("Ors", Parameter.Type.String, "10",
                IntArrayList.wrap(new int[]{10}),
                "Number of OR clauses");
        ors.setParser((String rangeString, StringBuilder errors) -> Utilities.parseIntRange(rangeString,0, errors));
        parameters.add(ors);

        Parameter ands = new Parameter("Ands", Parameter.Type.String, "0",
                IntArrayList.wrap(new int[]{0}),
                "Number of AND clauses");
        ands.setParser((String rangeString, StringBuilder errors) -> Utilities.parseIntRange(rangeString,0, errors));
        parameters.add(ands);

        Parameter equivs = new Parameter("Equivs", Parameter.Type.String, "0",
                IntArrayList.wrap(new int[]{0}),
                "Number of EQUIV clauses");
        equivs.setParser((String rangeString, StringBuilder errors) -> Utilities.parseIntRange(rangeString, 0,errors));
        parameters.add(equivs);

        Parameter atleasts = new Parameter("Atleasts", Parameter.Type.String, "0",
                IntArrayList.wrap(new int[]{0}),
                "Number of ATLEAST clauses");
        atleasts.setParser((String rangeString, StringBuilder errors) -> Utilities.parseIntRange(rangeString, 0,errors));
        parameters.add(atleasts);

        Parameter atmosts = new Parameter("Atmosts", Parameter.Type.String, "0",
                IntArrayList.wrap(new int[]{0}),
                "Number of ATMOST clauses");
        atmosts.setParser((String rangeString, StringBuilder errors) -> Utilities.parseIntRange(rangeString,0, errors));
        parameters.add(atmosts);

        Parameter exactlies = new Parameter("Exactlies", Parameter.Type.String, "0",
                IntArrayList.wrap(new int[]{0}),
                "Number of EXACTLY clauses");
        exactlies.setParser((String rangeString, StringBuilder errors) -> Utilities.parseIntRange(rangeString, 0,errors));
        parameters.add(exactlies);

        Parameter intervals = new Parameter("Intervals", Parameter.Type.String, "0",
                IntArrayList.wrap(new int[]{0}),
                "Number of INTERVAL clauses");
        intervals.setParser((String rangeString, StringBuilder errors) -> Utilities.parseIntRange(rangeString,0, errors));
        parameters.add(intervals);

        Parameter seed = new Parameter("Seed", Parameter.Type.String, "0",
                IntArrayList.wrap(new int[]{0}),
                "Seed for random number generator (non-negative integer)");
        seed.setParser((String rangeString, StringBuilder errors) -> Utilities.parseIntRange(rangeString, 0, errors));
        parameters.add(seed);
        parameters.setDescription("Generates Clauses with a Random Number Generator.");
        parameters.setOperation((Parameters params, StringBuilder errors) -> {
            ArrayList<ProblemGenerator> generators = new ArrayList<>();
            makeProblemGenerator(params, generators);
            ArrayList<InputClauses> clauses = new ArrayList<>();
            for(ProblemGenerator generator : generators) {
                clauses.add(generator.generateProblem(errors));}
            return clauses;});
        return parameters;
    }

    /** constructs a random clause set generator.
     *
     * @param parameters   the original specification (only for documentation).
     * @param seed         the seed for the random number generator.
     * @param predicates   the number of predicates.
     * @param length       the clause length [min,max]
     * @param redundant    if true then complementary and double literals are allowed.
     * @param ors          the number of disjunctions (or).
     * @param ands         the number of conjunctions (and).
     * @param equivs       the number of equivalences.
     * @param atleasts     the number of atleast clauses.
     * @param atmosts      the number of atmost clauses.
     * @param exactlies    the number of exactly clauses.
     * @param intervals    the number of interval clauses.
     */
    public RandomClauseSetGenerator(HashMap<String,String> parameters, int seed, int predicates, int[] length, boolean redundant,
                                    int ors, int ands, int equivs, int  atleasts, int atmosts, int exactlies,
                                    int intervals) {
        this.parameters = parameters;
        this.seed       = seed;
        this.predicates = predicates;
        this.length     = length;
        this.redundant  = redundant;
        this.ors        = ors;
        this.ands       = ands;
        this.equivs     = equivs;
        this.atleasts   = atleasts;
        this.atmosts    = atmosts;
        this.exactly    = exactlies;
        this.intervals  = intervals;
    }

    public RandomClauseSetGenerator(int seed, int predicates, IntArrayList length, boolean redundant,
                                    int ors, int ands, int equivs, int  atleasts, int atmosts, int exactlies,
                                    int intervals) {
        this.seed       = seed;
        this.predicates = predicates;
        this.length     = new int[]{length.getInt(0),length.getInt(1)};
        this.redundant  = redundant;
        this.ors        = ors;
        this.ands       = ands;
        this.equivs     = equivs;
        this.atleasts   = atleasts;
        this.atmosts    = atmosts;
        this.exactly    = exactlies;
        this.intervals  = intervals;
    }

    /**
     * Generates and adds new problem generators based on the provided parameters.
     *
     * @param parameters The parameters containing the values necessary to create the problem generators.
     * @param generators The list of problem generators to add the newly created generators to.
     */
    public static void makeProblemGenerator(Parameters parameters,ArrayList<ProblemGenerator> generators) {
        IntArrayList predicates =  (IntArrayList)parameters.parameters.get(0).value;
        IntArrayList length =      (IntArrayList)parameters.parameters.get(1).value;
        boolean redundant   = (Boolean)parameters.parameters.get(2).value;
        IntArrayList ors        = (IntArrayList)parameters.parameters.get(3).value;
        IntArrayList ands       = (IntArrayList)parameters.parameters.get(4).value;
        IntArrayList equivs     = (IntArrayList)parameters.parameters.get(5).value;
        IntArrayList atleasts   = (IntArrayList)parameters.parameters.get(6).value;
        IntArrayList atmosts    = (IntArrayList)parameters.parameters.get(7).value;
        IntArrayList exactly    = (IntArrayList)parameters.parameters.get(8).value;
        IntArrayList intervals  = (IntArrayList)parameters.parameters.get(9).value;
        IntArrayList seeds      = (IntArrayList)parameters.parameters.get(10).value;

        for(ArrayList<Object> p : (ArrayList<ArrayList>)Utilities.crossProduct(toArrayList(predicates),toArrayList(ors),
                toArrayList(ands),toArrayList(equivs),toArrayList(atleasts),toArrayList(atmosts),
                toArrayList(exactly),toArrayList(intervals),toArrayList(seeds))) {
            int predicatesv         = (int)p.get(0);
            int orsv = (int)p.get(1);
            int andsv = (int)p.get(2);
            int equivsv = (int)p.get(3);
            int atleastsv = (int)p.get(4);
            int atmostsv = (int)p.get(5);
            int exactlyv = (int)p.get(6);
            int intervalsv = (int)p.get(7);
            int seedsv = (int)p.get(8);
            generators.add(new RandomClauseSetGenerator(seedsv, predicatesv, length, redundant,
            orsv, andsv, equivsv, atleastsv, atmostsv, exactlyv,intervalsv));}}



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
                "length:     two integers: min-max, specifies the number of literals per clause.\n" +
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



    /** generates the clause set.
     *
     * @param errors for error messages.
     * @return  the new InputClauses.
     */
    public InputClauses generateProblem(StringBuilder errors) {
        String problemName = "Random_" + ++problemCounter;
        String info = "Randomly generated clauses by: " + toString()+":\n";
        InputClauses inputClauses = new InputClauses("",predicates,null,info);
        Random rnd = new Random(seed);
        int[] id = {0};
        if(ors       != 0) {generateClauses(inputClauses,id, Quantifier.OR,ors,rnd);
            info += "\ndisjunctions:" + ors;}
        if(ands      != 0) {generateClauses(inputClauses,id, Quantifier.AND,ands,rnd);
            info += "\nconjunctions:" + ands;}
        if(equivs    != 0) {generateClauses(inputClauses,id, Quantifier.EQUIV,equivs,rnd);
            info += "\nequivlences:" + equivs;}
        if(atleasts  != 0) {generateClauses(inputClauses,id, Quantifier.ATLEAST,atleasts,rnd);
            info += "\natleasts:" + atleasts;}
        if(atmosts   != 0) {generateClauses(inputClauses,id, Quantifier.ATMOST,atmosts,rnd);
            info += "\natmosts:" + atmosts;}
        if(exactly   != 0) {generateClauses(inputClauses,id, Quantifier.EXACTLY,exactly,rnd);
            info += "\nexactlies:" + exactly;}
        if(intervals != 0) {generateClauses(inputClauses,id, Quantifier.INTERVAL,intervals,rnd);
            info += "\nintervals:" + intervals;}

        inputClauses.problemId = problemName;
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
            int min = length[0];
            int max = (length.length > 1) ? length[1] : length[0];
            int clauseLength = (min == max) ? min : min + rnd.nextInt(max-min+1);
            if(Quantifier.EQUIV == quantifier && clauseLength == 1) {++clauseLength;}
            int[] clause = new int[clauseLength+start];
            clause[0] = ++id[0];
            clause[1] = quantifier.ordinal();
            if(quantifier.isQuantifier()) clause[2] = rnd.nextInt(clauseLength)+1;
            if(quantifier == Quantifier.INTERVAL) {
                min = clause[2];
                max = rnd.nextInt(clauseLength)+1;
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
        //st.append("Parameters:    ").append(parameters.toString()).append("\n");
        st.append("predicates:    ").append(predicates).append("\n");
        st.append("seed:          ").append(seed).append("\n");
        st.append("clause length: ").append(Arrays.toString(length));
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


