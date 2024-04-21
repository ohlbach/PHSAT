package ProblemGenerators;

import Datastructures.Clauses.InputClauses;
import Datastructures.Clauses.Quantifier;
import Management.Parameter;
import Management.Parameters;
import Utilities.Utilities;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.*;

import static Utilities.Utilities.toArrayList;
import static java.lang.Boolean.parseBoolean;

/**
 * This generator generates clause sets with randomly generated literals.
 *
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

    /** for enumerating the problems. */
    private static int problemCounter = 0;
    /** the default seed for the frame */
    private static int seedDefault = 0;
    /** the seed for the random number generator */
    private final int seed;
    /** the default predicates for the frame */
    private static int predicatesDefault = 10;
    /** the number of predicates */
    private final int predicates;
    /** the default length for the frame */
    private static int[] lengthDefault = new int[]{3,3};
    /** the clause length */
    private final int[] length;
    /** the default redundant for the frame */
    private static boolean redundantDefault = false;
    /** if true then complementary and double literals are allowed */
    private final boolean redundant;
    /** the default ors for the frame */
    private static int orsDefault = 10;
    /** number of disjunctions (or) */
    private final int ors;
    /** the default ands for the frame */
    private static int andsDefault = 0;
    /** number of conjunctions (and) */
    private final int ands;
    /** the default equivs for the frame */
    private static int equivsDefault = 0;
    /** number of equivalences */
    private final int equivs;
    /** the default atleasts for the frame */
    private static int atleastsDefault = 0;
    /** number of atleast clauses */
    private final int atleasts;
    /** the default atmosts for the frame */
    private static int atmostsDefault = 0;
    /** number of atmost clauses */
    private final int atmosts;
    /** the default exactlies for the frame */
    private static int exactliesDefault = 0;
    /** number of exactly clauses */
    private final int exactlies;
    /** the default intervals for the frame */
    private static int intervalsDefault = 0;
    /** number of interval clauses */
    private final int intervals;

    /**Sets the default values for the Random Clauses Generator.
     *
     * The method is called in ProblemGenerator
     *
     * @param defaults the list of default values
     * @return void
     */
    public static void setDefaults(ArrayList<String> defaults) {
        if(defaults == null) {return;}
        StringBuilder errors = new StringBuilder();
        try{
            for(String line : defaults) {
                String[] parts = line.split("\\s*=\\s*",2);
                if(parts.length != 2) {continue;}
                String variable = parts[0];
                String value = parts[1];
                switch(variable.toLowerCase()) {
                    case "seed":        seedDefault        = Integer.parseInt(value); break;
                    case "predicates":  predicatesDefault  = Integer.parseInt(value); break;
                    case "redundant":   redundantDefault   = parseBoolean(value);     break;
                    case "lengths":     ArrayList<int[]> lengths = parseLength(value,errors);
                                        if(lengths != null) lengthDefault = lengths.get(0);break;
                    case "ors":         orsDefault         = Integer.parseInt(value); break;
                    case "ands":        andsDefault        = Integer.parseInt(value); break;
                    case "equivs":      equivsDefault      = Integer.parseInt(value); break;
                    case "atleasts":    atleastsDefault    = Integer.parseInt(value); break;
                    case "atmosts":     atmostsDefault     = Integer.parseInt(value); break;
                    case "exactlies":   exactliesDefault   = Integer.parseInt(value); break;
                    case "intervals":   intervalsDefault   = Integer.parseInt(value); break;
                }}}
        catch(NumberFormatException e) {
            if(!errors.isEmpty()) System.err.println("Error in default Parameters for Random Number Generator:\n"+ errors.toString());
            System.err.println("Error in default Parameters for Random Number Generator:\n"+e.getMessage());
            System.exit(1);}
        if(!errors.isEmpty()) {System.err.println("Error in default Parameters for Random Number Generator:\n"+ errors.toString());
            System.exit(1);}
    }
    /**
     * Creates a Parameters object for generating random clauses.
     *
     * @return the created Parameters object
     */
    public static Parameters makeParameter() {
        Parameters parameters = new Parameters("Randomly Generated Clauses");
        Parameter predicates = new Parameter("Predicates",Parameter.Type.String, Integer.toString(predicatesDefault),
                IntArrayList.wrap(new int[]{predicatesDefault}),
                """
                        Number of predicates (atleast 1)
                        The integer values of all parameters except Length can be specified as 'ranges'
                         with the following syntactic possibilities:
                          List:       3,6,7
                          Range:      3 to 10
                          With steps: 3 to 10 step 2

                        Each integer in a range causes a separate clause set to be generated.
                        The number of clause sets is the product of all the lengths of the ranges.""");
        predicates.setParser((String rangeString, StringBuilder errors) -> Utilities.parseIntRange(rangeString,1,errors));
        parameters.add(predicates);

        String lengthStrings = (lengthDefault[0] == lengthDefault[1]) ?
                Integer.toString(lengthDefault[0]) : lengthDefault[0]+"-"+lengthDefault[1];
        Parameter length = new Parameter("Length", Parameter.Type.String, lengthStrings,
                lengthDefault,
                "Length of clauses: length or min-max (atleast 1)\n"+
                "Examples: 3 (3 literals in a clause), 2-3 (2 or 3 literals in a clause). ");
        length.setParser((String lengthString, StringBuilder errors) -> parseLength(lengthString,errors));
        parameters.add(length);

        Parameter redundant = new Parameter("Redundant", Parameter.Type.Boolean, ""+redundantDefault,
                redundantDefault,
                "Generate redundant clauses\n"+
                "If false then multiple and complementary occurrences of literals are avoided.");
        parameters.add(redundant);

        Parameter ors = new Parameter("Ors", Parameter.Type.String, Integer.toString(orsDefault),
                IntArrayList.wrap(new int[]{orsDefault}),
                "Number of OR clauses (disjunctions)\n"+
                "Atleast 1 literal must be true to make the clause true.");
        ors.setParser((String rangeString, StringBuilder errors) -> Utilities.parseIntRange(rangeString,0, errors));
        parameters.add(ors);

        Parameter ands = new Parameter("Ands", Parameter.Type.String, Integer.toString(andsDefault),
                IntArrayList.wrap(new int[]{andsDefault}),
                "Number of AND clauses (conjunction)\n"+
                "All literals must be true to make the clause true.");
        ands.setParser((String rangeString, StringBuilder errors) -> Utilities.parseIntRange(rangeString,0, errors));
        parameters.add(ands);

        Parameter equivs = new Parameter("Equivs", Parameter.Type.String, Integer.toString(equivsDefault),
                IntArrayList.wrap(new int[]{equivsDefault}),
                "Number of EQUIV clauses\n"+
                "The literals of an equivalence must either all be true or all be false.");
        equivs.setParser((String rangeString, StringBuilder errors) -> Utilities.parseIntRange(rangeString, 0,errors));
        parameters.add(equivs);

        Parameter atleasts = new Parameter("Atleasts", Parameter.Type.String, Integer.toString(atleastsDefault),
                IntArrayList.wrap(new int[]{atleastsDefault}),
                "Number of ATLEAST clauses\n"+
                "A clause 'atleast n p_1,...' is true if atleast n of the literals are true.");
        atleasts.setParser((String rangeString, StringBuilder errors) -> Utilities.parseIntRange(rangeString, 0,errors));
        parameters.add(atleasts);

        Parameter atmosts = new Parameter("Atmosts", Parameter.Type.String, Integer.toString(atmostsDefault),
                IntArrayList.wrap(new int[]{atmostsDefault}),
                "Number of ATMOST clauses\n"+
                        "A clause 'atmost n p_1,...' is true if atmost n of the literals are true.");
        atmosts.setParser((String rangeString, StringBuilder errors) -> Utilities.parseIntRange(rangeString,0, errors));
        parameters.add(atmosts);

        Parameter exactlies = new Parameter("Exactlies", Parameter.Type.String, Integer.toString(exactliesDefault),
                IntArrayList.wrap(new int[]{exactliesDefault}),
                "Number of EXACTLY clauses\n"+
                "A clause '= n p_1,...' is true if exactly n literals in the clause is true.");
        exactlies.setParser((String rangeString, StringBuilder errors) -> Utilities.parseIntRange(rangeString, 0,errors));
        parameters.add(exactlies);

        Parameter intervals = new Parameter("Intervals", Parameter.Type.String, Integer.toString(intervalsDefault),
                IntArrayList.wrap(new int[]{intervalsDefault}),
                "Number of INTERVAL clauses\n"+
                "An interval clause '[n,m] p_1,...' is true if between n and m literals are true.");
        intervals.setParser((String rangeString, StringBuilder errors) -> Utilities.parseIntRange(rangeString,0, errors));
        parameters.add(intervals);

        Parameter seed = new Parameter("Seed", Parameter.Type.String, Integer.toString(seedDefault),
                IntArrayList.wrap(new int[]{seedDefault}),
                "Seed for random number generator (non-negative integer)");
        seed.setParser((String rangeString, StringBuilder errors) -> Utilities.parseIntRange(rangeString, 0, errors));
        parameters.add(seed);
        parameters.setDescription("Generates clauses with randomly chosen literals.");
        parameters.setOperation((Parameters params, StringBuilder errors) -> {
            ArrayList<ProblemGenerator> generators = new ArrayList<>();
            makeProblemGenerators(params, generators);
            ArrayList<InputClauses> clauses = new ArrayList<>();
            for(ProblemGenerator generator : generators) {
                clauses.add(generator.generateProblem(errors));}
            return clauses;});
        return parameters;
    }

    /** parses a lengths string.
     *
     * lengthString: <br>
     *  - comma separated <br>
     *  - integer or integer-integer
     *
     * @param lengthString the string to be parsed.
     * @param errors       for adding error messages
     * @return an ArrayList of int-Arrays [min,max]
     */
    private static ArrayList<int[]> parseLength(String lengthString, StringBuilder errors) {
        if(lengthString == null || lengthString.isEmpty()) {
            errors.append("empty length string\n");
            return null;}
        ArrayList<int[]> lengthsArray = new ArrayList<>();
        for(String lengths : lengthString.split("\\s*,\\s*")) {
            String[] parts = lengths.trim().split("\\s*-\\s*");
            try {switch (parts.length) {
                    case 1: int n = Utilities.parseInteger(parts[0],1,errors);
                        lengthsArray.add(new int[]{n,n});
                        break;
                    case 2:
                        int min = Utilities.parseInteger(parts[0],1,errors);
                        if(min >= 0) {
                            int max = Utilities.parseInteger(parts[1],min,errors);
                            lengthsArray.add(new int[]{min,max});}
                        break;
                    default: errors.append("Illegal length format: " + lengthString + "\n");}}
            catch(Exception ex) {errors.append(ex);}}
        return errors.isEmpty() ? lengthsArray : null;}

    /**
     * Generates a random clause set based on the provided parameters.
     *
     * @param seed        The seed value for the random number generator.
     * @param predicates  The number of predicates in the clause set.
     * @param length      The length of the clauses in the clause set.
     * @param redundant   A boolean flag indicating if the clause set should be redundant.
     * @param ors         The number of clauses with the OR quantifier.
     * @param ands        The number of clauses with the AND quantifier.
     * @param equivs      The number of clauses with the EQUIV quantifier.
     * @param atleasts    The number of clauses with the ATLEAST quantifier.
     * @param atmosts     The number of clauses with the ATMOST quantifier.
     * @param exactlies   The number of clauses with the EXACTLY quantifier.
     * @param intervals   The number of clauses with the INTERVAL quantifier.
     */
    public RandomClauseSetGenerator(int seed, int predicates, int[] length, boolean redundant,
                                    int ors, int ands, int equivs, int  atleasts, int atmosts, int exactlies,
                                    int intervals) {
        this.seed       = seed;
        this.predicates = predicates;
        this.length     = length;
        this.redundant  = redundant;
        this.ors        = ors;
        this.ands       = ands;
        this.equivs     = equivs;
        this.atleasts   = atleasts;
        this.atmosts    = atmosts;
        this.exactlies = exactlies;
        this.intervals  = intervals;
    }

    /**
     * Generates and adds new problem generators based on the provided parameters.
     *
     * @param parameters The parameters containing the values necessary to create the problem generators.
     * @param generators The list of problem generators to add the newly created generators to.
     */
    public static void makeProblemGenerators(Parameters parameters,ArrayList<ProblemGenerator> generators) {
        int i = -1;
        IntArrayList predicates =  (IntArrayList)parameters.parameters.get(++i).value;
        ArrayList<Object> lengths =(ArrayList)parameters.parameters.get(++i).value;
        boolean redundant       = (Boolean)parameters.parameters.get(++i).value;
        IntArrayList ors        = (IntArrayList)parameters.parameters.get(++i).value;
        IntArrayList ands       = (IntArrayList)parameters.parameters.get(++i).value;
        IntArrayList equivs     = (IntArrayList)parameters.parameters.get(++i).value;
        IntArrayList atleasts   = (IntArrayList)parameters.parameters.get(++i).value;
        IntArrayList atmosts    = (IntArrayList)parameters.parameters.get(++i).value;
        IntArrayList exactly    = (IntArrayList)parameters.parameters.get(++i).value;
        IntArrayList intervals  = (IntArrayList)parameters.parameters.get(++i).value;
        IntArrayList seeds      = (IntArrayList)parameters.parameters.get(++i).value;

        for(ArrayList p : (ArrayList<ArrayList>)Utilities.crossProduct(toArrayList(predicates),lengths,
                toArrayList(ors),
                toArrayList(ands),toArrayList(equivs),toArrayList(atleasts),toArrayList(atmosts),
                toArrayList(exactly),toArrayList(intervals),toArrayList(seeds))) {
            i = -1;
            int predicatesv = (int)p.get(++i);
            int[] length    = (int[])p.get(++i);
            int orsv        = (int)p.get(++i);
            int andsv       = (int)p.get(++i);
            int equivsv     = (int)p.get(++i);
            int atleastsv   = (int)p.get(++i);
            int atmostsv    = (int)p.get(++i);
            int exactlyv    = (int)p.get(++i);
            int intervalsv  = (int)p.get(++i);
            int seedsv      = (int)p.get(++i);
            generators.add(new RandomClauseSetGenerator(seedsv, predicatesv, length, redundant,
            orsv, andsv, equivsv, atleastsv, atmostsv, exactlyv,intervalsv));}}


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
        if(exactlies != 0) {generateClauses(inputClauses,id, Quantifier.EXACTLY, exactlies,rnd);
            info += "\nexactlies:" + exactlies;}
        if(intervals != 0) {generateClauses(inputClauses,id, Quantifier.INTERVAL,intervals,rnd);
            info += "\nintervals:" + intervals;}

        inputClauses.info = info;
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
                    if(found) {--i;} else clause[i] = literal;}}
            inputClauses.addClause(clause);}
    }

    /** generates a description of the generator.
     *
     * @return a description of the generator.
     */
    public String toString() {
        StringBuilder st = new StringBuilder();
        st.append("Random Clause Set Generator\n");
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
        if(exactlies != 0) st.append("exactlies:     ").append(exactlies).append("\n");
        if(intervals != 0) st.append("intervals:     ").append(intervals).append("\n");
        if(ors != 0) st.append("cp-ratio for disjunctions: ").append((float)ors/(float)predicates);
        return st.toString();
    }
}


