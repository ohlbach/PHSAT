package ProblemGenerators;

import Datastructures.Clauses.InputClauses;
import Datastructures.Clauses.Quantifier;
import Management.Parameter;
import Management.Parameters;
import Utilities.Utilities;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;

/** Generates the Coloring Problem for Pythagoraen Triples.
 *
 * A pythagoraen triple consists of three numbers a,b,c with a^2 + b^2 = c^2. <br>
 "Examples: 3^2 + 4^2 = 5^2  or  5180^2 + 5865^2 = 7825^2 <br>
 "The problem is: is it possible to colour the numbers of all triples up to a given number
 "with two colours such that in each triple two colours are needed. <br>
 "It turns out that this is possible for all triples up to 7824. <br>
 "For the triples up to 7825 (default) at least one triple has to be coloured with one colour. <br><br>
 "There are two parameters: <br>
 "  smallest this the smalles c with a^2 + b^2 = c^2.<br>
 "  largest This is the largest number c with a^2 + b^2 = c^2.<br><br>
 "Each triple a,b,c generates an interval clause [1,2] a,b,c <br>
 "A model for the clauses indicates a possible colouring.
 */
public class PythagoraenTriples extends ProblemGenerator{
    private static final HashSet<String> keys = new HashSet<>();
    static { // these are the allowed keys in the specification.
        Collections.addAll(keys, "generator","maximum");
    }

    private static int minimumDefault = 5;
    private static int maximumDefault = 7825;

    private final int minimum;
    private final int maximum;

    /**Sets the default values for the Pythagoraen Triples Generator.
     *
     * - minimum<br>
     * - maximum<br>
     * The method is called in ProblemGenerator
     *
     * @param defaults the list of default values
     * @return void
     */
    public static void setDefaults(ArrayList<String> defaults) {
        if(defaults == null) {return;}
        try{
            StringBuilder errors = new StringBuilder();
            for(String line : defaults) {
                String[] parts = line.split("\\s*=\\s*",2);
                if(parts.length != 2) {continue;}
                String variable = parts[0];
                String value = parts[1];
                switch(variable.toLowerCase()) {
                    case "minimum":  minimumDefault  = Integer.parseInt(value); break;
                    case "maximum":  maximumDefault  = Integer.parseInt(value); break;
                    }}}
        catch(NumberFormatException e) {
            System.err.println("Error in default Parameters for PigeonHoleGenerator:\n"+e.getMessage());
            System.exit(1);}
    }

    /**Creates and returns a Parameters object for the makeParameter method.
     *
     * Two parameters are created: <br>
     * - minimum, a single integer &ge; 3<br>
     * - maximum, a range of integers. Each individual integer causes a separate set of clauses to be generated.<br>
     * Both parameters are provided with a parser which parses the string to an integer (minimum) or a IntArrayList (maximum).<br>
     * A finalCheck makes sure that minimum &le; maximum.<br><br>
     * The operation parameter actually calls the generator to generate the clauses.
     *
     * @return a Parameters object containing the required parameters and their configurations.
     */
    public static Parameters makeParameter() {
        Parameter minimum = new Parameter("Smallest Number",Parameter.Type.String, Integer.toString(minimumDefault),minimumDefault,
                "smallest z with x^2 + y^2 = z^2");
        minimum.setParser((String min, StringBuilder errors) ->  {
            Integer minInteger = Utilities.parseInteger(null,min,errors);
            if(minInteger == null) {return null;}
            if (minInteger < 5) {errors.append("Smallest Number " + minInteger + " is less than 5"); return null;}
            return minInteger;});
        Parameter maximum = new Parameter("Largest Number",Parameter.Type.String, Integer.toString(maximumDefault),maximumDefault,
                "largest z with x^2 + y^2 = z^2");
        maximum.setParser((String numbers, StringBuilder errors) ->  {
            IntArrayList range = Utilities.parseIntRange(numbers,errors);
            return range;});
        Parameters parameters = new Parameters("PTriples");
        parameters.add(minimum);
        parameters.add(maximum);
        parameters.setDescription("""
                Pythagoraen Triples Coloring Problem:
                A Pythagoraen Triple consists of three numbers a,b,c with a^2 + b^2 = c^2
                Examples: 3^2 + 4^2 = 5^2  or  5180^2 + 5865^2 = 7825^2
                The problem is: is it possible to colour the numbers of all triples up to a given number
                with two colours such that in each triple two colours are needed.
                It turns out that this is possible for all triples up to 7824.
                For the triples up to 7825 (default) at least one triple has to be coloured with one colour.

                There are two parameters:
                 - 'smallest' is the smallest number c with a^2 + b^2 = c^2  (c is atleast 3).
                 - 'largest'  is the largest number c with a^2 + b^2 = c^2.
                 'largest' may specify several different problems by giving comma separated number sequences:
                 for example: 10,20,30 or 10 to 30 or 10 to 30 step 2

                Each triple a,b,c generates an interval clause [1,2] a,b,c
                A model for the clauses indicates a possible colouring.""");

        parameters.setFinalCheck((Parameters params, StringBuilder errors) -> {
            Parameter smallest = params.parameters.get(0);
            Parameter largest = params.parameters.get(1);
            if(smallest.value == null || largest.value == null) {return true;}
            int mi = (Integer) smallest.value;
            int ma = ((IntArrayList) largest.value).getInt(0);
            if(mi > ma) {
                errors.append("Smallest value > first largest value: "+ mi + " > " + ma);
                return false;}
            return true;});

        parameters.setOperation((Parameters params, StringBuilder errors) -> {
            ArrayList<ProblemGenerator> generators = new ArrayList<>();
            makeProblemGenerator(params, generators);
            ArrayList<InputClauses> clauses = new ArrayList<>();
            for(ProblemGenerator generator : generators) {
                clauses.add(generator.generateProblem(errors));}
            return clauses;});

        return parameters;}


    /** constructs the generator for the triple coloring problem.
     *
     * @param minimum   the smallest c for the triples a^2 + b^2 = c^2.
     * @param maximum   the largest c for the triples a^2 + b^2 = c^2.
     */
    public PythagoraenTriples(int minimum, int maximum) {
        this.minimum = minimum;
        this.maximum = maximum;}

    /** generates for a range of pigeons and a range of holes a sequence of pigeonhole specifications.
     * The pigeons and holes may be ranges like '4,5,6' or '5 to 10' or '5 to 11 step 2'.
     *
     * @param parameters  contains a HashMap with keys "pigeons" and "holes" and "capacity" and "quantifier".
     * @param generators  for adding new problem generators.
     * @param errors      for error messages.
     * @param warnings    for warnings.
     */
    public static void makeProblemGenerator(HashMap<String,String> parameters,
                                            ArrayList<ProblemGenerator> generators,
                                            StringBuilder errors, StringBuilder warnings) {
        assert parameters != null;
        String prefix = "Pythagoraen Triples Generator: ";
        for (String key : parameters.keySet()) {
            if (!keys.contains(key)) {
                warnings.append(prefix).append("Unknown key in parameters: ").append(key).append("\n").
                        append("Allowed keys: ").append(keys).append("\n");
            }
        }
        String maxString = parameters.get("maximum");
        Integer maximum;
        if(maxString != null) {
            maximum = Utilities.parseInteger(prefix + "maximum: ",maxString,errors);
            if(maximum == null) return;}
        else maximum = 7825;
        generators.add(new PythagoraenTriples(3,maximum));
    }

    /**
     * Creates the problem generators for Pythagorean triples based on the given parameters.
     *
     * @param parameters  the parameters required to configure the problem generator
     * @param generators  the list of problem generators to add the new Pythagorean triples generator to
     */
    public static void makeProblemGenerator(Parameters parameters,
                                            ArrayList<ProblemGenerator> generators) {
        assert parameters != null;
        int smallest = (int)parameters.parameters.get(0).value;
        IntArrayList largest = (IntArrayList)parameters.parameters.get(1).value;
        for(int maximum : largest)
            generators.add(new PythagoraenTriples(smallest,maximum));}


    /** generates the clauses for the problem.
     *
     * Example for maximum = 17:<br>
     * [1,2] 3,4,5   <br>
     * [1,2] 6,8,10  <br>
     * [1,2] 5,12,13 <br>
     * [1,2] 9,12,15 <br>
     * [1,2] 8,15,17
     *
     * @param errors no effect
     * @return the new InputClauses
     * */
    @Override
    public InputClauses generateProblem(StringBuilder errors) {
        int quantifier = Quantifier.INTERVAL.ordinal();
        String problemName = "PT_" + minimum + "-"+maximum;
        int identifier = 0;
        String info = "Colouring Pythagoraen Triples from "+ minimum + " up to " + maximum;
        inputClauses = new InputClauses(problemName,maximum,null,info);
        for(int i = minimum; i <= maximum; ++i) {
            int i2 = i*i;
            for(int p1 = 2; p1 < i; ++p1) {
                int p1s = p1 * p1;
                int p2 = sqrt(i2 - p1s);
                if (p2 == 0) continue;
                if (p2 < p1) break;
                if(p1*p1 + p2*p2 != i2) {errors.append("No pythagoraen triple: " +p1+ ", " + p2 + ", " + i); continue;}
                int[] clause = new int[7];
                clause[0] = ++identifier;
                clause[1] = quantifier;
                clause[2] = 1;
                clause[3] = 2;
                clause[4] = p1;
                clause[5] = p2;
                clause[6] = i;
                inputClauses.addClause(clause);}}
        inputClauses.nextId = ++identifier;
        return inputClauses;}

    /** computes the integer square root of an integer.
     *
     * @param n any integer > 0
     * @return 0 (sqrt is no integer) or the integer sqaure root.
     */
    private static int sqrt(int n) {
        assert n > 0;
        double rootd = Math.sqrt(n);
        int rooti = Math.round((float)rootd);
        if(rooti*rooti == n) return rooti;
        rooti += 1;
        if(rooti*rooti == n) return rooti;
        rooti -= 2;
        if(rooti*rooti == n) return rooti;
        return 0;
    }
    /** a string representation of the parameters
     *
     * @return a string representation of the parameters
     */
    public String toString() {
        return "Colouring Pythagoraen Triples generator for triples from " + minimum + " up to " + maximum;}
}

