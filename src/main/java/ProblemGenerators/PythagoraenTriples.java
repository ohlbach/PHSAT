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

/** A pythagoraen triple consists of three numbers a,b,c with a^2 + b^2 = c^2. <br>
 "Examples: 3^2 + 4^2 = 5^2  or  5180^2 + 5865^2 = 7825^2 <br>
 "The problem is: is it possible to colour the numbers of all triples up to a given number
 "with two colours such that in each triple two colours are needed. <br>
 "It turns out that this is possible for all triples up to 7824. <br>
 "For the triples up to 7825 (default) at least one triple has to be coloured with one colour. <br>
 "There is just one parameter: <br>
 "  maximium This is the largest number c with a^2 + b^2 = c^2.<br>
 "Each triple a,b,c generates an interval clause [1,2] a,b,c <br>
 "A model for the clauses indicates a possible colouring.
 */
public class PythagoraenTriples extends ProblemGenerator{
    private static final HashSet<String> keys = new HashSet<>();
    static { // these are the allowed keys in the specification.
        Collections.addAll(keys, "generator","maximum");
    }

    public static Parameters makeParameter() {
        Parameter minimum = new Parameter("Smallest Number",Parameter.Type.String, "3",3,
                "smallest z with x^2 + y^2 = z^2");
        minimum.setParser((String min, StringBuilder errors) ->  {
            Integer minInteger = Utilities.parseInteger(null,min,errors);
            if(minInteger == null) {return null;}
            if (minInteger < 3) {errors.append("Smallest Number " + minInteger + " is less than 3"); return null;}
            return minInteger;});
        Parameter maximum = new Parameter("Largest Number",Parameter.Type.String, "7825",
                IntArrayList.wrap(new int[]{7825}),
                "largest z with x^2 + y^2 = z^2");
        maximum.setParser((String numbers, StringBuilder errors) ->  {
            IntArrayList range = Utilities.parseIntRange(numbers,errors);
            if(range == null) return null;

            return range;});
        Parameters parameters = new Parameters("PTriples");
        parameters.add(minimum);
        parameters.add(maximum);
        parameters.setDescription(
                "A Pythagoraen Triple consists of three numbers a,b,c with a^2 + b^2 = c^2\n"+
                        "Examples: 3^2 + 4^2 = 5^2  or  5180^2 + 5865^2 = 7825^2\n"+
                        "The problem is: is it possible to colour the numbers of all triples up to a given number\n"+
                        "with two colours such that in each triple two colours are needed.\n"+
                        "It turns out that this is possible for all triples up to 7824.\n"+
                        "For the triples up to 7825 (default) at least one triple has to be coloured with one colour.\n"+
                        "There is just one parameter:\n"+
                        "  maximium This is the largest number c with a^2 + b^2 = c^2.\n"+
                        "Each triple a,b,c generates an interval clause [1,2] a,b,c\n"+
                        "A model for the clauses indicates a possible colouring.");

        parameters.setFinalCheck((Parameters params, StringBuilder errors) -> {
            Parameter smallest = params.parameters.get(0);
            Parameter largest = params.parameters.get(1);
            if(smallest.value == null || largest.value == null) {return true;}
            int mi = (Integer) smallest.value;
            int ma = ((IntArrayList) largest.value).get(0);
            if(mi > ma) {
                errors.append("Smallest value > first largest value: "+ mi + " > " + ma);
                return false;}
            return true;});
        return parameters;}

    private final int minimum;
    private final int maximum;

    /** creates the generator.
     *
     * @param maximum   the largest c for the triples a^2 + b^2 = c^2.
     */
    public PythagoraenTriples(int minimum, int maximum) {
        this.minimum = maximum;
        this.maximum = maximum;
    }

    /** returns a help string.
     *
     * @return a help string.
     */
    public static String help() {
        return "A pythagoraen triple consists of three numbers a,b,c with a^2 + b^2 = c^2\n"+
                "Examples: 3^2 + 4^2 = 5^2  or  5180^2 + 5865^2 = 7825^2\n"+
                "The problem is: is it possible to colour the numbers of all triples up to a given number\n"+
                "with two colours such that in each triple two colours are needed.\n"+
                "It turns out that this is possible for all triples up to 7824.\n"+
                "For the triples up to 7825 (default) at least one triple has to be coloured with one colour.\n"+
                "There is just one parameter:\n"+
                "  maximium This is the largest number c with a^2 + b^2 = c^2.\n"+
                "Each triple a,b,c generates an interval clause [1,2] a,b,c\n"+
                "A model for the clauses indicates a possible colouring." ;
    }

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

    public static void makeProblemGenerator(Parameters parameters,
                                            ArrayList<ProblemGenerator> generators) {
        assert parameters != null;
        String prefix = "Pythagoraen Triples Generator: ";
        int smallest = (int)parameters.parameters.get(0).value;
        IntArrayList largest = (IntArrayList)parameters.parameters.get(1).value;
        for(int maximum : largest)
            generators.add(new PythagoraenTriples(smallest,maximum));
    }


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
        String problemName = "PT_" + maximum;
        int identifier = 0;
        String info = "Colouring Pythagoraen Triples up to " + maximum;
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
        double rootd = Math.sqrt((double) n);
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
        return "Colouring Pythagoraen Triples generator up to " + maximum;}
}

