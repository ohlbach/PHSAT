package ProblemGenerators;

import Datastructures.Clauses.InputClauses;
import Datastructures.Clauses.Quantifier;
import Datastructures.Symboltable;
import Management.Parameter;
import Management.Parameters;
import Utilities.Interval;
import Utilities.Utilities;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

/**
 * Created by ohlbach on 09.09.2018.<br>
 *
 * This class is for generating variations of pigeonhole problems of arbitrary size.<br>
 * There are n holes and m pigeons <br>
 * Each pigeon must be placed into exactly one hole. <br>
 * Each hole, however, may have different capacities: <br>
 *   atleast n pigeons may be put into a hole <br>
 *   atmost  n pigeons may be put into a hole <br>
 *   exactly n pigeons may be put into a hole.<br>
 *   [min,max] pigeons may be put into a hole.
 */
public final class PigeonHoleGenerator extends ProblemGenerator {

    /** the number of pigeons */
    private final int pigeons;

    /** the number of holes */
    private final int holes;

    /** the capacity of the holes [connective,amount] */
    private final Object[] capacity;

    /** creates the generator.
     *
     * @param pigeons   the number of pigeons.
     * @param holes     the number of holes.
     * @param capacity  the number of pigeons in each hole.
     */
    public PigeonHoleGenerator(int pigeons, int holes, Object[] capacity) {
        this.pigeons = pigeons;
        this.holes = holes;
        this.capacity = capacity;
    }

    /**
     * Creates a Parameters object with pre-defined parameters pigeons,holes,capacity
     *
     * @return the created Parameters object
     */
    public static Parameters makeParameter() {
        Parameter pigeons = new Parameter("Pigeons",Parameter.Type.String, "3",
                IntArrayList.wrap(new int[]{3}),
                "Number of pigeons (atleast 3)");
        pigeons.setParser((String pigeonString, StringBuilder errors) ->  {
            IntArrayList pigeonRange = Utilities.parseIntRange(pigeonString,errors);
            if(pigeonRange == null) {return null;}
            if (pigeonRange.getInt(0) < 3) {
                errors.append("There should be at least 3 pigeons " + pigeonRange.toString()); return null;}
            return pigeonRange;});

        Parameter holes = new Parameter("Holes",Parameter.Type.String, "2",
                IntArrayList.wrap(new int[]{2}),
                "Number of holes (atleast 2)");
        holes.setParser((String numbers, StringBuilder errors) ->  {
            IntArrayList holesRange = Utilities.parseIntRange(numbers,errors);
            if(holesRange == null) return null;
            if (holesRange.getInt(0) < 2) {
                errors.append("There should be at least 2 holes " + holesRange.toString()); return null;}
            return holesRange;});
        Parameter capacity = new Parameter("Capacity",Parameter.Type.String,"1",
                parseCapacity("1",new StringBuilder()),
                "HoleCapacity, i.e. number of pigeons per hole.\n" +
                        "Comma separated: either [min,max] or < or <= amout or > or >= amount or just amount\n"+
                        "Examples: '2' (exactly 2) or '[1,2], 3' (either one or two, and exactly 3)\n"+
                        "Each alternative generates a new clause set.");
        capacity.setParser((String cap, StringBuilder errors) ->  {return parseCapacity(cap,errors);});

        Parameters parameters = new Parameters("PigeonHoles");
        parameters.add(pigeons);
        parameters.add(holes);
        parameters.add(capacity);
        parameters.setDescription("Pigeon Hole Problem:\n" +
                "Can one put a number of pigeons into a given number or holes?\n" +
                "Each pigeon must be put into exactly one hole.\n"+
                "Each hole has a certain capacity for taking pigeons.\n"+
                "The parameters are:\n"+
                "pigeons:    range of pigeons\n" +
                "holes:      range of holes.\n" +
                "The ranges may be like '4,5,6' or '5 to 10' or '5 to 11 step 2'.\n" +
                "capacities:  comma separated: either [min,max] or < or <= amout or > or >= amount or just amount.\n"+
                "             Example: 2,[1,2],>3,<= 5");

        parameters.setOperation((Parameters params, StringBuilder errors) -> {
            ArrayList<ProblemGenerator> generators = new ArrayList<>();
            makeProblemGenerator(params, generators);
            ArrayList<InputClauses> clauses = new ArrayList<>();
            for(ProblemGenerator generator : generators) {
                clauses.add(generator.generateProblem(errors));}
            return clauses;});
        return parameters;}

    /**
     * Parses the capacity string and returns an ArrayList of Object arrays representing the parsed capacities.
     * If there are any errors during parsing, they are appended to the provided StringBuilder.
     *
     * @param capacity the capacity string to parse
     * @param errors a StringBuilder to append any errors encountered during parsing
     * @return an ArrayList of Object arrays representing the parsed capacities
     */
    public static ArrayList<Object[]> parseCapacity(String capacity, StringBuilder errors) {
        ArrayList<Object[]> capacities = new ArrayList<>();
        capacity = capacity.trim();
        try{int cap = Integer.parseInt(capacity);
            capacities.add(new Object[]{Quantifier.EXACTLY, cap});
            return capacities;}
        catch(NumberFormatException ignored) {}

        String[] parts = capacity.split("\\s*,\\s*");
        int length = parts.length;
        boolean erraneous = false;
        for(int i = 0; i < length; i++) {
            String part = parts[i];
            try{int cap = Integer.parseInt(part);
                capacities.add(new Object[]{Quantifier.EXACTLY, cap});
                continue;}
            catch(NumberFormatException ignored) {}
            if(part.charAt(0) == '[') {
                if(i == length-1) {errors.append("malformed interval in " + capacity); return null;}
                if (!parts[i+1].endsWith("]")) {errors.append("malformed interval in " + capacity);
                    erraneous = true; i +=2; continue;}
                try{
                    int minCapacity = Integer.parseInt(part.substring(1));
                    int maxCapacity = Integer.parseInt(parts[i+1].substring(0, parts[i+1].length() - 1));
                        capacities.add(new Object[]{Quantifier.INTERVAL, new Interval(minCapacity, maxCapacity)});}
                catch(NumberFormatException ex) {
                    errors.append("malformed interval in " + capacity); erraneous = true; i +=2; continue;}
                ++i;
                continue;}
            try{
                int limit;
                switch(part.charAt(0)) {
                    case '<':
                        if(part.charAt(1) == '=') {limit = Integer.parseInt(part.substring(2).trim());}
                        else {limit = Integer.parseInt(part.substring(1).trim())-1;}
                        if(limit < 1) {
                            errors.append("malformed interval in " + capacity); erraneous = true; continue;}
                        capacities.add(new Object[]{Quantifier.ATMOST,limit});
                        break;

                    case '>':
                        if(part.charAt(1) == '=') {limit = Integer.parseInt(part.substring(2).trim());}
                        else {limit = Integer.parseInt(part.substring(1).trim())+1;}
                        if(limit < 1) {
                            errors.append("malformed interval in " + capacity); erraneous = true; continue;}
                        capacities.add(new Object[]{Quantifier.ATLEAST,limit});
                        break;

                    default: errors.append("malformed interval in " + capacity); erraneous = true;
                }}
            catch(Exception ex) {
                errors.append("malformed capacity in " + capacity); erraneous = true; i +=2;
            }}
        return erraneous ? null : capacities;
    }

    /**
     * Generates and adds new problem generators based on the provided parameters.
     *
     * @param parameters The parameters containing the values necessary to create the problem generators.
     * @param generators The list of problem generators to add the newly created generators to.
     */
    public static void makeProblemGenerator(Parameters parameters,ArrayList<ProblemGenerator> generators) {
        IntArrayList pigeons =    (IntArrayList)parameters.parameters.get(0).value;
        IntArrayList holes =      (IntArrayList)parameters.parameters.get(1).value;
        ArrayList capacities = (ArrayList)parameters.parameters.get(2).value;
        for(ArrayList<Object> p : (ArrayList<ArrayList>)Utilities.crossProduct(toArrayList(holes),toArrayList(pigeons),capacities)) {
            int holesv         = (int)p.get(0);
            int pigeonsv       = (int)p.get(1);
            Object[] capacityv = (Object[])p.get(2);
            generators.add(new PigeonHoleGenerator(holesv,pigeonsv,capacityv));}}


    /**
     * Converts an IntArrayList to an ArrayList<Integer>.
     *
     * @param list the IntArrayList to convert
     * @return an ArrayList<Integer> representing the same elements as the input IntArrayList
     */
    private static ArrayList toArrayList(IntArrayList list) {
        ArrayList intList = new ArrayList<>();
        for(int i : list) intList.add(i);
        return intList;}





    /** generates the clauses for the problem.
     *  Example: 3 pigeons, 3 holes, capacity atmost 2<br>
     *  exactly 1 P1H1, P1H2, P1H3<br>
     *  exactly 1 P2H1, P2H2, P2H3<br>
     *  exactly 1 P3H1, P3H2, P3H3<br>
     *  <br>
     *  atmost 2 P1H1, P2H1, P3H1<br>
     *  atmost 2 P1H2, P2H2, P3H2<br>
     *  atmost 2 P1H3, P2H3, P3H3<br>
     *
     * @param errors    no effect
     * @return the new InputClauses
     */
    public InputClauses generateProblem(StringBuilder errors){
        Quantifier quantifier = (Quantifier)capacity[0];
        String problemName = "PH_p"+pigeons+"_h"+holes;
        int min=0, max=0, amount=0;
        String capcity = capacityString(capacity);
        problemName += "_c"+capcity;
        if(quantifier == Quantifier.INTERVAL) {
            Interval interval = (Interval)capacity[1];
            min = interval.min;
            max = interval.max;}
        else {amount = (int)capacity[1];}

        Symboltable symboltable = new Symboltable(pigeons*holes);
        int predicates = 0;
        for(int pigeon = 1; pigeon <= pigeons; ++pigeon) {
            for(int hole = 1; hole <= holes; ++hole) {
                symboltable.setName(++predicates,"P"+pigeon+"H"+hole);}}

        String info = "Pigeon Hole example with " + pigeons + " pigeons in " + holes + " holes\n"+
                    "capacity: "  + " " + capcity + " pigeons in a hole.";
        inputClauses = new InputClauses(problemName,predicates,symboltable,info);

        // each hole can take a number of pigeons.
        int id = 0;
        int extra = quantifier == Quantifier.INTERVAL ? 4 : 3;

        for(int hole = 1; hole <= holes; ++hole) {
            int[] clause = new int[pigeons+extra];
            clause[0] = ++id;
            clause[1] = quantifier.ordinal();
            int index = 2;
            if(quantifier == Quantifier.INTERVAL) {
                clause[2] = min;
                clause[3] = max;
                index = 3;}
            else clause[2] = amount;
            for(int pigeon = 1; pigeon <= pigeons; ++pigeon) {
                clause[++index] = symboltable.getPredicate("P"+pigeon+"H"+hole);}
            inputClauses.addClause(clause);}

        // a pigeon can be put in exactly one hole
        for(int pigeon = 1; pigeon <= pigeons; ++pigeon){
            int[] clause = new int[holes+3];
            clause[0] = ++id;
            clause[1] = Quantifier.EXACTLY.ordinal();
            clause[2] = 1;
            for(int hole = 1; hole <= holes; ++hole) {
                clause[hole+2] = symboltable.getPredicate("P"+pigeon+"H"+hole);}
            inputClauses.addClause(clause);}

        inputClauses.nextId = ++id;
        return inputClauses;
    }

    /** turns the capacity into a string */
    private static String capacityString(Object[] capacity) {
        Quantifier quantifier = (Quantifier)capacity[0];
        if(quantifier == Quantifier.INTERVAL) {
            Interval interval = (Interval)capacity[1];
            return "[" + interval.min + "," + interval.max +"]";}
        else return quantifier.abbreviation+(int)capacity[1];}


    /** a string representation of the parameters
     *
     * @return a string representation of the parameters
     */
    public String toString() {
        return "Pigeon hole generator with " + pigeons + " pigeons in " + holes +
                " holes\n  capacity: " + capacityString(capacity) +
                " pigeons in a hole.";}

    }
