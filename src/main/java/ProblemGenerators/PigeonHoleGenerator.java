package ProblemGenerators;

import Datastructures.Clauses.Quantifier;
import Datastructures.Clauses.InputClauses;
import Datastructures.Symboltable;
import Management.Monitor.Monitor;
import Utilities.Interval;
import Utilities.Utilities;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;

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
    /** contains all allowed keys */
    private static final HashSet<String> keys = new HashSet<>();
    static { // these are the allowed keys in the specification.
        Collections.addAll(keys, "generator","pigeons", "holes", "capacities", "quantifiers");
    }

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


    /** returns a help string.
     *
     * @return a help string.
     */
    public static String help() {
        return "Pigeon Hole Generator for putting a number of pigeons into a number or holes\n" +
                "Each pigeon must be put into exactly one hole\n"+
                "Each hole has some capacity for taking pigeons\n"+
                "The keys are:\n"+
                "pigeons:    range of pigeons\n" +
                "holes:      range of holes.\n" +
                "The numbers may be ranges like '4,5,6' or '5 to 10' or '5 to 11 step 2'.\n" +
                "capacities:  comma separated: either [min,max] or <= amout or >= amount or = amount";
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
                                            StringBuilder errors, StringBuilder warnings){
        assert parameters != null;
        String prefix = "Pigeon Hole Generator: ";
        for(String key : parameters.keySet()) {
            if(!keys.contains(key)) {
                warnings.append(prefix).append("Unknown key in parameters: ").append(key).append("\n").
                        append("Allowed keys: ").append(keys).append("\n");}}

        String hole       = parameters.get("holes");
        String pigeon     = parameters.get("pigeons");
        String capacity   = parameters.get("capacities");

        boolean erroneous = false;

        ArrayList holes = null;
        if(hole == null) {
            errors.append(prefix).append("No holes specified.\n"); erroneous = true;}
        else {holes = Utilities.parseIntRange(prefix + "holes: ",hole,errors);
            if(holes == null) {errors.append("\n"); erroneous = true;}}

        ArrayList pigeons = null;
        if(pigeon == null) {
            errors.append(prefix).append("No pigeons specified.\n");}
        else {pigeons = Utilities.parseIntRange(prefix+"pigeons: ",pigeon,errors);
            if(pigeons == null) {errors.append("\n"); erroneous = true;}}

        ArrayList capacities = new ArrayList();
        if(capacity ==null) {capacities.add(new Object[]{Quantifier.EXACTLY,1});}
        else {
            String[] parts = capacity.split("\\s*,\\s*");
            for(int i = 0; i < parts.length; ++i) {
                String part = parts[i].trim();
                if(part.charAt(0) == '[') {
                    if(i == parts.length-1 && !parts[i+1].endsWith("]")) {
                        errors.append("no proper interval: '").append(part).append(",").append(parts[i + 1]).append("'\n"); erroneous = true;}
                    else {Integer max = null;
                        Integer min = Utilities.parseInteger(part.substring(1));
                        if(min == null || min < 0) {
                            errors.append("no proper interval: '").append(part).append(",").append(parts[i + 1]).append("'\n"); erroneous = true;}
                        else {max = Utilities.parseInteger(parts[i+1].substring(0,parts[++i].length()-1));
                            if(max == null || max < min) {
                                errors.append("no proper interval: '").append(parts[i - 1]).append(",").append(parts[i]).append("'\n"); erroneous = true;}}
                        if(!erroneous)
                            capacities.add(new Object[]{Quantifier.INTERVAL,
                                    min < max ? new Interval(min,max) : new Interval(max,min)});}}
                else {
                    String[] quantified = part.split("\\s+");
                    if(quantified.length != 2) {
                        errors.append("malformed quantification: '").append(part).append("\n"); erroneous = true;}
                     Integer amount = Utilities.parseInteger(quantified[1]);
                    if(amount == null){
                        errors.append("malformed quantification: '").append(part).append("\n"); erroneous = true;}
                    else {
                        switch(quantified[0]) {
                            case "<=": capacities.add(new Object[]{Quantifier.ATMOST,amount}); break;
                            case ">=": capacities.add(new Object[]{Quantifier.ATLEAST,amount}); break;
                            case "=":  capacities.add(new Object[]{Quantifier.EXACTLY,amount}); break;
                            default: errors.append("malformed quantification: '").append(part).append("\n"); erroneous = true;}}}}}

        if(erroneous) {return;}

        ArrayList<ArrayList> list = Utilities.crossProduct(holes,pigeons,capacities);
        for(ArrayList<Object> p : list ) {
            int holesv         = (int)p.get(0);
            int pigeonsv       = (int)p.get(1);
            Object[] capacityv = (Object[])p.get(2);
            if(holesv <= 0) {
                errors.append(prefix).append("holes is not positive ").append(holesv); erroneous = true;}
            if(pigeonsv <= 0) {
                errors.append(prefix).append("pigeons is not positive ").append(pigeonsv); erroneous = true;}
            if(erroneous) return;
            generators.add(new PigeonHoleGenerator(holesv,pigeonsv,capacityv));}
}




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
    public InputClauses generateProblem(Monitor errors){
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
