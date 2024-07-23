package ProblemGenerators;

import Datastructures.Clauses.InputClauses;
import Datastructures.Clauses.Quantifier;
import Datastructures.Symboltable;
import Management.Parameter;
import Management.Parameters;
import Datastructures.ValueType;
import Utilities.Interval;
import Utilities.Utilities;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

import static Utilities.Utilities.toArrayList;

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

    /** the default value for the pigeons, to be overwritten by setDefaults */
    private static int pigeonsDefault = 3;

    /** the number of pigeons */
    private final int pigeons;

    /** the default value for the holes, to be overwritten by setDefaults */
    private static int holesDefault = 3;
    /** the number of holes */
    private final int holes;

    /** the default value for the capacity, to be overwritten by setDefaults */
    private static ArrayList<Object[]> capacityDefault = new ArrayList<>();

    static {capacityDefault.add(new Object[]{Quantifier.EXACTLY,1});}
    /** the capacity of the holes [connective,amount] */
    private final Object[] capacity;


    /**Sets the default values for the PigeonHoleGenerator.
     *
     * - pigeons<br>
     * - holes<br>
     * - capacity<br>
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
                    case "pigeons":  pigeonsDefault  = Integer.parseInt(value); break;
                    case "holes":    holesDefault    = Integer.parseInt(value) ; break; // nicht optimal
                    case "capacity": capacityDefault = (ArrayList<Object[]>)new ValueType.Quantifications(true,1,Integer.MAX_VALUE).parseValue(value,errors);
                        if(!errors.isEmpty()) {
                            System.err.println("Default Parameters for PigeonHoleGenerator");
                            System.err.println(errors.toString());
                            System.exit(1);}}}}
        catch(NumberFormatException e) {
            System.err.println("Error in default Parameters for PigeonHoleGenerator:\n"+e.getMessage());
            System.exit(1);}
    }

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
        StringBuilder errors = new StringBuilder();
        Parameter selected = new Parameter("Select", Parameter.DisplayType.Button,
                new ValueType.Booleans(),false,
                "Select the Pigeon Hole Clause Set Generator");

        Parameter pigeons = new Parameter("Pigeons", Parameter.DisplayType.String,
                new ValueType.Integers(3,Integer.MAX_VALUE,true),pigeonsDefault,
                "Number of pigeons (atleast 3)");

        Parameter holes = new Parameter("Holes", Parameter.DisplayType.String,
                new ValueType.Integers(3,Integer.MAX_VALUE,true),holesDefault,
                "Number of holes (atleast 2)");
        Parameter capacity = new Parameter("Capacity", Parameter.DisplayType.String,
                new ValueType.Quantifications(true,1,Integer.MAX_VALUE),capacityDefault,
                """
                        HoleCapacity, i.e. number of pigeons per hole.
                        Comma separated: either [min,max] or < or <= amout or > or >= amount or just amount
                        Examples: '2' (exactly 2) or '[1,2], 3' (either one or two, and exactly 3)
                        Each alternative generates a new clause set.""");

        Parameters parameters = new Parameters("Pigeon Hole Generator");
        parameters.add(selected);
        parameters.add(pigeons);
        parameters.add(holes);
        parameters.add(capacity);
        parameters.setDescription("""
                Pigeon Hole Problem:
                Can one put a number of pigeons into a given number or holes?
                Each pigeon must be put into exactly one hole.
                Each hole has a certain capacity for taking pigeons.
                The parameters are:
                pigeons:    range of pigeons
                holes:      range of holes.
                The ranges may be like '4,5,6' or '5 to 10' or '5 to 11 step 2'.
                capacities:  comma separated: either [min,max] or < or <= amout or > or >= amount or just amount.
                             Example: 2,[1,2],>3,<= 5""");

        parameters.setOperation((Parameters params, StringBuilder errorss) -> {
            ArrayList<ProblemGenerator> generators = new ArrayList<>();
            makeProblemGenerators(params, generators);
            ArrayList<InputClauses> clauses = new ArrayList<>();
            for(ProblemGenerator generator : generators) {
                clauses.add(generator.generateProblem(errorss));}
            return clauses;});
        if(!errors.isEmpty()) {
            System.err.println("PigeonHoleGenerator: Errors in makeParameter:\n"+ errors);
            System.exit(1);}
        return parameters;}

    /**
     * Generates and adds new problem generators based on the provided parameters.
     *
     * @param parameters The parameters containing the values necessary to create the problem generators.
     * @param generators The list of problem generators to add the newly created generators to.
     */
    public static void makeProblemGenerators(Parameters parameters, ArrayList<ProblemGenerator> generators) {
        int i = 0;
        IntArrayList pigeons = (IntArrayList) parameters.parameters.get(++i).value;
        IntArrayList holes = (IntArrayList) parameters.parameters.get(++i).value;
        ArrayList capacities = (ArrayList) parameters.parameters.get(++i).value;
        for (ArrayList p : (ArrayList<ArrayList>) Utilities.crossProduct(toArrayList(holes), toArrayList(pigeons), capacities)) {
            int holesv = (int) p.get(0);
            int pigeonsv = (int) p.get(1);
            Object[] capacityv = (Object[]) p.get(2);
            generators.add(new PigeonHoleGenerator(holesv, pigeonsv, capacityv));
        }
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
    public InputClauses generateProblem(StringBuilder errors){
        Quantifier quantifier = (Quantifier)capacity[0];
        String problemName = "PH_p"+pigeons+"_h"+holes;
        int min=0, max=0, amount=0;
        String capcity = capacityASCIIString(capacity);
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

    /** turns the capacity into an ASCII string */
    private static String capacityASCIIString(Object[] capacity) {
        Quantifier quantifier = (Quantifier)capacity[0];
        if(quantifier == Quantifier.INTERVAL) {
            Interval interval = (Interval)capacity[1];
            return "i" + interval.min + "_" + interval.max;}
        else return Quantifier.asciiName(quantifier)+(int)capacity[1];}

    /** turns the capacity into a string */
    private static String capacityString(Object[] capacity) {
        Quantifier quantifier = (Quantifier)capacity[0];
        if(quantifier == Quantifier.INTERVAL) {
            Interval interval = (Interval)capacity[1];
            return "[" + interval.min + "," + interval.max+"]";}
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
