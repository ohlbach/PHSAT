package Generators;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseList;
import Datastructures.Model;
import Datastructures.Status;
import Generators.ClauseGenerator;
import Generators.ClauseSetGenerator;
import Utilities.Utilities;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Random;

/**
 * Created by ohlbach on 26.08.2018.
 *
 * This generator is for generating clause sets with randomly generated literals
 */
public class RandomClauseSetGenerator extends ClauseSetGenerator {
    private int seed;
    private int predicates;
    private int numberClauses;
    private int maxClauseLength;
    boolean precise;

    public static HashMap<String,Object> parseSingleParameters(HashMap<String,String> parameters, StringBuffer errors, StringBuffer warnings){
        HashMap<String,Object> control = new HashMap<>();
        String value = parameters.get("seed");
        control.put("seed",value == null ? 0 : Utilities.parseInteger("RandomClauseSetGenerator seed",value,errors));
        value = parameters.get("predicates");
        if(value == null) {errors.append("RandomClauseSetGenerator: no number of predicates defined.");}
        else {control.put("seed",value == null ? 0 : Utilities.parseInteger("RandomClauseSetGenerator predicates",value,errors));}
        value = parameters.get("clauses");
        if(value == null) {errors.append("RandomClauseSetGenerator: no number of clauses defined.");}
        else {control.put("clauses",value == null ? 0 : Utilities.parseInteger("RandomClauseSetGenerator clauses",value,errors));}
        value = parameters.get("length");
        if(value == null) {errors.append("RandomClauseSetGenerator: no number of clauses defined.");}
        else {control.put("length",value == null ? 0 : Utilities.parseInteger("RandomClauseSetGenerator length",value,errors));}
        value = parameters.get("precise");
        control.put("precise",value != null);
        return control;}


    public static ArrayList<HashMap<String,Object>> parseSeriesParameters(HashMap<String,String> parameters, StringBuffer errors, StringBuffer warnings){

        String seed       = parameters.get("seed");
        String predicate = parameters.get("predicates");
        String clause    = parameters.get("clauses");
        String length     = parameters.get("length");
        boolean precise    = parameters.get("precise") != null;

        ArrayList<Integer> seeds = null;
        if(seed == null) {seeds = new ArrayList<>(); seeds.add(0);}
        else {seeds = Utilities.parseRange("RandomClauseSetGenerator seed",seed,errors);}

        ArrayList<Integer> predicates = null;
        if(predicate == null) {errors.append("RandomClauseSetGenerator: no number of predicates defined.");}
        else {predicates = Utilities.parseRange("RandomClauseSetGenerator predicate",predicate,errors);}

        ArrayList<Integer> clauses = null;
        if(clause == null) {errors.append("RandomClauseSetGenerator: no number of clauses defined.");}
        else {clauses = Utilities.parseRange("RandomClauseSetGenerator predicate",clause,errors);}

        ArrayList<Integer> lengths = null;
        if(length == null) {errors.append("RandomClauseSetGenerator: no number of clauses defined.");}
        else {lengths = Utilities.parseRange("RandomClauseSetGenerator length",length,errors);}

        if(seeds == null || predicates == null || clauses == null || length == null) {return null;}

        ArrayList<HashMap<String,Object>> control = new ArrayList<>();
        for(ArrayList<Integer> values : Utilities.crossProduct(seeds,predicates,clauses,lengths)) {
            HashMap<String,Object> cntr = new HashMap<>();
            cntr.put("seed",cntr.get(0));
            cntr.put("predicates",cntr.get(1));
            cntr.put("clauses",cntr.get(2));
            cntr.put("length",cntr.get(3));
            cntr.put("precise", precise);
            control.add(cntr);}
        return control;}





        /** constructs a ClauseSetGenerator for randomly generated clauses.
         * Double literals and tautologies are not created.
         * Unit clauses are put into the initial model and used to simplify the clauses (forward).
         *
         * @param clauseGenerator for generating clauses
         * @param seed            for initialising the random number generator
         * @param predicates      the number of predicates
         * @param numberClauses   the number of clauses to be generated
         * @param maxClauseLength the maximum number of literals in the clause
         * @param precise         if true then clauses with double literals are filled up.
         */
    public RandomClauseSetGenerator(ClauseGenerator clauseGenerator, int seed,
                                    int predicates, int numberClauses, int maxClauseLength, boolean precise) {
        this.clauseGenerator = clauseGenerator;
        this.seed = seed;
        this.predicates = predicates;
        this.numberClauses = numberClauses;
        this.maxClauseLength = maxClauseLength;
        this.precise = precise;}

    /** generates the clause set
     *
     * @return the Status object with all the information about the clause set.
     */
    public Status generate() {
        ArrayList<Integer> literals = new ArrayList();
        Status stat = new Status();
        stat.seed = seed;
        Model model = new Model(predicates);
        clauseList = new ClauseList(numberClauses,model,null);
        clauseList.info = "Randomly generated clauses with seed " + seed;
        stat.clauseList = clauseList;
        Random rnd = new Random(seed);
        int clauseCounter = 1;
        while(clauseList.size() < numberClauses) {
            literals.clear();
            while(literals.size() < maxClauseLength) {
                int literal = rnd.nextInt(predicates)+1;
                if(rnd.nextBoolean()) {literal = -literal;}
                if(literals.contains(-literal)) {literals.clear();continue;}
                if(precise && literals.contains(literal)) {continue;}
                literals.add(literal);}
            int status = simplify(literals,model);
            if(status == -1) {
                stat.unsatisfiable = true;
                stat.falseClause = literals.toString();
                return stat;}
            if(status == 0) {
                Clause clause = clauseGenerator.newClause(clauseCounter++,literals);
                clauseList.addClause(clause);}}
        stat.toBeExamined = true;
        return stat;}
}
