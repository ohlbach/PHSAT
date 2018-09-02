package Generators;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseList;
import Datastructures.Model;
import Datastructures.Status;
import Generators.ClauseGenerator;
import Generators.ClauseSetGenerator;

import java.util.ArrayList;
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
