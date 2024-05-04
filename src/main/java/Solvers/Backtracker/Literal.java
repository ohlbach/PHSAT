package Solvers.Backtracker;

public class Literal extends Datastructures.Literal<Clause> {

    /** constructs a Literal object
     *
     * @param literal      the literal itself
     * @param multiplicity the number of occurrences in a quantified clause.
     */
    public Literal(int literal, int multiplicity) {
        super(literal,multiplicity);}


}

