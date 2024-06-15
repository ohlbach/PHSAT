package Solvers.Backtracker;

public class Literal extends Datastructures.Literal<Clause> {

    protected int timestamp = 0;

    /** constructs a Literal object
     *
     * @param literal      the literal itself
     * @param multiplicity the number of occurrences in a quantified clause.
     */
    public Literal(int literal, int multiplicity) {
        super(literal,multiplicity);}


}

