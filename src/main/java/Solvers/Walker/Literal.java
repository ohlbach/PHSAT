package Solvers.Walker;

/** A literal object contains the literal itself and is part of a clause and a literal index.
 * The literal index is a doubly connected list.
 * The Literal object has no active methods.
 */
public class Literal extends Datastructures.Literal<Clause> {

    protected int flipScorePart;

    /** constructs a Literal object
     *
     * @param literal      the literal itself
     * @param multiplicity the number of occurrences in a quantified clause.
     */
    public Literal(int literal, int multiplicity) {
        super(literal,multiplicity);}

    /** clones the literal, except previousLiteral and nextLiteral.
     *
     * @param literal a new literal.
     * @return the cloned literal.
     */
    public Literal clone(int literal) {
        Literal newLiteral = new Literal(literal,multiplicity);
        newLiteral.flipScorePart = flipScorePart;
        return newLiteral;}

}

