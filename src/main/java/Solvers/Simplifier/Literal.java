package Solvers.Simplifier;

import Datastructures.Symboltable;

/** A literal object contains the literal itself and is part of a clause and a literal index.
 * The literal index is a doubly connected list.
 * The Literal object has no active methods.
 */
public class Literal {
    /** the literal itself */
    protected int literal;
    /** the number of occurrences in a quantified clause. */
    protected int multiplicity;
    /** the clause containing the literal */
    protected Clause clause;
    /** pointer to the previous literal in a doubly connected list */
    protected Literal previousLiteral;
    /** pointer to the next literal in a doubly connected list */
    protected Literal nextLiteral;

    /** constructs a Literal object
     *
     * @param literal      the literal itself
     * @param multiplicity the number of occurrences in a quantified clause.
     */
    public Literal(int literal, int multiplicity) {
        this.literal = literal;
        this.multiplicity = multiplicity;}

    /** returns a string representation of the literal: for example 3^2
     * @return a tring representation of the literal. */
    public String toString() {
        return toString(null);}

    /** returns a string representation of the literal: for example p^2.
     *
     * @param symboltable the symboltable.
     * @return a string representation of the literal.
     */
    public String toString(Symboltable symboltable) {
        String lit = Symboltable.toString(literal,symboltable);
        return (multiplicity == 1) ? lit : lit+"^"+multiplicity;}
}
