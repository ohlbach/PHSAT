package Solvers.Resolution;

import Datastructures.Symboltable;

import java.util.ArrayList;

/** A literal object contains the literal itself and is part of a clause and a literal index.
 * <br>
 * The literal index is a doubly connected list. <br>
 * The Literal object has no active methods.
 */
public class Literal {
    /** the literal itself. */
    int literal;

    /** the number of occurrences in a quantified clause. */
    int multiplicity;

    /** the clause containing the literal. */
    Clause clause;

    /** pointer to the previous literal in a doubly connected list. */
    Literal previousLiteral;

    /** pointer to the next literal in a doubly connected list. */
    Literal nextLiteral;

    /** constructs a Literal object
     *
     * @param literal      the literal itself
     * @param multiplicity the number of occurrences in a quantified clause.
     */
    public Literal(int literal, int multiplicity) {
        this.literal = literal;
        this.multiplicity = multiplicity;}

    /** turns the list of literals into a string of literal names or numbers.
     *
     * @param literals    a list of literals.
     * @param symboltable null or a symboltable.
     * @return the literals as a string of names or numbers.
     */
    public String toString(ArrayList<Literal> literals, Symboltable symboltable) {
        if(literals == null || literals.isEmpty()) return "";
        StringBuilder st = new StringBuilder();
        st.append(Symboltable.toString(literals.get(0).literal,symboltable));
        for(int i = 1; i < literals.size(); ++i)
            st.append(",").append(Symboltable.toString(literals.get(i).literal,symboltable));
        return st.toString();}

    /** returns a string representation of the literal: for example 3^2.
     *
     * @return a string representation of the literal. */
    public String toString() {
        return toString(null);}

    /** returns a string representation of the literal: for example p^2.
     *
     * @param symboltable null or the symboltable.
     * @return a string representation of the literal.
     */
    public String toString(Symboltable symboltable) {
        String lit = Symboltable.toString(literal,symboltable);
        return (multiplicity == 1) ? lit : lit+"^"+multiplicity;}
}
