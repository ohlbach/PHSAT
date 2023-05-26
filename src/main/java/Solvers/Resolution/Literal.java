package Solvers.Resolution;

import Datastructures.Symboltable;

import java.util.ArrayList;

/** A literal object contains the literal itself together with its multiplicity. In addition, it is part of a clause and a literal index.
 * <br>
 * The literal index is a doubly connected list. It is supported by the attributes previousLiteral and nextLiteral.  <br>
 * The Literal object has no active methods.
 */
public class Literal {
    /** the literal itself. */
    int literal;

    /** the number of occurrences in a quantified clause. */
    int multiplicity;

    /** the clause containing the literal.
     * Literals may get removed from a clause. In this case the 'clause' attribute is set to null. */
    Clause clause;

    /** pointer to the previous Literal in a doubly connected list. */
    Literal previousLiteral;

    /** pointer to the next Literal in a doubly connected list. */
    Literal nextLiteral;

    /** constructs a Literal object.
     *
     * @param literal      the literal itself.
     * @param multiplicity the number of occurrences in a quantified clause.
     */
    public Literal(int literal, int multiplicity) {
        this.literal = literal;
        this.multiplicity = multiplicity;}

    /** turns the list of literals into a string of literal names or numbers.
     * <br>
     * If the multiplicity is 1, then only the literal itself is part of the string.
     * If the multiplicity is &gt;1 then literal^multiplicity becomes part of the string.
     *
     * @param literals    a list of literals.
     * @param symboltable null or a symboltable.
     * @return the literals as a string of names or numbers, possibly followed by their multiplicity.
     */
    public static String toString(ArrayList<Literal> literals, Symboltable symboltable) {
        if(literals == null || literals.isEmpty()) return "";
        StringBuilder st = new StringBuilder();
        st.append(Symboltable.toString(literals.get(0).literal,symboltable));
        int multiplicity = literals.get(0).multiplicity;
        if(multiplicity > 1) st.append("^").append(multiplicity);
        for(int i = 1; i < literals.size(); ++i) {
            multiplicity = literals.get(i).multiplicity;
            st.append(",").append(Symboltable.toString(literals.get(i).literal,symboltable));
            if(multiplicity > 1) st.append("^").append(multiplicity);}
        return st.toString();}

    /** returns a string representation of the literal: for example 3^2.
     *
     * @return a string representation of the literal. */
    public String toString() {
        return toString(null);}

    /** returns a string representation of the literal: for example p^2.
     *
     * @param symboltable null or the symboltable.
     * @return a string representation of the literal, possibly followed by their multiplicity.
     */
    public String toString(Symboltable symboltable) {
        String lit = Symboltable.toString(literal,symboltable);
        return (multiplicity == 1) ? lit : lit+"^"+multiplicity;}
}
