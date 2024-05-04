package Datastructures;

import java.util.ArrayList;

/**
 * Represents a literal in a clause.
 * <p>
 * This is a superclass to be subclassed in the solvers.<br>
 * As LinkedItem it can become part of a doubly linked list (for example a Literal index).
 * <br>
 * It is parameterized with a Clause data structure.
 * Therefore, it may have a pointer to the clause containing the literal.
 *
 */
public class Literal<Clause> extends LinkedItem {
    /** the literal itself */
    public int literal;
    /** the number of occurrences in a quantified clause. */
    public int multiplicity;
    /** the clause containing the literal */
    public Clause clause;

    /** constructs a Literal object
     *
     * @param literal      the literal itself
     * @param multiplicity the number of occurrences in a quantified clause.
     */
    public Literal(int literal, int multiplicity) {
        this.literal = literal;
        this.multiplicity = multiplicity;}

    /** clones the literal, except previousLiteral and nextLiteral.
     *
     * @param literal a new literal.
     * @return the cloned literal.
     */
    public Literal<Clause> clone(int literal) {
        Literal<Clause> newLiteral = new Literal<>(literal, multiplicity);
        newLiteral.clause = clause;
        return newLiteral;}

    /** turns the list of literals into a string of literal names or numbers.
     *
     * @param literals    a list of literals.
     * @param symboltable null or a symbol table.
     * @return the literals as a string of names or numbers.
     */
    public String toString(ArrayList<Literal<Clause>> literals, Symboltable symboltable) {
        if(literals == null || literals.isEmpty()) return "";
        StringBuilder st = new StringBuilder();
        st.append(literals.get(0).toString(symboltable,0));
        for(int i = 1; i < literals.size(); ++i)
            st.append(",").append(literals.get(i).toString(symboltable,0));
        return st.toString();}

    /** returns a string representation of the literal: for example 3^2
     * @return a string representation of the literal. */
    public String toString() {
        return toString(null,0);}

    /** returns a string representation of the literal: for example p^2.
     *
     * @param symboltable the symbol table.
     * @return a string representation of the literal.
     */
    public String toString(Symboltable symboltable, int ignore) {
        String lit = Symboltable.toString(literal,symboltable);
        return (multiplicity == 1) ? lit : lit+"^"+multiplicity;}}
