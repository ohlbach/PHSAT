package Datastructures.Literals;

import Datastructures.Clauses.Clause;
import Datastructures.Symboltable;
import Utilities.Positioned;

import java.util.function.Function;

/**
 * Created by Ohlbach on 25.08.2018.<br>
 *
 * A CLiteral is a literal within a clause.<br>
 * Besides the literal, it contains a pointer to the clause and the clausePosition within the clause.
 * A CLiteral can be subclassed to carry more information.
 */
public class CLiteral implements Positioned {
    public int literal;                // the literal
    public short multiplicity = 1;     // the number of occurrences of the literal in the clause
    public Clause clause = null;       // the clause
    public int clausePosition = -1;    // the clausePosition of the literal within the clause.
    public int indexPosition = -1;     // the position in a literal index.
    public int timestamp = 0;
    public Object aux = null;          // an auxiliary storage point

    /** creates a CLiteral without a clause
     *
     * @param literal the literal
     */
    public CLiteral(int literal) {
        this.literal = literal;}

    /** creates a CLiteral and sets the clause
     *
     * @param literal    the literal
     * @param clause     the clause containing the literal
     * @param position   the clausePosition of the literal within the clause
     */
    public CLiteral(int literal, Clause clause, int position, short multiplicity) {
        this.literal = literal;
        this.clause = clause;
        this.clausePosition = position;
        this.multiplicity = multiplicity;}


    /** adds the pointer to the clause and the clausePosition within the clause
     *
     * @param clause    the clause
     * @param position  the clausePosition within the clause
     */
    public void setClause(Clause clause, int position) {
        assert position >= 0;
        this.clause = clause;
        this.clausePosition = position;}

    /** constructs a new CLiteral independent of a clause.
     *
     * @return the clone.
     */
    public CLiteral clone() {
        return new CLiteral(literal);}


    /** sets the position in a literal index
     *
     * @param indexPosition an integer
     */
    public void setPosition(int indexPosition) {
        this.indexPosition = indexPosition;}

    /** returns the position in a literal index.
     *
     * @return the position in a literal index.
     */
    public int getPosition() {return indexPosition;}

    /** returns just the literal.
     *
     * @return the literal as a String.
     */
    public String toString() {
        return Integer.toString(literal);}

    public String toString(Symboltable symboltable) {
        return (symboltable == null) ? Integer.toString(literal) : symboltable.toString(literal);}

    /** returns just the literal.
     *
     * @return the literal as a String.
     */
    public String toString(Function<Clause,String> clauseString) {
        return Integer.toString(literal) + "@" + clauseString.apply(clause);}

    /** returns just  name of the literal.
     *
     * @param symboltable for mapping numbers to names
     * @return the literal name as a String.
     */
    public String toString(Symboltable symboltable, Function<Clause,String> clauseString) {
        return ((symboltable == null) ? toString() : symboltable.toString(literal)) +"@" + clauseString.apply(clause);}


}

