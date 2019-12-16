package Datastructures.Literals;

import Datastructures.Symboltable;
import Utilities.Positioned;

/**
 * Created by Ohlbach on 25.08.2018.<br>
 *
 * A CLiteral is a literal within a clause.<br>
 * Besides the literal, it contains a pointer to the clause and the clausePosition within the clause.
 * A CLiteral can be subclassed to carry more information.
 */
public class CLiteral<Clause> implements Positioned {
    public int literal;                // the literal
    public Clause clause = null;       // the clause
    public int clausePosition = -1;    // the clausePosition of the literal within the clause.
    public int indexPosition = -1;     // the position in a literal index.
    public int timestamp = 0;

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
    public CLiteral(int literal, Clause clause, int position) {
        this.literal = literal;
        this.clause = clause;
        this.clausePosition = position;}


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

    /** deletes the back-pointer to the clause, making it garbage.
     */
    public void delete() {
        clause = null;}

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

    /** returns just  name of the literal.
     *
     * @param symboltable for mapping numbers to names
     * @return the literal name as a String.
     */
    public String toString(Symboltable symboltable) {
        return (symboltable == null) ? toString() : symboltable.getLiteralName(literal);}


}
