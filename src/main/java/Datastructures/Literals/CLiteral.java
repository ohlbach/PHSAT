package Datastructures.Literals;

import Datastructures.Clauses.Clause;

/**
 * Created by Ohlbach on 25.08.2018.
 *
 * A CLiteral is a literal within a clause.
 * Besides the literal, it contains a pointer to the clause and the position within the clause.
 */
public class CLiteral {
    public int literal;   // the literal
    private Clause clause; // the clause
    private int position;  // the position of the literal within the clause.
    public int timestamp = 0;

    /** creates a CLiteral without a clause
     *
     * @param literal the literal
     */
    public CLiteral(int literal) {
        this.literal = literal;
        position = -1;}

    /** creates a CLiteral and sets the clause
     *
     * @param literal    the literal
     * @param clause     the clause containing the literal
     * @param position   the position of the literal within the clause
     */
    public CLiteral(int literal, Clause clause, int position) {
        this.literal = literal;
        this.clause = clause;
        this.position = position;}


    /** return the clause or null
     *
     * @return the clause or null
     */
    public Clause getClause() {return clause;}

    /** returns the position of the literal within the clause, or -1 if there is no clause
     *
     * @return the position of the literal within the clause, or -1 if there is no clause
     */
    public int getPosition() {return position;}

    /** adds the pointer to the clause and the position within the clause
     *
     * @param clause    the clause
     * @param position  the position within the clause
     */
    public void setClause(Clause clause, int position) {
        assert position >= 0;
        this.clause = clause;
        this.position = position;}

    /** removes the clause and sets the position to -1
     */
    public void removeClause() {
        clause = null;
        position = -1;}

    /** generates a String literal@clause,position
     *
     * @return a String literal@clause,position
     */
    public String toFullString() {
        String st = Integer.toString(literal);
        if(clause != null) {st = st+"@"+Integer.toString(clause.number)+","+Integer.toString(position);}
        return st;}

    /** returns just the literal.
     *
     * @return the literal as a String.
     */
    public String toString() {
        return Integer.toString(literal);}

}
