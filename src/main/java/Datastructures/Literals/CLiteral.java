package Datastructures.Literals;

import Datastructures.Clauses.Clause;
import Datastructures.Symboltable;

import java.util.IdentityHashMap;
import java.util.InputMismatchException;

/**
 * Created by Ohlbach on 25.08.2018.<br/>
 *
 * A CLiteral is a literal within a clause.<br/>
 * Besides the literal, it contains a pointer to the clause and the position within the clause.
 * A CLiteral can be subclassed to carry more information.
 */
public class CLiteral  implements Comparable<CLiteral>{
    public int literal;          // the literal
    public Clause clause = null; // the clause
    public int position = -1;    // the position of the literal within the clause.
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
     * @param position   the position of the literal within the clause
     */
    public CLiteral(int literal, Clause clause, int position) {
        this.literal = literal;
        this.clause = clause;
        this.position = position;}


    /** adds the pointer to the clause and the position within the clause
     *
     * @param clause    the clause
     * @param position  the position within the clause
     */
    public void setClause(Clause clause, int position) {
        assert position >= 0;
        this.clause = clause;
        this.position = position;}


    /** generates a String literal@clause,position
     *
     * @param symboltable for mapping numbers to names
     * @return a String literal@clause,position
     */
    public String toFullString(Symboltable symboltable) {
        if(symboltable == null) {return toFullString();}
        String id = "";
        if(clause != null) {id = clause.id;}
        String st = symboltable.getLiteralName(literal);
        return st+"@"+id+","+Integer.toString(position);}

    /** generates a String literal@clause,position
     *
     * @return a String literal@clause,position
     */
    public String toFullString() {
        String id = "";
        if(clause != null) {id = clause.id;}
        return Integer.toString(literal)+"@"+id+","+Integer.toString(position);}

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
        return symboltable == null ? toString() : symboltable.getLiteralName(literal);}

    /** compares two literals.
     * If they belong to different clauses then the clause problemId are compared.
     * If they belong to the same clause, then the literals are compared
     *
     * @param cLiteral another CLiteral.
     * @return -1,0,1 according to the order
     */
    public int compareTo(CLiteral cLiteral) {
        if(clause != cLiteral.clause) {return String.CASE_INSENSITIVE_ORDER.compare(clause.id,cLiteral.clause.id);}
        return Integer.compare(literal,cLiteral.literal);}

}
