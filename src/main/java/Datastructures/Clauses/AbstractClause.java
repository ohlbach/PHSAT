package Datastructures.Clauses;

import Datastructures.Literals.CLiteral;
import Datastructures.Symboltable;

import java.util.ArrayList;

/** An abstract clause is just a list of literals (CLiterals).
 * It may be subclassed to represent clauses as disjunctions of literals, or disjointness clauses.
 *
 * Created by ohlbach on 13.09.2018.
 */
public class AbstractClause {
    /** for enumerating the clauses */
    public int number;
    /** the literals */
    public ArrayList<CLiteral> cliterals;
    /** a timestamp to be used by corresponding algorithms */
    public int timestamp = 0;

    /** constructs a clause
     *
     * @param number   the clause number
     * @param size  the estimated number of literals
     */
    public AbstractClause(int number, int size) {
        this.number = number;
        cliterals = new ArrayList<CLiteral>(size);}

    /** return the current number of literals
     *
     * @return the current number of literals
     */
    public int size() {return cliterals.size();}

    /** checks if the clause is empty
     *
     * @return true if the clause is empty.
     */
    public boolean isEmpty() {return cliterals.isEmpty();}

    /** checks if the literal is in the clause
     *
     * @param literal a literal
     * @return true if the literal is in the clause.
     */
    public boolean contains(int literal) {
        for(CLiteral cliteral : cliterals) {
            if(cliteral.literal == literal) {return true;}}
        return false;}

    /** adds a cliteral to the end of the clause
     *
     * @param cliteral the literal to be added.
     * @return +1 if the literal is already in the clause, -1 if -literal is in the clause, otherwise 0.
     */
    public int addCLiteral(CLiteral cliteral) {
        int literal = cliteral.literal;
        if(cliterals.contains(literal)) {return 1;}
        if(cliterals.contains(-literal)) {return -1;}
        int position = cliterals.size();
        cliterals.add(cliteral);
        cliteral.setClause(this,position);
        return 0;}

    /** removes a cliteral from the clause.
     *
     * @param literal the literal to be removed.
     */
    public void removeLiteral(CLiteral literal) {
        int position = literal.getPosition();
        for(int pos = position; pos < cliterals.size()-1; ++pos) {
            CLiteral nextliteral = cliterals.get(pos+1);
            nextliteral.setClause(this,pos);
            cliterals.set(pos,nextliteral);}
        literal.removeClause();}

    public String id() {return Integer.toString(number);}

    /** generates a string: clause-number: literals
     *
     * @return a string: clause-number: literals
     */
    public String toString() {
        return toString((""+number).length(),null);}

    /** generates a string: clause-number: literals
     *
     * @param symboltable for mapping numbers to names
     * @return a string: clause-number: literals
     */
    public String toString(Symboltable symboltable) {
        return toString((""+number).length(),symboltable);
    }

    /** generates a string: clause-number: non-false literals
     *
     * @param numberSize the length of the number-string
     * @param symboltable for mapping numbers to names
     * @return a string: clause-number: non-false literals
     */
    public String toString(int numberSize, Symboltable symboltable) {
        StringBuffer st = new StringBuffer();
        String id = id();
        String n = String.format("%"+numberSize+"s",id);
        st.append(n).append(": ");
        int size = cliterals.size();
        for(int position = 0; position < size; ++position) {
            st.append( cliterals.get(position).toString(symboltable));
            if(position < size-1) {st.append(", ");}}
        return st.toString();}

}
