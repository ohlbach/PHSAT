package Datastructures.Clauses;

import Datastructures.Literals.CLiteral;
import Datastructures.Symboltable;
import Utilities.Positioned;
import Utilities.Sizable;

import java.util.ArrayList;
import java.util.Iterator;

/** A clause is just a list of CLiterals.
 * It may represent disjunctions, conjunctions or any other logical list structure.
 *
 * Created by ohlbach on 13.09.2018.
 */
public class Clause implements Iterable<CLiteral<Clause>>, Positioned, Sizable {
    /** for identifying the clause */
    public String id;
    /** the literals */
    public ArrayList<CLiteral<Clause>> cliterals;
    /** indicates that the clause has been removed */
    public boolean removed = false;
    /** for sorting clauses, for example in a listPosition queue */
    public int listPosition = 0;
    /** positive, negative or mixed */
    public ClauseStructure structure = null;
    /** a timestamp to be used by corresponding algorithms */
    public int timestamp = 0;


    /** constructs a clause
     *
     * @param id   the clause problemId
     * @param size  the estimated number of literals
     */
    public Clause(String id, int size) {
        this.id = id;
        cliterals = new ArrayList<CLiteral<Clause>>(size);}

    /** constructs a new clause with given literals
     *
     * @param id        the id of the new clause
     * @param cLiterals the list of CLiterals
     */
    public Clause(String id, ArrayList<CLiteral<Clause>> cLiterals) {
        this.id = id;
        for(int i = 0; i < cLiterals.size(); ++i) {
            cLiterals.get(i).setClause(this,i);}
        cliterals = cLiterals;
        setStructure();}

    /** generates a clause from a basicClause (which should be a disjunction)
     * double literals are removed
     * complementary literals cause the structure to be TAUTOLOGY
     *
     * @param id           the name of the clause
     * @param basicClause  a basic clause [number,type,literal1,...]
     */
    public Clause(String id, int[] basicClause) {
        this.id = id;
        int length = basicClause.length;
        cliterals = new ArrayList<>(length-2);
        for(int i = 2; i < length; ++i) {
            int literal = basicClause[i];
            cliterals.add(new CLiteral<Clause>(literal,this,cliterals.size()));}
        setStructure();}



    /** checks if the clause is positive, negative or mixed and sets the corresponding value for the structure.
     */
    private void setStructure() {
        int positive = 0;
        int negative = 0;
        for(CLiteral cLiteral : cliterals) {
            if(cLiteral.literal > 0) {++positive;} else {++negative;}}
        structure = ClauseStructure.MIXED;
        if(positive == 0) {structure = ClauseStructure.NEGATIVE;}
        else {if(negative == 0) {structure = ClauseStructure.POSITIVE;}}}

    /** returns the list position, or -1
     *
     * @return the list position, or -1
     */
    public int getPosition() {return listPosition;}

    /** sets the list position
     *
     * @param position a position within a list.
     */
    public void setPosition(int position) {listPosition = position;}

    /** deletes the back-pointers in the cliterals, making them garbage */
    public void delete() {
        for(CLiteral cliteral : cliterals) {cliteral.delete();}}


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

    /** checks if the clause has only positive literals
     *
     * @return true if the clause has only positive literals
     */
    public boolean isPositive() {return structure == ClauseStructure.POSITIVE;}

    /** checks if the clause has only negative literals
     *
     * @return true if the clause has only negative literals.
     */
    public boolean isNegative() {return structure == ClauseStructure.NEGATIVE;}

    /** gets the literal at the given clausePosition
     *
     * @param position a literal clausePosition
     * @return the cliteral at that clausePosition.
     */
    public CLiteral<Clause> getCLiteral(int position) {
        assert position >= 0 && position < cliterals.size();
        return cliterals.get(position);}

    /** gets the literal at the given clausePosition
     *
     * @param position a literal clausePosition
     * @return the literal at that clausePosition.
     */
    public int getLiteral(int position) {
        assert position >= 0 && position < cliterals.size();
        return cliterals.get(position).literal;}

    /** checks if the literal is in the clause
     *
     * @param literal a literal
     * @return the literal's clausePosition in the clause, or -1
     */
    public int contains(int literal) {
        for(int i = 0; i < cliterals.size(); ++i) {
            if(cliterals.get(i).literal == literal) {return i;}}
        return -1;}


    /** adds a cliteral to the end of the clause, without checking for double literals and tautologies.
     *
     * @param cliteral the literal to be added.
     */
    public void add(CLiteral cliteral) {
        int position = cliterals.size();
        cliterals.add(cliteral);
        cliteral.setClause(this,position);
        setStructure();}

    /** removes a cliteral from the clause.
     *
     * @param cLiteral the literal to be removed.
     */
    public void remove(CLiteral cLiteral) {
        removeAtPosition(cLiteral.clausePosition);}

    /** removes a cliteral at the given clausePosition from the clause.
     *
     * @param position the clausePosition of the literal to be removed
     */
    public void removeAtPosition(int position) {
        int size = cliterals.size();
        assert position >= 0 && position < size;
        for(int pos = position; pos < size-1; ++pos) {
            CLiteral nextliteral = cliterals.get(pos+1);
            nextliteral.clausePosition = pos;
            cliterals.set(pos,nextliteral);}
        cliterals.remove(size-1);
        setStructure();}


    /** checks if the literals in this, except cLiteral also occur in clause2
     *
     * @param cLiteral a cLiteral in 'this'
     * @param clause2 any other clause
     * @return true if the literals in this, except cLiteral also occur in clause2
     */
    public boolean isSubset(CLiteral cLiteral, Clause clause2) {
        boolean found = true;
        for(CLiteral cl : cliterals) {
            if(cl != cLiteral & clause2.contains(cl.literal) < 0) {found = false; break;}}
        return found;}

    /** checks if the clause has double literals
     *
     * @return true if the clause has double literals.
     */
    public boolean hasDoubles() {
        int size = cliterals.size();
        for(int i = 0; i < size-1; ++i) {
            int literal = cliterals.get(i).literal;
            for(int j = i+1; j < size; ++j) {
                if(literal == cliterals.get(j).literal) {return true;}}}
        return false;}

    /** checks if the clause has complementary literals, e.g. p and -p
     *
     * @return true if the clause has complementary literals.
     */
    public boolean hasComplementaries() {
        int size = cliterals.size();
        for(int i = 0; i < size-1; ++i) {
            int literal = cliterals.get(i).literal;
            for(int j = i+1; j < size; ++j) {
                if(literal == -cliterals.get(j).literal) {return true;}}}
        return false;}

    /** removes multiple occurrences of literals
     *
     * @return true if there were multiple occurrences of literals
     */
    public boolean removeDoubles() {
        boolean doubles = false;
        for(int i = 0; i < cliterals.size()-1; ++i) {
            int literal = cliterals.get(i).literal;
            for(int j = i+1; j < cliterals.size(); ++j) {
                CLiteral<Clause> cliteral = cliterals.get(j);
                if(literal == cliteral.literal) {
                    removeAtPosition(j--);
                    doubles = true;}}}
        setStructure();
        return doubles;}

    /** generates a string: clause-number: literals
     *
     * @return a string: clause-number: literals
     */
    public String toString() {
        return toString(id.length(),null);}

    /** generates a string: clause-number: literals
     *
     * @param symboltable for mapping numbers to names
     * @return a string: clause-number: literals
     */
    public String toString(Symboltable symboltable) {
        return toString(id.length(),symboltable);
    }

    /** generates a string: clause-number: non-false literals
     *
     * @param numberSize the length of the number-string
     * @param symboltable for mapping numbers to names
     * @return a string: clause-number: non-false literals
     */
    public String toString(int numberSize, Symboltable symboltable) {
        StringBuffer st = new StringBuffer();
        String n = String.format("%"+numberSize+"s",id);
        st.append(n).append(":").append("(");
        int size = cliterals.size();
        for(int position = 0; position < size; ++position) {
            st.append( cliterals.get(position).toString(symboltable));
            if(position < size-1) {st.append(",");}};
        st.append(")");
        return st.toString();}

    /** gets an iterator over the literals
     *
     * @return the iterator over the literals
     */
    @Override
    public Iterator<CLiteral<Clause>> iterator() {
        return cliterals.iterator();}
}
