package Datastructures.Clauses;

import Datastructures.Literals.CLiteral;
import Datastructures.Symboltable;
import Utilities.Positioned;
import Utilities.Sizable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.Iterator;

/** A clause is just a list of CLiterals.
 * It may represent disjunctions, conjunctions or any other logical list structure.
 *
 * Created by ohlbach on 13.09.2018.
 */
public class Clause implements Iterable<CLiteral>, Positioned, Sizable {
    /** for identifying the clause */
    public int id;
    /** the literals */
    public ArrayList<CLiteral> cliterals;
    /** indicates that the clause has been removed */
    public boolean removed = false;
    /** for sorting clauses, for example in a listPosition queue */
    public int listPosition = -1;
    /** positive, negative or mixed */
    public ClauseStructure structure = null;
    /** a timestamp to be used by corresponding algorithms */
    public int timestamp = 0;
    /** contains the list of basicClause ids which produced this clause */
    public IntArrayList origins = null;

    public Clause(int id) {
        this.id = id;
        cliterals = new ArrayList<CLiteral>();}

    /** constructs a clause
     *
     * @param id   the clause problemId
     * @param size  the estimated number of literals
     */
    public Clause(int id, int size) {
        this.id = id;
        cliterals = new ArrayList<CLiteral>(size);}

    /** constructs a new clause with given literals
     *
     * @param id        the id of the new clause
     * @param cLiterals the list of CLiterals
     */
    public Clause(int id, ArrayList<CLiteral> cLiterals) {
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
    public Clause(int id, int[] basicClause) {
        this.id = id;
        int length = basicClause.length;
        cliterals = new ArrayList<>(length-2);
        for(int i = 2; i < length; ++i) {
            int literal = basicClause[i];
            cliterals.add(new CLiteral(literal,this,cliterals.size()));}
        setStructure();}



    /** checks if the clause is positive, negative or mixed and sets the corresponding value for the structure.
     */
    public ClauseStructure getStructure() {
        int positive = 0;
        int negative = 0;
        for(CLiteral cLiteral : cliterals) {
            if(cLiteral.literal > 0) {++positive;} else {++negative;}}
        ClauseStructure structure = ClauseStructure.MIXED;
        if(positive == 0) {structure =  ClauseStructure.NEGATIVE;}
        else {if(negative == 0) {structure =  ClauseStructure.POSITIVE;}}
        return structure;}

    public void setStructure() {
        structure = getStructure();}


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
    public CLiteral getCLiteral(int position) {
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

    /** converts the cLiterals into an array of integers
     *
     * @return the literals as integers.
     */
    public int[] getLiterals() {
        int size = cliterals.size();
        int[] literals = new int[size];
        for(int i = 0; i < size; ++i) {
            literals[i] = cliterals.get(i).literal;}
        return literals;}

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
        cliteral.setClause(this,position);}

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
                CLiteral cliteral = cliterals.get(j);
                if(literal == cliteral.literal) {
                    removeAtPosition(j--);
                    doubles = true;}}}
        return doubles;}

    /** joins the origins (Ids of the basicClauses which caused this clause)
     *
     * @param clauses a list of clauses
     * @param clause an additional clauses
     * @return the joined orignins of all the clauses
     */
    public static IntArrayList joinOrigins(ArrayList<Clause> clauses, Clause clause) {
        IntArrayList origins = new IntArrayList();
        if(clause != null) origins.addAll(clause.origins);
        for(Clause clause1 : clauses) {origins.addAll(clause1.origins);}
        return origins;}

    /** generates a string: clause-number: literals
     *
     * @return a string: clause-number: literals
     */
    public String toString() {
        return toString(null);}



    /** generates a string: clause-number: non-false literals
     *
     * @param symboltable for mapping numbers to names
     * @return a string: clause-number: non-false literals
     */
    public String toString(Symboltable symboltable) {
        StringBuffer st = new StringBuffer();
        st.append(Integer.toString(id));
        st.append(":(");
        int size = cliterals.size();
        for(int position = 0; position < size; ++position) {
            st.append(cliterals.get(position).toString(symboltable));
            if(position < size-1) {st.append(",");}};
        st.append(")");
        return st.toString();}

    /** gets an iterator over the literals
     *
     * @return the iterator over the literals
     */
    @Override
    public Iterator<CLiteral> iterator() {
        return cliterals.iterator();}


    /** checks the clause and its literals for consistency.
     * If an inconsistency is detected then an error message is printed and the entire system stops.
     */
    public void check() {
        if(removed) {return;}
        for(int i = 0; i < cliterals.size(); ++i) {
            CLiteral cliteral = cliterals.get(i);
            Clause clause = cliteral.clause;
            if(clause == null) {
                System.out.println("Error in Clause.check: clause" + id + ", literal " +
                        cliteral.literal + " has no clause");
                System.exit(1);}
            if(clause != this) {
                System.out.println("Error in Clause.check: clause" + id + ", literal " +
                        cliteral.literal + " has a different clause " + clause.id);
                System.exit(1);}
            if(cliteral.clausePosition != i) {
                System.out.println("Error in Clause.check: clause" + id + ", literal " +
                        cliteral.literal + " has wrong position " + cliteral.clausePosition);
                System.exit(1);}}

        if(structure == null) {
            System.out.println("Error in Clause.check: clause" + id + " has no structure");
            System.exit(1);}

        if(structure != getStructure()) {
            System.out.println("Error in Clause.check: clause" + id + " has wrong structure: " + structure.toString());
            System.exit(1);}
        }

}
