package Datastructures;

import Datastructures.Clauses.Quantifier;

import java.util.ArrayList;

/**
 * Represents a Clause in a logic system.
 * <P>
 * The clause extends LinkedItem and can therefore be part of a doubly connected linked list.
 *
 * @param <Literal> the type of Literal objects in the clause.
 */
public class Clause<Literal extends Datastructures.Literal> extends LinkedItem {
    /** the identifier for the clause. */
    public int id;
    /** the version number (for simplified clauses) */
    public int version;
    /** the quantifier */
    public Quantifier quantifier;
    /** the lower limit for Interval clauses. */
    public int min;
    /** the upper limit for Interval clauses. */
    public int max;
    /** the sum of all multiplicities of the literals. */
    public int expandedSize;

    /** the list of all Literal objects in the clause. */
    public ArrayList<Literal> literals = new ArrayList<>();

    /**
     * Constructs a Clause object with the given parameters.
     *
     * @param id the id of the clause
     * @param version the version of the clause
     * @param quantifier the quantifier of the clause
     * @param min the minimum value of the clause
     * @param max the maximum value of the clause
     * @param expandedSize the expanded size of the clause
     */
    public Clause(int id, int version, Quantifier quantifier, int min, int max, int expandedSize) {
        this.id = id;
        this.version =version;
        this.quantifier =quantifier;
        this.min =min;
        this.max =max;
        this.expandedSize = expandedSize;}


    /** finds the Literal with the given literal.
     *
     * @param literal a literal.
     * @return null or a Literal with the given literal.
     */
    Literal findLiteral(int literal) {
        for(Literal literalObject : literals) {
            if(literalObject.literal == literal) return literalObject;}
        return null;}


    /** returns the number of Literal objects in the clause.
     *
     * @return the number of Literal objects in the clause.
     */
    public int size() {return literals.size();}

    /** returns the sum of the literal's multiplicities.
     *
     * @return the sum of the literal's multiplicities.
     */
    public int expandedSize() {return expandedSize;}

    /** checks if the clause is true because of its limits.
     *
     * @return true if the clause is true because of its limits.
     */
    public boolean isTrue() {
        return min <= 0 && max >= expandedSize ;}

    /** checks if the clause is false because of its limits.
     *
     * @return true if the clause is false because of its limits.
     */
    public boolean isFalse() {
        return min > expandedSize || max < 0 || max < min;}

    /** turns the clause into a string.
     *
     * @param symboltable null or a symboltable.
     * @param size 0 or the length of the clause number string.
     * @return a string representation of the clause.
     */
    public String toString(Symboltable symboltable, int size) {
        String name = Integer.toString(id);
        if(version != 0) name += "."+version;
        StringBuilder st = new StringBuilder();
        st.append((size == 0) ? name : String.format("%"+size+"s",name)).append(": ");
        switch(quantifier) {
            case OR: break;
            case EXACTLY:
            case ATLEAST:  st.append(quantifier.abbreviation).append(min).append(" "); break;
            case ATMOST:   st.append(quantifier.abbreviation).append(max).append(" "); break;
            case INTERVAL: st.append("[").append(min).append(",").append(max).append("] ");}
        if(!literals.isEmpty()) {
            int length = literals.size()-1;
            for(int i = 0; i < length; ++i) {
                st.append(literals.get(i).toString(symboltable,0)).append(quantifier.separator);}
            st.append(literals.get(length).toString(symboltable,0));}
        return st.toString();}


}
