package Datastructures.Literals;

import java.util.*;
import java.util.function.Function;

/** This is the abstract superclass for mapping predicates to their occurrences in the clauses.
 * Subclasses may differ in the representation of these lists, for example sorted according to the size of the clauses.
 * Created by Ohlbach on 25.08.2018.
 */
public abstract class LiteralIndex {
    public int predicates;   // number of predicates


    public LiteralIndex(int predicates) {
        assert predicates > 0;
        this.predicates = predicates;}

    /** adds a literal to the index
     *
     * @param cliteral the literal to be added
     */
    public abstract void addLiteral(CLiteral cliteral);

    /** removes the literal from the index (in constant time)
     *
     * @param cliteral the literal to be removed.
     */
    public abstract void removeLiteral(CLiteral cliteral);


    /** returns the CLiterals with the given literal (integer)
     *
     * @param literal the literal (integer)
     * @return the list of occurrences (CLiterals)
     */
    public abstract AbstractCollection<CLiteral> getLiterals(int literal);
    /** returns the number of cLiterals indexed by this literal
     *
     * @param literal a literal
     * @return the number of cLiterals indexed by this literal
     */
    public abstract int size(int literal);

    public abstract boolean isEmpty(int literal);

    /** checks if the literal is pure, i.e. there are no complementary literals.
     *  If the literal is not at all in the index, it is not considered pure.
     *
     * @param literal a literal
     * @return true if the literal is pure, i.e. there are no complementary literals.
     */
    public boolean isPure(int literal) {
        return isEmpty(-literal);}

    /** returns all pure literals
     *
     * @return all pure literals.
     */
    public ArrayList<Integer> pureLiterals() {
        ArrayList<Integer> pure = new ArrayList<>();
        for(int predicate = 1; predicate < predicates; ++predicate) {
            if(isPure(predicate)) {pure.add(predicate);}
            else {if(isPure(-predicate)) {pure.add(-predicate);}}}
        return pure;}


    /** returns an iterator over the list of occurrences of the given literal.
     *
     * @param literal a literal
     * @return an iterator over all CLiterals (occurrences of the literal in the clauses).
     */
    public abstract Iterator<CLiteral> iterator(int literal);

    /** comprises the index into a string
     *
     * @return the entire index as string.
     */
    public String toString() {
        return toString((cLiteral -> cLiteral.toString()));}

    /** comprises the index into a string
     *
     * @param literalString a function for mapping cLiterals to strings
     * @return the entire index as string.
     */
    public String toString(Function<CLiteral,String> literalString) {
        StringBuilder st = new StringBuilder();
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            StringBuilder posString = null;
            StringBuilder negString = null;
            Iterator<CLiteral> it = iterator(predicate);
            if(!isEmpty(predicate)) {
                posString = new StringBuilder();
                while(it.hasNext()) {
                    CLiteral lit = it.next();
                    posString.append(literalString.apply(lit)).append("@"+lit.clausePosition).append(",");}}
            if(!isEmpty(-predicate)) {
                negString = new StringBuilder();
                it = iterator(-predicate);
                while(it.hasNext()) {
                    CLiteral lit = it.next();
                    posString.append(literalString.apply(lit)).append("@"+lit.clausePosition).append(",");}}
            if(posString != null) {
                st.append(" ").append(Integer.toString(predicate)).append(": ").append(posString).append("\n");}
            if(negString != null)
                st.append(Integer.toString(-predicate)).append(": ").append(negString).append("\n");}
        return st.toString();}

}
