package Solvers.Walker;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.Connective;
import Datastructures.Symboltable;
import Utilities.Utilities;

import java.util.Formatter;
import java.util.Locale;

/** This is a special clause type for the Walker solver */
public class WClause {

    public final int id;                       // the clause identifier
    protected final Connective connective;     // OR, ATLEAST, ATMOST or EXACTLY
    protected final int quantifier;            // the quantifier of numeric types
    public final int[] literals;               // the literals
    protected boolean isLocallyTrue  = false;  // truth in the local model
    protected boolean isGloballyTrue = false;  // truth in the global model
    protected boolean hasDoubles     = false;  // indicates that there are multiple occurrences in the literals
    protected int[] multiplicities;            // maps literal positions to the number of literal occurrences

    /** copies a clause to the WClause-type
     *
     * @param clause a clause
     */
    public WClause(Clause clause) {
        id = clause.id;
        connective = clause.connective;
        quantifier = clause.interval.min;
        literals = new int[clause.size()];
        for(int i = 0; i < clause.size(); ++i) literals[i] = clause.getLiteral(i);
        int size = literals.length;
        multiplicities = new int[clause.size()];
        for(int i = 0; i < size; ++i) {
            int literal = literals[i];
            multiplicities[i] = 1;
            for(int j = 0; j < i; ++j) {
                if(literal == literals[j]) {
                    ++multiplicities[j]; ++multiplicities[i];
                    hasDoubles = true;}}}
        if(!hasDoubles) multiplicities = null;} // if !hasDoubles: garbage collection

    /** returns the number of occurrences of the given literal within the clause
     *
     * @param literal a literal
     * @return the number of occurrences within the clause.
     */
    public int multiplicity(int literal) {
        assert Utilities.contains(literals,literal)  >= 0;
        if(multiplicities == null) return 1;
        for(int i = 0; i < literals.length; ++i) {
            if(literals[i] == literal) return multiplicities[i];}
        return 0;}

    /** returns the number of occurrences of the literal at the given position within the clause
     *
     * @param position a literal position
     * @return the number of occurrences within the clause.
     */
    public int multiplicityByPosition(int position) {
        assert (position >= 0) && (position < literals.length);
        if(multiplicities == null) return 1;
        return multiplicities[position];}


    /** generates a string: clause-id: literals GT LT  (for Globally True and Locally True)
     *
     * @return a string: clause-id: literals GT LT  (for Globally True and Locally True)
     */
    public String toString() {
        return toString(0,null);}

    /** generates a string: clause-id: [type] literals GT LT  (for Globally True and Locally True)
     *
     * @param width: 0 or the width of the id-part.
     * @param symboltable for mapping numbers to names
     * @return a string: clause-id: [type] literals GT LT  (for Globally True and Locally True)
     */
    public String toString(int width, Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        if(width > 0) {
            Formatter format = new Formatter(st, Locale.GERMANY);
            format.format("%-"+(width+ connective.prefix.length())+"s", connective.prefix+id+":");}
        else st.append(connective.prefix+id+": ");
        if(connective.isQuantifier()) st.append(connective + " " + quantifier + ": ");
        int size = literals.length;
        for(int position = 0; position < size; ++position) {
            st.append(Symboltable.toString(literals[position],symboltable));
            if(position < size-1) {st.append(connective.separator);}}
        if(isGloballyTrue) st.append(" GT");
        if(isLocallyTrue)  st.append(" LT");
        return st.toString();}

}
