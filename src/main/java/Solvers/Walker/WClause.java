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
    protected final Connective connective;     // OR, ATLEAST ASTMOST, INTERVAL
    protected final short minLimit;              // the left limit of the interval
    protected final short maxLimit;              // the right limit of the interval
    public final int[] literals;               // the literals
    protected boolean isLocallyTrue  = false;  // truth in the local model
    protected boolean hasDoubles     = false;  // indicates that there are multiple occurrences in the literals
    protected short[] multiplicities;            // maps literal positions to the number of literal occurrences
    protected int loopCounter = -1;
    public int position = -1;

    /** copies a clause to the WClause-type
     *
     * @param clause a clause
     */
    public WClause(Clause clause) {
        id = clause.id;
        connective = clause.connective;
        minLimit = clause.minLimit;
        maxLimit = clause.maxLimit;
        int size = clause.size();
        literals = new int[size];
        multiplicities = new short[size];
        for(int i = 0; i < size; ++i) {
            literals[i] = clause.getLiteral(i);
            multiplicities[i] = clause.getCLiteral(i).multiplicity;
            hasDoubles |= multiplicities[i] > 1;}
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

    public int nextLiteral() {
        return literals[++loopCounter % literals.length];}


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
     * @return a string: clause-id: [type] min-max literals GT LT  (for Globally True and Locally True)
     */
    public String toString(int width, Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        if(width > 0) {
            Formatter format = new Formatter(st, Locale.GERMANY);
            format.format("%-"+(width+ connective.prefix.length())+"s", connective.prefix+id+":");}
        else st.append(connective.prefix+id+": ");
        if(connective != Connective.OR) st.append("atlest " + minLimit + ": ");
        int size = literals.length;
        for(int position = 0; position < size; ++position) {
            st.append(Symboltable.toString(literals[position],symboltable));
            if(position < size-1) {st.append(connective.separator);}}
        if(isLocallyTrue)  st.append(" LT");
        return st.toString();}

}
