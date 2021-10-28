package Solvers.Walker;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseType;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.Int2IntArrayMap;

import java.util.Arrays;
import java.util.Formatter;
import java.util.Locale;

/** This is a special clause type for the Walker solver */
public class WClause {

    protected final int id;
    protected final ClauseType clauseType;
    protected final int quantifier;
    protected final int[] literals;
    protected boolean isLocallyTrue  = false;
    protected boolean isGloballyTrue = false;
    protected boolean hasDoubles     = false;
    protected Int2IntArrayMap multiplicities = null;

    /** copies a clause to the WClause-type
     *
     * @param clause a clause
     */
    public WClause(Clause clause) {
        id = clause.id;
        clauseType = clause.clauseType;
        quantifier = clause.quantifier;
        literals = new int[clause.size()];
        for(int i = 0; i < clause.size(); ++i) literals[i] = clause.getLiteral(i);
        int size = literals.length;
        if(clauseType.isNumeric()) {
            Int2IntArrayMap multiplicities = new Int2IntArrayMap(clause.size());
            for(int i = 0; i < size; ++i) {
                int literal = literals[i];
                int mult = multiplicities.get(literal)+1;
                multiplicities.put(literal,mult);
                hasDoubles |= mult > 1;}
            if(hasDoubles) this.multiplicities = multiplicities;}}


    /** generates a string: clause-id: literals GT LT  (for Globally True and Locally True)
     *
     * @return a string: clause-id: literals GT LT  (for Globally True and Locally True)
     */
    public String toString() {
        return toString(0,null);}

    /** generates a string: clause-id: literals GT LT  (for Globally True and Locally True)
     *
     * @param width: 0 or the width of the id-part.
     * @param symboltable for mapping numbers to names
     * @return a string: clause-id: literals GT LT  (for Globally True and Locally True)
     */
    public String toString(int width, Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        if(width > 0) {
            Formatter format = new Formatter(st, Locale.GERMANY);
            format.format("%-"+(width+clauseType.prefix.length())+"s", clauseType.prefix+id+":");}
        else st.append(clauseType.prefix+id+": ");
        if(clauseType.isNumeric()) st.append(clauseType + " " + quantifier + ": ");
        int size = literals.length;
        for(int position = 0; position < size; ++position) {
            st.append(Symboltable.toString(literals[position],symboltable));
            if(position < size-1) {st.append(clauseType.separator);}}
        if(isGloballyTrue) st.append(" GT ");
        if(isLocallyTrue) st.append(" LT ");
        return st.toString();}

}
