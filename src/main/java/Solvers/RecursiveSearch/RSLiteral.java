package Solvers.RecursiveSearch;

import Datastructures.Symboltable;

public class RSLiteral {
    protected final int literal;
    protected final short multiplicity;
    protected final RSClause clause;
    protected RSNode node = null;

    public RSLiteral(int literal, short multiplicity, RSClause clause) {
        this.literal = literal;
        this.multiplicity = multiplicity;
        this.clause = clause;
    }

    /** turns the clause into a string
     *
     * @return the clause as string
     */
    public String toString() {
        return toString(null);}

    /** turns the clause into a string
     *
     * @param symboltable  null or a symboltable
     * @return             the clause as string
     */
    public String toString(Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
            if(node != null) st.append("|"+Symboltable.toString(node.literal,symboltable)+"|");
            st.append(Symboltable.toString(literal,symboltable));
            if(multiplicity > 1) st.append("^"+multiplicity);
        return st.toString();}



}
