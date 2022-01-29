package Solvers.RecursiveSearch;

import Datastructures.Symboltable;

public class RSLiteral {
    protected final int literal;
    protected final short multiplicity;
    protected final RSClause clause;
    protected RSNode rsNode = null;

    public RSLiteral(int literal, short multiplicity, RSClause clause) {
        this.literal = literal;
        this.multiplicity = multiplicity;
        this.clause = clause;
    }

    /** blocks the literal.
     *  If there is only one unblocked literal left, it is added to the rsNode.
     *
     * @param rsNode the current search node.
     * @return null or the now empty or contradictory clause.
     */
    protected RSClause blockLiteral(RSNode rsNode) {
        this.rsNode = rsNode;
        clause.move(this);
        rsNode.addRSLiteral(this);

        if(clause.minLimit == 1) {
            switch(clause.unblockedSize()) {
                case 0: return clause; // empty clause
                case 1: rsNode.addLiteral(clause.rsLiterals[0].literal);
            return null;}}

        int expandedSize = clause.expandedSize();
        if(expandedSize == clause.minLimit) { // atleast 3 p^2,q -> true(p,q)
            for(RSLiteral rsLiteral : clause.rsLiterals) {rsNode.addLiteral(rsLiteral.literal);}
            return null;}

        if(expandedSize > clause.minLimit) {// contradiction like atleast 3 p,q
            this.rsNode = null;
            RSLiteral rsLiteral = clause.rsLiterals[0];
            rsLiteral.rsNode = rsNode;
            rsNode.changeLastRSLiteral(rsLiteral);
            return clause;}
        return null;}

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
            if(rsNode != null) st.append("|"+Symboltable.toString(literal,symboltable)+"|");
            st.append(Symboltable.toString(literal,symboltable));
            if(multiplicity > 1) st.append("^"+multiplicity);
        return st.toString();}



}
