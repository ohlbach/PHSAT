package Solvers.RecursiveSearch;

import Datastructures.Symboltable;

/** This class models the literals for clauses in the RecursiveSearch (RS) Solver.
 *  A literal can be blocked or unblocked.<br>
 *  For example if the literal r in the clause p,q,r is temporarily false in the search node,
 *  it is marked with the node itself. This has a similar effect as removing the literal.
 *  The difference is that removing the search node (on backtracking) unblocks the literal.
 */
public class RSLiteral {
    protected final int literal;          // the literal itself
    protected final short multiplicity;   // its multiplicity (p^n)
    protected final RSClause clause;      // the clause containing the literal
    protected RSNode rsNode = null;       // a node in the recursive search which blocks the literal

    /** constructs a new RS-literal
     *
     * @param literal      the literal itself
     * @param multiplicity its multiplicity
     * @param clause       the clause containing the literal
     */
    public RSLiteral(int literal, short multiplicity, RSClause clause) {
        this.literal      = literal;
        this.multiplicity = multiplicity;
        this.clause       = clause;}

    /** blocks the literal. Blocking a literal is like removing a false literal.
     *  A blocked literal can be unblocked later on.
     *  Several actions are possible: <br>
     *  - If the literal is the last unblocked literal, the clause becomes empty, and is returned as empty clause
     *  - If there is only one unblocked literal left, it is temporarily true and added to the rsNode.<br>
     *  - If the remaining number of unblocked literals (with multiplicities) equals the clause's minLimit,
     *     all unblocked literals become true and are added to the rsNode. <br>
     *     Example: atleast 3 p^2,q makes p and q true.<br>
     *  - If the remaining number of unblocked literals (with multiplicities) is smaller than the
     *    clause's minLimit, the clause is returned as contradictious. <br>
     *    Example: atleast 4 p,q^2 is contradictory.
     *
     *   The blocked RSLiteral is added to the rsNode's rsLiterals (for later unblocking it again).
     *
     * @param rsNode the current search node.
     * @return null or the now empty or contradictory clause.
     */
    protected RSClause declareFalse(RSNode rsNode) {
        this.rsNode = rsNode;
        clause.move(this);
        rsNode.addRSLiteral(this);

        if(clause.minLimit == 1) {
            switch(clause.unblockedSize()) {
                case 0: return clause; // empty clause
                case 1: rsNode.addTrueLiteral(clause.rsLiterals[0].literal);
            return null;}}

        int expandedSize = clause.expandedSize();
        if(expandedSize == clause.minLimit) { // atleast 3 p^2,q -> true(p,q)
            for(RSLiteral rsLiteral : clause.rsLiterals) {
                if(rsLiteral.rsNode != null) break;
                rsNode.addTrueLiteral(rsLiteral.literal);}
            return null;}

        if(expandedSize > clause.minLimit) {// contradiction like atleast 3 p,q
            this.rsNode = null;
            RSLiteral rsLiteral = clause.rsLiterals[0];
            rsLiteral.rsNode = rsNode;
            rsNode.changeLastRSLiteral(rsLiteral);
            return clause;}
        return null;}

    /** unblocks the literal
     */
    protected void unblock() {
        rsNode = null;}

    /** turns the clause into a string
     *
     * @return the clause as string
     */
    public String toString() {
        return toString(null);}

    /** turns the clause into a string.
     *  The most general form is |blocking literal|literal^multiplicity
     *
     * @param symboltable  null or a symboltable
     * @return             the literal as string
     */
    public String toString(Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
            if(rsNode != null) st.append("|"+Symboltable.toString(rsNode.selectedLiteral(),symboltable)+"|");
            st.append(Symboltable.toString(literal,symboltable));
            if(multiplicity > 1) st.append("^"+multiplicity);
        return st.toString();}
}
