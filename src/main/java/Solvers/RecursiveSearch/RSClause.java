package Solvers.RecursiveSearch;

import Datastructures.Clauses.Clause;
import Datastructures.Literals.CLiteral;
import Datastructures.Symboltable;

import java.util.ArrayList;
import java.util.Formatter;
import java.util.Locale;

/** This class models clauses for the RecursiveSearch solver.
 * A clause is essentially a list of literals, represented as RSLiteral data structures.
 * Once created, a clause will never be changed.
 *
 * The clauses represent atleast-clauses. E.g. atleast 2 p,q^2,r<br>
 * Ordinary disjunctions are a special case with minLimit = 1.
 *
 * However, its literals can be blocked, which has the same meaning as removing a literal.
 * The difference is that the literals can be unblocked at backtracking.
 *
 * When a literal is blocked, it is moved to the back of the list in a way which represents the
 * search tree. <br>
 * Example: Clause C: p,q,r,s <br>
 * Search Node -q (q is false now)<br>
 * C becomes: p,r,s,|-q|,q (-q) represents the choice in the search tree<br>
 * Suppose a consequence of -q is that r becomes false<br>
 * C becomes: p,s,|-q|r,|-q|q <br>
 * Suppose now -p is chosen. <br>
 * C becomes: s,|-p|,p,|-q|r,|-q|q <br>
 * etc.<br>
 * On backtracking, p, is unblocked first and <br>
 * C becomes: s,p,|-q|r,|-q|q <br>
 * etc.
 */
public class RSClause {
    protected final int id;        // the clause's identifier
    protected short minLimit;      // the limit, e.g. atleast limit p,q,r,s ...
    protected int timestamp = 0;   // to be used in subsumption and replacement resolution algorithms.
    protected final RSLiteral[] rsLiterals; // the list of literals

    /** constructs a clause
     *
     * @param id          its identifier
     * @param minLimit    its limit
     * @param rsLiterals  its literals
     */
    public RSClause(int id, short minLimit, RSLiteral[] rsLiterals) {
        this.id = id;
        this.minLimit = minLimit;
        this.rsLiterals = rsLiterals;}

    /** turns clauses in the Clause data structure to RSClauses.
     *  - or- and atleast clauses are just copied.<br>
     *  - atmost clauses are turned into atleast clauses.<br>
     *    Example: atmost 2 p,q,r^2,s,t -> atleast 3 -p,-q,-r^2,-s,-t<br>
     *    Interval clauses become two atleast clauses.
     *    Example: [2,4] p,q,r,s,t -> atleast 2 p,q,r,s,t and atleast 1 -p,-q,-r,-s,-t<br>
     *    Notice that the second clause gets a negative id.
     *
     * @param clause    a clause in the Clause data structure
     * @param rsClauses for adding the generated RSClauses.
     */
    public static void newRSClauses(Clause clause, ArrayList<RSClause> rsClauses) {
        rsClauses.clear();
        RSClause rsClause;
        int size = clause.cliterals.size();
        RSLiteral[] literals = new RSLiteral[size];
        switch(clause.connective) {
            case OR:
            case ATLEAST:
                rsClause = new RSClause(clause.id,clause.minLimit,literals);
                for(int i = 0; i < size; ++i) {
                    CLiteral cLiteral = clause.cliterals.get(i);
                    literals[i] = new RSLiteral(cLiteral.literal,cLiteral.multiplicity,rsClause);}
                 rsClauses.add(rsClause);
                return;
            case ATMOST:
                rsClause = new RSClause(clause.id,(short)(clause.expandedSize()-clause.maxLimit),literals);
                for(int i = 0; i < size; ++i) {
                    CLiteral cLiteral = clause.cliterals.get(i);
                    literals[i] = new RSLiteral(-cLiteral.literal,cLiteral.multiplicity,rsClause);}
                rsClauses.add(rsClause);
                return;
            case INTERVAL:
            case EXACTLY:
                rsClause = new RSClause(clause.id,clause.minLimit,literals);
                for(int i = 0; i < size; ++i) {
                    CLiteral cLiteral = clause.cliterals.get(i);
                    literals[i] = new RSLiteral(cLiteral.literal,cLiteral.multiplicity,rsClause);}
                rsClauses.add(rsClause);

                literals = new RSLiteral[size];
                rsClause = new RSClause(-clause.id,(short)(clause.expandedSize()-clause.maxLimit),literals);
                for(int i = 0; i < size; ++i) {
                    CLiteral cLiteral = clause.cliterals.get(i);
                    literals[i] = new RSLiteral(-cLiteral.literal,cLiteral.multiplicity,rsClause);}
                rsClauses.add(rsClause);
                break;}}

    /** computes the number of unblocked literals
     *
     * @return the number of unblocked literals
     */
    protected int unblockedSize() {
        int length = rsLiterals.length;
        for(int i = 0; i < length; ++i) {
            if(rsLiterals[i].rsNode != null) return i;}
        return length;}

    /** computes the number of unblocked literals with multiplicities
     *
     * @return the number of unblocked literals
     */
    protected int expandedSize() {
        int size = 0;
        for (RSLiteral rsLiteral : rsLiterals) {
            if (rsLiteral.rsNode == null) size += rsLiteral.multiplicity;
            else break;}
        return size;}


    protected void blockClause(RSLiteral rsLiteral, RSNode rsNode) {
        RSLiteral firstRSLiteral = rsLiterals[0];
        if(firstRSLiteral.rsNode != null) return;
        if(rsLiteral.multiplicity <= minLimit) {
            firstRSLiteral.rsNode = rsNode;
            rsNode.addRSLiteral(firstRSLiteral);
            return;}
        rsLiteral.rsNode = rsNode;
        move(rsLiteral);
        minLimit -= rsLiteral.multiplicity;
        rsNode.addRSLiteral(rsLiteral);}

    /** This method blocks the clause entirely, for example because of subsumption
     *
     * @param rsNode the current search node.
     */
    protected void block(RSNode rsNode) {
        rsLiterals[0].rsNode = rsNode;
        rsNode.addRSLiteral(rsLiterals[0]);}

    protected boolean isBlocked() {
        return rsLiterals[0].rsNode != null;}

    protected void move(RSLiteral rsLiteral) {
        int pos1 = 0;
        for(int i = 0; i < rsLiterals.length; ++i) {
            if(rsLiterals[i] == rsLiteral) pos1 = i;
            if(rsLiterals[i].rsNode != null)  {
                rsLiterals[pos1] = rsLiterals[i-1];
                rsLiterals[i-1] = rsLiteral;
                return;}}}


    /** checks if the literal is in the clause
     *
     * @param literal a literal
     * @return true if the literal is in the clause
     */
    public boolean contains(int literal) {
        int size = rsLiterals.length;
        for(int i = 0; i < size; i += 3) {
            if(rsLiterals[i+1].literal == literal) return true;}
        return false;}

    /** turns the clause into a string
     *
     * @return             the clause as string
     */
    public String toString(){
        return toString(0,null);}

    /** turns the clause into a string
     *
     * @param width        0 or the width of the id-string
     * @param symboltable  null or a symboltable
     * @return             the clause as string
     */
    public String toString(int width, Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        if(width > 0) {
            Formatter format = new Formatter(st, Locale.GERMANY);
            format.format("%-"+width+"d:",id);}
        else st.append(id+": ");
        if(minLimit != 1) st.append(">= "+ minLimit);
        int size = rsLiterals.length;
        for(int i = 0; i < size; ++i) {
            st.append(rsLiterals[i].toString(symboltable));
            if(i < size-3) st.append(",");}
        return st.toString();}


}
