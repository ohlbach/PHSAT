package Solvers.Simplifier;

import Datastructures.Symboltable;

/** The Clauses class maintains a doubly connected list of Clause objects.
 */
public class Clauses {

    /** the first clause in the list. */
    public Clause firstClause = null;

    /** the last clause in the list. */
    public Clause lastClause = null;

    /** the number of clauses in the list.*/
    public int size = 0;

    /** adds a clause at the end of the list.
     *
     * @param clause the clause to be added.
     */
    public void addClause(Clause clause) {
        if(firstClause == null) {firstClause = clause; lastClause = clause; return;}
        clause.previousClause = lastClause;
        lastClause.nextClause = clause;
        lastClause = clause;
        ++size;}

    /** removes a clause from the list
     *
     * @param clause the clause to be removed.*/
    public void removeClause(Clause clause) {
        if(clause.nextClause == null) {
            if(clause.previousClause == null) return;
            lastClause = clause.previousClause; clause.previousClause = null; return;}
        if(clause.previousClause == null) {
            firstClause = clause.nextClause; clause.nextClause = null; return;}
        Clause previous = clause.previousClause;
        Clause next = clause.nextClause;
        previous.nextClause = next;
        next.previousClause = previous;
        clause.previousClause = null;
        clause.nextClause = null;
        --size;}

    /** returns the number of clauses in the list.
     *
     * @return the number of clauses in the list.
     */
    public int size() {return size;}

    /** generates a string containing all clauses in the list.
     *
     * @return a string containing all clauses in the list.
     */
    public String toString() {
        return toString(null);}

    /** generates a string containing all clauses in the list.
     *
     * @param symboltable null or a symboltable.
     * @return a string containing all clauses in the list.
     */
    public String toString(Symboltable symboltable) {
        if(firstClause == null) return "";
        StringBuilder st = new StringBuilder();
        int maxId = 0;
        Clause clause = firstClause;
        while(clause != null) {
            maxId = Math.max(maxId,clause.id);
            clause = clause.nextClause;}
        int size = Integer.toString(maxId).length();
        clause = firstClause;
        while(clause != null) {
            st.append(clause.toString(symboltable,size)).append("\n");
            clause = clause.nextClause;}
        return st.toString();
    }

}
