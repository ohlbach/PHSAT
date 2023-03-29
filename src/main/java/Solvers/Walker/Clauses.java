package Solvers.Walker;

import Datastructures.Symboltable;

/** This class implements a bidirectionally linked list of clauses.
 * <br>
 * It relies on the clause's attributes: previousClause and nextClause.<br>
 * The attribute isInList indicates that the clause is in the list.<br>
 * Adding and removing clauses requires only constant time.<br>
 * The list is used to store the false clauses in the walker solver.
 */
public class Clauses {

    /** the first clause in the list. */
    Clause firstClause;
    /** the last clause in the list. */
    Clause lastClause;

    /** constructs an empty list.
     */
    public Clauses() {}

    /** adds a clause to the front of the list.
     *
     * @param clause the clause to be added.
     */
    public void addToFront(Clause clause) {
        clause.isInList = true;
        clause.previousClause = null; clause.nextClause = null;
        if(firstClause == null) {firstClause = clause; lastClause = clause; return;}
        if(firstClause.nextClause == null) {lastClause = firstClause;}
        clause.nextClause = firstClause;
        firstClause.previousClause = clause;
        firstClause = clause;
    }
    /** adds a clause to the back of the list.
     *
     * @param clause the clause to be added.
     */
    public void addToBack(Clause clause) {
        clause.isInList = true;
        clause.previousClause = null; clause.nextClause = null;
        if(firstClause == null) {firstClause = clause; lastClause = clause; return;}
        if(firstClause.nextClause == null) {lastClause = firstClause;}
        clause.previousClause = lastClause;
        lastClause.nextClause = clause;
        lastClause = clause;}

    /** removes the given clause from the list.
     *
     * @param clause the clause to be removed.
     */
    public void remove(Clause clause) {
        clause.isInList = false;
        if(lastClause == firstClause) {lastClause = null; firstClause = null; return;}
        if(clause.previousClause == null) {
            firstClause = clause.nextClause;
            return;}
        if(clause.nextClause == null) {
            lastClause = clause.previousClause;
            return;}
        Clause claus = clause.previousClause;
        claus.nextClause = clause.nextClause;
        clause.nextClause.previousClause = claus;
    }

    /** counts the clauses in the list.
     *
     * @return the number of clauses in the list.
     */
    public int size() {
        if(firstClause == null) return 0;
        int counter = 0;
        Clause clause = firstClause;
        while(clause != null) {
            if(!clause.isInList) {clause = clause.nextClause; continue;}
            ++counter; clause = clause.nextClause;}
        return counter;}

    /** collects the clauses in a string, one per line.
     *
     * @return the clauses as a string.
     */
    public String toString() {
        return toString(null);}

    /** collects the clauses in a string, one per line.
     *
     * @param symboltable null or a symboltable.
     * @return the clauses as a string.
     */
    public String toString(Symboltable symboltable) {
        if(firstClause == null) return "";
        StringBuilder st = new StringBuilder();
        Clause clause = firstClause;
        while(clause != null) {
            if(!clause.isInList) {clause = clause.nextClause; continue;}
            st.append(clause.toString(symboltable,0)).append("\n");
            clause = clause.nextClause;}
        return st.toString();}
    
}
