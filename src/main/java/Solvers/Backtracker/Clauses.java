package Solvers.Backtracker;

import Datastructures.Symboltable;

public class Clauses {

    /** the first clause in the list. */
    Clause firstClause;
    /** the last clause in the list. */
    Clause lastClause;

    /** the number of clauses in the list */
    int size = 0;

    /** constructs an empty list.*/
    Clauses() {}

    /** adds a clause to the back of the list.
     *
     * @param clause the clause to be added.
     */
    void addClause(Clause clause) {
        ++size;
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
    void remove(Clause clause) {
        --size;
        if(lastClause == firstClause) {lastClause = null; firstClause = null; return;}
        if(clause.previousClause == null) {
            firstClause = clause.nextClause;
            firstClause.previousClause = null;
            return;}
        if(clause.nextClause == null) {
            lastClause = clause.previousClause;
            lastClause.nextClause = null;
            return;}
        Clause claus = clause.previousClause;
        claus.nextClause = clause.nextClause;
        clause.nextClause.previousClause = claus;
    }

    boolean isEmpty() {
        return firstClause == null;}

    /** counts the clauses in the list.
     *
     * @return the number of clauses in the list.
     */
    int size() {return size;}

    /** returns the nth clause in the list.
     *
     * @param n an integer.
     * @return the nth clause in the list.
     */
    Clause getClause(int n) {
        int counter = -1;
        Clause clause = firstClause;
        while(clause != null) {
            if(++counter == n) return clause;
            clause = clause.nextClause;}
        return null;}

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
            st.append(clause.toString(symboltable,5)).append("\n");
            clause = clause.nextClause;}
        return st.toString();}

}
