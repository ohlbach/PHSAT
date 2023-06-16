package Solvers.Resolution;

import Datastructures.Symboltable;

/** The Clauses class maintains a doubly connected list of Clause objects.<br>
 * New clauses are appended at the end of the list.<br>
 * Clauses are removed with the following precautions:<br>
 * - clause.exists is set to false;<br>
 * - clause.nextClause remains as it is. This way iterating with a pointer to the removed clause still works.
 *   One has just to check clause.exists.
 */
public class Clauses {

    /** the first clause in the list. */
    Clause firstClause = null;

    /** the last clause in the list. */
    Clause lastClause = null;

    /** the number of clauses in the list.*/
    int size = 0;

    /** the number of clauses with positive literals only. */
    int positiveClauses = 0;

    /** the number of clauses with negative literals only. */
    int negativeClauses = 0;

    /** the number of clauses with &gt;= limit positive and negative literals. */
    int positiveNegativeClauses = 0;

    /** the number of clauses with &gt;= limit positive literals, but not enough negative literals. */
    int mixedPositiveClauses = 0;

    /** the number of clauses with &gt;= limit negative literals, but not enough positive literals. */
    int mixedNegativeClauses = 0;

    /** the number of clauses where neither the number of positive nor the number of negative literals are sufficient to make the clause true. */
    int mixedMixedClauses = 0;

    /** +1 if a model for positive literals is sufficient,
     * -1 if a model for negative literals is sufficient,
     * 0 otherwise. */
    byte status = 0;

    /** updates the clause numbers (positive, negative etc.)
     *
     * @param clause a clause
     * @param change typically +1 or -1. The change is added to the corresponding clause type number.
     */
    void updateClauseNumbers(final Clause clause, final int change) {
        switch(clause.clauseType) {
            case POSITIVE:         positiveClauses         += change; break;
            case NEGATIVE:         negativeClauses         += change; break;
            case POSITIVENEGATIVE: positiveNegativeClauses += change; break;
            case MIXEDPOSITIVE:    mixedPositiveClauses    += change; break;
            case MIXEDNEGATIVE:    mixedNegativeClauses    += change; break;
            case MIXEDMIXED:       mixedMixedClauses       += change;}
        status = 0;
        if(mixedMixedClauses == 0) {
            if(negativeClauses == 0 && mixedNegativeClauses == 0)       status = 1;
            else {if(positiveClauses == 0 && mixedPositiveClauses == 0) status = -1;}
    }}

    /** checks the consistency of the clause numbers.
     *
     * @return null or an error message.
     */
    String checkClauseNumbers() {
        Clause clause = firstClause;
        int positiveClauses = 0;
        int negativeClauses = 0;
        int positiveNegativeClauses = 0;
        int mixedPositiveClauses = 0;
        int mixedNegativeClauses = 0;
        int mixedMixedClauses = 0;
        while(clause != null) {
            if(!clause.exists) clause = clause.nextClause;
            switch(clause.clauseType) {
                case POSITIVE:         positiveClauses         += 1; break;
                case NEGATIVE:         negativeClauses         += 1; break;
                case POSITIVENEGATIVE: positiveNegativeClauses += 1; break;
                case MIXEDPOSITIVE:    mixedPositiveClauses    += 1; break;
                case MIXEDNEGATIVE:    mixedNegativeClauses    += 1; break;
                case MIXEDMIXED:       mixedMixedClauses       += 1;}
            clause = clause.nextClause;}
        StringBuilder st = new StringBuilder();
        if(positiveClauses != this.positiveClauses) st.append("Positive Clauses: expected: ").
                append(this.positiveClauses).append( " actual: ").append(positiveClauses).append("\n");
        if(negativeClauses != this.negativeClauses) st.append("Negative Clauses: expected: ").
                append(this.negativeClauses).append( " actual: ").append(negativeClauses).append("\n");
        if(positiveNegativeClauses != this.positiveNegativeClauses) st.append("PositiveNegative Clauses: expected: ").
                append(this.positiveNegativeClauses).append( " actual: ").append(positiveNegativeClauses).append("\n");
        if(mixedPositiveClauses != this.mixedPositiveClauses) st.append("MixedPositive Clauses: expected: ").
                append(this.mixedPositiveClauses).append( " actual: ").append(mixedPositiveClauses).append("\n");
        if(mixedNegativeClauses != this.mixedNegativeClauses) st.append("MixedNegative Clauses: expected: ").
                append(this.mixedNegativeClauses).append( " actual: ").append(mixedNegativeClauses).append("\n");
        if(mixedMixedClauses != this.mixedMixedClauses) st.append("MixedMixed Clauses: expected: ").
                append(this.mixedMixedClauses).append( " actual: ").append(mixedMixedClauses).append("\n");
        if(st.length() == 0) return null;
        return "Inconsistent Numbers in Clauses:\n" + st.toString();
    }


    /** adds a clause at the end of the list.
     *
     * @param clause the clause to be added.
     * @return the new number of clauses.
     */
    public int addClause(final Clause clause) {
        clause.exists = true;
        updateClauseNumbers(clause,1);
        if(firstClause == null) {firstClause = clause; lastClause = clause; size = 1;  return 1;}
        clause.previousClause = lastClause;
        lastClause.nextClause = clause;
        lastClause = clause;
        return ++size;}

    /** removes a clause from the list.<br>
     * clause.exists is set to false.<br>
     * The removed clause's nextClause remains as it is.
     * This way forward iterations with a pointer pointing to the removed clause still work.<br>
     * One has to check clause.exists!
     *
     * @param clause the clause to be removed.
     * @return the new number of clauses in the list.
     * */
    public int removeClause(final Clause clause) {
        if(!clause.exists) return size;
        clause.exists = false;
        if(clause.nextClause == null) { // it is the clause at the end of the chain.
            if(clause == firstClause) {size = 0; firstClause = null;
                clause.previousClause = null;  return 0;}
            if(clause.previousClause == null)  {
                System.out.println("ERROR: trying to remove not inserted clause: " + clause);
                new Exception().printStackTrace();
                System.exit(1);} // the clause is not linked.
            final Clause previousClause = clause.previousClause;
            lastClause = previousClause; clause.previousClause = null;
            previousClause.nextClause = null; return --size;}
        if(clause.previousClause == null) { // it is the first clause in the chain
            firstClause = clause.nextClause; firstClause.previousClause = null; return --size;}

        final Clause previous = clause.previousClause;  // now the clause is in the middle.
        final Clause next = clause.nextClause;
        previous.nextClause = next;
        next.previousClause = previous;
        clause.previousClause = null;
        return --size;}

    /** returns the number of clauses in the list.
     *
     * @return the number of clauses in the list.
     */
    public int size() {return size;}

    /** checks if the list is empty.
     *
     * @return true if the list is empty.
     */
    public boolean isEmpty() {return firstClause == null;}

    /** removes all clauses from the index
     * This is mainly for testing purposes.
     */
    public void clear() {
        firstClause = null;
        lastClause = null;
        size = 0;
        positiveClauses          = 0;
        negativeClauses          = 0;
        positiveNegativeClauses  = 0;
        mixedPositiveClauses     = 0;
        mixedNegativeClauses     = 0;
        mixedMixedClauses        = 0;}

    /** returns a survey of the clause numbers as string.
     *
     * @return a survey of the clause numbers as string.
     */
    public String numbers() {
        return "Clause Statistics:" +
                "\nnumber of clauses:         " + size +
                "\npositive clauses:          " + positiveClauses +
                "\nnegative clauses:          " + negativeClauses +
                "\npositive-negative clauses: " + positiveNegativeClauses +
                "\nmixed positive clauses:    " + mixedPositiveClauses +
                "\nmixed negative clauses:    " + mixedNegativeClauses +
                "\nmixed-mixed clauses:       " + mixedMixedClauses;
    }

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
    public String toString(final Symboltable symboltable) {
        if(firstClause == null) return "";
        StringBuilder st = new StringBuilder();
        int maxId = 0;
        Clause clause = firstClause;
        while(clause != null) {
            maxId = Math.max(maxId,clause.identifier);
            clause = clause.nextClause;}
        int size = Integer.toString(maxId).length();
        clause = firstClause;
        while(clause != null) {
            st.append(clause.toString(symboltable,size)).append("\n");
            clause = clause.nextClause;}
        return st.toString();
    }

}
