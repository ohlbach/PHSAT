package Datastructures.Literals;

import java.util.LinkedList;

/**
 * Created by Ohlbach on 25.08.2018.
 *
 * This index maps literals (integers) to their CLiterals (clause occurrences)
 */
public class LiteralIndex {
    private int predicates;   // number of predicates
    private LinkedList<CLiteral>[] posOccurrences;  // maps each positive predicate to the list of occurrences
    private LinkedList<CLiteral>[] negOccurrences;  // maps each negative predicate to the list of occurrences

    /** constructs an index for a given number of predicates
     *
     * @param predicates the number of predicates
     */
    public LiteralIndex(int predicates) {
        assert predicates > 0;
        this.predicates = predicates;
        posOccurrences = new LinkedList[predicates+1];
        negOccurrences = new LinkedList[predicates+1];}

    /** adds a literal to the index
     *
     * @param cliteral the literal to be added
     */
    public void addLiteral(CLiteral cliteral) {
        int literal = cliteral.literal;
        int predicate = Math.abs(literal);
        LinkedList<CLiteral>[] list = literal > 0 ? posOccurrences : negOccurrences;
        LinkedList<CLiteral> lits = list[predicate];
        if(lits == null) {
            lits = new LinkedList<CLiteral>();
            list[predicate] = lits;}
        lits.add(cliteral);}

    /** removes the literal from the index
     *
     * @param cliteral the literal to be removed.
     */
    public void removeLiteral(CLiteral cliteral) {
        int literal = cliteral.literal;
        int predicate = Math.abs(literal);
        if(literal > 0) {posOccurrences[predicate].remove(cliteral);}
        else {negOccurrences[predicate].remove(cliteral);}
    }

    /** returns the CLiterals with the given literal (integer)
     *
     * @param literal the literal (integer)
     * @return the list of occurrences (CLiterals)
     */
    public LinkedList<CLiteral> getLiterals(int literal) {
        assert literal != 0 && (Math.abs(literal) <= predicates);
        return literal > 0 ? posOccurrences[literal] : negOccurrences[-literal];}

    /** comrises the index into a string
     *
     * @return the entire index as string.
     */
    public String toString() {
        StringBuffer st = new StringBuffer();
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            LinkedList<CLiteral> pos = posOccurrences[predicate];
            LinkedList<CLiteral> neg = negOccurrences[predicate];
            StringBuffer posString = null;
            StringBuffer negString = null;
            if(pos != null) {
                posString = new StringBuffer();
                for(CLiteral lit : pos) {posString.append(lit.toFullString()).append(",");}}
            if(neg != null) {
                negString = new StringBuffer();
                for(CLiteral lit : neg) {negString.append(lit.toFullString()).append(",");}}
            if(posString != null) {
                st.append(" ").append(Integer.toString(predicate)).append(": ").append(posString).append("\n");}
            if(negString != null)
                st.append(Integer.toString(-predicate)).append(": ").append(negString).append("\n");}
        return st.toString();}

}
