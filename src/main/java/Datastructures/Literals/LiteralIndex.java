package Datastructures.Literals;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.function.Consumer;

/**
 * Created by Ohlbach on 25.08.2018.
 *
 * This index maps literals (integers) to their CLiterals (clause occurrences)
 */
public class LiteralIndex {
    private int predicates;   // number of predicates
    private LinkedList<CLiteral>[] posOccurrences;  // maps each positive predicate to the list of occurrences
    private LinkedList<CLiteral>[] negOccurrences;  // maps each negative predicate to the list of occurrences
    private static LinkedList<CLiteral> emptyList = new LinkedList<>();
    public ArrayList<Consumer<Integer>> purityObservers = new ArrayList<>();

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
        LinkedList<CLiteral> list =  literal > 0 ? posOccurrences[literal] : negOccurrences[-literal];
        if(list == null) {return;}
        list.remove(cliteral);
        if(list.isEmpty()) {for(Consumer<Integer> observer : purityObservers) {observer.accept(-literal);}}}

    /** removes the literal from the index
     *
     * @param cliteral the literal to be removed.
     */
    public void removeLiteral(int literal, CLiteral cliteral) {
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
        LinkedList<CLiteral> list =  literal > 0 ? posOccurrences[literal] : negOccurrences[-literal];
        return list == null ? emptyList : list;}

    /** checks if the literal is pure, i.e. there are no complementary literals
     *
     * @param literal a literal
     * @return true if the literal is pure, i.e. there are no complementary literals.
     */
    public boolean isPure(int literal) {
        LinkedList<CLiteral> occurrence = (literal > 0) ? negOccurrences[literal] : posOccurrences[-literal];
        return occurrence == null || occurrence.isEmpty();}


    /** returns all pure literals
     *
     * @return all pure literals.
     */
    public ArrayList<Integer> pureLiterals() {
        ArrayList<Integer> pure = new ArrayList<>();
        for(int predicate = 1; predicate < predicates; ++predicate) {
            if(isPure(predicate)) {pure.add(predicate);}
            else {if(isPure(-predicate)) {pure.add(-predicate);}}}
        return pure;}

    /** comprises the index into a string
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
