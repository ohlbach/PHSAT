package Datastructures.Literals;

import Datastructures.Clauses.Clause;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.PriorityQueue;
import java.util.function.Consumer;

/**
 * Created by Ohlbach on 25.08.2018.
 *
 * This index maps literals (integers) to their CLiterals (clause occurrences)
 */
public class LiteralIndex {
    private int predicates;   // number of predicates
    private PriorityQueue<CLiteral>[] posOccurrences;  // maps each positive predicate to the list of occurrences
    private PriorityQueue<CLiteral>[] negOccurrences;  // maps each negative predicate to the list of occurrences
    private static PriorityQueue<CLiteral> emptyList = new PriorityQueue<>();
    public ArrayList<Consumer<Integer>> purityObservers = new ArrayList<>();
    private Comparator<CLiteral> comparator = Comparator.comparingInt(clit->{
        Clause clause = clit.clause;
        return clause == null ? 0 : clause.size();});

    /** constructs an index for a given number of predicates
     *
     * @param predicates the number of predicates
     */
    public LiteralIndex(int predicates) {
        assert predicates > 0;
        this.predicates = predicates;
        posOccurrences = new PriorityQueue[predicates+1];
        negOccurrences = new PriorityQueue[predicates+1];}

    /** constructs an index for a given number of predicates
     *
     * @param predicates the number of predicates
     * @param comparator for comparing two CLiterals.
     */
    public LiteralIndex(int predicates, Comparator<CLiteral> comparator) {
        assert predicates > 0;
        this.predicates = predicates;
        this.comparator = comparator;
        posOccurrences = new PriorityQueue[predicates+1];
        negOccurrences = new PriorityQueue[predicates+1];}



    /** adds a literal to the index
     *
     * @param cliteral the literal to be added
     */
    public void addLiteral(CLiteral cliteral) {
        int literal = cliteral.literal;
        int predicate = Math.abs(literal);
        PriorityQueue<CLiteral>[] list = literal > 0 ? posOccurrences : negOccurrences;
        PriorityQueue<CLiteral> lits = list[predicate];
        if(lits == null) {
            lits = new PriorityQueue<CLiteral>(comparator);
            list[predicate] = lits;}
        lits.add(cliteral);}

    /** removes the literal from the index
     *
     * @param cliteral the literal to be removed.
     */
    public void removeLiteral(CLiteral cliteral) {
        int literal = cliteral.literal;
        PriorityQueue<CLiteral> list =  literal > 0 ? posOccurrences[literal] : negOccurrences[-literal];
        if(list == null) {return;}
        list.remove(cliteral);
        if(list.isEmpty()) {
            if(literal > 0) {posOccurrences[literal] = null;}
            else            {negOccurrences[-literal] = null;}
            for(Consumer<Integer> observer : purityObservers) {observer.accept(-literal);}}}


    /** returns the CLiterals with the given literal (integer)
     *
     * @param literal the literal (integer)
     * @return the list of occurrences (CLiterals)
     */
    public PriorityQueue<CLiteral> getLiterals(int literal) {
        assert literal != 0 && (Math.abs(literal) <= predicates);
        PriorityQueue<CLiteral> list =  literal > 0 ? posOccurrences[literal] : negOccurrences[-literal];
        return list == null ? emptyList : list;}

    /** returns the number of cLiterals indexed by this literal
     *
     * @param literal a literal
     * @return the number of cLiterals indexed by this literal
     */
    public int size(int literal) {
        PriorityQueue<CLiteral> list =  literal > 0 ? posOccurrences[literal] : negOccurrences[-literal];
        return list == null ? 0 : list.size();}

    /** checks if the literal is pure, i.e. there are no complementary literals.
     *  If the literal is not at all in the index, it is not considered pure.
     *
     * @param literal a literal
     * @return true if the literal is pure, i.e. there are no complementary literals.
     */
    public boolean isPure(int literal) {
        int predicate = Math.abs(literal);
        if(posOccurrences[predicate] == null && negOccurrences[predicate] == null) {return false;}
        PriorityQueue<CLiteral> occurrence = (literal > 0) ? negOccurrences[literal] : posOccurrences[-literal];
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
        StringBuilder st = new StringBuilder();
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            PriorityQueue<CLiteral> pos = posOccurrences[predicate];
            PriorityQueue<CLiteral> neg = negOccurrences[predicate];
            StringBuilder posString = null;
            StringBuilder negString = null;
            if(pos != null) {
                posString = new StringBuilder();
                for(CLiteral lit : pos) {posString.append(lit.toFullString()).append(",");}}
            if(neg != null) {
                negString = new StringBuilder();
                for(CLiteral lit : neg) {negString.append(lit.toFullString()).append(",");}}
            if(posString != null) {
                st.append(" ").append(Integer.toString(predicate)).append(": ").append(posString).append("\n");}
            if(negString != null)
                st.append(Integer.toString(-predicate)).append(": ").append(negString).append("\n");}
        return st.toString();}

}
