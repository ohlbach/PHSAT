package Datastructures.Literals;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseStructure;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.PriorityQueue;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

/**
 * Created by  Ohlbach on 25.08.2018.
 *
 * This index maps literals (integers) to their CLiterals (clause occurrences)
 */
public class LiteralIndex {
    private int predicates;   // number of predicates
    private PriorityQueue<CLiteral>[] posOccurrences;  // maps each positive predicate to the list of occurrences
    private PriorityQueue<CLiteral>[] negOccurrences;  // maps each negative predicate to the list of occurrences
    private int[][] posOccurrenceCounters = null;
    private int[][] negOccurrenceCounters = null;
    private static PriorityQueue<CLiteral> emptyList = new PriorityQueue<>();
    public ArrayList<BiConsumer<Integer,ClauseStructure>> purityObservers = new ArrayList<>();
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


    /** initializes all OccurrenceCounters
     * The OccurrenceCounter for positive literals counts [posOccurrences,mixedOccurrences]<br>
     * The OccurrenceCounter for negative literals counts [negOccurrences,mixedOccurrences]<br>
     * posOccurrences counts how often the literal occurrs in positive clauses,
     * the others are analogous.
     */
    public void initializeOccurrenceCounters() {
        posOccurrenceCounters = new int[predicates+1][];
        negOccurrenceCounters = new int[predicates+1][];
        for(int predicate = 1; predicate < predicates; ++predicate) {
            posOccurrenceCounters[predicate] = new int[]{0,0};
            negOccurrenceCounters[predicate] = new int[]{0,0};}}

    /** updates the Occurrence counters
     *
     * @param cliteral the literal for which the counter is to be updated.
     */
    public void addLiteralOccurrence(CLiteral cliteral) {
        int literal = cliteral.literal;
        if(literal > 0) {++posOccurrenceCounters[literal][cliteral.clause.structure == ClauseStructure.POSITIVE ? 0:1];}
        else {++negOccurrenceCounters[literal][cliteral.clause.structure == ClauseStructure.NEGATIVE ? 0:1];}}


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
        lits.add(cliteral);
        if(posOccurrenceCounters != null && cliteral.clause != null) {addLiteralOccurrence(cliteral);}}



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
            else            {negOccurrences[-literal] = null;}}
        if(posOccurrenceCounters != null && cliteral.clause != null) {
            if(literal > 0) {--posOccurrenceCounters[literal][cliteral.clause.structure == ClauseStructure.POSITIVE ? 0:1];}
            else {--negOccurrenceCounters[literal][cliteral.clause.structure == ClauseStructure.NEGATIVE ? 0:1];}
            int predicate = Math.abs(literal);
            ClauseStructure st = purityStatus(predicate);
            if(st != null) {for(BiConsumer<Integer,ClauseStructure> observer : purityObservers) {observer.accept(predicate,st);}}}
        }



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


    /** checks if the predicate is pure within the indexed clauses.
     * A positive literal is pure if its negation does not occur in the negative clauses.
     * A negative literal is pure if its negation does not occur in the positive clauses.
     * If a predicate occurs only in the mixed clauses then it is positive pure and negative pure.
     * The actual purity status, however, depends also on the ImplicationGraph.
     *
     * @param predicate a predicate
     * @return POSTITIVE or NEGATIVE or BOTH
     */
    public ClauseStructure purityStatus(int predicate) {
        assert predicate > 0;
        int posPos = posOccurrenceCounters[predicate][0];
        int negNeg = negOccurrenceCounters[predicate][0];
        if(posPos == 0 && negNeg == 0) {return ClauseStructure.BOTH;}
        if(posPos == 0) {return ClauseStructure.NEGATIVE;}
        if(negNeg == 0) {return ClauseStructure.POSITIVE;}
        return null;}

    /** Determines if the positive predicate or its negation should be considered pure.
     * The predicate must be both positive and negative pure (it occurs only in the mixed clauses).
     * In this case the version which shortens most clauses should be chosen.
     *
     * @param predicate a predicate
     * @return +predicate or -predicate, the version which shortens most clauses.
     */
    public int preferredPurityStatus(int predicate) {
        assert predicate > 0;
        return  posOccurrenceCounters[predicate][1] >  negOccurrenceCounters[predicate][1] ? -predicate : predicate;}

    /** returns for all pure predicate [predicate,ClauseStructure]
     *
     * @return all pure predicates.
     */
    public ArrayList<Object[]> pureLiterals() {
        ArrayList<Object[]> pure = new ArrayList<>();
        for(int predicate = 1; predicate < predicates; ++predicate) {
            ClauseStructure st = purityStatus(predicate);
            if(st != null) {pure.add(new Object[]{predicate,st});}}
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
