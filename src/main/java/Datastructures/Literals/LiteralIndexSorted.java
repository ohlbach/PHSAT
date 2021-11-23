package Datastructures.Literals;

import Utilities.BucketSortedList;

import java.util.*;
import java.util.function.Function;

/**
 * Created by ohlbach on 25.06.2019.
 */
public class LiteralIndexSorted extends LiteralIndex {
    private Function<CLiteralOld,Integer> bucketIndex = cliteral->cliteral.clause.size();
    private BucketSortedList<CLiteralOld>[] posOccurrences;  // maps each positive predicate to the list of occurrences
    private BucketSortedList<CLiteralOld>[] negOccurrences;  // maps each negative predicate to the list of occurrences
    private ArrayList<CLiteralOld> emptyList = new ArrayList();

    /** constructs an index for a given number of predicates
     *
     * @param predicates the number of predicates
     */
    public LiteralIndexSorted(int predicates) {
        super(predicates);
        posOccurrences = new BucketSortedList[predicates + 1];
        negOccurrences = new BucketSortedList[predicates + 1];
    }

    /** constructs an index for a given number of predicates
     *
     * @param predicates the number of predicates
     */
    public LiteralIndexSorted(int predicates, Comparator<CLiteralOld> comparator) {
        super(predicates);
        posOccurrences = new BucketSortedList[predicates + 1];
        negOccurrences = new BucketSortedList[predicates + 1];
    }


    /** adds a literal to the index
     *
     * @param cliteral the literal to be added
     */
    public void addLiteral(CLiteralOld cliteral) {
        int literal = cliteral.literal;
        int predicate = Math.abs(literal);
        BucketSortedList<CLiteralOld>[] list = literal > 0 ? posOccurrences : negOccurrences;
        BucketSortedList<CLiteralOld> lits = list[predicate];
        if(lits == null) {
            lits = new BucketSortedList(bucketIndex);
            list[predicate] = lits;}
        lits.add(cliteral);}

    /** removes the literal from the index (in constant time)
     *
     * @param cliteral the literal to be removed.
     */
    public void removeLiteral(CLiteralOld cliteral) {
        int literal = cliteral.literal;
        BucketSortedList<CLiteralOld> list =  literal > 0 ? posOccurrences[literal] : negOccurrences[-literal];
        if(list == null) {return;}
        int size = list.size()-1;
        if(size == 0) {
            if(literal > 0) {posOccurrences[literal] = null;}
            else            {negOccurrences[-literal] = null;}}
        else {list.remove(cliteral);}
    }

    /** removes all entries for the given literal
     *
     * @param literal a literal
     */
    public void clearLiteral(int literal) {
        if(literal > 0) {posOccurrences[literal] = null;}
        else            {negOccurrences[-literal] = null;}}

    /** removes all entries for the given literal
     *
     * @param predicate a predicate
     */
    public void clearPredicate(int predicate) {
        assert predicate > 0;
        posOccurrences[predicate] = null;
        negOccurrences[-predicate] = null;}



    /** returns the CLiterals with the given literal (integer)
     *
     * @param literal the literal (integer)
     * @return the list of occurrences (CLiterals)
     */
    public AbstractCollection<CLiteralOld> getLiterals(int literal) {
        assert literal != 0 && (Math.abs(literal) <= predicates);
        BucketSortedList<CLiteralOld> list =  literal > 0 ? posOccurrences[literal] : negOccurrences[-literal];
        return list == null ? emptyList : list.getAllItems();}



    /** returns the number of cLiterals indexed by this literal
     *
     * @param literal a literal
     * @return the number of cLiterals indexed by this literal
     */
    public int size(int literal) {
        BucketSortedList<CLiteralOld> list =  literal > 0 ? posOccurrences[literal] : negOccurrences[-literal];
        return list == null ? 0 : list.size();}



    public boolean isEmpty(int literal) {
        BucketSortedList<CLiteralOld> list =  literal > 0 ? posOccurrences[literal] : negOccurrences[-literal];
        return list == null || list.isEmpty();}

    public Iterator<CLiteralOld> iterator(int literal) {
        BucketSortedList<CLiteralOld> list =  literal > 0 ? posOccurrences[literal] : negOccurrences[-literal];
        return (list == null) ? emptyList.iterator() : list.iterator();}


    /** This method generates an iterator which iterates over the items in the bucket starting with bucket[position]
     *
     * @return an iterator for iterating over the items in the buckets.
     */
    public Iterator<CLiteralOld> iteratorFrom(int literal, int position) {
        BucketSortedList<CLiteralOld> list =  literal > 0 ? posOccurrences[literal] : negOccurrences[-literal];
        return (list == null) ? emptyList.iterator() : list.iteratorFrom(position);}

    /** This method generates an iterator which iterates over the items in the bucket ending with bucket[position]
     *
     * @return an iterator for iterating over the items in the buckets.
     */
    public Iterator<CLiteralOld> iteratorTo(int literal, int position) {
        BucketSortedList<CLiteralOld> list =  literal > 0 ? posOccurrences[literal] : negOccurrences[-literal];
        return (list == null) ? emptyList.iterator() : list.iteratorTo(position);}



}
