package Utilities;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Function;

/** This class can be used to map items to item occurrences, for example literals to literal occurrences.
 * Created by ohlbach on 05.12.2019.
 *
 * The items are sorted:<br>
 *     - by an item index (getItemIndex), e.g. for a CLiteral the literal<br>
 *     - by a bucket index (getBucketIndex), e.g. clause length of the clauses containing the literal occurrence.<br>
 *  In the literal application, this allows one to iterate over clauses containing the literals,<br>
 *      - either from smaller clauses to larger clauses<br>
 *      - or from clauses with a given clause length to longer clauses<br>
 *      - or from small clauses up to clauses with a given length.
 *
 */
public class BucketSortedIndex<T extends Positioned> {
    private BucketSortedList<T>[] posOccurrences;  // maps each positive predicate to the list of occurrences
    private BucketSortedList<T>[] negOccurrences;  // maps each negative predicate to the list of occurrences

    private Function<T,Integer> getItemIndex;      // in the literal example: cLiterals -> literals
    private Function<T,Integer> getBucketIndex;    // in the literal example: cLiterals -> clause length

    private static final ArrayList emptyList = new ArrayList(); // just to optimize things.

    /** Constructs a new index
     *
     * @param size             the maximum item index (e.g. the number of predicates)
     * @param getItemIndex     maps e.g. literal occurrences to literals
     * @param getBucketIndex   maps e.g. literal occurrences to clause length
     */
    public BucketSortedIndex(int size, Function<T,Integer> getItemIndex , Function<T,Integer> getBucketIndex) {
        this.getItemIndex = getItemIndex;
        this.getBucketIndex = getBucketIndex;
        posOccurrences = new BucketSortedList[size+1];
        negOccurrences = new BucketSortedList[size+1];}


    /** adds an item to the index
     *
     * @param item the item to be added
     */
    public void add(T item) {
        int itemIndex    = getItemIndex.apply(item);
        int itemIndexAbs = Math.abs(itemIndex);
        BucketSortedList<T>[] list = itemIndex > 0 ? posOccurrences : negOccurrences;
        BucketSortedList<T> items = list[itemIndexAbs];
        if(items == null) {
            items = new BucketSortedList(getBucketIndex);
            list[itemIndexAbs] = items;}
        items.add(item);}


    /** removes the literal from the index (in constant time)
     *
     * @param item the literal to be removed.
     */
    public void remove(T item) {
        int itemIndex = getItemIndex.apply(item);
        BucketSortedList<T> list =  itemIndex > 0 ? posOccurrences[itemIndex] : negOccurrences[-itemIndex];
        if(list == null) {return;}
        boolean removed = list.remove(item);
        if(removed) {
            int size = list.size();
            if(size == 0) {
                if(itemIndex > 0) {posOccurrences[itemIndex] = null;}
                else              {negOccurrences[-itemIndex] = null;}}}}

    /** removes all entries for the given itemIndex
     *
     * @param itemIndex an item index
     */
    public void clearOne(int itemIndex) {
        if(itemIndex > 0) {posOccurrences[itemIndex]  = null;}
        else              {negOccurrences[-itemIndex] = null;}}

    /** removes positive and negative entries for the given itemIndex
     *
     * @param itemIndex an item index
     */
    public void clearBoth(int itemIndex) {
        posOccurrences[itemIndex]  = null;
        negOccurrences[itemIndex] = null;}

    /** returns the items with the given itemIndex (integer)
     *
     * @param itemIndex the itemIndex (integer)
     * @return the list of all items
     */
    public ArrayList<T> getAllItems(int itemIndex) {
        BucketSortedList<T> list =  itemIndex > 0 ? posOccurrences[itemIndex] : negOccurrences[-itemIndex];
        return list == null ? emptyList : list.getAllItems();}

    /** For the application as literal index, it returns the cLiterals for a given literal and a given clause size
     *
     * @param itemIndex    for example, the literal
     * @param bucketIndex  for example, the clause size
     * @return             for example, the cLiterals with the given literal and the given clause size.
     */
    public ArrayList<T> getItems(int itemIndex, int bucketIndex) {
        BucketSortedList<T> list =  itemIndex > 0 ? posOccurrences[itemIndex] : negOccurrences[-itemIndex];
        return  list == null ? emptyList : list.getBucket(bucketIndex);}


    /** returns the number of items indexed by this itemIndex
     *
     * @param itemIndex a itemIndex
     * @return the number of items indexed by this itemIndex
     */
    public int size(int itemIndex) {
        BucketSortedList<T> list =  itemIndex > 0 ? posOccurrences[itemIndex] : negOccurrences[-itemIndex];
        return list == null ? 0 : list.size();}

    /** returns the number of cLiterals indexed by this literal
     *
     * @param literal a literal
     * @return the number of cLiterals indexed by this literal
     */
    public int size01(int literal) {
        BucketSortedList<T> list =  literal > 0 ? posOccurrences[literal] : negOccurrences[-literal];
        return list == null ? 0 : list.size01();}

    /** checks if the list with the given index is empty
     *
     * @param itemIndex an item index
     * @return true if the list is empty
     */
    public boolean isEmpty(int itemIndex) {
        BucketSortedList<T> list =  itemIndex > 0 ? posOccurrences[itemIndex] : negOccurrences[-itemIndex];
        return list == null || list.isEmpty();}

    /** checks if there is exactly one item with index 'itemIndex' in the index
     *
     * @param itemIndex an item index
     * @return true if there is exactly one item with index 'itemIndex' in the index
     */
    public boolean containsOne(int itemIndex) {
        BucketSortedList<T> list =  itemIndex > 0 ? posOccurrences[itemIndex] : negOccurrences[-itemIndex];
        return list != null && list.containsOne();}

    /** checks if the item is in the index
     *
     * @param item the item to be checked
     * @return true ir the item is in the index
     */
    public boolean contains(T item) {
        int itemIndex    = getItemIndex.apply(item);
        int itemIndexAbs = Math.abs(itemIndex);
        BucketSortedList<T>[] list = itemIndex > 0 ? posOccurrences : negOccurrences;
        if(itemIndexAbs >= list.length) {return false;}
        BucketSortedList<T> items = list[itemIndexAbs];
        if(items == null) {return false;}
        return items.contains(item);}

    /** checks for all predicates the purity and elimination status.
     * For a predicate p <br>
     *     if size(p) == 0 and size(-p) > 0 then p is pure and inserted into zeros. <br>
     *     if size(-p) == 0 and size(p) > 0 then -p is pure and inserted into zeros. <br>
     *     if size(p) == 1 and size(-p) > 0 then p can be eliminated and is inserted into ones. <br>
     *     if size(-p) == 0 and size(p) > 0 then -p can be eliminated and is inserted into zeros.
     *
     * @param zeros for inserting the pure literals
     * @param ones  for inserting the elimination literals
     * @return true if a pure or elimination literal is found.
     */
    public boolean size01(int predicates, ArrayList<Integer> zeros, ArrayList<Integer> ones) {
        zeros.clear(); ones.clear();
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            int size01p = size01(predicate);
            int size01n = size01(-predicate);
            if(size01p == 0 && size01n != 0) {zeros.add(predicate);  continue;}
            if(size01n == 0 && size01p != 0) {zeros.add(-predicate); continue;}
            if(size01p == 1 && size01n != 0) {ones.add(predicate); continue;}
            if(size01n == 1 && size01p != 0) {ones.add(-predicate); continue;}
        }
        return !zeros.isEmpty() || !ones.isEmpty();}

    public boolean zeroes(int predicates,ArrayList<Integer> zeros) {
        zeros.clear();
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            if(isEmpty(predicate) && !isEmpty(-predicate)) {zeros.add(predicate); continue;}
            if(isEmpty(-predicate) && !isEmpty(predicate)) {zeros.add(-predicate); continue;}}
        return !zeros.isEmpty();}

    public int oneOccurrence(int predicates) {
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            if(containsOne(predicate) && !isEmpty(-predicate)) {return predicate;}
            if(containsOne(-predicate) && !isEmpty(predicate)) {return -predicate;}}
        return 0;}


    /** This method generates an iterator which iterates over the items in the index
     *
     * @param itemIndex an item index
     * @return an iterator for iterating over the items in the buckets.
     */
    public Iterator<T> iterator(int itemIndex) {
        BucketSortedList<T> list =  itemIndex > 0 ? posOccurrences[itemIndex] : negOccurrences[-itemIndex];
        return (list == null) ? emptyList.iterator() : list.iterator();}

    /** This method generates an iterator which iterates over the items in the bucket starting with bucket[startPosition]
     *
     * @param itemIndex an item index
     * @param startPosition the start position in the buckets with the given itemIndex
     * @return an iterator for iterating over the items in the buckets.
     */
    public Iterator<T> iteratorFrom(int itemIndex, int startPosition) {
        BucketSortedList<T> list =  itemIndex > 0 ? posOccurrences[itemIndex] : negOccurrences[-itemIndex];
        return (list == null) ? emptyList.iterator() : list.iteratorFrom(startPosition);}

    /** This method generates an iterator which iterates over the items in the bucket ending with bucket[endPosition]
     *
     * @param itemIndex an item index
     * @param endPosition the end position +1 in the buckets with the given itemIndex
     * @return an iterator for iterating over the items in the buckets.
     */
    public Iterator<T> iteratorTo(int itemIndex, int endPosition) {
        BucketSortedList<T> list =  itemIndex > 0 ? posOccurrences[itemIndex] : negOccurrences[-itemIndex];
        return (list == null) ? emptyList.iterator() : list.iteratorTo(endPosition);}

    /** This method returns a used iterator back to the pool
     *
     * @param itemIndex an iterm index
     * @param iterator  the used iterator
     */
    public void pushIterator(int itemIndex, Iterator iterator) {
        BucketSortedList<T> list =  itemIndex > 0 ? posOccurrences[itemIndex] : negOccurrences[-itemIndex];
        if(list != null) list.pushIterator((BucketSortedList.BucketIterator)iterator);}

    /** This method returns an iterator which iterates over the items in the index
     *
     * @param itemIndex an item index
     * @return an iterator for iterating over the items in the buckets.
     */
    public Iterator<T> popIterator(int itemIndex) {
        BucketSortedList<T> list =  itemIndex > 0 ? posOccurrences[itemIndex] : negOccurrences[-itemIndex];
        return (list == null) ? emptyList.iterator() : list.popIterator();}

    /** This method returns an iterator which iterates over the items in the bucket starting with bucket[startPosition]
     *
     * @param itemIndex an item index
     * @param startPosition the start position in the buckets with the given itemIndex
     * @return an iterator for iterating over the items in the buckets.
     */
    public Iterator<T> popIteratorFrom(int itemIndex, int startPosition) {
        BucketSortedList<T> list =  itemIndex > 0 ? posOccurrences[itemIndex] : negOccurrences[-itemIndex];
        return (list == null) ? emptyList.iterator() : list.popIteratorFrom(startPosition);}

    /** This method returns an iterator which iterates over the items in the bucket ending with bucket[endPosition]
     *
     * @param itemIndex an item index
     * @param endPosition the end position +1 in the buckets with the given itemIndex
     * @return an iterator for iterating over the items in the buckets.
     */
    public Iterator<T> popIteratorTo(int itemIndex, int endPosition) {
        BucketSortedList<T> list =  itemIndex > 0 ? posOccurrences[itemIndex] : negOccurrences[-itemIndex];
        return (list == null) ? emptyList.iterator() : list.popIteratorTo(endPosition);}


    /** comprises the index to a string.
     *  It uses the item's toString method
     *
     * @return the entire index as a string.
     */

    public String toString() {
        return toString((item -> item.toString()));}


    /** comprises the index into a string
     *
     * @param toString a function for mapping items to a string.
     * @return the entire index as string.
     */
    public String toString(Function<T,String> toString) {
        int size = posOccurrences.length;
        StringBuilder st = new StringBuilder();
        for(int index = 1; index < size; ++index) {
            StringBuilder posString = null;
            StringBuilder negString = null;
            Iterator<T> it = iterator(index);
            if(!isEmpty(index)) {
                posString = new StringBuilder();
                while(it.hasNext()) {
                    T lit = it.next();
                    posString.append(toString.apply(lit)).append(",");}}
            if(!isEmpty(-index)) {
                negString = new StringBuilder();
                it = iterator(-index);
                while(it.hasNext()) {
                    T lit = it.next();
                    negString.append(toString.apply(lit)).append(",");}}
            if(posString != null) {
                st.append(" ").append(Integer.toString(index)).append(": ").append(posString).append("\n");}
            if(negString != null)
                st.append(Integer.toString(-index)).append(": ").append(negString).append("\n");}
        return st.toString();}

    /** checks the index for consistency.
     * If an inconsistency occurs then an error message is printed and the system stops.
     *
     * @param name of the index
     */
    public void check(String name) {
            int size = posOccurrences.length;
            for(int i = 0; i < size; ++i) {
                BucketSortedList<T> list = posOccurrences[i];
                if(list != null) {
                    list.check("+"+name);
                    for(T item : list) {
                        if(getItemIndex.apply(item) != i) {
                            System.out.println("Error on BucketSortedIndex " + name + ": item " + item.toString() + "" +
                                    " is in index list " + i + " and not in " + getItemIndex.apply(item));
                            new Exception().printStackTrace();
                            System.exit(1);}}}

                list = negOccurrences[i];
                if(list != null) {
                    list.check("-"+name);
                    for(T item : list) {
                        if(getItemIndex.apply(item) != -i) {
                            System.out.println("Error on BucketSortedIndex " + name + ": item " + item.toString() + "" +
                                    " is in index list " + -i + " and not in " + -getItemIndex.apply(item));
                            new Exception().printStackTrace();
                            System.exit(1);}}}}}
}
