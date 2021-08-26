package Utilities;

import Datastructures.Clauses.Clause;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.Random;
import java.util.Stack;
import java.util.function.Function;

/** This class can store items according to an integer attribute 'bucket' (&ge; 0).
 * Created by ohlbach on 08.06.2019.
 *
 * For each bucket a separate ArrayList is created. The items with this bucket are stored (unsorted) in this list.
 * In order to guarantee adding and removing items in constant time, the items must
 * implement the Positioned interface.
 *
 * Example for the application of this class: <br>
 *     - Literal occurrences contained in clauses are sorted according to the clause length. <br>
 *     - Literal occurrences in shorter clauses come before their occurrence in longer clauses.
 *
 */
public class BucketSortedList<T extends Positioned> implements Iterable<T> {
    /** contains the items, ordered according to the bucket index */
    private ArrayList<ArrayList<T>> buckets;
    /** supplied by the Items class */
    private Function<T,Integer> getBucket;

    /** Constructs a new BucketSorted object
     *
     * @param getBucket     returns the bucket number of the item
     */
    public BucketSortedList(Function<T,Integer> getBucket) {
        this.getBucket = getBucket;
        buckets = new ArrayList<ArrayList<T>>();}

    /** adds a new item to the end of the bucket which s determined by the getBucket method.
     *
     * @param item the item to be added
     */
    public void add(T item) {
        int bucketIndex = getBucket.apply(item);
        int size = buckets.size();
        if(size <= bucketIndex) {
            buckets.ensureCapacity(bucketIndex+1);
            for(int i = size; i <= bucketIndex; ++i) {buckets.add(new ArrayList<T>());}}
        ArrayList<T> bucket = buckets.get(bucketIndex);
        item.setPosition(bucket.size());
        bucket.add(item);}


    /** checks if the item is contained in the buckets
     *
     * @param item an item to be checked
     * @return true if the item is contained in the buckets.
     */
    public boolean contains(T item) {
        int bucketIndex = getBucket.apply(item);
        if(bucketIndex < 0 || bucketIndex >= buckets.size()) {return false;}
        ArrayList<T> bucket = buckets.get(bucketIndex);
        int position = item.getPosition();
        if(position < 0 || position >= bucket.size()) {return false;}
        return item == bucket.get(position);}

    /** removes the item from the bucket (in constant time).
     *  The last item in the item's bucket is moved to the item's current itemPosition.
     *  Trailing empty buckets are removed.
     *
     * @param item the item to be removed
     * @return true if the item was actually removed
     */
    public boolean remove(T item) {
        int bucketIndex = getBucket.apply(item);
        if(bucketIndex  < 0 || bucketIndex >= buckets.size()) {return false;}
        ArrayList<T> bucket = buckets.get(bucketIndex);
        int size = bucket.size();
        int itemPosition =  item.getPosition();
        if(itemPosition < 0 || itemPosition >= size || bucket.get(itemPosition) != item) {return false;}
        item.setPosition(-1);
        if(size == 1) {
            assert 0 == itemPosition;
            bucket.clear(); return true;}
        T lastItem = bucket.get(--size);
        bucket.set(itemPosition,lastItem);
        lastItem.setPosition(itemPosition);
        bucket.remove(size);
        for(int i = buckets.size()-1; i >= 0; --i) { // garbage collection
            if(buckets.get(i).isEmpty()) {buckets.remove(i);}
            else {break;}}
        return true;}

    public int getRandomIndex(Random random) {
        int size = size();
        if(size == 0) {return 0;}
        return random.nextInt(random.nextInt(size)+1);}

    /** returns the index's item
     *  items in smaller buckets are chosen more likely (quadratic) than items in larger buckets.
     *
     * @param index the index in the aggregated lists.
     * @return the item with this index
     */
    public T getItem(int index) {
        int size = size();
        int accumulator = 0;
        for(ArrayList<T> bucket : buckets) {
            int bucketSize = bucket.size();
            int rest = index - accumulator;
            if(rest < bucketSize) {return bucket.get(rest);}
            accumulator += bucketSize;}
        return null;}

    /** joins all items in a new list.
     *
     * @return the joined list of all items.
     */
    public ArrayList<T> getAllItems() {
        ArrayList<T> list = new ArrayList<T>(size());
        for(ArrayList<T> items : buckets) {list.addAll(items);}
        return list;}


    /** returns the buckset with this index
     *
     * @param bucketIndex the index of the bucket
     * @return the bucket with this index.
     */
    public ArrayList<T> getBucket(int bucketIndex) {
        return buckets.get(bucketIndex);}

    /** checks if the buckets are empty
     *
     * @return true if there are no items in the bucket
     */
    public boolean isEmpty() {
        for(ArrayList<T> bucket : buckets) {if(!bucket.isEmpty()) {return false;}}
        return true;}

    /** counts the number of items in the buckets
     *
     * @return the number of items in the buckets
     */
    public int size() {
        int size = 0;
        for(ArrayList<T> bucket : buckets) {size += bucket.size();}
        return size;}

    /** checks if the size of the buckets is 0 or 1
     *
     * @return -1 if the size is > 1, otherwise the size (0 or 1).
     */
    public int size01() {
        int size = 0;
        for(ArrayList<T> bucket : buckets) {
            size += bucket.size();
            if(size > 1) {return -1;}}
        return size;}

    /** checks if the sum of sizes of the buckets is exactly
     *
     * @return true if the sum of the bucket sizes is excactly 1
     */
    public boolean containsOne() {
        int size = 0;
        for(ArrayList<T> bucket : buckets) {
            size += bucket.size();
            if(size > 1) {return false;}}
        return size == 1;}



    /** returns the number of items in the corresponding bucket
     *
     * @return the number of items in buckets(bucket)
     */
    public int size(int bucket) {
        if(bucket < 0 || bucket >= buckets.size()) {return 0;}
        return buckets.get(bucket).size();}

    /** empties the bucket with the given index
     *
     * @param bucket an index for a bucket.
     */
    public void clearBucket(int bucket) {
        if(bucket <0 || bucket >= buckets.size()) {return;}
        buckets.get(bucket).clear();}

    /** lists the entire list as string.
     *
     */
    public String toString() {
        return toString((item-> item.toString()));}

    /** lists the entire list as string.
     *
     * @param itemString a function for mapping an item to a string
     * @return the entire list as string.
     */
    public String toString(Function<T,String> itemString) {
        StringBuilder st = new StringBuilder();
        for(int i = 0; i < buckets.size(); ++i) {
            ArrayList<T> bucket = buckets.get(i);
            if(bucket == null || bucket.isEmpty()) {continue;}
            st.append("Bucket " + i + "\n");
            for(T item : bucket) {st.append("  ").append(itemString.apply(item)).append("\n");}}
        return st.toString();}

    /** checks if all items are in the correct position.
     * If an error is detected, a message is printed and the system stops.
     *
     * @param name of the list
     */
    public void check(String name) {
        for(int i = 0; i < buckets.size(); ++i) {
            ArrayList<T> bucket = buckets.get(i);
            if(bucket == null) {continue;}
            for(int j = 0; j < bucket.size(); ++j) {
                T item = bucket.get(j);
                if(getBucket.apply(item) != i) {
                    System.out.println("Error in BucketSortedList " + name + ": item " + item.toString() +
                            " with bucket " + getBucket.apply(item) + " is in the wrong bucket " + i);
                    new Exception().printStackTrace();
                    System.exit(1);}
                if(item.getPosition() != j) {
                    System.out.println("Error in BucketSortedList " + name + ": item " + item.toString() +
                            " in bucket " + i + " is not in position " + item.getPosition() + ", but in " + j);
                    new Exception().printStackTrace();
                    System.exit(1);}}}}

    /** This method generates an iterator which iterates over the items in the bucket.
     *
     * @return an iterator for iterating over the items in the buckets.
     */
    public BucketIterator iterator() {
        return new BucketIterator(0,buckets.size()-1);}

    /** This method generates an iterator which iterates over the items in the bucket starting with buckets[bucket]
     *
     * @return an iterator for iterating over the items in the buckets.
     */
    public BucketIterator iteratorFrom(int bucket) {
        return new BucketIterator(bucket, buckets.size()-1);}

    /** This method generates an iterator which iterates over the items in the bucket ending with buckets[bucket]
     *
     * @return an iterator for iterating over the items in the buckets.
     */
    public BucketIterator iteratorTo(int bucket) {
        return new BucketIterator(0, bucket);}

    /** collects unused iterators */
    private Stack<BucketIterator> iteratorStack = new Stack<BucketIterator>();

    /** gives an iterator back to the list
     *
     * @param iterator an iterator which is no longer used
     */
    public void pushIterator(Iterator iterator) {iteratorStack.push((BucketIterator)iterator);}

    /** This method returns an iterator which iterates over the items in the bucket.
     * It may reuse old iterators
     *
     * @return an iterator for iterating over the items in the buckets.
     */
    public BucketIterator popIterator() {
        if(iteratorStack.isEmpty()) {return new BucketIterator(0,buckets.size()-1);}
        else {
            BucketIterator iterator = iteratorStack.pop();
            iterator.reset(0,buckets.size()-1);
        return iterator;}}

    /** This method returns an iterator which iterates over the items in the bucket, starting with bucket 'bucket'
     * It may reuse old iterators
     *
     * @param bucket the start bucket index.
     * @return an iterator for iterating over the items in the buckets.
     */
    public BucketIterator popIteratorFrom(int bucket) {
        if(iteratorStack.isEmpty()) {return new BucketIterator(bucket,buckets.size()-1);}
        else {
            BucketIterator iterator = iteratorStack.pop();
            iterator.reset(bucket,buckets.size()-1);
            return iterator;}}

    /** This method returns an iterator which iterates over the items in the bucket, ending with bucket 'bucket' + 1
     * It may reuse old iterators
     *
     * @param bucket the end bucket index.
     * @return an iterator for iterating over the items in the buckets.
     */
    public BucketIterator popIteratorTo(int bucket) {
        if(iteratorStack.isEmpty()) {return new BucketIterator(0,bucket);}
        else {
            BucketIterator iterator = iteratorStack.pop();
            iterator.reset(0,bucket);
            return iterator;}}


    /** constructs a new BucketIterator
     */
    public class BucketIterator implements Iterator<T> {
        int bucketIndex = 0;    // iterates through the buckets
        int positionIndex = -1;   // iterates through a single bucket
        int bucketEnd = 0;    // the index +1 of the last bucket

        /**
         * generates an iterator which iterates over buckets[bucketStart] until buckets[bucketEnd-1]
         *
         * @param bucketStart the index of the first bucket
         * @param bucketEnd   the index of the last bucket
         */
        public BucketIterator(int bucketStart, int bucketEnd) {
            this.bucketEnd = Math.min(buckets.size()-1, bucketEnd);
            for(bucketIndex = bucketStart; bucketIndex <= bucketEnd; ++bucketIndex) {
                if(!buckets.get(bucketIndex).isEmpty()) {break;}}}

        /**
         * This method allows one to reuse the iterator
         *
         * @param bucketStart the index of the first bucket
         * @param bucketEnd   the index + 1 of the last bucket
         */
        public void reset(int bucketStart, int bucketEnd) {
            this.bucketEnd = Math.min(buckets.size()-1, bucketEnd);
            bucketIndex = bucketStart;
            positionIndex = -1;
        }

        /**
         * This method allows one to reuse the iterator
         */
        public void reset() {
            this.bucketEnd = buckets.size()-1;
            bucketIndex = 0;
            positionIndex = -1;
        }


        /**
         * checks if there is a next item in the buckets.
         *
         * @return true if there is another item in the buckets
         */
        public boolean hasNext() {
            if (bucketIndex > bucketEnd) {return false;}
            for (; bucketIndex <= bucketEnd; ++bucketIndex) {
                ArrayList<T> bucket = buckets.get(bucketIndex);
                if (positionIndex >= bucket.size()-1) {
                    positionIndex = -1;
                    continue;}
                ++positionIndex;
                return true;}
            return false;}

        /** yields the next item in the buckets and moves the pointers forward to the next item
         *
         * @return the next item in the buckets.
         */
        public T next() {
            return buckets.get(bucketIndex).get(positionIndex);}

        /** removes the item at the current position.*/
        public void remove() {
            ArrayList<T> bucket = buckets.get(bucketIndex);
            bucket.remove(positionIndex);
            for(int i = positionIndex; i < bucket.size(); ++i) {
                T item = bucket.get(i);
                item.setPosition(item.getPosition()-1);}
            --positionIndex;}

        /** removes the item while keeping the iterator intact.
         *
         * @param item the item to be removed
         */
        public void remove(T item) {
            int bucketIndex = getBucket.apply(item);
            ArrayList<T> bucket = buckets.get(bucketIndex);
            int position = item.getPosition();
            bucket.remove(position);
            for(int i = position; i < bucket.size(); ++i) {
                T itm = bucket.get(i);
                itm.setPosition(itm.getPosition()-1);}
            if(this.bucketIndex == bucketIndex && position == positionIndex) --positionIndex;}}

    /** checks the position of the bucket elements,
     *  used for testing
     */
        private void checkPosition() {
            for(ArrayList<T> list : buckets) {
                for(int i = 0; i < list.size(); ++i) {
                    if(list.get(i).getPosition() != i) {
                        System.out.println("POS " + list.get(i).toString() + " " + i + " " + list.get(i).getPosition());
                    } }}}
}
