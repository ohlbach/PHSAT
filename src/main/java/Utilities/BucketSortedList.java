package Utilities;

import java.util.ArrayList;
import java.util.Random;
import java.util.function.Function;

/** This class can store items according to an attribute 'bucket' (&ge; 0).
 * Created by ohlbach on 08.06.2019.
 *
 * For each bucket a separate ArrayList is created. The items with this bucket are stored (unsorted) in this list.
 * In order to guarantee adding and removing items in constant time, the items must
 * implement the Positioned interface.
 */
public class BucketSortedList<T extends Positioned> {
    private ArrayList<ArrayList<T>> buckets;
    private Function<T,Integer> getBucket;

    /** Constructs a new BucketSorted object
     *
     * @param getBucket     returns the bucket number of the item
     */
    public BucketSortedList(Function<T,Integer> getBucket) {
        this.getBucket = getBucket;
        buckets = new ArrayList<ArrayList<T>>();}

    /** adds a new item to the buckets
     *
     * @param item the item to be added
     */
    public void add(T item) {
        int position = getBucket.apply(item);
        if(position >= buckets.size()) {
            for(int i = buckets.size(); i <= position; ++i) {buckets.add(new ArrayList<T>());}}
        ArrayList<T> bucket = buckets.get(position);
        item.setPosition(bucket.size());
        bucket.add(item);}

    /** checks if the item is contained in the buckets
     *
     * @param item an item to be checked
     * @return true if the item is contains in the buckets.
     */
    public boolean contains(T item) {
        int index = getBucket.apply(item);
        if(index < 0 || index >= buckets.size()) {return false;}
        ArrayList<T> bucket = buckets.get(index);
        int position = item.getPosition();
        if(position < 0 || position >= buckets.size()) {return false;}
        return item == bucket.get(position);}

    /** removes the item from the bucket (in constant time).
     *  The last item in the item's bucket is moved to the item's current clausePosition.
     *
     * @param item the item to be removed
     */
    public void remove(T item) {
        int bucketPosition = getBucket.apply(item);
        if(bucketPosition  < 0 || bucketPosition >= buckets.size()) {return;}
        ArrayList<T> bucket = buckets.get(bucketPosition);
        int size = bucket.size();
        int itemPosition =  item.getPosition();
        if(itemPosition < 0 || itemPosition >= size || bucket.get(itemPosition) != item) {return;}
        item.setPosition(-1);
        if(size == 1) {
            assert 0 == itemPosition;
            bucket.clear(); return;}
        T lastItem = bucket.get(--size);
        bucket.set(itemPosition,lastItem);
        lastItem.setPosition(itemPosition);
        bucket.remove(size);
        for(int i = buckets.size()-1; i >= 0; --i) { // garbage collection
            if(buckets.get(i).isEmpty()) {buckets.remove(i);}
            else {break;}}}

    /** returns a randomly chosen item.
     *  items in smaller buckets are chosen more likely (quadratic) than items in larger buckets.
     *
     * @param random the random number generator
     * @return a randomly chosen item.
     */
    public T getRandom(Random random) {
        int size = buckets.size();
        if(size == 0) {return null;}
        int bucketNumber = 0;
        if(size > 1) {
            bucketNumber = random.nextInt(size);
            if(bucketNumber > 0) {bucketNumber = random.nextInt(bucketNumber+1);}}
        ArrayList<T> bucket = buckets.get(bucketNumber);
        if(bucket.isEmpty()) {
            int i = 0;
            while(true) {
                ++i;
                if(bucketNumber - i >= 0) {
                    bucket = buckets.get(i);
                    if(!bucket.isEmpty()) {break;}}
                if(bucketNumber + i < size) {
                    bucket = buckets.get(i);
                    if(!bucket.isEmpty()) {break;}}}}
        size = bucket.size();
        if(size == 1) {return bucket.get(0);}
        return bucket.get(random.nextInt(size));}

    /** checks if the buckets are empty
     *
     * @return true if there are no items in the bucket
     */
    public boolean isEmpty() {
        return buckets.isEmpty();}

    /** counts the number of items in the buckets
     *
     * @return the number of items in the buckets
     */
    public int size() {
        int size = 0;
        for(ArrayList<T> bucket : buckets) {size += bucket.size();}
        return size;}

    /** returns the number of items in the corresponding bucket
     *
     * @return the number of items in buckets(bucket)
     */
    public int size(int bucket) {
        if(bucket <0 || bucket >= buckets.size()) {return 0;}
        return buckets.get(bucket).size();}

    /** lists the entire list as string.
     *
     * @return the entire list as string.
     */
    public String toString() {
        StringBuilder st = new StringBuilder();
        for(int i = 0; i < buckets.size(); ++i) {
            ArrayList<T> bucket = buckets.get(i);
            if(bucket.isEmpty()) {continue;}
            st.append("Bucket " + i + "\n");
            for(T item : bucket) {st.append("  ").append(item.toString()).append("\n");}}
        return st.toString();}
}
