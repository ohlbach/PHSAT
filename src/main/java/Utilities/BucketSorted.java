package Utilities;

import java.util.ArrayList;
import java.util.Random;
import java.util.function.BiConsumer;
import java.util.function.Function;

/** This class can store items according to an attribute 'size' (&ge; 0).
 * Created by ohlbach on 08.06.2019.
 *
 * For each size a separate ArrayList (bucket) is created. The items with this size are stored (unsorted) in this list.
 * In order to guarantee adding and removing items in constant time, the items must store an int-value,
 * which can be used to store the position within the bucket.
 */
public class BucketSorted<T> {
    private ArrayList<ArrayList<T>> buckets;
    private Function<T,Integer>   getSize;
    private Function<T,Integer>   getPosition;
    private BiConsumer<T,Integer> setPosition;

    /** Constructs a new BucketSorted object
     *
     * @param getSize     returns the size of the item
     * @param getPosition returns the position of the item
     * @param setPosition stores the position into the item
     */
    public BucketSorted(Function<T,Integer> getSize, Function<T,Integer> getPosition, BiConsumer<T,Integer> setPosition) {
        this.getSize     = getSize;
        this.getPosition = getPosition;
        this.setPosition = setPosition;
        buckets = new ArrayList<ArrayList<T>>();}

    /** adds a new item to the bukets
     *
     * @param item the item to be added
     */
    public void add(T item) {
        int position = getSize.apply(item);
        if(position >= buckets.size()) {
            for(int i = buckets.size(); i <= position; ++i) {buckets.add(new ArrayList<T>());}}
        ArrayList<T> bucket = buckets.get(position);
        setPosition.accept(item,bucket.size());
        bucket.add(item);}

    /** removes the item from the bucket (in constant time).
     *  The last item in the item's bucket is moved to the item's current position.
     *
     * @param item the item to be removed
     */
    public void remove(T item) {
        int bucketPosition = getSize.apply(item);
        if(bucketPosition >= buckets.size()) {return;}
        ArrayList<T> bucket = buckets.get(bucketPosition);
        int size = bucket.size();
        if(size == 1) {
            assert 0 == getPosition.apply(item);
            bucket.clear(); return;}
        int itemPosition =  getPosition.apply(item);
        assert itemPosition < bucket.size();
        T lastItem = bucket.get(--size);
        bucket.set(itemPosition,lastItem);
        setPosition.accept(lastItem,itemPosition);
        bucket.remove(size);
        size = buckets.size()-1;
        if(bucket.isEmpty() && bucketPosition == size) {buckets.remove( size);}}

    /** returns a randomly chosen item.
     *  items with smaller size are chosen more likely (quadratic) than items with larger size.
     *
     * @param random
     * @return
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
            for(int i = bucketNumber-1; i >= 0; --i) {
                bucket = buckets.get(i);
                if(!bucket.isEmpty()) {break;}}
            if(bucket.isEmpty()) {
                for(int i = bucketNumber+1; i < size; ++i) {
                    bucket = buckets.get(i);
                    if(!bucket.isEmpty()) {break;}}}}
        size = bucket.size();
        if(size == 1) {return bucket.get(0);}
        return bucket.get(random.nextInt(size));}

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
