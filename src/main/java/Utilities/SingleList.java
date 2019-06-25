package Utilities;

import java.util.ArrayList;
import java.util.Iterator;

/** This class allows one to store items in an array list (unsorted).
 *  The items themselves keep the position within the list.
 *  Therefore removing an item can be done in constant time.
 * Created by ohlbach on 16.06.2019.
 */
public class SingleList<T extends Positioned> implements Iterable<T> {
    private ArrayList<T> items;

    /** constructs a new list
     *
     */
    public SingleList() {
        items = new ArrayList<T>();}

    /** constructs a new list with a initial size
     *
     * @param size the initial size
     */
    public SingleList(int size) {
        items = new ArrayList<T>(size);}

    /** adds an item to the end of the list
     *
     * @param item the item to be added
     */
    public void addItem(T item) {
        item.setPosition(items.size());
        items.add(item);}

    /** removes an item from the list.
     *  In order to do the operation in constant time,
     *  the last item in the list is moved to the removed item's position.
     *
     * @param item
     */
    public void removeItem(T item) {
        int position = item.getPosition();
        assert items.get(position) == item;
        if(items.size() == 1) {
            items.clear();
            return;}
        int size = items.size()-1;
        T lastItem = items.get(size);
        lastItem.setPosition(position);
        item.setPosition(-1);
        items.set(position, lastItem);
        items.remove(size);}


    /** checks if the list contains the item
     *
     * @param item the item to be checked
     * @return true if the item is at the right position in the list.
     */
    public boolean contains(T item) {
        int position = item.getPosition();
        if(position < 0 || position >= items.size()) {return false;}
        return items.get(position) == item;}

    /** returns the number of items in the list
     *
     * @return the numnber of items in the list
     */
    public int size() {
        return items.size();}

    /** checks if the list is empty
     *
     * @return true if the list is empty
     */
    public boolean isEmpty() {
        return items.isEmpty();}

    /** returns the iterator over the list
     *
     * @return the iterator over the list
     */
    @Override
    public Iterator<T> iterator() {
        return (Iterator<T>)items.iterator();}

    /** turns the list into a string
     *
     * @return the list as a string.
     */
    public String toString() {
        StringBuilder st = new StringBuilder();
        for(Positioned item : items) {
            st.append(item.toString()).append("\n");}
        return st.toString();}


}
