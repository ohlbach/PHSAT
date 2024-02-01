package Datastructures;

/** This is a doubly linked list of LinkedItems, such as clauses.
 * The pointers to the previous and next item in the list are contained in the LinkedItem.
 * Therefore there is no extra list with pointers to the items necessary.
 * New items can be added to the front or back of the list, but not in the middle.
 *
 * @param <Item> a class like Clause which extends LinkedItem.
 */
public class LinkedItemList<Item extends LinkedItem> {

    /** An arbitrary title of the list*/
    String title;
    /** the first item in the list. */
    LinkedItem firstLinkedItem;
    /** the last item in the list. */
    LinkedItem lastLinkedItem;

    /** the number of items in the list */
    int size = 0;

    /** constructs an empty list.
     *
     * @param title null or a title for the list.
     */
    public LinkedItemList(String title) {this.title = title;}

    /** adds an item to the front of the list.
     *
     * @param linkedItem the item to be added.
     */
    public void addToFront(Item linkedItem) {
        ++size;
        linkedItem.isInList = true;
        linkedItem.previousItem = null; linkedItem.nextItem = null;
        if(firstLinkedItem == null) {
            firstLinkedItem = linkedItem; lastLinkedItem = linkedItem; return;}
        if(firstLinkedItem.nextItem == null) {
            lastLinkedItem = firstLinkedItem;}
        linkedItem.nextItem = firstLinkedItem;
        firstLinkedItem.previousItem = linkedItem;
        firstLinkedItem = linkedItem;
    }
    /** adds an item to the back of the list.
     *
     * @param linkedItem the item to be added.
     */
    public void addToBack(Item linkedItem) {
        ++size;
        linkedItem.isInList = true;
        linkedItem.previousItem = null; linkedItem.nextItem = null;
        if(firstLinkedItem == null) {
            firstLinkedItem = linkedItem; lastLinkedItem = linkedItem; return;}
        if(firstLinkedItem.nextItem == null) {
            lastLinkedItem = firstLinkedItem;}
        linkedItem.previousItem = lastLinkedItem;
        lastLinkedItem.nextItem = linkedItem;
        lastLinkedItem = linkedItem;}

    /** removes the given item from the list.
     * This is done in constant time.
     *
     * @param linkedItem the item to be removed.
     */
    public void remove(Item linkedItem) {
        --size;
        linkedItem.isInList = false;
        if(lastLinkedItem == firstLinkedItem) {
            lastLinkedItem = null; firstLinkedItem = null; return;}
        if(linkedItem.previousItem == null) {
            firstLinkedItem = linkedItem.nextItem;
            firstLinkedItem.previousItem = null;
            return;}
        if(linkedItem.nextItem == null) {
            lastLinkedItem = linkedItem.previousItem;
            lastLinkedItem.nextItem = null;
            return;}
        LinkedItem item = linkedItem.previousItem;
        item.nextItem = linkedItem.nextItem;
        linkedItem.nextItem.previousItem = item;
    }

    /** counts the items in the list.
     *
     * @return the number of items in the list.
     */
    public int size() {return size;}

    /** returns the nth item in the list.
     * This requires a linear search through the list.
     *
     * @param n an integer.
     * @return the nth item in the list.
     */
    public LinkedItem getLinkedItem(int n) {
        int counter = -1;
        LinkedItem linkedItem = firstLinkedItem;
        while(linkedItem != null) {
            if(++counter == n) return linkedItem;
            linkedItem = linkedItem.nextItem;}
        return null;}

    /** collects the items in a string, one per line.
     *
     * @return the items as a string.
     */
    public String toString() {
        return toString(null);}

    /** collects the items in a string, one per line.
     *
     * @param symboltable null or a symboltable.
     * @return the items as a string.
     */
    public String toString(Symboltable symboltable) {
        String title = (this.title == null) ? "": this.title+"\n";
        if(firstLinkedItem == null) return title;
        StringBuilder st = new StringBuilder();
        st.append(title);
        LinkedItem linkedItem = firstLinkedItem;
        while(linkedItem != null) {
            st.append(linkedItem.toString(symboltable,5)).append("\n");
            linkedItem = linkedItem.nextItem;}
        return st.toString();}

}
