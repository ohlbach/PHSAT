package Datastructures;

/** This is a doubly linked list of LinkedItems, such as clauses.
 * The pointers to the previous and next item in the list are contained in the LinkedItem.
 * Therefore there is no extra list with pointers to the items necessary.
 * New items can be added to the front or back of the list, but not in the middle.
 *
 * @param <Item> a class like Clause which extends LinkedItem.
 */
public class LinkedItemList<Item extends LinkedItem>  {

    /** An arbitrary title of the list*/
    String title;
    /** the first item in the list. */
    public Item firstLinkedItem;
    /** the last item in the list. */
    public Item lastLinkedItem;

    /** the number of items in the list */
    public int size = 0;

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


    /** removes a item from the list.<br>
     * item.exists is set to false.<br>
     * The removed item's nextClause remains as it is.
     * This way forward iterations with a pointer pointing to the removed item still work.<br>
     * One has to check item.exists!
     *
     * @param item the item to be removed.
     * @return the new number of clauses in the list.
     * */
    public int remove(Item item) {
        if(!item.isInList) return size;
        item.isInList = false;
        if(item.nextItem == null) { // it is the item at the end of the chain.
            if(item == firstLinkedItem) {size = 0; firstLinkedItem = null;
                item.previousItem = null;  return 0;}
            if(item.previousItem == null)  {
                System.out.println("ERROR: trying to remove not inserted item: " + item);
                new Exception().printStackTrace();
                System.exit(1);} // the item is not linked.
            final Item previousClause = (Item) item.previousItem;
            lastLinkedItem = previousClause; item.previousItem = null;
            previousClause.nextItem = null; return --size;}
        if(item.previousItem == null) { // it is the first item in the chain
            firstLinkedItem = (Item)item.nextItem; firstLinkedItem.previousItem = null; return --size;}

        final LinkedItem previous = item.previousItem;  // now the item is in the middle.
        final LinkedItem next = item.nextItem;
        previous.nextItem = next;
        next.previousItem = previous;
        item.previousItem = null;
        return --size;}

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
    public Item getLinkedItem(int n) {
        int counter = -1;
        Item linkedItem = firstLinkedItem;
        while(linkedItem != null) {
            if(++counter == n) return linkedItem;
            linkedItem = (Item)linkedItem.nextItem;}
        return null;}

    /** checks if the list is empty */
    public boolean isEmpty() {
        return firstLinkedItem == null;}

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
