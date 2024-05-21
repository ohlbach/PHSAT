package Datastructures;

/** This is the abstract superclass of classes like Clause,
 * which can become part of a doubly linked list.
 * To this end there are pointers to the previous and next item right within the class.
 * Therefore, there need not be an extra list with pointers to the items.
 */
public abstract class LinkedItem {
    /** indicates whether the item is in a list or not.*/
    public boolean isInList;
    /** pointer to the previous item in the list. */
    public LinkedItem previousItem;

    /** pointer to the next item in the list */
    public LinkedItem nextItem;

    /**
     * Clears the current item in the list.
     * <p>
     * This method sets the isInList field to false and nullifies the references
     * to the previous and next items in the list. This effectively removes the
     * current item from the list.
     */
    public void clear() {
        isInList = false;
        previousItem = null;
        nextItem = null;}

    /** turns the item into a string.
     *
     * @param symboltable null or a symboltable.
     * @param size 0 or the length of the item number string.
     * @return a string representation of the item.
     */
    abstract public String toString(Symboltable symboltable, int size);
}
