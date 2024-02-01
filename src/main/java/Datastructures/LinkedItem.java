package Datastructures;

/** This is the abstract superclass of classes like Clause,
 * which can become part of a doubly linked list.
 * To this end there are pointers to the previous and next item right within the class.
 * Therefore there need not be an extra list with pointers to the items.
 */
public abstract class LinkedItem {
    /** indicates whether the item is in a list or not.*/
    boolean isInList;
    /** pointer to the previous item in the list. */
    LinkedItem previousItem;

    /** pointer to the next item in the list */
    LinkedItem nextItem;

    /** turns the item into a string.
     *
     * @param symboltable null or a symboltable.
     * @param size 0 or the length of the item number string.
     * @return a string representation of the item.
     */
    abstract public String toString(Symboltable symboltable, int size);
}
