package Utilities;

import java.util.Iterator;

/** This is an iterator which iterates over parts of a string.
 * The string is split by a regular expression and the iterator iterates
 * over the parts of the string.
 */
public class StringIterator implements Iterator<String> {

    /** The splitparts of the string */
    private String[] parts;
    /** The number of split parts */
    private int length;

    /** the current position within the splitparts.*/
    private int position = 0;

    /** creates the iterator
     *
     * @param string            the string to be split into parts.
     * @param regularExpression by this regular expression.
     */
    public StringIterator(String string, String regularExpression) {
        parts = string.split(regularExpression);
        length = parts.length;}

    /** checks if there is still another splitpart.
     *
     * @return true if there are more splitparts.
     */
    @Override
    public boolean hasNext() {
        return position < length;}

    /** gets the next splitpart
     *
     * @return the next splitpart.
     */
    @Override
    public String next() {
        return parts[position++];}
}

