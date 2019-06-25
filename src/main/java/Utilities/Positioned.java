package Utilities;

/** This interface can be implemented by classes which provide a position within a list,
 * for example clauses within ArrayLists of clauses.
 *
 * Created by ohlbach on 16.06.2019.
 */
public interface Positioned {
    /** returns the position */
    public int getPosition();

    /** sets the position
     *
     * @param position a position within a list.
     */
    public void setPosition(int position);
}
