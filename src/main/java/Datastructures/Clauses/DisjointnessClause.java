package Datastructures.Clauses;

/**
 * Created by ohlbach on 13.09.2018.
 */
public class DisjointnessClause extends AbstractClause {

    /** constructs a clause
     *
     * @param number   the clause number
     * @param maxSize  the maximum number of literals
     */
    public DisjointnessClause(int number, int maxSize) {
        super(number,maxSize);}

    /** returns the clause's id
     *
     * @return the clause number, preceded by 'd'.
     */
    public String id() {
        return "d"+super.id();}

}
