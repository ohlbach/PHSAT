package Datastructures.Results;

/** This class represents the result of a solver which has been aborted.
 * Created by ohlbach on 21.05.2019.
 */
public class Aborted extends Result {
    private String reason; // why it was aborted

    /** is not okay
     *
     * @return false
     */
    public boolean isOkay() {return false;}

    /** constructs an Aborted reason
     *
     * @param reason why the solver was aborted.
     */
    public Aborted(String reason) {
        this.reason = reason;}

    /** just returns the reason for the abortion
     *
     * @return the reason for the abortion.
     */
    public String toString() {return reason;}
}
