package Datastructures.Results;

import Datastructures.Symboltable;

/** This class represents the result of a solver which has been aborted.
 * Created by ohlbach on 21.05.2019.
 */
public class Aborted extends Result {
    /** the reason why the program has been aborted */
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
    public Aborted(String problemId, String solverId,String reason) {
        super(problemId,solverId);
        this.reason = reason;}

    /** just returns the reason for the abortion
     *
     * @return the reason for the abortion.
     */
    public String toString() {return reason;}
    @Override
    public String toString(Symboltable symboltable) {return reason;}
}
