package Datastructures.Results;

import Datastructures.Symboltable;

/** This class represents the result of a solver which has been aborted.
 * Created by ohlbach on 21.05.2019.
 */
public class Aborted extends Result {
    /** the reason why the program has been aborted */
    private String reason; // why it was aborted

    /** constructs an Aborted reason
     *
     * @param reason why the solver was aborted.
     */
    public Aborted(String problemId, String solverId, long startTime, String reason) {
        super(problemId,solverId,startTime);
        this.reason = reason;}

    /** just returns the reason for the abortion
     *
     * @return the reason for the abortion.
     */
    @Override
    public String toString() {
        return super.toString()+reason;}
    @Override
    public String toString(Symboltable symboltable) {
        return super.toString(symboltable)+reason;}
    @Override
    public String toString(Symboltable symboltable, boolean trackReasoning) {
            return super.toString(symboltable,trackReasoning)+reason;}
}
