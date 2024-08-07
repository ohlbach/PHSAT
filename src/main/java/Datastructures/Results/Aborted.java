package Datastructures.Results;

/** This class represents the result of a solver which has been aborted.
 * Created by ohlbach on 21.05.2019.
 */
public class Aborted extends Result {
    /** constructs an Aborted reason
     *
     * @param reason why the solver was aborted.
     */
    public Aborted(String problemId, String solverId, String reason) {
        super(problemId,solverId,"aborted");
        message = (symboltable -> "Solver aborted: " + reason);}

}
