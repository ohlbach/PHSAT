package Datastructures.Results;

import Datastructures.Clauses.BasicClauseList;
import Datastructures.Symboltable;
import Datastructures.Theory.Model;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

/** This class represents the final reason for an unsatisfiability in the clauses.
 * Created by ohlbach on 14.09.2018.
 */
public class Unsatisfiable extends Result {
    private String reason;

    /** creates an Unsatisfiable object with a reason
     *
     * @param reason for the unsatisfiability
     * @param origins the basic clause ids causing the unsatisfiability
     */
    public Unsatisfiable(String reason, IntArrayList origins) {
        super();
        this.origins = origins;
        this.reason = reason;}

    /** creates an Unsatisfiable object with a reason
     *
     * @param reason for the unsatisfiability
     * @param inferenceStep the step causing the unsatisfiability
     */
    public Unsatisfiable(String reason, InferenceStep inferenceStep) {
        super();
        this.inferenceStep = inferenceStep;
        this.reason = reason;}



    /** just returns the reason for the unsatisfiability
     *
     * @return the reason for the unsatisfiability
     */
    public String toString() {
        String string =  "Unsatisfiable: " + reason;
        if(origins != null) string += "\n  participating clauses: " + origins.toString();
        return string;}
}
