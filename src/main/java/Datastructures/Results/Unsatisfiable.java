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
    private String reason = null;


    /** creates an Unsatisfiable object with a reason
     *
     * @param reason for the unsatisfiability
     * @param inferenceStep the step causing the unsatisfiability
     */
    public Unsatisfiable(String reason, InferenceStep inferenceStep) {
        super();
        this.inferenceStep = inferenceStep;
        this.reason = reason;}

    /** creates an Unsatisfiable object with a reason
     *
     * @param inferenceStep the step causing the unsatisfiability
     */
    public Unsatisfiable(InferenceStep inferenceStep) {
        super();
        this.inferenceStep = inferenceStep;}


    /** just returns the reason for the unsatisfiability
     *
     * @return the reason for the unsatisfiability
     */
    public String toString() {
        String string =  "Unsatisfiable:\n";
        if(reason != null) string += reason;
        if(inferenceStep != null) {
            string += inferenceStep.toString();
            string += "\nparticipating clauses: " + inferenceStep.origins().toString();}
        return string;}
}
