package Datastructures.Results;

import InferenceSteps.InferenceStep;

import java.util.ArrayList;

import static Utilities.Utilities.sortIntArray;

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


    /** returns the reason for the unsatisfiability, usually the entire proof
     *
     * @return the reason for the unsatisfiability
     */
    public String toString() {
        StringBuilder st = new StringBuilder();
        st.append("Unsatisfiable:\n");
        if(reason != null) st.append(reason).append("\n");
        if(inferenceStep != null) {
            st.append("Sequence of Inference Steps:\n");
            ArrayList<InferenceStep> steps = new ArrayList<>();
            inferenceStep.inferenceSteps(steps);
            if(!steps.contains(inferenceStep)) steps.add(inferenceStep);
            for(InferenceStep step : steps) {st.append(step.toString()).append("\n");}
            st.append("\nParticipating Clauses: " + sortIntArray(inferenceStep.origins()).toString());
            st.append("\n\nDefinitions of the Inference Rules Used in the Refutation:");
            for(String rule : inferenceStep.rules(steps)) {st.append("\n\n").append(rule);}}
        return st.toString();}
}
