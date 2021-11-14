package Datastructures.Results;

import Datastructures.Clauses.Clause;
import Datastructures.Symboltable;
import Datastructures.Theory.Model;
import InferenceSteps.InferenceStep;
import Solvers.Walker.WClause;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

import static Utilities.Utilities.joinIntArrays;
import static Utilities.Utilities.sortIntArray;

/** This class represents the final reason for an unsatisfiability in the clauses.
 * Created by ohlbach on 14.09.2018.
 */
public class Unsatisfiable extends Result {
    private String reason = null;
    private IntArrayList origins = null;
    private Clause falseClause = null;

    public Unsatisfiable(Clause falseClause) {
        super();
        this.falseClause = falseClause;
        this.inferenceStep = falseClause.inferenceStep;}

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

    public Unsatisfiable(WClause wClause, Model model, Symboltable symboltable) {
        super();
        origins = IntArrayList.wrap(new int[]{wClause.id});
        for(int literal : wClause.literals) {
            InferenceStep step = model.getInferenceStep(-literal);
            if(step != null) joinIntArrays(origins,step.origins());}
        reason = "False Clause " + wClause.toString(0,symboltable) + " in the model " +
                model.toString(symboltable);}

    /** returns the reason for the unsatisfiability, usually the entire proof
     *
     * @return the reason for the unsatisfiability
     */
    public String toString() {
        return toString(null);}

    /** returns the reason for the unsatisfiability, usually the entire proof
     *
     * @param symboltable null or a symboltable
     * @return the reason for the unsatisfiability
     */
    public String toString(Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        st.append("UNSATISFIABLE:\n");
        if(falseClause != null) {st.append("False Clause: " + falseClause.toString(0,symboltable) + "\n");}
        if(reason != null) st.append(reason).append("\n");
        String originsSt = null;
        if(origins != null) {originsSt = sortIntArray(origins).toString();}
        else {
            if(inferenceStep != null) {
                origins = inferenceStep.origins();
                if(origins != null) originsSt = sortIntArray(inferenceStep.origins()).toString();}}
        if(originsSt != null) st.append("\nParticipating Clauses: ").append(originsSt).append("\n");
        if(inferenceStep != null) {
            st.append("Sequence of Inference Steps:\n");
            ArrayList<InferenceStep> steps = new ArrayList<>();
            inferenceStep.inferenceSteps(steps);
            if(!steps.contains(inferenceStep)) steps.add(inferenceStep);
            for(InferenceStep step : steps) {
                if(step.getClass() != InferenceSteps.Input.class)
                    st.append(step.toString(symboltable)).append("\n");}
            st.append("\n\nDefinitions of the Inference Rules Used in the Refutation:");
            for(String rule : inferenceStep.rules(steps)) {st.append("\n\n").append(rule);}}
        return st.toString();}
}
