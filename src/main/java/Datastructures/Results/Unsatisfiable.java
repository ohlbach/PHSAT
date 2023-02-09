package Datastructures.Results;

import Datastructures.Statistics.Statistic;
import Datastructures.Symboltable;
import InferenceSteps.InfInputClause;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

import static Utilities.Utilities.sortIntArray;

/** This class represents the final reason for an unsatisfiability in the clauses.
 * Created by ohlbach on 14.09.2018.
 */
public abstract class Unsatisfiable extends Result {
    public Class solverClass = null;
    public String solverId   = null;
    public String problemId  = null;
    public Symboltable symboltable = null;
    public Statistic statistic = null;

    /** must generate a short description of the unsatisfiability
     *
     * @param symboltable  null or a symboltable
     * @return a short description of the unsatisfiability
     */
    public abstract String description(Symboltable symboltable);

    /** joins the inference steps for the literals
     *
     * @param steps for adding the inference steps
     */
    public abstract void inferenceSteps(ArrayList<InferenceStep> steps);

    /** must return the list of input clause Ids which are responsible for the unsatrisfiability.
     *
     * @return the list of input clause Ids which are responsible for the unsatrisfiability.
     */
    public abstract IntArrayList inputClauseIds();


    /** returns the reason for the unsatisfiability, usually the entire proof
     *
     * @return the reason for the unsatisfiability
     */
    public String toString() {
        StringBuilder st = new StringBuilder();
        st.append("CONTRADICTION FOUND: ");
        if(solverClass != null) {
            st.append( "by solver ").append(solverClass.getSimpleName()).append(": ").append(solverId).append("\n");}
        if(problemId != null) st.append(" in problem ").append(problemId);
        st.append("\n").append(description(symboltable)).append("\n");
        IntArrayList inputClauseIds = inputClauseIds();
        if(inputClauseIds != null) {
            st.append("Contributing basic clauses: ").append(sortIntArray(inputClauseIds).toString()).append("\n");}
        ArrayList<InferenceStep> steps = new ArrayList<>();
        inferenceSteps(steps);
        if(!steps.isEmpty()) {
            st.append("Sequence of Inference Steps:\n");
            for(InferenceStep step : steps) {
                if(step.getClass() != InfInputClause.class)
                    st.append(step.toString(symboltable)).append("\n");}

            st.append("\n\nDefinitions of the Inference Rules Used in the Refutation:\n");
            for(String rule : InferenceStep.rules(steps)) {st.append("\n").append(rule).append("\n");}}
        return st.toString();}
}
