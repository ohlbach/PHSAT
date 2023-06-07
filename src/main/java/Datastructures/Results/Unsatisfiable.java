package Datastructures.Results;

import Datastructures.Symboltable;
import InferenceSteps.InfInputClause;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

/** This class represents the final reason for an unsatisfiability in the clauses.
 * It must be subclassed for representing more specific reasons for the unsatisfiability.
 */
public abstract class Unsatisfiable extends Result {


    /** calls the super-constructor.
     *
     * @param problemId the problem where the unsatisfiability was discovered.
     * @param solverId  the solver which discovered the unsatisfiability.
     */
    public Unsatisfiable(String problemId, String solverId) {
        super(problemId,solverId);}

    /** must generate a short description of the unsatisfiability
     *
     * @param symboltable  null or a symboltable
     * @return a short description of the unsatisfiability
     */
    public abstract String description(Symboltable symboltable);


    /** returns the reason for the unsatisfiability, usually the entire proof.
     *
     * @return the reason for the unsatisfiability.
     */
    public String toString() {
        return toString(null);}

    /** returns the reason for the unsatisfiability, usually the entire proof.
     *
     * @param symboltable null or a symboltable.
     * @return the reason for the unsatisfiability.
     */
    public String toString(Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        st.append("CONTRADICTION FOUND: ");
        if(solverId != null) {st.append( "by solver '").append(solverId).append("' ");}
        if(problemId != null) st.append("in problem '").append(problemId).append("'");
        st.append("\n").append(description(symboltable)).append("\n");
        ArrayList<InferenceStep> steps = new ArrayList<>();
        IntArrayList inputIds = new IntArrayList();
        inferenceSteps(steps,inputIds);
        if(!inputIds.isEmpty()) {
            st.append("Contributing input clauses: ").append(inputIds.toString()).append("\n");}
        if(!steps.isEmpty()) {
            st.append("Sequence of Inference Steps (except InputClause):\n");
            for(InferenceStep step : steps) {
                if(!(step instanceof InfInputClause)) st.append(step.toString(symboltable)).append("\n");}
            st.append("\n\nDefinitions of the Inference Rules Used in the Refutation:\n");
            for(String rule : InferenceStep.rules(steps)) {st.append(rule).append("\n");}}
        return st.toString();}
}
