package Datastructures.Results;

import Datastructures.Symboltable;
import InferenceSteps.InfInputClause;
import InferenceSteps.InferenceStep;
import Utilities.Utilities;
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
        return toString(null,false);}

    /** returns the reason for the unsatisfiability, usually the entire proof.
     *
     * @param symboltable null or a symboltable.
     * @return the reason for the unsatisfiability.
     */
    public String toString(Symboltable symboltable, boolean trackReasoning) {
        StringBuilder st = new StringBuilder();
        st.append("CONTRADICTION FOUND: ");
        if(solverId != null) {st.append( "by solver '").append(solverId).append("' ");}
        if(problemId != null) st.append("in problem '").append(problemId).append("'");
        ArrayList<InferenceStep> steps = new ArrayList<>();
        IntArrayList inputIds = new IntArrayList();
        if(trackReasoning) inferenceSteps(steps,inputIds);
       if(!steps.isEmpty()) {
            st.append("\n"+steps.size()+ " inference steps (InputClause is not listed):\n");
            for(InferenceStep step : steps) {
                if(!(step instanceof InfInputClause)) st.append(step.toString(symboltable)).append("\n");}
            st.append("\n\nDefinitions of the Inference Rules Used in the Refutation:\n");
            for(String rule : InferenceStep.rules(steps)) {st.append(rule).append("\n");}}
       st.append("\n").append(description(symboltable)).append("\n");
       if(!inputIds.isEmpty()) {
            st.append(inputIds.size()).append(" contributing input clauses:\n").append(Utilities.intArrayListToString(inputIds)).append("\n");}
        return st.toString();}
}
