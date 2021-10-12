package InferenceSteps;

import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import java.util.ArrayList;

/** This is the abstract superclass of all Inference Steps
 */
public abstract class InferenceStep {

    public abstract String title();

    /** returns a description of the inference steps
     *
     * @return a description of the inference step
     */
    public abstract String rule();

    /** returns the inference step as string
     *
     * @param symboltable null or a symboltable
     * @return the inference step as string
     */
    public abstract String toString(Symboltable symboltable);


    /** collects the basicClause ids of all clauses causing the current inference
     *
     * @return the list of basic clause ids of all clauses causing the current inference
     */
    public abstract IntArrayList origins();

    /** collects the inference steps culminating in this in the list steps
     * Double occurrences are to be avoided.
     *
     * @param steps a list for collecting the inference steps.
     */
    public abstract void inferenceSteps(ArrayList<InferenceStep> steps);

    /** collects all rules which are used for the inference steps
     *
     * @param steps a list of inference steps
     * @return the list of rules used for the steps.
     */
    public ArrayList<String> rules(ArrayList<InferenceStep> steps) {
        ArrayList<String> rules = new ArrayList<>();
        for(InferenceStep step : steps) {
            String rule = step.rule();
            if(!rules.contains(rule)) rules.add(rule);}
        return rules;}

    public String toString() {
        return toString(null);}
}