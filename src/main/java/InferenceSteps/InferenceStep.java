package InferenceSteps;

import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.function.Consumer;

/** This is the abstract superclass of all Inference Steps
 */
public abstract class InferenceStep {

    /** the title of the inference step.
     *
     * @return the title of the inference step.
     */
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

    /**
     * Returns the string representation of the inference step.
     * <br>
     * The symbol table is null.
     *
     * @return The string representation of the inference step.
     */
    public String toString() {
        return toString(null);};

    /** collects the inference steps culminating in this in the list steps
     * Double occurrences are to be avoided.
     *  collects the inputClause ids of all clauses causing the current inference
     *
     * @param steps a list for collecting the inference steps.
     * @param ids a list of identifiers of input clauses
     */
    public void inferenceSteps(ArrayList<InferenceStep> steps, IntArrayList ids) {
        if(!steps.contains(this)) {steps.add(this);}}

    /** collects all rules which are used for the inference steps
     *
     * @param steps a list of inference steps
     * @return the list of rules used for the steps.
     */
    public static ArrayList<String> rules(ArrayList<InferenceStep> steps) {
        ArrayList<String> rules = new ArrayList<>();
        for(InferenceStep step : steps) {
            String rule = step.rule();
            if(rule != null && !rules.contains(rule)) rules.add(rule);}
        return rules;}

    /**
     * Verifies the rule.
     *
     * @param monitor      the consumer for monitoring the verification process
     * @param symboltable  the symbol table for mapping predicate names to integers
     * @return true if the verification is successful, false otherwise
     */
    public boolean verify(Consumer<String> monitor, Symboltable symboltable) {return true;}

}
