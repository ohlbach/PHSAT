package InferenceSteps;

import Datastructures.Clause;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

/** This inference step represents the transformation of a unit-clause to a component of the model */

public class InfUnitClause extends InferenceStep {
    /** the title of the inference rule
     *
     * @return the title of the inference rule
     */
    @Override
    public String title() {
        return "Unit Clause";}

    /** the inference rule
     *
     * @return the inference rule
     */
    @Override
    public String rule() {
        return "C: p => true(p)";}

    /** the unit clause.*/
    private Clause clause;

    /** constructs the inference rule
     *
     * @param clause The unit clause to be transformed
     * @param reasoner The name of the reasoner performing the inference
     */
    public InfUnitClause(Clause clause, String reasoner) {
        super(reasoner);
        this.clause = clause;}

    /**
     * Returns a string representation of the InferenceStep object, including the title and the clause.
     *
     * @param symboltable the symboltable used for mapping predicate names to integers
     * @return a string representation of the InferenceStep object
     */
    @Override
    public String toString(Symboltable symboltable) {
        return title() + " " + clause.toString(symboltable,0);
    }

    /**
     * Collects the inference steps culminating in this step in a list.
     * Double occurrences are to be avoided.
     * Also collects the inputClause ids of all clauses causing the current inference.
     *
     * @param steps     A list for collecting the inference steps.
     * @param ids       A list of identifiers of input clauses.
     * @param reasoners A list for collecting the reasoners.
     */
    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps, IntArrayList ids, ArrayList<String>reasoners) {
        if(steps.contains(this)) {return;}
        super.inferenceSteps(steps, ids, reasoners);
        if(!ids.contains(clause.id)) ids.add(clause.id);
        if(clause.inferenceSteps != null) {
            for(InferenceStep step : clause.inferenceSteps) {step.inferenceSteps(steps,ids,reasoners);}}}
}
