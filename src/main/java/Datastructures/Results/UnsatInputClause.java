package Datastructures.Results;

import Datastructures.Clauses.InputClauses;
import Datastructures.Symboltable;
import InferenceSteps.InfInputClause;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

/** This signals an Unsatifiability which comes from a contradictory input clause,
 * for example an equivalence p == -p.
  */
public class UnsatInputClause extends Unsatisfiable {
    private final int[] inputClause;

    /** constructs an Unsatisfiability from an unsatisfiable clause
     *
     * @param inputClause a contradictory InputClause
     */
    public UnsatInputClause(int[] inputClause) {
        this.inputClause = inputClause;}

    /** returns a description of the contradiction.
     *
     * @param symboltable null or a symboltable
     * @return a description of the contradiction.
     */
    @Override
    public String description(Symboltable symboltable) {
        return "Contradictory clause " + InputClauses.toString(0, inputClause,symboltable)+"\n";}

    /** adds InfInputClause to the inference steps.
     *
     * @param steps for adding the inference steps
     */
    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        if(steps != null)
            steps.add(new InfInputClause(inputClause[0]));}

    /** returns just the id of the input clause.
     *
     * @return the id of the input clause wrapped into an IntArrayList.
     */
    @Override
    public IntArrayList origins() {
        return IntArrayList.wrap(new int[]{inputClause[0]});}
}
