package InferenceSteps;

import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

/** This not really an inference step.
 * Instead of this it documents that the clause comes from some input clauses.
 * The list of inputClauseIds can be extended if different clauses are joined into one clause.
 */
public class InfInputClause extends InferenceStep {

    public static String title = "Input";

    public static String rule = "Input";

    /** collects the identifiers of InputClauses.*/
    private final IntArrayList inputClauseIds;

    /** Constructs a new instance
     *
     * @param inputClauseId id of the input clause
     */
    public InfInputClause(int inputClauseId) {
        this.inputClauseIds = IntArrayList.wrap(new int[]{inputClauseId});}

    /** Constructs a new instance
     *
     * @param inputClauseIds ids of the input clauses
     */
    public InfInputClause(IntArrayList inputClauseIds) {
        this.inputClauseIds = inputClauseIds;}

    /** clones the object
     *
     * @return a clone of the object.
     */
    public InfInputClause clone() {
        return new InfInputClause(inputClauseIds.clone());}

    /** adds a new inputClauseId to the list.
     *
     * @param inputClauseId another inputClauseId
     */
    public void addInputClauseId(int inputClauseId) {
        inputClauseIds.add(inputClauseId);}

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        String st = "" + inputClauseIds.getInt(0);
        if(inputClauseIds.size() == 1) return "Input Clause Id: " + st;
        for(int i = 2; i < inputClauseIds.size(); ++i) st += "," + inputClauseIds.getInt(i);
        return "Input: Clauses " + st;}

    @Override
    public IntArrayList inputClauseIds() {
        return inputClauseIds;}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        if(!steps.contains(this)) steps.add(this);}
}
