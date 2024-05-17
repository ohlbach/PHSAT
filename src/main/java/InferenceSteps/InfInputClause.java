package InferenceSteps;

import Datastructures.Clauses.InputClauses;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

/** This not really an inference step.
 * Instead of this it documents that the clause comes from some input clauses.
 */
public class InfInputClause extends InferenceStep {

    public static String title = "Input";

    public static String rule = "Input";

    /** collects the identifiers of InputClauses.*/
    public int inputClauseId;

    /** the clause as a string. */
    public String clause;

    public int[] inputClause = null;

    /** Constructs a new instance
     *
     * @param inputClauseId id of the input clause
     * @param clause the clause as a string.
     */
    public InfInputClause(int inputClauseId, String clause) {
        this.inputClauseId = inputClauseId;
        this.clause = clause;}

    public InfInputClause(int[] inputClause, String clause) {
        this.inputClause = inputClause;
        this.clause = clause;}

    /** Constructs a new instance
     *
     * @param inputClauseId id of the input clause
     */
    public InfInputClause(int inputClauseId) {
        this.inputClauseId = inputClauseId;}


    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        if(inputClause != null) {
            return "Input: Clause " + InputClauses.toString(0,inputClause,symboltable) + " -> " + clause;}
        return clause == null ?
                "Input: Clause " + inputClauseId :
                "Input: Clause " + inputClauseId + " yields " + clause;}


    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps, IntArrayList ids) {
        if(!steps.contains(this)) {
            steps.add(this);
            if(!ids.contains(inputClauseId)) ids.add(inputClauseId);}}
}
