package Solvers.Simplifier;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

/** This class documents an inference step where a true literal is extracted from a atleast-clause.<br>
 *  Example: atleast 4 p^2,q^2,r. p and q are extracted because they must be true.
 *  Each instance of the class documents a single extraction.
 */
public class InfTrueLiteral extends InferenceStep {

    private final static String title = "True Literal Extraction";
    @Override
    public String title() {
        return title;}

    private final static String rule =
            "C: atleast n p^k,phi and (EL(C) - k) < n implies true(p)";

    private final String clause;
    private final int literal;
    private final int id;

    private final InferenceStep inferenceStep;

    /** constructs a new InferenceStep.
     *
     * @param clause the clause as string (because it can be changed later).
     * @param id the clause's identifier.
     * @param literal the extracted true literal.
     * @param inferenceStep the inferences step which produced the clause.
     */
    public InfTrueLiteral(String clause, int id, int literal, InferenceStep inferenceStep) {
        this.clause = clause;
        this.id = id;
        this.literal = literal;
        this.inferenceStep = inferenceStep;}

    @Override
    public String rule() {
        return title + ":\n" + rule;}


    @Override
    public String toString(Symboltable symboltable) {
        return clause + " implies true(" + Symboltable.toString(literal,symboltable) + ")";}

    @Override
    public IntArrayList inputClauseIds() {
        IntArrayList ids = inferenceStep.inputClauseIds();
        if(ids == null) return IntArrayList.wrap(new int[]{id});
        if(!ids.contains(id)) ids.add(id);
        return ids;}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        if(steps != null) steps.add(this);}
}
