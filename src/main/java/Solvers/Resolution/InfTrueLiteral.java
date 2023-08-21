package Solvers.Resolution;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

/** This class documents an inference step where a true literal is extracted from a atleast-clause.<br>
 *  Example: atleast 4 p^2,q^2,r. p and q are extracted because they must be true.
 *  Each instance of the class documents a single extraction.
 */
public class InfTrueLiteral extends InferenceStep {

    private final static String title = "True Literal Derivation";
    @Override
    public String title() {
        return title;}

    private final static String rule =
            "clause and true(l1,...,ln) -> true(l)";

    private final String clauseBefore;
    private final String clauseString;
    private final IntArrayList oldTrueLiterals;
    private final int newTrueLiteral;
    private final ArrayList<InferenceStep> inferenceSteps;

    /** constructs a new InferenceStep.
     *
     * @param clause a clause
     * @param oldTrueLiterals the true literals in the clause.
     * @param trueLiteralsInferenceSteps which caused the true literals in the clause.
     * @param newTrueLiteral the derived true literal.
     * @param symboltable null or a symboltable.
     */
    public InfTrueLiteral(String clauseBefore, Clause clause, IntArrayList oldTrueLiterals,
                          ArrayList<InferenceStep> trueLiteralsInferenceSteps, int newTrueLiteral, Symboltable symboltable) {
        this.clauseBefore   = clauseBefore;
        clauseString       = clause.toString(symboltable,0);
        this.oldTrueLiterals = oldTrueLiterals.clone();
        this.newTrueLiteral = newTrueLiteral;
        inferenceSteps = new ArrayList<>();
        inferenceSteps.add(clause.inferenceSteps);
        inferenceSteps.addAll(trueLiteralsInferenceSteps);}

    @Override
    public String rule() {
        return title + ":\n" + rule;}

    public String info(Symboltable symboltable) {
        return clauseBefore + " and true(" + Symboltable.toString(oldTrueLiterals,symboltable) +") -> "
                + clauseString + " and true("+ Symboltable.toString(newTrueLiteral,symboltable)+ ")";}

    @Override
    public String toString(Symboltable symboltable) {
        return title + "\n  " + info(symboltable);}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps, IntArrayList ids) {
        if(steps.contains(this)) return;
        for(InferenceStep step: inferenceSteps) step.inferenceSteps(steps,ids);
        steps.add(this);}
}
