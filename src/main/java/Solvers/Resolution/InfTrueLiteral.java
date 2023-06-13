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

    private final String trueLiterals;
    private final int trueLiteral;

    private final InferenceStep inferenceStep;

    /** constructs a new InferenceStep.
     *
     * @param clause a clause
     * @param trueLiterals a list of true literals.
     * @param trueLiteral the derived true literal.
     * @param symboltable null or a symboltable.
     */
    public InfTrueLiteral(String clauseBefore, Clause clause, IntArrayList trueLiterals, int trueLiteral, Symboltable symboltable) {
        this.clauseBefore   = clauseBefore;
        clauseString       = clause.toString(symboltable,0);
        this.trueLiterals  = Symboltable.toString(trueLiterals,symboltable);
        this.trueLiteral   = trueLiteral;
        this.inferenceStep = clause.inferenceStep;}

    @Override
    public String rule() {
        return title + ":\n" + rule;}

    public String info(Symboltable symboltable) {
        return clauseBefore + " and true(" + trueLiterals +") -> " + clauseString + " and true("+ Symboltable.toString(trueLiteral,symboltable)+ ")";}

    @Override
    public String toString(Symboltable symboltable) {
        return title + "\n  " + info(symboltable);}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps, IntArrayList ids) {
        if(steps.contains(this)) return;
        inferenceStep.inferenceSteps(steps,ids);
        steps.add(this);}
}
