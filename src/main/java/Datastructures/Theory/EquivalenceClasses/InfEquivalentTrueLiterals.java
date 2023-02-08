package Datastructures.Theory.EquivalenceClasses;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

/** documents truth of equivalent literals.
 * As soon as a literal in an equivalence class becomes true, all equivalent literals also become true.*/
public class InfEquivalentTrueLiterals extends InferenceStep {

    /** title of the rule */
    public static final String title = "Equivalent True Literals";

    /** specifies the rule */
    public static final String rule =
            title + ":\n" +
                    "p=q=...=s and true/false(p) -> true/false(q,...,s)";

    /** the id of the EQUIV input clause */
    private final int origin;

    /** a literal which is true in the model */
    private final int oldTrueLiteral;

    /** a literal which was inferred to be true. */
    private final int newTrueLiteral;

    /** null or the inference step which cause the truth of oldTrueLiteral */
    private final InferenceStep inferenceStep;

    /** constructs the instance of the class
     *
     * @param origin          the id of the EQUIV input clause.
     * @param oldTrueLiteral  the literal which was true in the model.
     * @param newTrueLiteral  the equivalent literal which became true
     * @param inferenceStep   null or the inference step which caused the truth of the oldTrueLiteral
     */
    public InfEquivalentTrueLiterals(int origin, int oldTrueLiteral, int newTrueLiteral, InferenceStep inferenceStep) {
        this.origin = origin;
        this.oldTrueLiteral = oldTrueLiteral;
        this.newTrueLiteral = newTrueLiteral;
        this.inferenceStep  = inferenceStep;}

    /** the title of the rule */
    @Override
    public String title() {
        return title;}

    /** the rule itself */
    @Override
    public String rule() {
        return rule;}

    /** turns the inference into a string
     *
     * @param symboltable null or a symboltable.
     * @return the inference as a string.
     */
    @Override
    public String toString(Symboltable symboltable) {
        String oldLiteral = Symboltable.toString(oldTrueLiteral,symboltable);
        String newLiteral = Symboltable.toString(newTrueLiteral,symboltable);
        return title + ":\n" + oldLiteral + " = " + newLiteral +
                " and true("+ oldLiteral + ") yields true(" + newLiteral +")";}

    /** the ids of all the input clauses which caused the truth of newTrueLiteral.
     *
     * @return the ids of all the input clauses which caused the truth of newTrueLiteral.
     */
    @Override
    public IntArrayList inputClauseIds() {
        if(inferenceStep == null) return null;
        IntArrayList origins = inferenceStep.inputClauseIds().clone();
        origins.add(origin);
        return origins;}

    /** adds this to the inference steps
     */
    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        if(steps != null && !steps.contains(this)) steps.add(this);}
}
