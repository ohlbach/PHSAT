package Datastructures.Theory.EquivalenceClasses;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

/** This class documents the application of a true/false literal to an entire equivalence class.
 *  All literals in the class also become true/false.
 */
public class InfApplyTrueLiteral extends InferenceStep {

    /** the literal which is true in the global model. */
    private int oldTrueLiteral;

    /** the literal which is now declared true. */
    private int newTrueLiteral;

    /** the inference step which caused the truth of the oldTrueLiteral in the model. */
    private InferenceStep trueInferenceStep;

    /** the inference step which caused the equivalence of oldTrueLiteral with the representative in the class. */
    private InferenceStep oldInferenceStep;

    /** the inference step which caused the equivalence with newTrueLiteral with the representative in the class. */
    private InferenceStep newInferenceStep;

    /** constructs the inference step which ocuments the application of a true/false literal to an entire equivalence class.
     *
     * @param oldTrueLiteral    the literal which is true in the global model.
     * @param newTrueLiteral    the literal which is now declared true.
     * @param trueInferenceStep the inference step which caused the truth of the oldTrueLiteral in the model.
     * @param oldInferenceStep  the inference step which caused the equivalence of oldTrueLiteral with the representative in the class.
     * @param newInferenceStep  the inference step which caused the equivalence with newTrueLiteral with the representative in the class.
     */
    public InfApplyTrueLiteral(int oldTrueLiteral, int newTrueLiteral, InferenceStep trueInferenceStep,
                               InferenceStep oldInferenceStep, InferenceStep newInferenceStep) {
        this.oldTrueLiteral    = oldTrueLiteral;
        this.newTrueLiteral    = newTrueLiteral;
        this.trueInferenceStep = trueInferenceStep;
        this.oldInferenceStep  = oldInferenceStep;
        this.newInferenceStep  = newInferenceStep;
    }
    @Override
    public String title() {
        return "True Literal Applied to Equivalence Class";}

    @Override
    public String rule() {
        return title() + "\n  l1 = l2 = ... = ln and true/false(li)\n"+
                "  ------------------------------------\n"+
                "   true/false(l1) ... true/false(ln)";}

    @Override
    public String toString(Symboltable symboltable) {
        return Symboltable.toString(oldTrueLiteral,symboltable) + " = " + Symboltable.toString(newTrueLiteral,symboltable) +
                " and true("+Symboltable.toString(oldTrueLiteral,symboltable)+ ") -> true("+
                Symboltable.toString(newTrueLiteral,symboltable)+")";}

    @Override
    public IntArrayList inputClauseIds() {
        IntArrayList oldIds = (oldInferenceStep == null) ? null : oldInferenceStep.inputClauseIds();
        IntArrayList newIds = (newInferenceStep == null) ? null : newInferenceStep.inputClauseIds();
        IntArrayList ids    = (trueInferenceStep == null) ? null: trueInferenceStep.inputClauseIds();
        IntArrayList result = Utilities.Utilities.unionIntArrayLists(oldIds,newIds,ids);
        return (result == null) ? new IntArrayList() : result;}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        if(trueInferenceStep != null && !steps.contains(trueInferenceStep)) steps.add(trueInferenceStep);
        if(oldInferenceStep != null && !steps.contains(oldInferenceStep)) steps.add(oldInferenceStep);
        if(newInferenceStep != null && !steps.contains(newInferenceStep)) steps.add(newInferenceStep);


    }
}
