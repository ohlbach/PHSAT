package Datastructures.Theory.EquivalenceClasses;

import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

/** This class documents a contradiction of the kind <br>
 *  existing equivalence class: p = ... x = y = ... and new equivalence: y = -x to be added.
 */
public class UnsatContradictoryEquivalence extends Unsatisfiable {

    /** a literal in the class */
    int oldLiteral;

    /** a new literal to be added to the class, but -newLiteral is already in the class.*/
    int newLiteral;

    /** the inference step that caused representative = oldLiteral */
    InferenceStep oldInferenceStep;

    /** the inference step that caused oldLiteral = newLiteral */
    InferenceStep newInferenceStep;

    /** constructs a new Unsatisfiability object.
     *
     * @param oldLiteral       a literal in the class
     * @param newLiteral       a new literal to be added to the class, but -newLiteral is already in the class
     * @param oldInferenceStep the inference step that caused representative = oldLiteral
     * @param newInferenceStep the inference step that caused oldLiteral = newLiteral
     */
    public UnsatContradictoryEquivalence(int oldLiteral, int newLiteral,
                                         InferenceStep oldInferenceStep, InferenceStep newInferenceStep) {
        this.oldLiteral = oldLiteral;
        this.newLiteral = newLiteral;
        this.oldInferenceStep = oldInferenceStep;
        this.newInferenceStep = newInferenceStep;
    }

    @Override
    public String description(Symboltable symboltable) {
        return "contradictory equivalences: " + Symboltable.toString(oldLiteral,symboltable) + " = " +
                Symboltable.toString(newLiteral,symboltable) + " = -" + Symboltable.toString(oldLiteral,symboltable);}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        if(oldInferenceStep != null && !steps.contains(oldInferenceStep)) steps.add(oldInferenceStep);
        if(newInferenceStep != null && !steps.contains(newInferenceStep)) steps.add(newInferenceStep);}

    @Override
    public IntArrayList inputClauseIds() {
        IntArrayList inputClauses1 = (oldInferenceStep != null) ? oldInferenceStep.inputClauseIds() : null;
        IntArrayList inputClauses2 = (newInferenceStep != null) ? newInferenceStep.inputClauseIds() : null;
        if(inputClauses1 == null) return inputClauses2;
        if(inputClauses2 == null) return inputClauses1;
        inputClauses1.clone().addAll(inputClauses1.size(),inputClauses2);
        return inputClauses1;}
}
