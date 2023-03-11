package Datastructures.Theory.EquivalenceClasses;

import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;

/** This class documents a contradiction of the kind <br>
 *  existing equivalence class: p = ... x = y = ... and new equivalence: y = -x to be added.
 */
public class UnsatContradictoryEquivalence extends Unsatisfiable {

    /** a literal in the class */
    int oldLiteral;

    /** a new literal to be added to the class, but -newLiteral is already in the class.*/
    int newLiteral;

    /** constructs a new Unsatisfiability object.
     *
     * @param oldLiteral       a literal in the class
     * @param newLiteral       a new literal to be added to the class, but -newLiteral is already in the class
     * @param oldInferenceStep the inference step that caused representative = oldLiteral
     * @param newInferenceStep the inference step that caused oldLiteral = newLiteral
     */
    public UnsatContradictoryEquivalence(int oldLiteral, int newLiteral,
                                         InferenceStep oldInferenceStep, InferenceStep newInferenceStep) {
        super();
        this.oldLiteral = oldLiteral;
        this.newLiteral = newLiteral;
        if(oldInferenceStep != null) inferenceSteps.add(oldInferenceStep);
        if(newInferenceStep != null) inferenceSteps.add(newInferenceStep);
    }

    @Override
    public String description(Symboltable symboltable) {
        return "contradictory equivalences: " + Symboltable.toString(oldLiteral,symboltable) + " = " +
                Symboltable.toString(newLiteral,symboltable) + " = -" + Symboltable.toString(oldLiteral,symboltable);}


}
