package Datastructures.Results;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;

/** This class reports about a derived literal which contradicts an earlier derived literal
 */
public class UnsatisfiableLiteral extends Unsatisfiable {
    private final int literal;

    /** constructs an Unsatisfiable exception for a contradictory literal
     *
     * @param literal       a contradictory literal
     * @param stepLiteral1  the inference step for the original literal
     * @param stepLiteral2  the inference step for the newly derived literal
     */
    public UnsatisfiableLiteral(int literal, InferenceStep stepLiteral1, InferenceStep stepLiteral2) {
        super(null,null);
        this.literal = literal;
        if(stepLiteral1 != null) inferenceSteps.add(stepLiteral1);
        if(stepLiteral2 != null) inferenceSteps.add(stepLiteral2);
        message = (symboltable -> "Unsatisfiable literal: " + Symboltable.toString(literal,symboltable));
    }



}
