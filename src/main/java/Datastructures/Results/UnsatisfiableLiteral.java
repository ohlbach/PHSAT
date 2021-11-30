package Datastructures.Results;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import java.util.ArrayList;
import static Utilities.Utilities.joinIntArrays;

/** This class reports about a derived literal which contradicts an earlier derived literal
 */
public class UnsatisfiableLiteral extends Unsatisfiable {
    private final int literal;
    private final InferenceStep stepLiteral1;
    private final InferenceStep stepLiteral2;

    /** constructs an Unsatisfiable exception for a contradictory literal
     *
     * @param literal       a contradictory literal
     * @param stepLiteral1  the inference step for the original literal
     * @param stepLiteral2  the inference step for the newly derived literal
     */
    public UnsatisfiableLiteral(int literal, InferenceStep stepLiteral1, InferenceStep stepLiteral2) {
        this.literal = literal;
        this.stepLiteral1 = stepLiteral1;
        this.stepLiteral2 = stepLiteral2;}

    @Override
    public String description(Symboltable symboltable) {
        return "Contradictory literal derived: " + Symboltable.toString(literal,symboltable);}

    /** joins the inference steps for the literals
     *
     * @param steps for adding the inference steps
     */
    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        if(stepLiteral1 != null) {
            stepLiteral1.inferenceSteps(steps);
            steps.add(stepLiteral1);}
        if(stepLiteral2 != null)
            stepLiteral2.inferenceSteps(steps);
            steps.add(stepLiteral2);}

    /** collects the basic clause ids of the clauses contributing to the unsatisfiability
     *
     * @return the basic clause ids of the clauses contributing to the unsatisfiability
     */
    @Override
    public IntArrayList origins() {
        IntArrayList origins = null;
        if(stepLiteral1 != null) {origins = stepLiteral1.origins();}
        if(stepLiteral2 != null) {return joinIntArrays(origins,stepLiteral2.origins());}
        return origins;}
}
