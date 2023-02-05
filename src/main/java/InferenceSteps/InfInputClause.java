package InferenceSteps;

import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

/** This not really an inference step.
 * Instead it documents that the clause comes from some input.
 *
 */
public class InfInputClause extends InferenceStep{

    public static String title = "Input";

    public static String rule = "Input";

    private int origin;

    /** The id of the input clause
     *
     * @param origin
     */
    public InfInputClause(int origin) {
        this.origin = origin;}

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        return "Input: Clause " + origin;}

    @Override
    public IntArrayList origins() {
        return IntArrayList.wrap(new int[]{origin});}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        if(!steps.contains(this)) steps.add(this);}
}
