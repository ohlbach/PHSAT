package InferenceSteps;

import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InferenceTest extends InferenceStep{
    public static final String title = "Test Inference";
    public static final String rule = title + "\nFor Testing Purposes";
    private final String comment;

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;
    }
    public InferenceTest(String comment) {
        this.comment = comment;
    }

    @Override
    public String toString(Symboltable symboltable) {
        return title + ":\n  " + comment;
    }

    @Override
    public IntArrayList inputClauseIds() {
        return IntArrayList.wrap(new int[]{1000000}); }

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        steps.add(this);}
}
