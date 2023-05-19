package Datastructures.Theory;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InferenceTest extends InferenceStep {
    @Override
    public String title() {
        return "InferenceTest";
    }

    @Override
    public String rule() {
        return "TestRule";
    }

    String comment;
    public InferenceTest(String comment) {
        this.comment = comment;}
    @Override
    public String toString(Symboltable symboltable) {
        return comment;
    }

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps, IntArrayList ids) {

    }
}
