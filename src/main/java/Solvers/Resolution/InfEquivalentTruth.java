package Solvers.Resolution;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfEquivalentTruth extends InferenceStep {

    private static final String title = "Propagation of Truth along Equivalences";
    @Override
    public String title() {return title;}

    private static final String rule = "p == q and true(p) -> true(q)";
    @Override
    public String rule() {return title + "\n  " + rule;}

    int literal1, literal2;
    InferenceStep infTruth;

    public InfEquivalentTruth(int literal1, int literal2, InferenceStep infTruth) {
        this.literal1 = literal1; this.literal2 = literal2;
        this.infTruth = infTruth;}

    @Override
    public String toString(Symboltable symboltable) {
        return title + "\n  " + Symboltable.toString(literal1,symboltable) + " == " + Symboltable.toString(literal2,symboltable) +
                " -> true(" + Symboltable.toString(literal1,symboltable) + ")";}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps, IntArrayList ids) {
        if(steps.contains(this)) return;
        infTruth.inferenceSteps(steps,ids);
        steps.add(this);}
}
