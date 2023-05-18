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
    public String rule() {return rule;}

    int literal1, literal2;
    InfEquivalence infEquivalence; InferenceStep infTruth;

    public InfEquivalentTruth(int literal1, int literal2, InfEquivalence infEquivalence, InferenceStep infTruth) {
        this.literal1 = literal1; this.literal2 = literal2;
        this.infEquivalence = infEquivalence; this.infTruth = infTruth;}

    @Override
    public String toString(Symboltable symboltable) {
        return Symboltable.toString(literal1,symboltable) + " == " + Symboltable.toString(literal2,symboltable) +
                "true(" + Symboltable.toString(literal1,symboltable) + ")";}

    @Override
    public void inputClauseIds(IntArrayList ids) {
        infEquivalence.inputClauseIds(ids);
        infTruth.inputClauseIds(ids);}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        infEquivalence.inferenceSteps(steps);
        infTruth.inferenceSteps(steps);
        steps.add(this);}
}
