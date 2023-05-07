package Solvers.Resolution;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfEquivalence extends InferenceStep {

    private final static String title = "Equivalence Derivation";
    @Override
    public String title() {return title;}

    private final static String rule = title + "\np,q and -p,-q -> p == q";
    @Override
    public String rule() {return rule;}

    private final Clause clause1,clause2;

    public InfEquivalence(Clause clause1, Clause clause2) {
        this.clause1 = clause1;
        this.clause2 = clause2;}
    @Override
    public String toString(Symboltable symboltable) {
        return clause1.toString(symboltable,0) + " and " + clause2.toString(symboltable,0) + " -> " +
        Symboltable.toString(clause1.literals.get(0).literal,symboltable) + " == " +
                Symboltable.toString(clause1.literals.get(1).literal,symboltable);}

    @Override
    public IntArrayList inputClauseIds() {
        IntArrayList ids = clause1.inferenceStep.inputClauseIds().clone();
        ids.addAll(clause2.inferenceStep.inputClauseIds());
        return ids;}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        clause1.inferenceStep.inferenceSteps(steps);
        clause2.inferenceStep.inferenceSteps(steps);
        steps.add(this);}
}
