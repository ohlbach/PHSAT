package Solvers.Resolution;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfMergeResolutionTwo extends InferenceStep {
    private final static String title = "Binary Merge Resolution";
    @Override
    public String title() {return title;}

    private final static String rule = title + "\n  p,q and -p,q -> true(q)";

    @Override
    public String rule() {return rule;}

    private final Clause clause1,clause2;
    private final int literal;
    public InfMergeResolutionTwo(Clause clause1, Clause clause2, int literal) {
        this.clause1 = clause1; this.clause2 = clause2; this.literal = literal;}
    @Override
    public String toString(Symboltable symboltable) {
        return title + "\n  " +clause1.toString(symboltable,0) + " and " + clause2.toString(symboltable,0) +
                " -> true(" + Symboltable.toString(literal,symboltable) + ")";}

    @Override
    public IntArrayList inputClauseIds() {
        IntArrayList list = clause1.inferenceStep.inputClauseIds().clone();
        list.addAll(clause2.inferenceStep.inputClauseIds());
        return list;}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        clause1.inferenceStep.inferenceSteps(steps);
        clause2.inferenceStep.inferenceSteps(steps);
        steps.add(this);}
}
