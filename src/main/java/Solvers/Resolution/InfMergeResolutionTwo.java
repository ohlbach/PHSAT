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

    public String info(Symboltable symboltable) {
        return clause1.toString(symboltable,0) + " and " + clause2.toString(symboltable,0) +
                " -> true(" + Symboltable.toString(literal,symboltable) + ")";}
    @Override
    public String toString(Symboltable symboltable) {
        return title + "  " + info(symboltable);}


    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps, IntArrayList ids) {
        if(steps.contains(this)) return;
        clause1.inferenceSteps.inferenceSteps(steps,ids);
        clause2.inferenceSteps.inferenceSteps(steps,ids);
        steps.add(this);}
}
