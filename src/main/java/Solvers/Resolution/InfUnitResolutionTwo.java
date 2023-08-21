package Solvers.Resolution;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfUnitResolutionTwo extends InferenceStep {

    private final static String title = "Unit Resolution with Binary Clause";
    @Override
    public String title() {
        return title;}

    private final static String rule = title + "\n  -p,q and true(p) -> true(q)";

    @Override
    public String rule() {
        return rule;}

    private Clause clause;
    private int oldTrueLiteral;
    private InferenceStep oldInferenceStep;
    private int newTrueLiteral;

    public InfUnitResolutionTwo(Clause clause, int oldTrueLiteral, InferenceStep oldInferenceStep, int newTrueLiteral) {
        this.clause = clause;
        this.oldTrueLiteral   = oldTrueLiteral;
        this.oldInferenceStep = oldInferenceStep;
        this.newTrueLiteral   = newTrueLiteral;}

    public String info(Symboltable symboltable) {
        return clause.toString() + " and true(" + Symboltable.toString(oldTrueLiteral,symboltable) +
                ") -> true("  + Symboltable.toString(newTrueLiteral,symboltable) + ")";}
    @Override
    public String toString(Symboltable symboltable) {
        return title + "\n  " + info(symboltable);}


    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps, IntArrayList ids) {
        if(steps.contains(this)) return;
        InferenceStep clauseStep = clause.inferenceSteps;
        if(clauseStep != null) clauseStep.inferenceSteps(steps,ids);
        if(oldInferenceStep != null) oldInferenceStep.inferenceSteps(steps, ids);
        steps.add(this);}
}
