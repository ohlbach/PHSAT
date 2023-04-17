package Solvers.Simplifier;

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

    @Override
    public String toString(Symboltable symboltable) {
        return title + "\n  " + clause.toString() + " and true(" + Symboltable.toString(oldTrueLiteral,symboltable) +
        ") -> true("  + Symboltable.toString(newTrueLiteral,symboltable) + ")";
    }

    @Override
    public IntArrayList inputClauseIds() {
        IntArrayList clauseIds = (clause.inferenceStep == null) ? null : clause.inferenceStep.inputClauseIds();
        IntArrayList oldIds    = (oldInferenceStep == null) ? null: oldInferenceStep.inputClauseIds();
        if(clauseIds == null) return oldIds;
        if(oldIds == null) return clauseIds;
        IntArrayList ids = clauseIds.clone();
        for(int id : oldIds) {
            if(!ids.contains(id)) ids.add(id);}
        return ids;}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        InferenceStep clauseStep = clause.inferenceStep;
        if(clauseStep != null) clauseStep.inferenceSteps(steps);
        if(oldInferenceStep != null) oldInferenceStep.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}
}
