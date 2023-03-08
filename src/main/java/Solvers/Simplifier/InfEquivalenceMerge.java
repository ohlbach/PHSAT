package Solvers.Simplifier;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfEquivalenceMerge extends InferenceStep {

    private static final String title = "Binary Equivalence Merge";
    @Override
    public String title() {
        return title;}

    private static final String rule = title + "\n  p,q and p = q -> true(p)";

    @Override
    public String rule() {
        return rule;}
    private Clause clause;
    private int representative;
    private InferenceStep equivalenceStep;
    public InfEquivalenceMerge(Clause clause, int representative, InferenceStep equivalenceStep) {
        this.clause          = clause;
        this.representative  = representative;
        this.equivalenceStep = equivalenceStep;}

    @Override
    public String toString(Symboltable symboltable) {
        return title + "\n " + clause.toString(symboltable,0) + " and " +
                Symboltable.toString(clause.literals.get(0).literal,symboltable) + " = " +
                Symboltable.toString(clause.literals.get(1).literal,symboltable) + " -> " +
                Symboltable.toString(representative,symboltable);}

    @Override
    public IntArrayList inputClauseIds() {
        IntArrayList ids = clause.inferenceStep.inputClauseIds().clone();
        for(int id : equivalenceStep.inputClauseIds()) {if(!ids.contains(id)) ids.add(id);}
        return ids;}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        clause.inferenceStep.inferenceSteps(steps);
        equivalenceStep.inferenceSteps(steps);
        steps.add(this);}
}
