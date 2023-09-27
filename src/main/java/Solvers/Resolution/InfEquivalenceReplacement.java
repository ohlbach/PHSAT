package Solvers.Resolution;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfEquivalenceReplacement extends InferenceStep {

    private final static String title = "Equivalence Replacement";
    @Override
    public String title() {return title;}

    private final static String rule = title + "\np,phi and p = q -> q,phi";

    @Override
    public String rule() {
        return rule;}

    String oldClause, newClause;
    IntArrayList trueLiterals = null;
    int representative,literal;
    InferenceStep equivalenceStep, clauseStep;

    public InfEquivalenceReplacement(InferenceStep equivalenceStep, String oldClause, String newClause) {
        this.equivalenceStep = equivalenceStep;
        this.oldClause       = oldClause;
        this.newClause       = newClause;}

    public String toString(Symboltable symboltable) {
        String eqStep = equivalenceStep.toString(symboltable);
        return title + "\n  " + eqStep + "\n" + oldClause + " -> " + newClause;}



    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps, IntArrayList ids) {
        if(steps.contains(this)) return;
        clauseStep.inferenceSteps(steps,ids);
        equivalenceStep.inferenceSteps(steps,ids);
        steps.add(this);}
}
