package Solvers.Simplifier;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfMergeResolutionIndirect extends InferenceStep {

    private static final String title = "Indirect Merge Resolution";
    @Override
    public String title() {
        return title;}

    private static final String rule = title +
            "\n  atleast n p^n',q_1^k_1,...,q_l^k_l and -p,s and  atleast m -s^(n-n'+1),q_1^m,...,q_l^m, phi -> atleast m q_1^m,...,q_l^m, phi";

    @Override
    public String rule() {
        return rule;}

    private String parentClause, resolventBefore, resolventAfter, twoClause;
    private InferenceStep inferenceStep1, inferenceStep2, inferenceStep3;
    public InfMergeResolutionIndirect(Clause parentClause, Clause twoClause, String resolventBefore, Clause resolventAfter, Symboltable symboltable) {
        this.parentClause    = parentClause.toString(symboltable,0);
        this.twoClause       = twoClause.toString(symboltable,0);
        this.resolventBefore = resolventBefore;
        this.resolventAfter  = resolventAfter.toString(symboltable,0);
        inferenceStep1       = parentClause.inferenceStep;
        inferenceStep2       = twoClause.inferenceStep;
        inferenceStep3       = resolventAfter.inferenceStep;}

    @Override
    public String toString(Symboltable symboltable) {
        return title + "\n  " + parentClause + " and " + twoClause + " and " + resolventBefore + " -> " + resolventAfter;}

    @Override
    public IntArrayList inputClauseIds() {
        IntArrayList ids = inferenceStep1.inputClauseIds().clone();
        for(int id : inferenceStep2.inputClauseIds()) {if(!ids.contains(id)) ids.add(id);}
        for(int id : inferenceStep3.inputClauseIds()) {if(!ids.contains(id)) ids.add(id);}
        return ids;}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        inferenceStep1.inferenceSteps(steps);
        inferenceStep2.inferenceSteps(steps);
        inferenceStep3.inferenceSteps(steps);
        steps.add(this);}
}

