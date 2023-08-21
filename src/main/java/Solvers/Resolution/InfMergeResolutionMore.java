package Solvers.Resolution;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfMergeResolutionMore extends InferenceStep {

    private static final String title = "Merge Resolution";
    @Override
    public String title() {return title;}

    private String rule;
    private static final String ruleTwo = title + "\n  p,q and atleast n -p,q^n,phi -> atleast n q^n,phi";
    private static final String ruleTwoOr = title + "\n  p,q and -p,q,phi -> q,phi";

    private static final String ruleMore = title +
            "\n  atleast n p^n',q_1^k_1,...,q_l^k_l and atleast m -p^n,q_1^m,...,q_l^m, phi -> atleast m q_1^m,...,q_l^m, phi";

    private static final String ruleMoreOr = title +
            "\n  p,q_1,...,q_l and -p,q_1,...,q_l, phi -> q_1,...,q_l, phi";

    @Override
    public String rule() {return rule;}

    private String parentClause, resolventBefore, resolventAfter;
    private InferenceStep inferenceStep1, inferenceStep2;
    public InfMergeResolutionMore(Clause parentClause, String resolventBefore, Clause resolventAfter, Symboltable symboltable) {
        if(parentClause.isDisjunction) {
             rule = (parentClause.size() == 2) ? ruleTwoOr : ruleMoreOr;}
        else rule = (parentClause.size() == 2) ? ruleTwo : ruleMore;
        this.parentClause    = parentClause.toString(symboltable,0);
        this.resolventBefore = resolventBefore;
        this.resolventAfter  = resolventAfter.toString(symboltable,0);
        inferenceStep1       = parentClause.inferenceSteps;
        inferenceStep2       = resolventAfter.inferenceSteps;}

    public String info() {
        return parentClause + " and " + resolventBefore + " -> " + resolventAfter + "  (Merge Resolution)";}
    @Override
    public String toString(Symboltable symboltable) {
        return title + "  " + info();}


    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps, IntArrayList ids) {
        if(steps.contains(this)) return;
        inferenceStep1.inferenceSteps(steps,ids);
        inferenceStep2.inferenceSteps(steps,ids);
        steps.add(this);}
}
