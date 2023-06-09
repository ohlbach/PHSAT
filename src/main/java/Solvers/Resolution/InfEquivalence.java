package Solvers.Resolution;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfEquivalence extends InferenceStep {

    private final static String title = "Equivalence Derivation";
    @Override
    public String title() {return title;}

    private final static String ruleTwoTwo = title + "\np,q and -p,-q -> p == q";

    private final static String ruleTwoThree = title + "\np,q and -r,-p,-q -> r => p == q";

    private final static String ruleThreeThree = title + "\n-r,p,q and -r,-p,-q -> t => p == q";

    private String rule;
    @Override
    public String rule() {return ruleTwoTwo;}

    private final Clause clause1,clause2;

    private int triggerLiteral = 0;

    public InfEquivalence(Clause clause1, Clause clause2) {
        this.clause1 = clause1;
        this.clause2 = clause2;
        rule = ruleTwoTwo;}

    public InfEquivalence(int triggerLiteral, Clause clause1, Clause clause2) {
        this.triggerLiteral = triggerLiteral;
        this.clause1 = clause1;
        this.clause2 = clause2;
        rule = clause1.size() == 2 ? ruleTwoThree : ruleThreeThree;}

    public String info(Symboltable symboltable) {
        String trigger = triggerLiteral == 0 ? "" : Symboltable.toString(triggerLiteral,symboltable) + " => ";
        return clause1.toString(symboltable,0) + " and " + clause2.toString(symboltable,0) + " -> " +
                trigger + Symboltable.toString(clause1.literals.get(0).literal,symboltable) + " == " +
                Symboltable.toString(clause1.literals.get(1).literal,symboltable);}

    @Override
    public String toString(Symboltable symboltable) {
        return title + "\n  " + info(symboltable);}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps, IntArrayList ids) {
        if(steps.contains(this)) return;
        clause1.inferenceStep.inferenceSteps(steps,ids);
        clause2.inferenceStep.inferenceSteps(steps,ids);
        steps.add(this);}
}
