package Solvers.Resolution;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfEquivalence extends InferenceStep {

    private final static String title = "Equivalence Derivation";
    @Override
    public String title() {return title;}

    private final static String ruleTwoTwo = title + "\np,q and -p,-q -> p == -q";

    private final static String ruleTwoThree = title + "\np,q and -r,-p,-q -> r => p == -q";

    private final static String ruleThreeThree = title + "\n-r,p,q and -r,-p,-q -> t => p == -q";

    private String rule;
    @Override
    public String rule() {return ruleTwoTwo;}

    private String clause1String = null;
    private String clause2String = null;

    private int literal1, literal2;
    private int triggerLiteral = 0;

    private InferenceStep step1 = null;
    private InferenceStep step2 = null;


    public InfEquivalence(Clause clause1, Clause clause2, int literal1, int literal2, Symboltable symboltable) {
        clause1String = clause1.toString(symboltable,0);
        clause2String = clause2.toString(symboltable,0);
        this.literal1 = literal1;
        this.literal2 = literal2;
        step1 = clause1.inferenceSteps;
        step2 = clause2.inferenceSteps;
        rule = ruleTwoTwo;}

    public InfEquivalence(Clause clause1, Clause clause2, int triggerLiteral,int literal1, int literal2, Symboltable symboltable) {
        clause1String = clause1.toString(symboltable,0);
        clause2String = clause2.toString(symboltable,0);
        this.triggerLiteral = triggerLiteral;
        this.literal1 = literal1;
        this.literal2 = literal2;
        step1 = clause1.inferenceSteps;
        step2 = clause2.inferenceSteps;
        rule = clause1.size() == 2 ? ruleTwoThree : ruleThreeThree;}

    public String info(Symboltable symboltable) {
        String trigger = triggerLiteral == 0 ? "" : Symboltable.toString(triggerLiteral,symboltable) + " => ";
        return clause1String + " and " + clause2String + " -> " +
                trigger + Symboltable.toString(literal1,symboltable) + " == " +
                Symboltable.toString(literal2,symboltable);}

    @Override
    public String toString(Symboltable symboltable) {
        return title + "\n  " + info(symboltable);}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps, IntArrayList ids) {
        if(steps.contains(this)) return;
        step1.inferenceSteps(steps,ids);
        step2.inferenceSteps(steps,ids);
        steps.add(this);}
}
