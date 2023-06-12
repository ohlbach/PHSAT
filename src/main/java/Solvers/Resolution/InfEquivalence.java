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

    private String clause1String = null;
    private String clause2String = null;

    private Literal literalObject1 = null;
    private Literal literalObject2 = null;

    private InferenceStep step1 = null;
    private InferenceStep step2 = null;

    private int triggerLiteral = 0;

    public InfEquivalence(Clause clause1, Clause clause2) {
        this.clause1 = clause1;
        this.clause2 = clause2;
        literalObject1 = clause1.literals.get(0);
        literalObject2 = clause1.literals.get(1);
        step1 = clause1.inferenceStep;
        step2 = clause2.inferenceStep;
        rule = ruleTwoTwo;}

    public InfEquivalence(int triggerLiteral, Literal literalObject1, Literal literalObject2, Symboltable symboltable) {
        this.triggerLiteral = triggerLiteral;
        this.literalObject1 = literalObject1;
        this.literalObject2 = literalObject2;
        this.clause1 = literalObject1.clause;
        this.clause2 = literalObject2.clause;
        clause1String = clause1.toString(symboltable,0);
        clause2String = clause2.toString(symboltable,0);
        step1 = clause1.inferenceStep;
        step2 = clause2.inferenceStep;
        rule = clause1.size() == 2 ? ruleTwoThree : ruleThreeThree;}

    public String info(Symboltable symboltable) {
        if(clause1String == null) clause1String = clause1.toString(symboltable,0);
        if(clause2String == null) clause2String = clause2.toString(symboltable,0);
        String trigger = triggerLiteral == 0 ? "" : Symboltable.toString(triggerLiteral,symboltable) + " => ";
        return clause1String + " and " + clause2String + " -> " +
                trigger + Symboltable.toString(literalObject1.literal,symboltable) + " == " +
                Symboltable.toString(-literalObject2.literal,symboltable);}

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
