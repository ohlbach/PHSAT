package Solvers.Simplifier;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfUnitResolution extends InferenceStep {

    private final static String title = "Unit Resolution";

    private final static String ruleDisjunction     = "p,q1,...,qn and false(p) -> q1,...,qn";
    private final static String ruleQuantifiedTrue  = "atleast n p^k,q1,...,qn and true(p) -> atleast (n-k) q1,...,qn";
    private final static String ruleQuantifiedFalse = "atleast n p^k,q1,...,qn and false(p) -> atleast n q1,...,qn";

    private String rule;

    private String clauseBefore;
    private Clause clauseAfter;
    private int literal;
    private boolean isTrue;
    private boolean isDisjunction;

    private InferenceStep literalStep;

    public InfUnitResolution(String clauseBefore, boolean isDisjunction,
                             int literal, boolean isTrue, InferenceStep literalStep,
                             Clause clauseAfter) {
        this.clauseBefore = clauseBefore;
        this.clauseAfter = clauseAfter;
        this.literal = literal;
        this.isTrue = isTrue;
        this.literalStep = literalStep;
        if(isDisjunction) {rule = ruleDisjunction; return;}
        rule = isTrue ? ruleQuantifiedTrue : ruleQuantifiedFalse;
    }

    @Override
    public String title() {return title;}

    @Override
    public String rule() { return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        return null;
    }

    @Override
    public IntArrayList inputClauseIds() {
        IntArrayList literalIds = literalStep.inputClauseIds();
        IntArrayList clauseIds = clauseAfter.inferenceStep.inputClauseIds();
        return null;
    }

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {

    }
}
