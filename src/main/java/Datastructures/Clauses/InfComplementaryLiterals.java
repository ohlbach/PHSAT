package Datastructures.Clauses;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfComplementaryLiterals extends InferenceStep {
    public static final String title = "Complementary Literals";
    public static final String rule =
            "n: p^m,-p^m,phi -> n-m:phi\n"+
            "n: p^m,-p^k,phi and m > k -> n-k: p^m-k,phi";

    private final Clause oldClause;
    private final Clause newClause;
    private final IntArrayList complementaryLiterals;

    public InfComplementaryLiterals(Clause oldClause, Clause newClause, IntArrayList complementaryLiterals) {
        this.oldClause = oldClause;
        this.newClause = newClause;
        this.complementaryLiterals = complementaryLiterals;}

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return title + ":\n" + rule;
    }

    @Override
    public String toString(Symboltable symboltable) {
        String literals = " at ";
        for(int i = 0; i < complementaryLiterals.size(); ++i) {
            literals += Symboltable.toString(complementaryLiterals.getInt(i),symboltable);
            if(i < complementaryLiterals.size()-1) literals += ",";}
        return title + ":\n"+oldClause.toString(0,symboltable) + literals + " -> " + newClause.toString(0,symboltable);
    }

    @Override
    public IntArrayList inputClauseIds() {
        InferenceStep step = oldClause.inferenceStep;
        return (step != null) ? step.inputClauseIds() : null;}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        InferenceStep step = oldClause.inferenceStep;
        if(step != null) step.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}

}
