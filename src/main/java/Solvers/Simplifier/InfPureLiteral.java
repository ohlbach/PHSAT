package Solvers.Simplifier;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfPureLiteral extends InferenceStep {

    private static final String title = "Pure Literal";
    @Override
    public String title() {
        return title;}

    private static final String rule =
            "If a literal l does not occur any more in the clauses, " +
            "its negation can be made true.";
    @Override
    public String rule() {
        return rule;}

    private final int pureLiteral;

    public InfPureLiteral(int pureLiteral) {
        this.pureLiteral = pureLiteral;}

    @Override
    public String toString(Symboltable symboltable) {
        return "Literal " + Symboltable.toString(pureLiteral, symboltable) + " is pure and becomes true.";}

    @Override
    public IntArrayList inputClauseIds() {
        return null;
    }

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        steps.add(this);}
}
