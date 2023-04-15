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

    private static final String rulePure =
            "If a literal l does not occur any more in the clauses,\n" +
            "its negation can be made true.";

    private static final String rulePartial =
            "If the clauses are 2-literal saturated\n" +
                    "and a literal l does not occur any more in the longer clauses,\n" +
                    "its negation can be made true.";

    @Override
    public String rule() {
        return partiallyPure ? rulePartial : rulePure;}

    private final int pureLiteral;
    private final boolean partiallyPure;

    public InfPureLiteral(int pureLiteral, boolean partiallyPure) {
        this.pureLiteral = pureLiteral;
        this.partiallyPure = partiallyPure;}

    @Override
    public String toString(Symboltable symboltable) {
        return "Literal " + Symboltable.toString(pureLiteral, symboltable) +
                " is" + (partiallyPure ? " partially" : "") +" pure and becomes true.";}

    @Override
    public IntArrayList inputClauseIds() {
        return new IntArrayList();}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        steps.add(this);}
}
