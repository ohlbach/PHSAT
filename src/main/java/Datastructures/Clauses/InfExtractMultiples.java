package Datastructures.Clauses;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfExtractMultiples extends InferenceStep {
    public static final String title = "Extract CNF from Multiple Literals";
    public static final String rule =
            "n: p_1^k_1, ... , p_m^k_m,phi and n-|phi| > |phi| -> CNF(n-|phi|: p_1^k_1, ... , p_m^k_m)";

    private final Clause originalClause;
    private final Clause cnfClause;

    public InfExtractMultiples(Clause originalClause, Clause cnfClause) {
        this.originalClause = originalClause;
        this.cnfClause = cnfClause;}

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return title + ":\n" + rule;}

    @Override
    public String toString(Symboltable symboltable) {
        return title + ":\n"+originalClause.toString(0,symboltable) + " -> " + cnfClause.toString(0,symboltable);}

    @Override
    public IntArrayList origins() {
        InferenceStep step = originalClause.inferenceStep;
        return (step != null) ? step.origins() : null;}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        InferenceStep step = originalClause.inferenceStep;
        if(step != null) step.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}
}
