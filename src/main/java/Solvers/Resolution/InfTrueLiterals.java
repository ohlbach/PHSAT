package Solvers.Resolution;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfTrueLiterals extends InferenceStep {

    private static final String title = "True Literals Applied to Clause";
    @Override
    public String title() {return title;}

    private static final String rule = "C: l1,...,ln and true(p1,...,pk) -> pi removed from C";
    @Override
    public String rule() {return rule;}

    private String clauseBefore, clauseAfter;
    private IntArrayList literals; private ArrayList<InferenceStep> inferenceSteps;
    public InfTrueLiterals (String clauseBefore, String clauseAfter, IntArrayList literals, ArrayList<InferenceStep> inferenceSteps) {
        this.clauseBefore = clauseBefore;
        this.clauseAfter = clauseAfter;
        this.literals = literals;
        this.inferenceSteps = inferenceSteps;
    }
    @Override
    public String toString(Symboltable symboltable) {
        return clauseBefore + " and true" + literals + " -> " + clauseAfter;}

    @Override
    public void inputClauseIds(IntArrayList ids) {
        for(InferenceStep step : inferenceSteps) step.inputClauseIds(ids);
        }

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        for(InferenceStep step : inferenceSteps) {step.inferenceSteps(steps);}}
}
