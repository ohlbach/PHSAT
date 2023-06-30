package Solvers.Resolution;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfTrueLiteralRemoval extends InferenceStep {

    private static final String title = "True Literal Removal";
    @Override
    public String title() {return title;}

    @Override
    public String rule() {
        return title + "\n clause and true(l1,...,ln) -> reduced clause";}

    String clauseBefore,clauseAfter;
    IntArrayList literals;
    ArrayList<InferenceStep> inferenceSteps = new ArrayList<>();

    public InfTrueLiteralRemoval(String clauseBefore, IntArrayList literals, ArrayList<InferenceStep> steps, Clause clause, Symboltable symboltable) {
        this.clauseBefore = clauseBefore;
        this.literals = literals.clone();
        this.clauseAfter = clause.toString(symboltable,0);
        inferenceSteps.add(clause.inferenceStep);
        inferenceSteps.addAll(steps);}

    public String info(Symboltable symboltable) {
        return clauseBefore + " and true(" + symboltable.toString(literals,symboltable)+") -> " + clauseAfter;}
    @Override
    public String toString(Symboltable symboltable) {
        return title + "\n  " + info(symboltable);
    }

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps, IntArrayList ids) {
        if(steps.contains(this)) return;
        for(InferenceStep step : inferenceSteps) step.inferenceSteps(steps,ids);
        steps.add(this);}
}
