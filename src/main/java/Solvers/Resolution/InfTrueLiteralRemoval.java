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
    InferenceStep inferenceStep;

    public InfTrueLiteralRemoval(String clauseBefore, IntArrayList literals, Clause clause, Symboltable symboltable) {
        this.clauseBefore = clauseBefore;
        this.literals = literals.clone();
        this.clauseAfter = clause.toString(symboltable,0);
        inferenceStep = clause.inferenceStep;}

    public String info(Symboltable symboltable) {
        return clauseBefore + " and true(" + symboltable.toString(literals,symboltable)+") -> " + clauseAfter;}
    @Override
    public String toString(Symboltable symboltable) {
        return title + "\n  " + info(symboltable);
    }

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps, IntArrayList ids) {
        if(steps.contains(this)) return;
        inferenceStep.inferenceSteps(steps,ids);
        steps.add(this);}
}
