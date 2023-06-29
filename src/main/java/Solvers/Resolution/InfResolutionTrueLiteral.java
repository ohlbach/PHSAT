package Solvers.Resolution;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfResolutionTrueLiteral extends InferenceStep {

    private String title = "Resolution Yields True Literal";
    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return title + "\n  parentClause1 + parentClause2 -> resolvent -> true(literal)";
    }

    String parentClause1,parentClause2;
    int literal;
    InferenceStep inferenceStep1,inferenceStep2;

    public InfResolutionTrueLiteral(Clause parent1, Clause parent2, int literal, Symboltable symboltable) {
        parentClause1 = parent1.toString(symboltable,0);
        parentClause2 = parent2.toString(symboltable,0);
        inferenceStep1 = parent1.inferenceStep;
        inferenceStep2 = parent2.inferenceStep;
        this.literal = literal;}

    public String info(Symboltable symboltable) {
        return parentClause1 + " and " + parentClause2 + " -> true(" + Symboltable.toString(literal,symboltable) + ")";}
    @Override
    public String toString(Symboltable symboltable) {
        return title + "\n  " + info(symboltable);}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps, IntArrayList ids) {
        if(steps.contains(this)) return;
        inferenceStep1.inferenceSteps(steps,ids);
        inferenceStep2.inferenceSteps(steps,ids);
        steps.add(this);}
}
