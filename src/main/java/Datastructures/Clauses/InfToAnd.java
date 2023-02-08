package Datastructures.Clauses;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfToAnd extends InferenceStep {
    public static final String title = "To And";
    public static final String rule = title+":\n clause is effectively a conjunction";

    private final Clause oldClause;
    private final Clause newClause;

    public InfToAnd(Clause oldClause, Clause newClause) {
        this.oldClause = oldClause;
        this.newClause = newClause;}


    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        return title + ":\n"+ oldClause.toString(0,symboltable) + " -> " + newClause.toString(0,symboltable);
    }

    @Override
    public IntArrayList inputClauseIds() {
        InferenceStep step = oldClause.inferenceStep;
        return step == null ? null : step.inputClauseIds();}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        InferenceStep step = oldClause.inferenceStep;
        if(step != null) step.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}
}
