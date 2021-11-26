package Datastructures.Clauses.Simplifiers;

import Datastructures.Clauses.Clause;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class MultipleLiteralsQuantified extends InferenceStep {

    private static final String title = "Multiple Literals in Quantified Clauses";
    private static final String rule = title + ":\n" +
            "Multiple Literals:      Q m p,p,q,... ->  Q m-1 p,q,...\n"+
            "Complementary Literals: Q m p,-p,q,... -> Q m-1 q,...\n"+
            "Q in atleast, atmost, exactly";

    private final Clause oldClause;
    private final Clause newClause;

    public MultipleLiteralsQuantified(Clause oldClause, Clause newClause) {
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
        return title + ":\n" + oldClause.toString(0,symboltable) + " -> " +
                newClause.toString(0,symboltable);}

    @Override
    public IntArrayList origins() {
        InferenceStep step = oldClause.inferenceStep;
        return step == null ? null : step.origins();}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        InferenceStep step = oldClause.inferenceStep;
        if(step != null) step.inferenceSteps(steps);
        if(!steps.contains(step)) steps.add(step);}
}
