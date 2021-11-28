package Datastructures.Clauses.Simplifiers;

import Datastructures.Clauses.Clause;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfDoubleAndComplementaryLiterals extends InferenceStep {

    private static final String title = "Multiple Literals in Quantified Clauses";
    private static final String rule = title + ":\n" +
            "Multiple Literals:      Q m p,p,q,... ->  Q m-1 p,q,...\n"+
            "Complementary Literals: Q m p,-p,q,... -> Q m-1 q,...\n"+
            "Q in atleast, atmost, exactly";

    private final Clause oldClause;
    private final Clause newClause;
    private final IntArrayList doubleLiterals;
    private final IntArrayList complementaryLiterals;

    public InfDoubleAndComplementaryLiterals(Clause oldClause, Clause newClause,
                                             IntArrayList doubleLiterals, IntArrayList complementaryLiterals) {
        this.oldClause = oldClause;
        this.newClause = newClause;
        this.doubleLiterals = doubleLiterals;
        this.complementaryLiterals = complementaryLiterals;}

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
