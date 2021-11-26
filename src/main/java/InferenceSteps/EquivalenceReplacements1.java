package InferenceSteps;

import Datastructures.Clauses.Clause;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

import static Utilities.Utilities.joinIntArrays;

public class EquivalenceReplacements1 extends InferenceStep {
    private final Clause oldClause;
    private final Clause newClause;
    private final int oldLiteral;
    private final int newLiteral;
    private final Clause equivalenceClause;
    public static final String title = "Equivalence Replacement";

    public static String rule = title + "\n" +
            "...,a,... and  a == b -> ...,b,...";

    public EquivalenceReplacements1(Clause oldClause, int oldLiteral, Clause newClause, int newLiteral, Clause equivalenceClause) {
        this.oldClause  = oldClause;
        this.oldLiteral = oldLiteral;
        this.newClause  = newClause;
        this.newLiteral = newLiteral;
        this.equivalenceClause = equivalenceClause;}

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        return title + ":\n"+oldClause.toString(0,symboltable) + " and " +
                Symboltable.toString(oldLiteral,symboltable) + " == " +
                Symboltable.toString(newLiteral,symboltable) + " -> " +
                newClause.toString(0,symboltable);}


    @Override
    public IntArrayList origins() {
        return joinIntArrays(oldClause.inferenceStep == null ? null : oldClause.inferenceStep.origins(),
                equivalenceClause.inferenceStep == null ? null : equivalenceClause.inferenceStep.origins());}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        if(oldClause.inferenceStep != null)         oldClause.inferenceStep.inferenceSteps(steps);
        if(equivalenceClause.inferenceStep != null) equivalenceClause.inferenceStep.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}
}
