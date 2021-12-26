package Datastructures.Clauses;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfIncreasedLimit extends InferenceStep {

    public final static String title = "Increased Limit";
    public final static String rule = title + ":\n" +
            ">= n l_1^m_1,...,.l_k^m_k  ->  >= m l_1^m_1,...,.l_k^m_k\n" +
            "where m is the sallest m > n which has still a model for the clause.\n" +
            "Example: >= 3 p^2,q^2 -> >= 4 p^2,q^2";

    private final Clause oldClause;
    private final Clause newClause;

    public InfIncreasedLimit(Clause oldClause, Clause newClause) {
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
        return oldClause.toString(0,symboltable) + " -> " + newClause.toString(0,symboltable);
    }

    @Override
    public IntArrayList origins() {
        InferenceStep step = oldClause.inferenceStep;
        return step == null ? null : step.origins();}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        InferenceStep step = oldClause.inferenceStep;
        if(step != null) step.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}
}
