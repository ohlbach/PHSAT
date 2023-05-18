package Datastructures.Clauses;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfSwitchAtleastAtmost extends InferenceStep {
    public static final String title ="Switch Atleast, Atmost";
    public static final String rule = title+":\natleast n l_1,...,l_k -> atmost k-n -l_1,...,-l_k and vice versa";

    private final Clause atleastClause;
    private final Clause atmostClause;

    public InfSwitchAtleastAtmost(Clause atleastClause, Clause atmostClause) {
        this.atleastClause = atleastClause;
        this.atmostClause = atmostClause;}

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return title;}

    @Override
    public String toString(Symboltable symboltable) {
        return atleastClause.toString(0,symboltable) + " -> " +
                atmostClause.toString(0,symboltable);}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps, IntArrayList ids) {
        if(steps.contains(this)) return;
        InferenceStep step = atleastClause.inferenceStep;
        if(step != null) step.inferenceSteps(steps,ids);
        steps.add(this);}
}
