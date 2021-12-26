package Datastructures.Clauses;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfAtmostToAtleast extends InferenceStep {
    public static final String title ="Atmost to Atleast";
    public static final String rule = "atmost n l_1,...,l_k -> atleast k-n -l_1,...,-l_k";

    private final Clause atmostClause;
    private final Clause atleastClause;

    public InfAtmostToAtleast(Clause atmostClause, Clause atleastClause) {
        this.atmostClause = atmostClause;
        this.atleastClause = atleastClause;}


    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return title + ":\n"+rule;}

    @Override
    public String toString(Symboltable symboltable) {
        return atmostClause.toString(0,symboltable) + " -> " +
                    atleastClause.toString(0, symboltable);}

    @Override
    public IntArrayList origins() {
        InferenceStep step = atmostClause.inferenceStep;
        return step == null ? null : step.origins();}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        InferenceStep step = atmostClause.inferenceStep;
        if(step != null) step.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}
}
