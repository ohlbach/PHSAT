package Datastructures.Clauses.SimplifiersOld;

import Datastructures.Clauses.Clause;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

/** documents a transformation atleast m l_1,...,l_m -> true(l_1,...,l_m) */

public class QuantifiedToModel extends InferenceStep {

    public static final String title = "True Literals from Quantified-Clause";

    public static final String rule = title + ":\n" +
            "atleast m l_1,...,l_m  -> true(l_1,...,l_m)\n"+
            "atmost 0 l_1,...,l_m   -> true(-l_1,...,-l_m)\n"+
            "exactly m l_1,...,l_m  -> true(l_1,...,l_m)\n";

    private final Clause clause;
    private final int literal;

    public QuantifiedToModel(Clause clause, int literal) {
        this.clause  = clause;
        this.literal = literal;}

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        return title + ":\n" +
                clause.toString(0,symboltable) + " -> true("+ Symboltable.toString(literal,symboltable)+")";}

    @Override
    public IntArrayList inputClauseIds() {
        InferenceStep step = clause.inferenceStep;
        return step == null ? null : step.inputClauseIds();}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        InferenceStep step = clause.inferenceStep;
        if(step != null) steps.add(step);
        if(!steps.contains(this)) steps.add(this);}
}