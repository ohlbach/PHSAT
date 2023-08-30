package Solvers.Resolution;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfUnitClause extends InferenceStep {

    public final String title = "Unit Clause";
    @Override
    public String title() {return title;}

    public final String rule = "Clause L -> true(L)";

    @Override
    public String rule() {return rule;}

    Clause clause;

    public InfUnitClause(Clause clause) {
        this.clause = clause;}
    @Override
    public String toString(Symboltable symboltable) {
        return title + ": "+ clause.toString(symboltable,0) + " -> " + "true("+
                Symboltable.toString(clause.literals.get(0).literal,symboltable) + ")";}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps, IntArrayList ids) {

    }
}
