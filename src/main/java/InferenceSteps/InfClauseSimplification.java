package InferenceSteps;

import Datastructures.Clause;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfClauseSimplification extends InferenceStep{
    @Override
    public String title() {
        return "Clause Simplification";}

    @Override
    public String rule() {
        return "";
    }

    private int[] clauseBefore;
    private Clause clauseAfter;
    private IntArrayList trueLiterals;

    public InfClauseSimplification(int[] clauseBefore, Clause clauseAfter, IntArrayList trueLiterals) {
        super();
        this.clauseBefore = clauseBefore;
        this.clauseAfter = clauseAfter;
        this.trueLiterals = trueLiterals;}

    @Override
    public String toString(Symboltable symboltable) {
        return Clause.toString(clauseBefore, symboltable) + " -> " + clauseAfter.toString(symboltable,0);
    }

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps, IntArrayList ids) {

    }
}
