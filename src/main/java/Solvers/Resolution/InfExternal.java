package Solvers.Resolution;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfExternal extends InferenceStep {

    private static String title = "True Literal Derived By External Solver";
    @Override
    public String title() {return title;}

    private static String rule = "External Solver -> true(literal)";

    @Override
    public String rule() {
        return rule;}

    private int literal;

    public InfExternal(int literal) {
        this.literal = literal;}

    @Override
    public String toString(Symboltable symboltable) {
        return "External Solver -> " + Symboltable.toString(literal,symboltable);}

    @Override
    public IntArrayList inputClauseIds() {
        return new IntArrayList();}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        steps.add(this);}
}
