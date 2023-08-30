package Solvers.Resolution;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfReduceToEssentialLiterals extends InferenceStep {

    public final String title = "Reduce To Essential Literals";
    @Override
    public String title() {
        return title;}

    public final String rule = "atleast n l_1^n,...,l_k^n, l_k+1^n1,...,l_k+l^nl and n1+...+nl < n -> l_1,...,l_k";
    @Override
    public String rule() {
        return rule;}

    String clauseBefore;
    String clauseAfter;

    public InfReduceToEssentialLiterals(String clauseBefore, Clause clause, Symboltable symboltable) {
        this.clauseBefore = clauseBefore;
        this.clauseAfter = clause.toString(symboltable,0);}

    @Override
    public String toString(Symboltable symboltable) {
        return title + ": " + clauseBefore + " -> " + clauseAfter;}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps, IntArrayList ids) {

    }
}
