package Solvers.Resolution;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfGCDReduction extends InferenceStep {

    public final String title = "GCD-Reduction";
    @Override
    public String title() {
        return title;}

    public final String rule = "atleast n l1^n1,...,lk^nk and g = gcd(n,n1,...,nk) -> alteast n/g l1^n1/g,...,lk^nk/g";
    @Override
    public String rule() {
        return rule;}

    String clauseBefore;
    String clauseAfter;
    int gcd;

    public InfGCDReduction(int gcd, String clauseBefore, String clauseAfter) {
        this.gcd = gcd;
        this.clauseBefore = clauseBefore;
        this.clauseAfter  = clauseAfter;
    }

    @Override
    public String toString(Symboltable symboltable) {
        return clauseBefore + " gcd = " + gcd + " -> " + clauseAfter;}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps, IntArrayList ids) {

    }
}
