package Solvers.Resolution;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfTrueLiteralReduction extends InferenceStep {
    public final String title = "TrueLiteralReduction";
    @Override
    public String title() {return title;}

    public final String rule = "atleast n l1^n_1,...,lk^n_k and n_1+...+n_i-1+n_i+1+...+n_k < n -> true(l_i)";
    @Override
    public String rule() {
        return rule;}

    String clauseBefore;
    String clauseAfter;
    IntArrayList literals;

    int literal = 0;

    public InfTrueLiteralReduction(String clauseBefore) {
        this.clauseBefore = clauseBefore;
        literals = new IntArrayList(2);}

    public InfTrueLiteralReduction(String clauseBefore, int literal) {
        this.clauseBefore = clauseBefore;
        this.literal = literal;}


    public void addLiteral(int literal) {
        literals.add(literal);}

    public void setClauseAfter(String clauseAfter) {
        this.clauseAfter = clauseAfter;}



    @Override
    public String toString(Symboltable symboltable) {
        return (literal != 0) ? title + ": " + clauseBefore + " -> true(" + Symboltable.toString(literal,symboltable) + ")" :
            title + ": " + clauseBefore + " -> true(" + Symboltable.toString(literals,symboltable) + ") and " + clauseAfter;}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps, IntArrayList ids) {

    }
}
