package Datastructures.Clauses;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import InferenceSteps.Input;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfAtmostToAtleast extends InferenceStep {
    public static final String title ="Atmost to Atleast";
    public static final String rule = "atmost n l_1,...,l_k -> atleast k-n -l_1,...,-l_k";

    private final int[] basicClause;
    private final Clause clause;

    public InfAtmostToAtleast(int[] basicClause, Clause clause) {
        this.basicClause = basicClause;
        this.clause = clause;}

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return title + ":\n"+rule;}

    @Override
    public String toString(Symboltable symboltable) {
        return BasicClauseList.clauseToString(0,basicClause,symboltable) + " -> " +
                clause.toString(0,symboltable);}

    @Override
    public IntArrayList origins() {
        return IntArrayList.wrap(new int[]{basicClause[0]});}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        steps.add(new Input(basicClause[0]));
        steps.add(this);}
}
