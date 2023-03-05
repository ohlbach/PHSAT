package Solvers.Simplifier;

import Datastructures.Clauses.InputClauses;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfExactlyToAtleast extends InferenceStep {

    private static final String title = "Exactly to Atleast";
    @Override
    public String title() {return title;}

    private static final String rule = title + "\n" +
            "exactly n l_1,...,l_k -> atleast n l_1,...,l_k and atleast (k-n) -l_1,...,-l_k";
    @Override
    public String rule() {return rule;}

    private final int[] exactlyClause;
    private final int[] atleastClause;

    public InfExactlyToAtleast(int[] exactlyClause, int[] atleastClause) {
        this.exactlyClause = exactlyClause;
        this.atleastClause = atleastClause;}

    @Override
    public String toString(Symboltable symboltable) {
        return title + "\n" + InputClauses.toString(0,exactlyClause,symboltable) +
                " -> " + InputClauses.toString(0,exactlyClause,symboltable);}


    @Override
    public IntArrayList inputClauseIds() {
        return IntArrayList.wrap(new int[]{exactlyClause[0]});}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        steps.add(this);}
}
