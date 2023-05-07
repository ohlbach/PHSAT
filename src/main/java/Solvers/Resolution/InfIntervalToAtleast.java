package Solvers.Resolution;

import Datastructures.Clauses.InputClauses;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfIntervalToAtleast extends InferenceStep {

    private static final String title = "Interval to Atleast";
    @Override
    public String title() {return title;}

    private static final String rule = title + "\n" +
            "interval [n,m] l_1,...,l_k -> atleast n l_1,...,l_k and atleast (k-m) -l_1,...,-l_k";
    @Override
    public String rule() {return rule;}
    private final int[] intervalClause;
    private final int[] atleastClause;

    public InfIntervalToAtleast(int[] intervalClause, int[] atleastClause) {
        this.intervalClause = intervalClause;
        this.atleastClause = atleastClause;}

    @Override
    public String toString(Symboltable symboltable) {
        return title + "\n" + InputClauses.toString(0, intervalClause,symboltable) +
                " -> " + InputClauses.toString(0, atleastClause,symboltable);}


    @Override
    public IntArrayList inputClauseIds() {
        return IntArrayList.wrap(new int[]{intervalClause[0]});}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        steps.add(this);}
}
