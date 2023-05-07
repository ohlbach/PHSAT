package Solvers.Resolution;

import Datastructures.Clauses.InputClauses;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfAtmostToAtleast extends InferenceStep {

    private static final String title = "Atmost to Atleast";
    @Override
    public String title() {return title;}

    private static final String rule = title + "\natmost n l_1,...,l_k -> atleast (k-n) -> -l_1,...,-l_k";
    @Override
    public String rule() {return rule;}
    private final int[] atmostClause;
    private final int[] atleastClause;
    public InfAtmostToAtleast(int[] atmostClause, int[] atleastClause) {
        this.atmostClause = atmostClause;
        this.atleastClause = atleastClause;}

    @Override
    public String toString(Symboltable symboltable) {
        return title + "\n" + InputClauses.toString(0,atmostClause,symboltable) +
                " -> " + InputClauses.toString(0,atleastClause,symboltable);}

    @Override
    public IntArrayList inputClauseIds() {
        return IntArrayList.wrap(new int[]{atmostClause[0]});}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        steps.add(this);}
}
