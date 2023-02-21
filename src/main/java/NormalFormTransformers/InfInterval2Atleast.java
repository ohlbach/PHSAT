package NormalFormTransformers;

import Datastructures.Clauses.InputClauses;
import Datastructures.Symboltable;
import InferenceSteps.InfInputClause;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfInterval2Atleast extends InferenceStep {

    private final int[] intervalClause;
    private final int[] atleastClause;

    public InfInterval2Atleast(int[] intervalClause, int[][] atleastClauses, int index) {
        this.intervalClause = intervalClause;
        this.atleastClause  = atleastClauses[index];
    }

    private static final String title = "ANFNormalizer: Interval To Atleast";

    private static final String rule = title + "\n" +
            "[min,max] l1...ln\n"+
            "-----------------\n"+
            "atleast min l1...ln\n"+
            "atleast n-max -l1...-ln";

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        return title + "\n" + InputClauses.toString(2, intervalClause,symboltable) + " -> "+
                InputClauses.toString(2,atleastClause,symboltable);}

    @Override
    public IntArrayList inputClauseIds() {
        return IntArrayList.wrap(new int[]{intervalClause[0]});}


    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        if(steps != null) {
            steps.add(new InfInputClause(intervalClause[0]));
            steps.add(this);
        }}

}
