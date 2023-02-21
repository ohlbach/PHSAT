package NormalFormTransformers;

import Datastructures.Clauses.InputClauses;
import Datastructures.Symboltable;
import InferenceSteps.InfInputClause;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfExactly2Atleast extends InferenceStep {

    private final int[] exactlyClause;
    private final int[] atleastClause;

    public InfExactly2Atleast(int[] exactlyClause, int[][] atleastClauses, int index) {
        this.exactlyClause = exactlyClause;
        this.atleastClause = atleastClauses[index];
    }

    private static final String title = "ANFNormalizer: Exactly To Atleast";

    private static final String rule = title + "\n" +
            "exactly k l1...ln\n"+
            "-----------------\n"+
            "atleast k l1...ln\n"+
            "atleast n-k -l1...-ln";

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        return title + "\n" + InputClauses.toString(2,exactlyClause,symboltable) + " -> "+
                InputClauses.toString(2,atleastClause,symboltable);}

    @Override
    public IntArrayList inputClauseIds() {
        return IntArrayList.wrap(new int[]{exactlyClause[0]});}


    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        if(steps != null) steps.add(new InfInputClause(exactlyClause[0]));}

}
