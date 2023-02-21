package NormalFormTransformers;

import Datastructures.Clauses.InputClauses;
import Datastructures.Symboltable;
import InferenceSteps.InfInputClause;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfAtmost2Atleast extends InferenceStep {
    private final int[] atmostClause;
    private final int[] atleastClause;

    private static final String title = "ANFNormalizer: Atmost To Atleast";

    @Override
    public String title() {return title;}

    private static final String rule = title + "\n" +
            "atmost k l1,...,ln -> atleast n-k -l1...-ln";

    public InfAtmost2Atleast(int[] atmostClause, int[] atleastClause) {
        this.atmostClause = atmostClause;
        this.atleastClause = atleastClause;}
    @Override
    public String rule() {return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        return title + "\n" + InputClauses.toString(2,atmostClause,symboltable) + " -> "+
                InputClauses.toString(2,atleastClause,symboltable);
    }

    @Override
    public IntArrayList inputClauseIds() {
        return IntArrayList.wrap(new int[]{atmostClause[0]});}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        if(steps != null) {
            steps.add(new InfInputClause(atmostClause[0]));
            steps.add(this);}}
}
