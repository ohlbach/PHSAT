package InferenceSteps;

import Datastructures.Symboltable;
import Datastructures.TwoLiteral.TwoLitClause;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

import static Utilities.Utilities.joinIntArrays;

public class EquivalenceDerivation extends InferenceStep {
    private final TwoLitClause clause1;
    private final TwoLitClause clause2;

    public static final String title = "Equivalence Derivation";

    public static final String rule = title +
            ":\n-p, q and p,-q -> p == q";

    public EquivalenceDerivation(TwoLitClause clause1,TwoLitClause clause2) {
        this.clause1 = clause1;
        this.clause2 = clause2;
    }

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        int literal1 =  clause1.literal1;
        int literal2 = -clause1.literal2;
        if(literal1 < 0) {literal1 *= -1; literal2 *= -1;}
        return clause1.toString("",symboltable) + " and " + clause2.toString("",symboltable) + " -> " +
                Symboltable.toString(literal1,symboltable) + " == " + Symboltable.toString(literal2,symboltable);}

    @Override
    public IntArrayList inputClauseIds() {
        return joinIntArrays(
                (clause1.inferenceStep != null) ? clause1.inferenceStep.inputClauseIds() : null,
                (clause2.inferenceStep != null) ? clause2.inferenceStep.inputClauseIds() : null);}


    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        if(clause1.inferenceStep != null)  clause1.inferenceStep.inferenceSteps(steps);
        if(clause2.inferenceStep != null)  clause2.inferenceStep.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}
}
