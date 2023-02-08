package InferenceSteps;

import Datastructures.Symboltable;
import Datastructures.TwoLiteral.TwoLitClause;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

import static Utilities.Utilities.joinIntArrays;

public class UnitResolution2 extends InferenceStep{
    private final TwoLitClause clause;
    private final int falseLiteral;
    private final InferenceStep inferenceStep;

    public static final String title = "Unit Resolution";

    public static final String rule = title +
            ":\np,q and false(p) -> true(q)";


    public UnitResolution2(TwoLitClause clause, int falseLiteral, InferenceStep inferenceStep) {
        this.clause = clause;
        this.falseLiteral = falseLiteral;
        this.inferenceStep = inferenceStep;
    }
    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;
    }

    @Override
    public String toString(Symboltable symboltable) {
        int resolvent = clause.literal1 == falseLiteral ? clause.literal2 : clause.literal1;
        return clause.toString("",symboltable) + " and false(" + Symboltable.toString(falseLiteral,symboltable) +
                ") -> true(" +Symboltable.toString(resolvent,symboltable) + ")";}

    @Override
    public IntArrayList inputClauseIds() {
        return joinIntArrays(
                clause.inferenceStep != null ? clause.inferenceStep.inputClauseIds() : null,
                inferenceStep.inputClauseIds());}


    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        if(clause.inferenceStep != null) clause.inferenceStep.inferenceSteps(steps);
        inferenceStep.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);
    }
}
