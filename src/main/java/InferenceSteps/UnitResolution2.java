package InferenceSteps;

import Datastructures.Symboltable;
import Datastructures.TwoLiteral.TwoLitClause;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class UnitResolution2 extends InferenceStep{
    private final TwoLitClause clause;
    private final int trueLiteral;
    private final InferenceStep inferenceStep;

    public UnitResolution2(TwoLitClause clause, int trueLiteral, InferenceStep inferenceStep) {
        this.clause = clause;
        this.trueLiteral = trueLiteral;
        this.inferenceStep = inferenceStep;
    }
    @Override
    public String rule() {
        return null;
    }

    @Override
    public String toString(Symboltable symboltable) {
        return null;
    }

    @Override
    public IntArrayList origins() {
        return null;
    }

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {

    }
}
