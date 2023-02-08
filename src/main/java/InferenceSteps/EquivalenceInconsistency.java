package InferenceSteps;

import Datastructures.Clauses.Clause;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class EquivalenceInconsistency extends InferenceStep {
    private final Clause eClause;
    private final int literal1;
    private final int literal2;

    public static final String title = "Equivalence Inconsistency";

    public static final String rule = title + ":\n"+
            "p == -p == q == ... -> false";

    public EquivalenceInconsistency(Clause eClause, int literal1, int literal2) {
        this.eClause = eClause;
        this.literal1 = literal1;
        this.literal2 = literal2; }

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        return title + ":\n" + eClause.toString(0,symboltable) + " at " +
                Symboltable.toString(literal1,symboltable) + " = " +
                        Symboltable.toString(literal2,symboltable) + " -> false";}

    @Override
    public IntArrayList inputClauseIds() {
        return (eClause.inferenceStep != null) ? eClause.inferenceStep.inputClauseIds() : null;}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        if(eClause.inferenceStep != null) eClause.inferenceStep.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}
}
