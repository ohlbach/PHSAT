package InferenceSteps;

import Datastructures.Clauses.Clause;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class NumericMultipleP extends InferenceStep{
    public static final String title = "Numeric Multiple P";

    public static final String rule = title + ":  Example:\n"+
            "atmost  2 p,p,p,q,r -> false(p) and atmost 2 q,r"+
            "exactly 2 p,p,p,q,r -> false(p) and exactly 2 q,r";

    private final Clause oldClause;
    private final Clause newClause;
    private final IntArrayList falseLiterals;

    public NumericMultipleP(Clause oldClause, Clause newClause, IntArrayList falseLiterals) {
        this.oldClause = oldClause;
        this.newClause = newClause;
        this.falseLiterals = falseLiterals;}

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        String st = "";
        int size = falseLiterals.size();
        for(int i = 0; i < size; ++i) {
            st += Symboltable.toString(falseLiterals.getInt(i),symboltable);
            if(i < size - 1) st += ",";}
        return title + ":\n" + oldClause.toString(0,symboltable) + " -> " +
            newClause.toString(0,symboltable) + " and false(" + st + ")";}

    @Override
    public IntArrayList inputClauseIds() {
        InferenceStep step = oldClause.inferenceStep;
        return (step == null) ? null : step.inputClauseIds();}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        InferenceStep step = oldClause.inferenceStep;
        if(step != null) step.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}
}
