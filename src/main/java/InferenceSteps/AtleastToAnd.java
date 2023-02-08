package InferenceSteps;

import Datastructures.Clauses.Clause;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class AtleastToAnd extends InferenceStep{
    public static final String title = "Atleast To And";

    public static final String rule = title + ": Example:\n"+
            "atleast 3 p,q,r -> p and q and r";

    private final Clause atleastClause;
    private final Clause andClause;

    public AtleastToAnd(Clause atleastClause, Clause andClause) {
        this.atleastClause = atleastClause;
        this.andClause = andClause;}

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        return title + ":\n" + atleastClause.toString(0,symboltable) + " -> " +
                andClause.toString(0,symboltable);}

    @Override
    public IntArrayList inputClauseIds() {
        InferenceStep step = atleastClause.inferenceStep;
        return step == null ? null : step.inputClauseIds();}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        InferenceStep step = atleastClause.inferenceStep;
        if(step != null) step.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}
}
