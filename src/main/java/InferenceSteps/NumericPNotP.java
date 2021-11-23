package InferenceSteps;

import Datastructures.Clauses.ClauseOld;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

/** Complementary literals: atleast 3 p,-p,q,r -> atleast 2 q,r and atmost 3 p,-p,q,r -> atmost 2 q,r*/
public class NumericPNotP extends InferenceStep{
    public static final String title = "Atleast/Atmost/Exactly p, not p";
    public static final String rule =
            title+": Examples:\n"+
                    "atleast  3 p,-p,q,r -> atleast 2 q,r\n"+
                    "atmost   3 p,-p,q,r -> atmost  2 q,r"+
                    "exactly  3 p,-p,q,r -> axactly 2 q,r";

    private final ClauseOld oldClause;
    private final ClauseOld newClause;

    public NumericPNotP(ClauseOld oldClause, ClauseOld newClause) {
        this.oldClause = oldClause;
        this.newClause = newClause;}

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        return title + ":\n" + oldClause.toString(0,symboltable) + " -> " +
                newClause.toString(0,symboltable);}

    @Override
    public IntArrayList origins() {
        InferenceStep step = oldClause.inferenceStep;
        return step == null ? null : step.origins();}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        InferenceStep step = oldClause.inferenceStep;
        if(step != null) step.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}
}
