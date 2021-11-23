package InferenceSteps;

import Datastructures.Clauses.ClauseOld;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

/** atleast 2 p,p,q -> true(p) */
public class AtleastPPQ extends InferenceStep{
    public static final String title = "Atleast ppq";

    public static final String rule = title+": Example:\n"+
            "atleast 2 p,p,q -> true(p)";

    private final ClauseOld clause;
    private final int literal;

    public AtleastPPQ(ClauseOld clause, int literal) {
        this.clause = clause;
        this.literal = literal;}

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        return title + "\n" + clause.toString(0,symboltable) +
        " -> true(" + Symboltable.toString(literal,symboltable)+ ")";}

    @Override
    public IntArrayList origins() {
        InferenceStep step = clause.inferenceStep;
        return (step == null) ? null : step.origins();}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        InferenceStep step = clause.inferenceStep;
        if(step != null) step.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}
}
