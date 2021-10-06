package InferenceSteps;

import Datastructures.Clauses.Clause;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class DisjointnessDouble extends InferenceStep {
    private final Clause dClause;
    private final int literal;

    public static final String title = "Disjointness Double";

    public static final String rule = title + ":\n"+
            "p != p != ... -> -p";

    public DisjointnessDouble(Clause dClause, int literal) {
        this.dClause = dClause;
        this.literal = literal; }

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        return title + ":\n" +
                dClause.toString(0,symboltable) + " -> " +
                Symboltable.toString(-literal,symboltable);}

    @Override
    public IntArrayList origins() {
        return dClause.inferenceStep == null ? null : dClause.inferenceStep.origins();}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        if(dClause.inferenceStep != null) dClause.inferenceStep.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}
}
