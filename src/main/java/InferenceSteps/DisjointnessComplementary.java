package InferenceSteps;

import Datastructures.Clauses.ClauseOld;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class DisjointnessComplementary extends InferenceStep {
    private final ClauseOld dClause;
    private final int doubleLiteral;
    private final int falseLiteral;

    public static final String title = "Disjointness Complementary";

    public static final String rule = title + ":\n"+
            "p != -p != q !=... -> -q";

    public DisjointnessComplementary(ClauseOld dClause, int doubleLiteral, int falseLiteral) {
        this.dClause = dClause;
        this.doubleLiteral = doubleLiteral;
        this.falseLiteral = falseLiteral; }

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        return title + ":\n" + dClause.toString(0,symboltable) + " at " +
                Symboltable.toString(doubleLiteral,symboltable) + " -> " +
                Symboltable.toString(falseLiteral,symboltable);}

    @Override
    public IntArrayList origins() {
        return dClause.inferenceStep == null ? null : dClause.inferenceStep.origins();}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        if(dClause.inferenceStep != null) dClause.inferenceStep.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}
}
