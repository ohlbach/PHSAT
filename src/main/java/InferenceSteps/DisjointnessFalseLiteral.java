package InferenceSteps;

import Datastructures.Clauses.ClauseOld;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

import static Utilities.Utilities.joinIntArrays;

public class DisjointnessFalseLiteral extends InferenceStep {
    private final ClauseOld oldClause;
    private final ClauseOld newClause;
    private final int falseLiteral;
    private final InferenceStep inferenceStep;

    public static final String title = "Disjointness False Literal";

    public static final String rule = title + ":\n"+
            "A false literal is removed .\n" +
            "p != q != r != ...\n"+
            "-p\n"+
            "------------------\n" +
            "   q != -r != ...";

    /**
     *
     * @param oldClause       the equivalence clause
     * @param newClause       the shortened equivalence clause
     * @param falseLiteral    the false literal
     * @param inferenceStep   which derived the true literal
     */
    public DisjointnessFalseLiteral(ClauseOld oldClause, ClauseOld newClause, int falseLiteral, InferenceStep inferenceStep) {
        this.oldClause = oldClause;
        this.newClause = newClause;
        this.falseLiteral = falseLiteral;
        this.inferenceStep = inferenceStep;}

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        return oldClause.toString(0,symboltable) + " and false(" +
                Symboltable.toString(falseLiteral,symboltable) + ") -> " +
                newClause.toString(0,symboltable);}

    @Override
    public IntArrayList origins() {
        return joinIntArrays(
                oldClause.inferenceStep == null ? null : oldClause.inferenceStep.origins(),
                inferenceStep != null ? null : inferenceStep.origins());}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        if(oldClause.inferenceStep != null) oldClause.inferenceStep.inferenceSteps(steps);
        if(inferenceStep != null) inferenceStep.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}
}

