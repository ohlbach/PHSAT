package InferenceSteps;

import Datastructures.Clauses.ClauseOld;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

import static Utilities.Utilities.joinIntArrays;

public class DisjointnessTrueLiteral extends InferenceStep{
    private final ClauseOld dClause;
    private final int trueLiteral;
    private final int derivedLiteral;
    private final InferenceStep inferenceStep;

    public static final String title = "Disjointness True Literals";

    public static final String rule = title + ":\n"+
            "A true literal causes all other disjoint literals to become false.\n" +
            "p != q != r != ... and true(p) -> -q, -r, ...";

    /**
     *
     * @param dClause         the equivalence clause
     * @param trueLiteral     the true literal
     * @param derivedLiteral  the derived true literal
     * @param inferenceStep   which derived the true literal
     */
    public DisjointnessTrueLiteral(ClauseOld dClause, int trueLiteral, int derivedLiteral, InferenceStep inferenceStep) {
        this.dClause = dClause;
        this.trueLiteral = trueLiteral;
        this.derivedLiteral = derivedLiteral;
        this.inferenceStep = inferenceStep;}

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        return title + ":\n" + dClause.toString(0,symboltable) + " and true(" +
                Symboltable.toString(trueLiteral,symboltable) + ") -> " +
                Symboltable.toString(derivedLiteral,symboltable);}

    @Override
    public IntArrayList origins() {
        return joinIntArrays(
                dClause.inferenceStep == null ? null : dClause.inferenceStep.origins(),
                inferenceStep == null ? null : inferenceStep.origins());}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        if(dClause.inferenceStep != null) dClause.inferenceStep.inferenceSteps(steps);
        if(inferenceStep != null) inferenceStep.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}
}
