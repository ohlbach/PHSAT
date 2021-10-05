package InferenceSteps;

import Datastructures.Clauses.Clause;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;

import static Utilities.Utilities.joinIntArrays;

public class EquivalentTrueLiteral extends InferenceStep{
    private final Clause eClause;
    private final int trueLiteral;
    private final int derivedLiteral;
    private final InferenceStep inferenceStep;

    public static final String title = "Equivalent True Literals";

    public static final String rule = title + ":\n"+
            "p == q == r == ... and true(p) -> true(q) and ...";

    /**
     *
     * @param eClause         the equivalence clause
     * @param trueLiteral     the true literal
     * @param derivedLiteral  the derived true literal
     * @param inferenceStep   which derived the true literal
     */
    public EquivalentTrueLiteral(Clause eClause, int trueLiteral, int derivedLiteral, InferenceStep inferenceStep) {
        this.eClause = eClause;
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
        return title + ":\n" + eClause.toString(0,symboltable) +
                " and true("+Symboltable.toString(trueLiteral,symboltable) +
                ") -> true(" + Symboltable.toString(derivedLiteral,symboltable) +")";}

    @Override
    public IntArrayList origins() {
        InferenceStep step = eClause.inferenceStep;
        return joinIntArrays(step != null ? step.origins() : null,
                inferenceStep != null ? inferenceStep.origins() : null);}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        InferenceStep step = eClause.inferenceStep;
        if(step != null) step.inferenceSteps(steps);
        if(inferenceStep != null) inferenceStep.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}
}
