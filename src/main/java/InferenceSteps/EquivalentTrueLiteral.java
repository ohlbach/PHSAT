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
            "p == q == r == ...\n"+
            "  true/false(p)\n"+
            "------------------\n" +
            "true/false(q), true/false(r),...";

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
        String s = eClause.toString(0,symboltable);
        int width = s.length();
        return title + ":\n" + s+"\n"+
                StringUtils.center(Symboltable.toString(trueLiteral,symboltable),width) + "\n" +
                StringUtils.repeat('-',width)+"\n"+
                StringUtils.center(Symboltable.toString(derivedLiteral,symboltable),width);}

    @Override
    public IntArrayList origins() {
        return joinIntArrays(eClause.inferenceStep.origins(),inferenceStep.origins());}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        eClause.inferenceStep.inferenceSteps(steps);
        inferenceStep.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}
}
