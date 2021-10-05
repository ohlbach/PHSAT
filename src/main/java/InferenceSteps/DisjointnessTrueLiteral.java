package InferenceSteps;

import Datastructures.Clauses.Clause;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;

import static Utilities.Utilities.joinIntArrays;

public class DisjointnessTrueLiteral extends InferenceStep{
    private final Clause dClause;
    private final int trueLiteral;
    private final int derivedLiteral;
    private final InferenceStep inferenceStep;

    public static final String title = "Disjointness True Literals";

    public static final String rule = title + ":\n"+
            "A true literal causes all other disjoint literals to become false.\n" +
            "p != q != r != ...\n"+
            "p\n"+
            "------------------\n" +
            "-q, -r, ...";

    /**
     *
     * @param dClause         the equivalence clause
     * @param trueLiteral     the true literal
     * @param derivedLiteral  the derived true literal
     * @param inferenceStep   which derived the true literal
     */
    public DisjointnessTrueLiteral(Clause dClause, int trueLiteral, int derivedLiteral, InferenceStep inferenceStep) {
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
        String s = dClause.toString(0,symboltable);
        int width = s.length();
        return title + ":\n" + s+"\n"+
                StringUtils.center(Symboltable.toString(trueLiteral,symboltable),width) + "\n" +
                StringUtils.repeat('-',width)+"\n"+
                StringUtils.center(Symboltable.toString(derivedLiteral,symboltable),width);}

    @Override
    public IntArrayList origins() {
        return joinIntArrays(dClause.inferenceStep.origins(),inferenceStep.origins());}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        dClause.inferenceStep.inferenceSteps(steps);
        inferenceStep.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}
}
