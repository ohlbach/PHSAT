package InferenceSteps;

import Datastructures.Clauses.Clause;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;

import static Utilities.Utilities.joinIntArrays;

/** specifies a unit resolution step where a false literal is removed from a clause */
public class UnitResolution extends InferenceStep{
    private final Clause oldClause;
    private final Clause newClause;
    private final int literal;
    private final InferenceStep inferenceStep;
    public static final String title = "Unit Resolution";

    public static String rule = title + ":\n" +
            "p,q,r,...\n"+
            "-p\n"+
            "---------\n"+
            "  q,r,...";

    /** specifies the unit resolution step
     *
     * @param oldClause      the original clause
     * @param literal        the true literal
     * @param newClause      the new clause without the false literal
     * @param inferenceStep  which produced the truth value of the literal
     */
    public UnitResolution(Clause oldClause, int literal, Clause newClause, InferenceStep inferenceStep) {
        this.oldClause = oldClause;
        this.literal = literal;
        this.newClause = newClause;
        this.inferenceStep = inferenceStep;}

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        String st = title + ":\n"+oldClause.toString(0,symboltable);
        int width = st.length();
        return st + "\n   " + Symboltable.toString(literal,symboltable)+"\n" +
                StringUtils.repeat('-',width) + "\n"+
                newClause.toString(0,symboltable);}

    @Override
    public IntArrayList origins() {
        return joinIntArrays(oldClause.inferenceStep.origins(),inferenceStep.origins());}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        oldClause.inferenceStep.inferenceSteps(steps);
        if(!steps.contains(inferenceStep)) steps.add(inferenceStep);
        if(!steps.contains(this)) steps.add(this);}
}
