package InferenceSteps;

import Datastructures.Clauses.Clause;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import org.apache.commons.lang3.StringUtils;

import static Utilities.Utilities.joinIntArrays;

/** specifies a unit resolution step where a false literal is removed from a clause */
public class UnitResolution extends InferenceStep{
    private Clause oldClause;
    private Clause newClause;
    private int literal;
    private InferenceStep inferenceStep;

    public static String rule = "Unit Resolution:\n"+
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
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        String st = "Unit Resoluton:\n"+oldClause.toString(0,symboltable);
        int width = st.length();
        return st + "\n   " + Symboltable.toString(literal,symboltable)+"\n" +
                StringUtils.repeat('-',width) + "\n"+
                newClause.toString(0,symboltable);}

    @Override
    public Object[] input() {
        return new Object[]{oldClause,literal,inferenceStep};}

    @Override
    public Object output() {
        return newClause;}

    @Override
    public IntArrayList origins() {
        return joinIntArrays(oldClause.inferenceStep.origins(),inferenceStep.origins());}
}
