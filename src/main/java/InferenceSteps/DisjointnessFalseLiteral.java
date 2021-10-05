package InferenceSteps;

import Datastructures.Clauses.Clause;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;

import static Utilities.Utilities.joinIntArrays;

public class DisjointnessFalseLiteral extends InferenceStep {
    private final Clause oldClause;
    private final Clause newClause;
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
    public DisjointnessFalseLiteral(Clause oldClause, Clause newClause, int falseLiteral, InferenceStep inferenceStep) {
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
        String s = oldClause.toString(0,symboltable);
        int width = s.length();
        return title + ":\n" + s+"\n"+
                StringUtils.center(Symboltable.toString(falseLiteral,symboltable),width) + "\n" +
                StringUtils.repeat('-',width)+"\n"+ newClause.toString(0,symboltable);}

    @Override
    public IntArrayList origins() {
        return joinIntArrays(oldClause.inferenceStep.origins(),inferenceStep.origins());}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        oldClause.inferenceStep.inferenceSteps(steps);
        inferenceStep.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}
}

