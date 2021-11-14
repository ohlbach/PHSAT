package InferenceSteps;

import Datastructures.Clauses.Clause;
import Datastructures.Symboltable;
import Datastructures.Theory.Model;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

import static Utilities.Utilities.joinIntArrays;

/** specifies a unit resolution step where a false literal is removed from a clause */
public class UnitResolution extends InferenceStep{
    private final Clause oldClause;
    private final Clause newClause;
    private final IntArrayList literals;
    private final ArrayList<InferenceStep> inferenceSteps = new ArrayList<>();
    public static final String title = "Unit Resolution";

    public static String rule = title + ":\n" +
            "p,q,r,s,...\n"+
            "-p\n"+
            "---------\n"+
            "  q,r,s,...";

    /** specifies the unit resolution step
     *
     * @param oldClause      the original clause
     * @param literals       the true literals
     * @param newClause      the new clause without the false literal
     * @param model          the model
     */
    public UnitResolution(Clause oldClause, Clause newClause, IntArrayList literals, Model model) {
        this.oldClause = oldClause;
        this.literals = literals;
        this.newClause = newClause;
        for(int literal : literals) inferenceSteps.add(model.getInferenceStep(literal));}

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        String truths = "";
        int size = literals.size();
        for(int i = 0; i < size; ++i) {
            int literal =literals.getInt(i);
            truths += Symboltable.toString(literal,symboltable);
            if(i < size-1) truths += ",";}
        return title + ":\n"+oldClause.toString(0,symboltable) + " and true(" + truths + ") -> " +
                newClause.toString(0,symboltable);}

    @Override
    public IntArrayList origins() {
        IntArrayList origins = oldClause.inferenceStep.origins();
        for(InferenceStep step : inferenceSteps) joinIntArrays(origins,step.origins());
        return origins;}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        if(oldClause.inferenceStep != null) oldClause.inferenceStep.inferenceSteps(steps);
        for(InferenceStep step : inferenceSteps) {
            if(step != null && !steps.contains(step)) steps.add(step);}
        if(!steps.contains(this)) steps.add(this);}
}
