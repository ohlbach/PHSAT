package InferenceSteps;

import Datastructures.Clauses.ClauseOld;
import Datastructures.Symboltable;
import Datastructures.Theory.Model;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

import static Utilities.Utilities.joinIntArrays;

/** documents truth of equivalent literals */
public class EquivalentTrueLiterals extends InferenceStep{

    public static final String title = "Equivalent True Literals";

    public static final String rule =
            title + ": Example:\n" +
                    "p=q=r=s and true(p) -> true(q,r,s)";

    private final ClauseOld clause;
    private final int trueLiteral;
    private final IntArrayList derivedLiterals;
    private final Model model;

    public EquivalentTrueLiterals(ClauseOld clause, int trueLiteral, IntArrayList derivedLiterals, Model model) {
        this.clause = clause;
        this.trueLiteral = trueLiteral;
        this.derivedLiterals = derivedLiterals;
        this.model = model;}

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        String st = title + ": " + clause.toString(0,symboltable) +
                " and true(" + Symboltable.toString(trueLiteral,symboltable)+") -> true(";
        int size = derivedLiterals.size();
        for(int i = 0; i < size; ++i) {
            st += Symboltable.toString(derivedLiterals.getInt(i),symboltable);
            if(i < size-1) st += ",";}
        return st + ")";}

    @Override
    public IntArrayList origins() {
        InferenceStep step = clause.inferenceStep;
        IntArrayList origins = (step == null) ? null : step.origins();
        step = model.getInferenceStep(trueLiteral);
        if(step != null) origins = joinIntArrays(origins,step.origins());
        return origins;}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        InferenceStep step = clause.inferenceStep;
        if(step != null) step.inferenceSteps(steps);
        step = model.getInferenceStep(trueLiteral);
        if(step != null) step.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}
}
