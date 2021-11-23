package InferenceSteps;

import Datastructures.Clauses.ClauseOld;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class AndToModel extends InferenceStep{
    public static final String title = "And To Model";

    public static final String rule = title + ": Example:\n"+
            "Clause p & q & r -> true(p,q,r)";

    private final ClauseOld clause;

    public AndToModel(ClauseOld clause) {
        this.clause = clause;}

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        String st = "";
        int size = clause.size();
        for(int i = 0; i < size; ++i) {
            st += Symboltable.toString(clause.getLiteral(i),symboltable);
            if(i < size -1) st += ",";}
        return title + ":\n" + clause.toString(0,symboltable) + " -> true("+ st + ")";}

    @Override
    public IntArrayList origins() {
        InferenceStep step = clause.inferenceStep;
        return step == null ? null : step.origins();
    }

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        InferenceStep step = clause.inferenceStep;
        if(step != null) step.inferenceSteps(steps);}
}
