package InferenceSteps;

import Datastructures.Clauses.Clause;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class AllToModel extends InferenceStep{
    public static final String title = "All To Model";

    public static final String rule = title + ": Examples:\n"+
            "atmost 0 p,q,r  -> true(-p,-q,-r)\n"+
            "exactly 3 p,q,r -> true(p,q,r)  ";

    private final Clause clause;
    private final int sign;

    public AllToModel(Clause clause, int sign) {
        this.clause = clause;
        this.sign = sign;}

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
            st += Symboltable.toString(sign*clause.getLiteral(i),symboltable);
            if(i < size -1) st += ",";}
        return title + ":\n" + clause.toString(0,symboltable) + " -> true(" + st + ")";}

    @Override
    public IntArrayList origins() {
        InferenceStep step = clause.inferenceStep;
        return step == null ? null : step.origins();}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        InferenceStep step = clause.inferenceStep;
        if(step != null) step.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}
}
