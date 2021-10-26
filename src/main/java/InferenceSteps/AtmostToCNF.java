package InferenceSteps;

import Datastructures.Clauses.Clause;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class AtmostToCNF  extends InferenceStep{

    public static final String title = "Atmost-Clause to Conjunctive Normal Form";

    public static final String rule = title+": Example:\n"+
            "atmost 2 p,q,r,s -> -p,-q,-r & -p,-q,-s & -p,-r,-s & -q,-r,-s ";

    private final Clause atmostClause;
    private final Clause orClause;

    public AtmostToCNF(Clause atmostClause, Clause orClause) {
        this.atmostClause = atmostClause;
        this.orClause = orClause;}

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        return title+"\n"+ atmostClause.toString(0,symboltable) + " -> " +
                orClause.toString(0,symboltable);}

    @Override
    public IntArrayList origins() {
        InferenceStep step = atmostClause.inferenceStep;
        return (step == null) ? null : step.origins();}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        InferenceStep step = atmostClause.inferenceStep;
        if(step != null) step.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}
}

