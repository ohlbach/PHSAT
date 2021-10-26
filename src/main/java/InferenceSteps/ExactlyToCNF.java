package InferenceSteps;

import Datastructures.Clauses.Clause;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class ExactlyToCNF extends InferenceStep{

    public static final String title = "Exactly-Clause to Conjunctive Normal Form";

    public static final String rule = title+": Example:\n"+
            "exactly 2 p,q,r -> cross-product((p&q&-r)|(p&-q&r)|(-p&q&r))";

    private final Clause exactlyClause;
    private final Clause orClause;

    public ExactlyToCNF(Clause exactlyClause, Clause orClause) {
        this.exactlyClause = exactlyClause;
        this.orClause = orClause;}

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        return title+"\n"+ exactlyClause.toString(0,symboltable) + " -> " +
                orClause.toString(0,symboltable);}

    @Override
    public IntArrayList origins() {
        InferenceStep step = exactlyClause.inferenceStep;
        return (step == null) ? null : step.origins();}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        InferenceStep step = exactlyClause.inferenceStep;
        if(step != null) step.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}
}
