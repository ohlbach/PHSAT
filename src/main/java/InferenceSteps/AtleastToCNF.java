package InferenceSteps;

import Datastructures.Clauses.Clause;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class AtleastToCNF extends InferenceStep{

    public static final String title = "Atleast-Clause to Conjunctive Normal Form";

    public static final String rule = title+": Example:\n"+
            "atleast 2 p,q,r -> cross-product((p&q)|(p&r)|(q&r))";

    private final Clause atleastClause;
    private final Clause orClause;
    public AtleastToCNF(Clause atleastClause, Clause orClause) {
        this.atleastClause = atleastClause;
        this.orClause = orClause;}

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        return title+"\n"+ atleastClause.toString(0,symboltable) + " -> " +
                orClause.toString(0,symboltable);}

    @Override
    public IntArrayList origins() {
        InferenceStep step = atleastClause.inferenceStep;
        return (step == null) ? null : step.origins();}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        InferenceStep step = atleastClause.inferenceStep;
        if(step != null) step.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}
}
