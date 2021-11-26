package Datastructures.Clauses.QuantifiedToCNF;

import Datastructures.Clauses.Clause;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

/** This class documents the transformation of exact-clauses to CNF */
public class InfExactlyToCNF extends InferenceStep {

    public static final String title = "Exactly-Clause to Conjunctive Normal Form";

    public static final String rule = title+ ":\n"+
            "exactly m p_1,...,p_n ->\n"+
            "(n over n-m+1) clauses with combinations of n-m+1 positive literals +\n"+
            "(n over m+1) clauses with combinations of m+1 negated literals.\n"+
            "Example:\n"+
            "exactly 2 1,2,3 -> [1: 1,2, 2: 1,3, 3: 2,3, 4: -1,-2,-3]";

    private final Clause exactlyClause;
    private final Clause orClause;

    public InfExactlyToCNF(Clause exactlyClause, Clause orClause) {
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
        return title+":\n"+ exactlyClause.toString(0,symboltable) + " -> " +
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
