package Solvers.Simplifier;

import Datastructures.Symboltable;
import Datastructures.Theory.Model;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

/** Documents an inference step where true/false literals are removed from a clause.
 */
public class InfUnitResolution extends InferenceStep {

    private final static String title = "Unit Resolution";

    private final static String ruleDisjunction     = "p,..., q1,...,qn and false(p,...) -> q1,...,qn";
    private final static String ruleQuantifiedTrue  = "atleast n p^k,..., q1,...,qn and true(p,...) -> atleast (n-k) q1,...,qn";
    private final static String ruleQuantifiedFalse = "atleast n p^k,..., q1,...,qn and false(p,...) -> atleast n q1,...,qn";

    private String rule;

    private String clauseBefore;
    private InferenceStep clauseStep;
    private String clauseAfter;
    private IntArrayList literals;
    private boolean isTrue;
    private boolean isDisjunction;

    private Model model;


    /** constructs a new inference step where true/false literals are removed from a clause.
     *
     * @param clauseBefore   the clause before removal (may be changed destductively).
     * @param clauseStep     the inference step that generated the clause.
     * @param isDisjunction  true if the clause was a disjunction.
     * @param literals       the literals to be removed
     * @param isTrue         true if the literals are true
     * @param clauseAfter    the clause after the removal
     * @param model          the model.
     */
    public InfUnitResolution(String clauseBefore, InferenceStep clauseStep, boolean isDisjunction,
                             IntArrayList literals, boolean isTrue, String clauseAfter, Model model) {
        this.clauseBefore  = clauseBefore;
        this.clauseStep    = clauseStep;
        this.isDisjunction = isDisjunction;
        this.literals      = literals;
        this.clauseAfter   = clauseAfter;
        this.isTrue        = isTrue;
        this.clauseAfter   = clauseAfter;
        this.model         = model;
        if(isDisjunction) {rule = ruleDisjunction; return;}
        rule = isTrue ? ruleQuantifiedTrue : ruleQuantifiedFalse;
    }

    @Override
    public String title() {return title;}

    @Override
    public String rule() { return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        return clauseBefore + (isTrue ? "true" : "false") + literals + " -> " + clauseAfter;
    }

    @Override
    public IntArrayList inputClauseIds() {
        IntArrayList clauseIds = clauseStep.inputClauseIds();
        if(clauseIds == null) clauseIds = new IntArrayList();
        IntArrayList literalIds = new IntArrayList();
        for(int literal : literals) {
            InferenceStep step = model.getInferenceStep(literal);
            if(step != null) literalIds.addAll(step.inputClauseIds());}
        for(int id : literalIds) {if(!clauseIds.contains(id)) clauseIds.add(id);}
        return clauseIds.isEmpty() ? null : clauseIds;}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        if(clauseStep != null) clauseStep.inferenceSteps(steps);
        for(int literal : literals) {
            InferenceStep step = model.getInferenceStep(literal);
            if(step != null) {step.inferenceSteps(steps);}}
        if(!steps.contains(this)) steps.add(this);}
}
