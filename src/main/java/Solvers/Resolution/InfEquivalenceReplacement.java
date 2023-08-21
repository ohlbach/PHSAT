package Solvers.Resolution;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfEquivalenceReplacement extends InferenceStep {

    private final static String title = "Equivalence Replacement";
    @Override
    public String title() {return title;}

    private final static String rule = title + "\np,phi and p = q -> q,phi";

    @Override
    public String rule() {
        return rule;}

    String oldClause, newClause;
    IntArrayList trueLiterals = null;
    int representative,literal;
    InferenceStep equivalenceStep, clauseStep;

    public InfEquivalenceReplacement(String oldClause, Clause newClause, IntArrayList trueLiterals,
                                     int representative, int literal, InferenceStep equivalenceStep, Symboltable symboltable) {
        this.oldClause       = oldClause;
        this.newClause       = newClause.literals.isEmpty() ? "" : newClause.toString(symboltable,0);
        this.trueLiterals    = trueLiterals;
        this.representative  = representative;
        this.literal         = literal;
        this.equivalenceStep = equivalenceStep;
        this.clauseStep      = newClause.inferenceSteps;
    }

    @Override
    public String toString(Symboltable symboltable) {
        return title + "\n  " + info(symboltable);}

    public String info(Symboltable symboltable) {
        String trueLits = (trueLiterals == null || trueLiterals.isEmpty())? "" : Symboltable.toString(trueLiterals,symboltable);
        if(!trueLits.isEmpty()) trueLits = "true("+trueLits+")";
        if(!newClause.isEmpty() && !trueLits.isEmpty()) trueLits = " and " + trueLits;
        return oldClause + " and " + Symboltable.toString(representative,symboltable) +
                " == " + Symboltable.toString(literal,symboltable) + " -> " + newClause + trueLits;}



    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps, IntArrayList ids) {
        if(steps.contains(this)) return;
        clauseStep.inferenceSteps(steps,ids);
        equivalenceStep.inferenceSteps(steps,ids);
        steps.add(this);}
}
