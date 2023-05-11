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
    int representative,literal;
    InferenceStep equivalenceStep, clauseStep;

    public InfEquivalenceReplacement(String oldClause, Clause newClause,
                                     int representative, int literal, InferenceStep equivalenceStep, Symboltable symboltable) {
        this.oldClause       = oldClause;
        this.newClause       = newClause.toString(symboltable,0);
        this.representative  = representative;
        this.literal         = literal;
        this.equivalenceStep = equivalenceStep;
        this.clauseStep      = newClause.inferenceStep;
    }

    @Override
    public String toString(Symboltable symboltable) {
        return title + "\n  " + oldClause + " and " + Symboltable.toString(representative,symboltable) +
                " = " + Symboltable.toString(literal,symboltable) + " -> " + newClause;}

    @Override
    public IntArrayList inputClauseIds() {
        IntArrayList ids = (clauseStep == null) ? new IntArrayList() : clauseStep.inputClauseIds().clone();
        //for(int id : equivalenceStep.inputClauseIds()) {if(!ids.contains(id)) ids.add(id);}
        return ids;
    }

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        if(clauseStep != null) clauseStep.inferenceSteps(steps);
        //equivalenceStep.inferenceSteps(steps);
        steps.add(this);}
}
