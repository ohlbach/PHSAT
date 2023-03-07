package Solvers.Simplifier;

import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class UnsatEmptyClause extends Unsatisfiable {

    private final Clause emptyClause;

    public UnsatEmptyClause(Clause emptyClause) {
        this.emptyClause = emptyClause;}
    @Override
    public String description(Symboltable symboltable) {
        return "Empty clause " + emptyClause.id + " derived";}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        emptyClause.inferenceStep.inferenceSteps(steps);}

    @Override
    public IntArrayList inputClauseIds() {
        return emptyClause.inferenceStep.inputClauseIds();}
}
