package Datastructures.Results;

import Datastructures.Clauses.Clause;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

/** This class reports about an unsatisfiable clause, for example an empty clause.*/

public class UnsatisfiableClause extends Unsatisfiable {
    private final Clause clause;

    /** constructs an Unsatisfiability from an unsatisfiable clause
     *
     * @param clause an unsatisfiable clause
     */
    public UnsatisfiableClause(Clause clause) {
        this.clause = clause;}

    @Override
    public String description(Symboltable symboltable) {
        return "Unsatisfiable clause " + clause.toString(0,symboltable)+"\n";}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        InferenceStep step = clause.inferenceStep;
        if(step != null) steps.add(step);}

    @Override
    public IntArrayList origins() {
        InferenceStep step = clause.inferenceStep;
        if(step != null) return step.origins();
        return null;}
}
