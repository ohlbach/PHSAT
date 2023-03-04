package Datastructures.Results;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.InputClauses;
import Datastructures.Symboltable;
import InferenceSteps.InfInputClause;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

/** This class reports about an unsatisfiable clause, for example an empty clause.*/

public class UnsatisfiableClause extends Unsatisfiable {
    private Clause clause = null;
    private int[] inputClause = null;

    /** constructs an Unsatisfiability from an unsatisfiable clause
     *
     * @param clause an unsatisfiable clause
     */
    public UnsatisfiableClause(Clause clause) {
        this.clause = clause;}

    public UnsatisfiableClause(int[] inputClause) {
        this.inputClause = inputClause;}


    @Override
    public String description(Symboltable symboltable) {
        return "Unsatisfiable clause " +
                ((clause == null) ? InputClauses.toString(0,inputClause,symboltable) :
                clause.toString(0,symboltable)+"\n");}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        InferenceStep step = (clause == null) ? clause.inferenceStep : new InfInputClause(inputClause[0]);
        if(step != null) steps.add(step);}

    @Override
    public IntArrayList inputClauseIds() {
        InferenceStep step = (clause == null) ? clause.inferenceStep : new InfInputClause(inputClause[0]);
        if(step != null) return step.inputClauseIds();
        return null;}
}
