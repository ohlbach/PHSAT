package Datastructures.Results;

import Datastructures.Clauses.Clause;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

import static Utilities.Utilities.joinIntArrays;

/** This class documents a contradiction arsing from two clauses with the same literals, but disjoint intervals
 */
public class UnsatisfiableInterval extends Unsatisfiable{
    private final Clause clause1;
    private final Clause clause2;

    /** constructs an Unsatisfiability from two clauses with the same literals, but disjoint intervals
     *
     * @param clause1 a clause
     * @param clause2 a clause
     */
    public UnsatisfiableInterval(Clause clause1, Clause clause2) {
        this.clause1 = clause1;
        this.clause2 = clause2;}

    @Override
    public String description(Symboltable symboltable) {
        int width = Math.max(clause1.getName().length(),clause2.getName().length());
        return "Two clauses with the same literals have disjoint intervals:\n" +
                clause1.toString(width,symboltable) + "\n"+
                clause2.toString(width,symboltable);}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        InferenceStep step1 = clause1.inferenceStep;
        InferenceStep step2 = clause2.inferenceStep;
        if(step1 != null) step1.inferenceSteps(steps);
        if(step2 != null) step2.inferenceSteps(steps);}

    @Override
    public IntArrayList origins() {
        InferenceStep step1 = clause1.inferenceStep;
        InferenceStep step2 = clause2.inferenceStep;
        IntArrayList origins = null;
        if(step1 != null) origins = step1.origins();
        if(step2 != null) origins = joinIntArrays(origins,step2.origins());
        return origins;}
}
