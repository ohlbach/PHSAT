package Datastructures.Clauses.AllClauses;

import Datastructures.Clauses.Clause;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

import static Utilities.Utilities.joinIntArrays;

/** This class documents the intersection of intervals of clauses with the same literals */
public class InfIntervalIntersection extends InferenceStep {
    private final Clause clause1;
    private final Clause clause2;
    private final Clause intersectionClause;

    /** Constructs an InferenceStep for two clauses with the same literals, and intersecting intervals
     *
     * @param clause1 a clause
     * @param clause2 a clause with the same literals
     * @param intersectionClause the new clause with the intersecting interval.
     */
    public InfIntervalIntersection(Clause clause1, Clause clause2, Clause intersectionClause) {
        this.clause1 = clause1;
        this.clause2 = clause2;
        this.intersectionClause = intersectionClause;}

    @Override
    public String title() {
        return "Intersection of Intervals";}

    @Override
    public String rule() {
        return "I: literals and J: literals ->  I intersects J: literals";}

    @Override
    public String toString(Symboltable symboltable) {
        return clause1.toString(0,symboltable) + " and " + clause2.toString(0,symboltable) +
                " -> " + intersectionClause.toString(0,symboltable);}

    @Override
    public IntArrayList origins() {
        InferenceStep step1 = clause1.inferenceStep;
        InferenceStep step2 = clause2.inferenceStep;
        IntArrayList origins = null;
        if(step1 != null) origins = step1.origins();
        if(step2 != null) origins = joinIntArrays(origins,step2.origins());
        return origins;}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        InferenceStep step1 = clause1.inferenceStep;
        InferenceStep step2 = clause2.inferenceStep;
        if(step1 != null) step1.inferenceSteps(steps);
        if(step2 != null) step2.inferenceSteps(steps);}
}
