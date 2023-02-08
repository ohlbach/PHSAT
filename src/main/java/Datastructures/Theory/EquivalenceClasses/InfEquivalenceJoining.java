package Datastructures.Theory.EquivalenceClasses;

import Datastructures.Clauses.Clause;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import static Utilities.Utilities.joinIntArrays;

public class InfEquivalenceJoining extends InferenceStep {
    private final Clause clause1;
    private final Clause clause2;
    private final Clause joinedClause;
    private final int literal;

    public static final String title = "Joining of Overlapping Equivalences";

    public static final String rule = title + ":\n"+
            "          p == q == ... == s\n"+
            "          p == r == ... == t\n"+
            "-----------------------------------\n"+
            "p == q == ... == s == r == ... == t";


    public InfEquivalenceJoining(Clause clause1, Clause clause2, int literal, Clause joinedClause) {
        this.clause1 = clause1;
        this.clause2 = clause2;
        this.literal = literal;
        this.joinedClause = joinedClause;}

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        return title + ":\n" + clause1.toString(0,symboltable) + " and " +
        clause2.toString(0,symboltable) + " at " + Symboltable.toString(literal,symboltable) + " -> " +
        joinedClause.toString(0,symboltable);}

    @Override
    public IntArrayList inputClauseIds() {
        return joinIntArrays(
                (clause1.inferenceStep != null) ? clause1.inferenceStep.inputClauseIds() : null,
                (clause2.inferenceStep != null) ? clause2.inferenceStep.inputClauseIds() : null);}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        if(clause1.inferenceStep != null) clause1.inferenceStep.inferenceSteps(steps);
        if(clause2.inferenceStep != null) clause2.inferenceStep.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}
}
