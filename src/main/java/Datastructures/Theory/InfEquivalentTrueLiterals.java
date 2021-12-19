package Datastructures.Theory;

import Datastructures.Clauses.Clause;
import Datastructures.Literals.CLiteral;
import Datastructures.Symboltable;
import Datastructures.Theory.Model;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

import static Utilities.Utilities.joinIntArrays;

/** documents truth of equivalent literals */
public class InfEquivalentTrueLiterals extends InferenceStep {

    public static final String title = "Equivalent True Literals";

    public static final String rule =
            title + ":\n" +
                    "p=q=...=s and true/false(p) -> true/false(q,...,s)";

    private final Clause clause;
    private final int literal;
    private final int sign;
    private final InferenceStep inferenceStep;

    public InfEquivalentTrueLiterals(Clause clause, int literal, int sign, InferenceStep inferenceStep) {
        this.clause = clause;
        this.literal = literal;
        this.sign = sign;
        this.inferenceStep = inferenceStep;}

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        String literals = "";
        for(CLiteral cLiteral : clause) {
            int lit = cLiteral.literal;
            if(Math.abs(lit) != Math.abs(literal))
                literals += Symboltable.toString(sign*lit,symboltable)+",";}
        literals = literals.substring(0,literals.length()-1);
        return title + ":\n" + clause.toString(0,symboltable) +
                " and true(" + Symboltable.toString(literal,symboltable)+") -> true("
                +literals + ")";}

    @Override
    public IntArrayList origins() {
        InferenceStep step = clause.inferenceStep;
        IntArrayList origins = (step == null) ? null : step.origins();
        if(inferenceStep != null) origins = joinIntArrays(origins,inferenceStep.origins());
        return origins;}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        InferenceStep step = clause.inferenceStep;
        if(step != null) step.inferenceSteps(steps);
        if(inferenceStep != null) inferenceStep.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}
}
