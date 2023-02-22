package Datastructures.Clauses.SimplifiersOld;

import Datastructures.Clauses.Clause;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfMultipleAndComplementaryLiterals extends InferenceStep {

    private static final String title = "Multiple and Complementary Literal Removal";
    private static final String rule = title + ":\n" +
            "Multiple Literals:      [1,n] p,p,q,... ->  [1,n-1] p,q,...\n"+
            "Complementary Literals: [n,m] p,-p,q,... -> [n-1,m-1] q,...\n"+
            "Multiple Literal Removal only in Or-Clauses";

    private final Clause oldClause;
    private final Clause newClause;
    private final IntArrayList multipleLiterals;
    private final IntArrayList complementaryLiterals;

    public InfMultipleAndComplementaryLiterals(Clause oldClause, Clause newClause,
                                               IntArrayList multipleLiterals, IntArrayList complementaryLiterals) {
        this.oldClause = oldClause;
        this.newClause = newClause;
        this.multipleLiterals = multipleLiterals;
        this.complementaryLiterals = complementaryLiterals;}

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        String st = "";
        if(!multipleLiterals.isEmpty()) st = "Multiple Literals: " + multipleLiterals.toString() + " ";
        if(!complementaryLiterals.isEmpty()) st += "Complementary Literals: " + complementaryLiterals.toString() + " ";
        if(st.length() > 0) st += "\n";
        return title + ":\n" + st + oldClause.toString(0,symboltable) + " -> " +
                newClause.toString(0,symboltable);}

    @Override
    public IntArrayList inputClauseIds() {
        InferenceStep step = oldClause.inferenceStep;
        return step == null ? null : step.inputClauseIds();}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        InferenceStep step = oldClause.inferenceStep;
        if(step != null) step.inferenceSteps(steps);
        if(!steps.contains(step)) steps.add(step);}
}
