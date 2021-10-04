package InferenceSteps;

import Datastructures.Clauses.Clause;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;

import static Utilities.Utilities.joinIntArrays;

public class EquivalenceReplacements extends InferenceStep {
    private final Clause oldClause;
    private final Clause newClause;
    private final int oldLiteral;
    private final int newLiteral;
    private final Clause equivalenceClause;
    public static final String title = "Equivalence Replacement";

    public static String rule = title + "\n" +
            "...,a,...\n"+
            "  a == b\n"+
            "---------\n"+
            "...,b,...";

    public EquivalenceReplacements(Clause oldClause, int oldLiteral, Clause newClause, int newLiteral, Clause equivalenceClause) {
        this.oldClause  = oldClause;
        this.oldLiteral = oldLiteral;
        this.newClause  = newClause;
        this.newLiteral = newLiteral;
        this.equivalenceClause = equivalenceClause;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        String st = title + ":\n"+oldClause.toString(0,symboltable);
        int width = st.length();
        st += "\n" +
                Symboltable.toString(oldLiteral,symboltable) + " == " +
                Symboltable.toString(newLiteral,symboltable) + "\n" +
                StringUtils.repeat('-',width) + "\n" +
                newClause.toString(0,symboltable);
        return st;}


    @Override
    public IntArrayList origins() {
        return joinIntArrays(oldClause.inferenceStep.origins(),equivalenceClause.inferenceStep.origins());
    }

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        oldClause.inferenceStep.inferenceSteps(steps);
        equivalenceClause.inferenceStep.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}
}
