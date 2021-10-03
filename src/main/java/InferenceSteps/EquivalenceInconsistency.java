package InferenceSteps;

import Datastructures.Clauses.Clause;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;

public class EquivalenceInconsistency extends InferenceStep {
    private final Clause eClause;
    private final int literal1;
    private final int literal2;

    public static final String title = "Equivalence Inconsistency";

    public static final String rule = title + ":\n"+
            "p == -p == q == ...\n"+
            "---------------------\n"+
            "       false";

    public EquivalenceInconsistency(Clause eClause, int literal1, int literal2) {
        this.eClause = eClause;
        this.literal1 = literal1;
        this.literal2 = literal2; }

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        String st = eClause.toString(0,symboltable);
        int width = st.length();
        return title + ":\n" + st + "\n" +
                StringUtils.center(Symboltable.toString(literal1,symboltable) + " = " +
                        Symboltable.toString(literal2,symboltable),width)+ "\n" +
                        StringUtils.repeat('-',width) + "\n" +
                StringUtils.center("false",width);}

    @Override
    public IntArrayList origins() {
        return eClause.inferenceStep.origins();}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        eClause.inferenceStep.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}
}
