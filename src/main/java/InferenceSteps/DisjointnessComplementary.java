package InferenceSteps;

import Datastructures.Clauses.Clause;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;

public class DisjointnessComplementary extends InferenceStep {
    private final Clause dClause;
    private final int doubleLiteral;
    private final int falseLiteral;

    public static final String title = "Disjointness False";

    public static final String rule = title + ":\n"+
            "p != +-p != q !=...\n"+
            "-------------------\n"+
            "           -q";

    public DisjointnessComplementary(Clause dClause, int doubleLiteral, int falseLiteral) {
        this.dClause = dClause;
        this.doubleLiteral = doubleLiteral;
        this.falseLiteral = falseLiteral; }

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        String st = dClause.toString(0,symboltable) + " at " +
                Symboltable.toString(doubleLiteral,symboltable);
        int width = st.length();
        return title + ":\n" + st + "\n" +
                StringUtils.repeat('-',width) + "\n" +
                StringUtils.center(Symboltable.toString(falseLiteral,symboltable), width);}

    @Override
    public IntArrayList origins() {
        return dClause.inferenceStep.origins();}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        dClause.inferenceStep.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}
}
