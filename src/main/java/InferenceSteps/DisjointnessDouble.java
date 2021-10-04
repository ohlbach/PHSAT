package InferenceSteps;

import Datastructures.Clauses.Clause;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;

public class DisjointnessDouble extends InferenceStep {
    private final Clause dClause;
    private final int literal;

    public static final String title = "Disjointness Double";

    public static final String rule = title + ":\n"+
            "p != p != ...\n"+
            "-------------\n"+
            "  -p";

    public DisjointnessDouble(Clause dClause, int literal) {
        this.dClause = dClause;
        this.literal = literal; }

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        String st = dClause.toString(0,symboltable);
        int width = st.length();
        return title + ":\n" + st + "\n" +
                StringUtils.repeat('-',width) + "\n" +
                StringUtils.center(Symboltable.toString(literal,symboltable), width);}

    @Override
    public IntArrayList origins() {
        return dClause.inferenceStep.origins();}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        dClause.inferenceStep.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}
}
