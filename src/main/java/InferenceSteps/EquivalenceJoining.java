package InferenceSteps;

import Datastructures.Clauses.Clause;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import org.apache.commons.lang3.StringUtils;
import java.util.ArrayList;
import static Utilities.Utilities.joinIntArrays;

public class EquivalenceJoining  extends InferenceStep {
    private final Clause clause1;
    private final Clause clause2;
    private final Clause joinedClause;
    private final int literal;

    public static final String title = "Equivalence Joining";

    public static final String rule = title + ":\n"+
            "Joining two overlapping equivalence clauses.\n" +
            "          p == q == ... == s\n"+
            "          p == r == ... == t\n"+
            "-----------------------------------\n"+
            "p == q == ... == s == r == ... == t";


    public EquivalenceJoining(Clause clause1, Clause clause2, int literal, Clause joinedClause) {
        this.clause1 = clause1;
        this.clause2 = clause2;
        this.literal = literal;
        this.joinedClause = joinedClause;
    }

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        String st1 = clause1.toString(0,symboltable);
        String st2 = clause2.toString(0,symboltable) + " at " + Symboltable.toString(literal,symboltable);
        String st3 = joinedClause.toString(0,symboltable);
        int width = Math.max(st1.length(),Math.max(st2.length(),st3.length()));
        return title + "\n" + st1 + "\n" + st2 + "\n" +
                StringUtils.repeat('-',width) + "\n" +
                st3;}

    @Override
    public IntArrayList origins() {
        return joinIntArrays(clause1.inferenceStep.origins(),clause2.inferenceStep.origins());}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        clause1.inferenceStep.inferenceSteps(steps);
        clause2.inferenceStep.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}
}
