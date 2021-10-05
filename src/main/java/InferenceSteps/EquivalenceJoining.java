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
    public IntArrayList origins() {
        return joinIntArrays(
                (clause1.inferenceStep != null) ? clause1.inferenceStep.origins() : null,
                (clause2.inferenceStep != null) ? clause2.inferenceStep.origins() : null);}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        if(clause1.inferenceStep != null) clause1.inferenceStep.inferenceSteps(steps);
        if(clause2.inferenceStep != null) clause2.inferenceStep.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}
}
