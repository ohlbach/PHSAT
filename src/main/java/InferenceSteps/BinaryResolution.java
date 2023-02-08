package InferenceSteps;

import Datastructures.Clauses.Clause;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;

import static Utilities.Utilities.joinIntArrays;

/** Binary resolution works for disjunctions and disjointnesses in the same way. */

public class BinaryResolution extends InferenceStep {
    private final Clause parent1;
    private final Clause parent2;
    private final int literal;
    private final Clause resolvent;

    public static final String title = "Binary Resolution";

    public static final String rule = title + "\n"+
            " p,a,...,e at p\n"+
            "-p,f,...,k\n"+
            "---------------\n"+
            "a,...,e,f,...,k";

    public BinaryResolution(Clause parent1, Clause parent2, int literal, Clause resolvent) {
        this.parent1 = parent1;
        this.parent2 = parent2;
        this.literal = literal;
        this.resolvent = resolvent;}

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        String st1 = parent1.toString(0,symboltable) + " at " + Symboltable.toString(literal,symboltable);
        String st2 = parent2.toString(0,symboltable);
        String st3 = resolvent.toString(0,symboltable);
        int width = Math.max(st1.length(), Math.max(st2.length(), st3.length()));
        return title + ":\n" + st1 + "\n" + st2 + "\n" + StringUtils.repeat('-',width) + "\n" + st3;}

    @Override
    public IntArrayList inputClauseIds() {
        return joinIntArrays(
                parent1.inferenceStep == null ? null : parent1.inferenceStep.inputClauseIds(),
                parent2.inferenceStep == null ? null : parent2.inferenceStep.inputClauseIds());}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        if(parent1.inferenceStep != null) parent1.inferenceStep.inferenceSteps(steps);
        if(parent2.inferenceStep != null) parent2.inferenceStep.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}
}
