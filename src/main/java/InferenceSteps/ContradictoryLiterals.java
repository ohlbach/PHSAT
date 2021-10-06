package InferenceSteps;

import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;

import static Utilities.Utilities.joinIntArrays;

public class ContradictoryLiterals extends InferenceStep{
    private final int literal;
    private final InferenceStep step1;
    private final InferenceStep step2;

    public static final String title = "Contradictory Literals";

    public static final String rule = title + ":\n"+
    "p and -p -> false";

    public ContradictoryLiterals(int literal, InferenceStep step1, InferenceStep step2) {
        this.literal = literal;
        this.step1 = step1;
        this.step2 = step2; }

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        String st1 = Symboltable.toString(literal,symboltable);
        String st2 = Symboltable.toString(-literal,symboltable);
        return title + ":\n" + st1 + " and " + st2  + " -> false";}

    @Override
    public IntArrayList origins() {
        return joinIntArrays(
                step1 == null ? null : step1.origins(),
                step2 == null ? null : step2.origins());}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        if(step1 != null) step1.inferenceSteps(steps);
        if(step2 != null) step2.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}
}
