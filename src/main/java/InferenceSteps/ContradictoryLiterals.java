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
    "   p \n"+
    "  -p\n" +
    "-----\n" +
    "false";
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
        String st3 = "false";
        int width = Math.max(st1.length(),Math.max(st2.length(),st3.length()));
        return title + ":\n" + StringUtils.center(st1,width) + "\n" + StringUtils.center(st2,width) + "\n" +
                StringUtils.repeat('-',width) + "\n" + StringUtils.center(st3,width);}

    @Override
    public IntArrayList origins() {
        return joinIntArrays(step1.origins(),step2.origins());}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        step1.inferenceSteps(steps);
        step2.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}
}
