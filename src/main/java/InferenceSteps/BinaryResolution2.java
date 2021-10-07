package InferenceSteps;

import Datastructures.Clauses.Clause;
import Datastructures.Symboltable;
import Datastructures.TwoLiteral.TwoLitClause;
import Datastructures.TwoLiteral.TwoLitClauses;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;

import static Utilities.Utilities.joinIntArrays;

public class BinaryResolution2 extends InferenceStep {
    private final TwoLitClause parent1;
    private final TwoLitClause parent2;
    private final TwoLitClause resolvent;

    public static final String title = "Binary Resolution";

    public static final String rule = title + "\n"+
            " p,q\n"+
            "-p,r\n"+
            "----\n"+
            "q,r";

    public BinaryResolution2(TwoLitClause parent1, TwoLitClause parent2, TwoLitClause resolvent) {
        this.parent1 = parent1;
        this.parent2 = parent2;
        this.resolvent = resolvent;}

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        String st1 = parent1.toString("",symboltable);
        String st2 = parent2.toString("",symboltable);
        String st3 = resolvent.toString("",symboltable);
        int width = Math.max(st1.length(), Math.max(st2.length(), st3.length()));
        return title + ":\n" + st1 + "\n" + st2 + "\n" +StringUtils.repeat('-',width) + "\n" + st3;}

    @Override
    public IntArrayList origins() {
        return joinIntArrays(
                parent1.inferenceStep == null ? null : parent1.inferenceStep.origins(),
                parent2.inferenceStep == null ? null : parent2.inferenceStep.origins());}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        if(parent1.inferenceStep != null) parent1.inferenceStep.inferenceSteps(steps);
        if(parent2.inferenceStep != null) parent2.inferenceStep.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}
}
