package InferenceSteps;

import Datastructures.Symboltable;
import Datastructures.TwoLiteral.TwoLitClause;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;

import static Utilities.Utilities.joinIntArrays;

public class UnitResolution2 extends InferenceStep{
    private final TwoLitClause clause;
    private final int trueLiteral;
    private final InferenceStep inferenceStep;

    public static final String title = "Unit Resolution";

    public static final String rule = title + ":\n"+
            " p,q\n"+
            "-p\n"+
            "---\n"+
            "q";


    public UnitResolution2(TwoLitClause clause, int trueLiteral, InferenceStep inferenceStep) {
        this.clause = clause;
        this.trueLiteral = trueLiteral;
        this.inferenceStep = inferenceStep;
    }
    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;
    }

    @Override
    public String toString(Symboltable symboltable) {
        String st = clause.toString("",symboltable);
        int width = st.length();
        int resolvent = clause.literal1 == trueLiteral ? clause.literal2 : clause.literal1;
        return title + ":\n" + st + Symboltable.toString(trueLiteral,symboltable) +
                StringUtils.repeat('-',width) + Symboltable.toString(resolvent,symboltable);}

    @Override
    public IntArrayList origins() {
        return joinIntArrays(clause.inferenceStep.origins(),inferenceStep.origins());}


    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        clause.inferenceStep.inferenceSteps(steps);
        inferenceStep.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);
    }
}
