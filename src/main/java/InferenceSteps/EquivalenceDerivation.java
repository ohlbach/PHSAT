package InferenceSteps;

import Datastructures.Symboltable;
import Datastructures.TwoLiteral.TwoLitClause;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;

import static Utilities.Utilities.joinIntArrays;

public class EquivalenceDerivation extends InferenceStep {
    private final TwoLitClause clause1;
    private final TwoLitClause clause2;

    public static final String title = "Equivalence Derivation";

    public static final String rule = title + ":\n" +
            "-p, q\n"+
            " p,-q\n"+
            "------\n"+
            "p == q";

    public EquivalenceDerivation(TwoLitClause clause1,TwoLitClause clause2) {
        this.clause1 = clause1;
        this.clause2 = clause2;
    }

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        String st1 = clause1.toString("",symboltable);
        String st2 = clause2.toString("",symboltable);
        int literal1 = -clause1.literal1;
        int literal2 = clause2.literal2;
        if(literal1 < 0) {literal1 *= -1; literal2 *= -1;}
        String st3 = Symboltable.toString(literal1,symboltable) + " == " + Symboltable.toString(literal2,symboltable);
        int width = Math.max(st1.length(),Math.max(st2.length(),st3.length()));
        return st1 + "\n" + st2 + "\n" + StringUtils.repeat('-',width) + st3;}

    @Override
    public IntArrayList origins() {
        return joinIntArrays(clause1.inferenceStep.origins(),clause2.inferenceStep.origins());}


    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        clause1.inferenceStep.inferenceSteps(steps);
        clause2.inferenceStep.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}
}
