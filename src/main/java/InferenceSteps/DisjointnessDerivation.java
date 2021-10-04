package InferenceSteps;

import Datastructures.Symboltable;
import Datastructures.TwoLiteral.TwoLitClause;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;

import static Utilities.Utilities.joinIntArrays;

public class DisjointnessDerivation extends InferenceStep{
    private final IntArrayList literals;
    private final TwoLitClause clause1;
    private final TwoLitClause clause2;
    private final TwoLitClause clause3;

    public static final String title = "Disjointness Derivation";

    public static final String rule = title + "\n"+
            "   -p,-q\n"+
            "   -p,-r\n"+
            "   -q,-r\n"+
            "-----------\n"+
            "p != q != r";

    public DisjointnessDerivation(IntArrayList literals, TwoLitClause clause1, TwoLitClause clause2, TwoLitClause clause3) {
        this.literals = literals;
        this.clause1 = clause1;
        this.clause2 = clause2;
        this.clause3 = clause3;}
    @Override
    public String rule() {
        return null;}

    @Override
    public String toString(Symboltable symboltable) {
        String st1 = clause1.toString("",symboltable);
        String st2 = clause2.toString("",symboltable);
        String st3 = clause3.toString("",symboltable);
        String st4 =
                Symboltable.toString(literals.getInt(0),symboltable) + " != " +
                Symboltable.toString(literals.getInt(1),symboltable) + " != " +
                Symboltable.toString(literals.getInt(2),symboltable) ;
        int width = Math.max(Math.max(st1.length(),st2.length()) ,Math.max(st3.length(),st4.length()));
        return title + ":\n" + st1 + "\n" + st2 + "\n" + st3 + "\n" +
                StringUtils.repeat('-',width) + "\n" + st4;}

    @Override
    public IntArrayList origins() {
        IntArrayList origins = clause1.inferenceStep.origins();
        origins  = joinIntArrays(origins,clause2.inferenceStep.origins());
        return joinIntArrays(origins,clause3.inferenceStep.origins());}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        clause1.inferenceStep.inferenceSteps(steps);
        clause2.inferenceStep.inferenceSteps(steps);
        clause3.inferenceStep.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}
}
