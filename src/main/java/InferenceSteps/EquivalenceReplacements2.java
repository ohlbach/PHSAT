package InferenceSteps;

import Datastructures.Clauses.Clause;
import Datastructures.Symboltable;
import Datastructures.TwoLiteral.TwoLitClause;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;

import static Utilities.Utilities.joinIntArrays;

public class EquivalenceReplacements2 extends InferenceStep{
    private final TwoLitClause oldClause;
    private final TwoLitClause newClause;
    private final int literal1;
    private final int representative1;
    private final Clause eClause1;
    private final int literal2;
    private final int representative2;
    private final Clause eClause2;

    public static final String title = "Equivalence Replacement";

    public static final String rule = title + ":\n"+
            " p,q\n"+
            "p = x\n"+
            "q = y\n"+
            "-----\n"+
            " x,y";

    public EquivalenceReplacements2(TwoLitClause oldClause, TwoLitClause newClause,
                                    int literal1, int representative1,Clause eClause1,
                                    int literal2, int representative2,Clause eClause2) {
        this.oldClause = oldClause;
        this.newClause = newClause;
        this.literal1  = literal1;
        this.representative1 = representative1;
        this.eClause1 = eClause1;
        this.literal2 = literal2;
        this.representative2 = representative2;
        this.eClause2 = eClause2;}

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        String st1 = oldClause.toString("",symboltable);
        String st2 = (eClause1 != null) ? (Symboltable.toString(literal1,symboltable) + " == " +
                Symboltable.toString(representative1,symboltable) + "\n") : "";
        String st3 = (eClause2 != null) ? (Symboltable.toString(literal2,symboltable) + " == " +
                Symboltable.toString(representative2,symboltable) + "\n") : "";
        int width = Math.max(st1.length(),Math.max(st2.length(),st3.length()));
        return title + ":\n" + st1 + st2 + st3 +
                StringUtils.repeat('-',width) + "\n" + st3;}

    @Override
    public IntArrayList origins() {
        IntArrayList origins = oldClause.inferenceStep.origins();
        if(eClause1 != null) origins = joinIntArrays(origins,eClause1.inferenceStep.origins());
        if(eClause2 != null) origins = joinIntArrays(origins,eClause2.inferenceStep.origins());
        return origins;}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        oldClause.inferenceStep.inferenceSteps(steps);
        if(eClause1 != null) eClause1.inferenceStep.inferenceSteps(steps);
        if(eClause2 != null) eClause2.inferenceStep.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}
}
