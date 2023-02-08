package InferenceSteps;

import Datastructures.Clauses.Clause;
import Datastructures.Symboltable;
import Datastructures.TwoLiteral.TwoLitClause;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;

import static Utilities.Utilities.joinIntArrays;

public class EquivalenceReplacements2 extends InferenceStep{
    private final TwoLitClause Clause;
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

    public EquivalenceReplacements2(TwoLitClause Clause, TwoLitClause newClause,
                                    int literal1, int representative1, Clause eClause1,
                                    int literal2, int representative2, Clause eClause2) {
        this.Clause = Clause;
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
        String st1 = Clause.toString("",symboltable) + "\n";
        String st2 = (eClause1 != null) ? (Symboltable.toString(literal1,symboltable) + " == " +
                Symboltable.toString(representative1,symboltable) + " from " +
                eClause1.toString(0,symboltable) +"\n") : "";
        String st3 = (eClause2 != null) ? (Symboltable.toString(literal2,symboltable) + " == " +
                Symboltable.toString(representative2,symboltable) + " from " +
                eClause2.toString(0,symboltable) +"\n") : "";
        String st4 = newClause.toString("",symboltable);
        int width = Math.max(Math.max(st1.length(),st2.length()),Math.max(st3.length(),st4.length()));
        return title + ":\n" + st1 + st2 + st3 +
                StringUtils.repeat('-',width) + "\n" + st4;}

    @Override
    public IntArrayList inputClauseIds() {
        IntArrayList origins = null;
        if(Clause.inferenceStep != null) origins = Clause.inferenceStep.inputClauseIds();
        if(eClause1 != null) origins = joinIntArrays(origins,eClause1.inferenceStep.inputClauseIds());
        if(eClause2 != null) origins = joinIntArrays(origins,eClause2.inferenceStep.inputClauseIds());
        return origins;}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        if(Clause.inferenceStep != null) Clause.inferenceStep.inferenceSteps(steps);
        if(eClause1 != null) eClause1.inferenceStep.inferenceSteps(steps);
        if(eClause2 != null) eClause2.inferenceStep.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}
}
