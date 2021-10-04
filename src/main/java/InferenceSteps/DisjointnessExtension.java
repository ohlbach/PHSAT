package InferenceSteps;

import Datastructures.Clauses.Clause;
import Datastructures.Literals.CLiteral;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;

import static Utilities.Utilities.joinIntArrays;

public class DisjointnessExtension extends InferenceStep {
    private final Clause dClause;
    private final int literal;
    private final Clause extended;
    private final ArrayList<CLiteral> cLiterals;

    public static final String title = "Disjointness Extension";

    public static final String rule = title + ":\n"+
            "p != q != ... != s  at a\n"+
            "a != p\n"+
            "a != q\n"+
            "....\n"+
            "a != s\n" +
            "-------------------------\n"+
            "p != q != ... != s != a";

    public DisjointnessExtension(Clause dClause, int literal, Clause extended, ArrayList<CLiteral> cLiterals) {
        this.dClause = dClause;
        this.literal = literal;
        this.extended = extended;
        this.cLiterals = cLiterals;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        String st = dClause.toString(0,symboltable) + " at " + Symboltable.toString(literal,symboltable);
        int width = st.length();
        for(CLiteral cliteral : cLiterals) {
            String s = cliteral.clause.toString(0,symboltable);
            width = Math.max(width,s.length());
            st += s + "\n";}
        return title + ":\n" + st + "\n" +
                StringUtils.repeat('-',width) + "\n" +
                extended.toString(0,symboltable);}

    @Override
    public IntArrayList origins() {
        IntArrayList origins = dClause.inferenceStep.origins();
        for(CLiteral step : cLiterals) origins = joinIntArrays(origins,step.clause.inferenceStep.origins());
        return origins;}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        dClause.inferenceStep.inferenceSteps(steps);
        for(CLiteral cLiteral : this.cLiterals) cLiteral.clause.inferenceStep.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}
}
