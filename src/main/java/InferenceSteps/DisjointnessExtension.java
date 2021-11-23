package InferenceSteps;

import Datastructures.Clauses.ClauseOld;
import Datastructures.Literals.CLiteralOld;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;

import static Utilities.Utilities.joinIntArrays;

public class DisjointnessExtension extends InferenceStep {
    private final ClauseOld dClause;
    private final int literal;
    private final ClauseOld extended;
    private final ArrayList<CLiteralOld> cLiterals;

    public static final String title = "Disjointness Extension";

    public static final String rule = title + ":\n"+
            "p != q != ... != s  at a\n"+
            "a != p\n"+
            "a != q\n"+
            "....\n"+
            "a != s\n" +
            "-------------------------\n"+
            "p != q != ... != s != a";

    public DisjointnessExtension(ClauseOld dClause, int literal, ClauseOld extended, ArrayList<CLiteralOld> cLiterals) {
        this.dClause = dClause;
        this.literal = literal;
        this.extended = extended;
        this.cLiterals = cLiterals;}

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        String st = dClause.toString(0,symboltable) + " extend by " + Symboltable.toString(literal,symboltable);
        int width = st.length();
        for(CLiteralOld cliteral : cLiterals) {
            String s = Symboltable.toString(cliteral.literal,symboltable) + " in " + cliteral.clause.toString(0,symboltable);
            width = Math.max(width,s.length());
            st += "\n"+ s;}
        return title + ":\n" + st + "\n" +
                StringUtils.repeat('-',width) + "\n" +
                extended.toString(0,symboltable);}

    @Override
    public IntArrayList origins() {
        IntArrayList origins = null;
        if( dClause.inferenceStep != null) origins = dClause.inferenceStep.origins();
        for(CLiteralOld step : cLiterals)
            origins = joinIntArrays(origins,
                    step.clause.inferenceStep == null ? null : step.clause.inferenceStep.origins());
        return origins;}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        if(dClause.inferenceStep != null) dClause.inferenceStep.inferenceSteps(steps);
        for(CLiteralOld cLiteral : this.cLiterals)
            if(cLiteral.clause.inferenceStep != null) cLiteral.clause.inferenceStep.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}
}
