package InferenceSteps;

import Datastructures.Symboltable;
import Datastructures.TwoLiteral.TwoLitClause;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;

import static Utilities.Utilities.joinIntArrays;

public class DisjointnessDerivation extends InferenceStep{
    private final IntArrayList literals;
    private final ArrayList<TwoLitClause> clauses;

    public static final String title = "Disjointness Derivation";

    public static final String rule = title + "\n"+
            "         -p,-q\n"+
            "         -p,-r\n"+
            "         ....\n" +
            "        -s,-t\n"+
            "-----------------------\n"+
            "p != q != r != ... != t";

    public DisjointnessDerivation(IntArrayList literals, ArrayList<TwoLitClause> clauses) {
        this.literals = literals;
        this.clauses = clauses;}

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        ArrayList<String> strings = new ArrayList<>();
        int width = 0;
        for(TwoLitClause clause : clauses) {
            String st =  clause.toString("",symboltable);
            width = Math.max(width,st.length());
            strings.add(st);}
        String st4 = "";
        for(int i = 0; i < literals.size()-1; ++i)
                st4 += Symboltable.toString(literals.getInt(i),symboltable) + " != ";
        st4 += Symboltable.toString(literals.getInt(literals.size()-1),symboltable);
        width = Math.max(width,st4.length());
        String result = title + "\n";
        for(String st : strings) result += StringUtils.center(st,width) + "\n";
        return result + StringUtils.repeat('-',width) + "\n" + st4;}

    @Override
    public IntArrayList inputClauseIds() {
        IntArrayList origins = null;
        for(TwoLitClause clause : clauses)
            if(clause.inferenceStep != null)
                origins  = joinIntArrays(origins,clause.inferenceStep.inputClauseIds());
        return origins;}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        for(TwoLitClause clause : clauses)
            if(clause.inferenceStep != null) clause.inferenceStep.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}
}
