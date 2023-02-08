package InferenceSteps;

import Datastructures.Clauses.InputClauses;
import Datastructures.Clauses.Clause;
import Datastructures.Symboltable;
import Datastructures.TwoLiteral.TwoLitClause;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;


/** explains the transition from the basicClause data structure to the internal Clause data structure.
 */
public class ClauseCopy extends InferenceStep {
    private int[] basicClause;
    private Clause clause = null;
    private TwoLitClause twoLitClause = null;
    public static String title = "Clause Copy";

    public static String rule = title + ":\n"+
            "Copies a basic clause like [id,type,literal1,...] to a Clause or TwoLitClause data structure";

    public ClauseCopy(int[] basicClause, Clause clause) {
        this.basicClause = basicClause;
        this.clause = clause;}

    public ClauseCopy(int[] basicClause, TwoLitClause clause) {
        this.basicClause = basicClause;
        this.twoLitClause = clause;}

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        String st = title + ":\n" + InputClauses.toString(0,basicClause,symboltable);
        int width = st.length();
        return st + "\n" + StringUtils.repeat('-',width) + "\n" +
                (clause != null ? clause.toString(0,symboltable) : twoLitClause.toString("",symboltable));}

    @Override
    public IntArrayList inputClauseIds() {
        return IntArrayList.wrap(new int[]{basicClause[0]});}

    /** adds this to steps
     *
     * @param steps a list for collecting the inference steps.
     */
    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        if(!steps.contains(this)) steps.add(this);}
}
