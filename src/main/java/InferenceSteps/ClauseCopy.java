package InferenceSteps;

import Datastructures.Clauses.BasicClauseList;
import Datastructures.Clauses.Clause;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;


/** explains the transition from the basicClause data structure to the internal Clause data structure.
 */
public class ClauseCopy extends InferenceStep {
    private int[] basicClause;
    private Clause clause;
    public static String title = "Clause Copy";

    public static String rule = title + ":\n"+
            "Copies a basic clause like [id,type,literal1,...] to a Clause data structure";

    public ClauseCopy(int[] basicClause, Clause clause) {
        this.basicClause = basicClause;
        this.clause = clause;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        String st = title + ":\n" + BasicClauseList.clauseToString(0,basicClause,symboltable);
        int width = st.length();
        return st + "\n" + StringUtils.repeat('-',width) + "\n" +
                clause.toString(0,symboltable);}

    @Override
    public int[] input() {
        return basicClause;
    }

    @Override
    public Clause output() {
        return clause;
    }


    @Override
    public IntArrayList origins() {
        return IntArrayList.wrap(new int[]{basicClause[0]});}

    /** adds this to steps
     *
     * @param steps a list for collecting the inference steps.
     */
    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        if(!steps.contains(this)) steps.add(this);}
}
