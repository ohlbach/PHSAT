package InferenceSteps;

import Datastructures.Clauses.BasicClauseList;
import Datastructures.Clauses.Clause;
import Datastructures.Symboltable;
import org.apache.commons.lang3.StringUtils;

/** explains the transition from the basicClause data structure to the internal Clause data structure.
 */
public class ClauseCopy extends InferenceStep {
    private int[] basicClause;
    private Clause clause;

    public static String rule =
            "Copies a basic clause like [id,type,literal1,...] to a Clause data structure";

    public ClauseCopy(int[] basicClause, Clause clause) {
        this.basicClause = basicClause;
        this.clause = clause;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        String st = BasicClauseList.clauseToString(0,basicClause,symboltable);
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
}
