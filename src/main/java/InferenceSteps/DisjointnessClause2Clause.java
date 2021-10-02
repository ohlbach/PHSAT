package InferenceSteps;

import Datastructures.Clauses.BasicClauseList;
import Datastructures.Clauses.Clause;
import Datastructures.Symboltable;
import org.apache.commons.lang3.StringUtils;

/** explains the transformation of a disjointness clause to a number of two-literal clauses
 *  The input can be a basicClause from the generators, or a derived Clause.
 */
public class DisjointnessClause2Clause  extends InferenceStep {
    private int[] basicClause;
    private Clause disjointnessClause = null;
    private Clause clause = null;


    public static String rule =
            "Disjointness Clauses are turned into corresponding two-literal clauses:\n"+
                    "p != q != ... != r != s\n"+
                    "-----------------------\n"+
                            "       -p,-q\n" +
                            "        ...\n" +
                            "       -r,-s";

    /** A basic disjointness clause is turned into two-literal clauses
     *
     * @param basicClause a basic disjointness clause
     * @param clause a two-literal clause
     */
    public DisjointnessClause2Clause(int[] basicClause, Clause clause) {
        this.basicClause = basicClause;
        this.clause = clause;}

    /** A disjointness clause is turned into two-literal clauses
     *
     * @param disjointnessClause a  disjointness clause
     * @param clause a two-literal clause
     */
    public DisjointnessClause2Clause(Clause disjointnessClause, Clause clause) {
        this.disjointnessClause = disjointnessClause;
        this.clause = clause;}


    @Override
    public String rule() {return rule;}

    /** explains the actual inference step
     *
     * @param symboltable null or a symboltable
     * @return the inference step as string.
     */
    @Override
    public String toString(Symboltable symboltable) {
        String st = basicClause != null ? BasicClauseList.clauseToString(0,basicClause,symboltable) :
                disjointnessClause.toString(0,symboltable);
        int size = st.length();
        return st + "\n" + StringUtils.repeat('-',size) + "\n" +
                StringUtils.center(clause.toString(0,symboltable),size);}

    /** returns the input (basic clause or disjointnessClause)
     *
     * @return the input to the inference step
     */
    @Override
    public Object input() {
        return basicClause != null ? basicClause : disjointnessClause;}

    /** returns the resulting two-literal clause
     *
     * @return the resulting two-literal clause
     */
    @Override
    public Clause output() {
        return clause;}

}
