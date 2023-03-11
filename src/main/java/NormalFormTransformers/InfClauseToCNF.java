package NormalFormTransformers;

import Datastructures.Clauses.Quantifier;
import Datastructures.Clauses.InputClauses;
import Datastructures.Symboltable;
import InferenceSteps.InfInputClause;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

/** This class documents the transformation of clauses to CNF.
 */
public class InfClauseToCNF extends InferenceStep {

    public static final String titleAtleast = "Atleast-clause to Conjunctive Normal Form (CNF)";
    public static final String titleAtmost  = "Atmost-clause to Conjunctive Normal Form (CNF)";
    public static final String titleExactly = "Exactly-clause to Conjunctive Normal Form (CNF)";
    public static final String titleInterval = "Interval-clause to Conjunctive Normal Form (CNF)";

    public String title,rule;

    public static final String ruleAtleast = titleAtleast+":\n"+
            "atleast m p_1,...,p_n ->" +
            "(n over (n-m+1)) clauses with all combinations of n-m+1 literals.\n"+
            "Example:\n"+
            "atleast 2: 1,2,3,4,5,6 ->\n"+
            "  0: 1v2v3v4v5\n" +
            "  1: 1v2v3v4v6\n" +
            "  2: 1v2v3v5v6\n" +
            "  3: 1v2v4v5v6\n" +
            "  4: 1v3v4v5v6\n" +
            "  5: 2v3v4v5v6";

    public static final String ruleAtmost = titleAtmost+":\n"+
            "atmost m q_1,...,q_n ->\n" +
            "(n over m+1) clauses with m+1 combinations of negated literals.\n"+
            "Example:\n"+
            "atmost 2 1,2,3,4,5 -> -p,-q,-r & -p,-q,-s & -p,-r,-s & -q,-r,-s "+
            "  0: -1v-2v-3\n" +
            "  1: -1v-2v-4\n" +
            "  2: -1v-2v-5\n" +
            "  3: -1v-3v-4\n" +
            "  4: -1v-3v-5\n" +
            "  5: -1v-4v-5\n" +
            "  6: -2v-3v-4\n" +
            "  7: -2v-3v-5\n" +
            "  8: -2v-4v-5\n" +
            "  9: -3v-4v-5";
    public static final String ruleExactly = titleExactly+ ":\n"+
            "exactly m p_1,...,p_n ->\n"+
            "(n over n-m+1) clauses with combinations of n-m+1 positive literals +\n"+
            "(n over m+1) clauses with combinations of m+1 negated literals.\n"+
            "Example: exactly 2 1, 2, 3 \n"+
            "  0: 1v2\n" +
            "  1: 1v3\n" +
            "  2: 2v3\n" +
            "  3: -1v-2v-3";

    public static final String ruleInterval = titleInterval+ ":\n"+
            "interval min,max p_1,...,p_n ->\n"+
            "(n over n-min+1) clauses with combinations of n-min+1 positive literals +\n"+
            "(n over max+1) clauses with combinations of max+1 negated literals.\n"+
            "Example: [2,3] 1, 2, 3, 4, 5\n"+
            " 0: 1v2v3v4\n" +
            "  1: 1v2v3v5\n" +
            "  2: 1v2v4v5\n" +
            "  3: 1v3v4v5\n" +
            "  4: 2v3v4v5\n" +
            "  5: -1v-2v-3v-4\n" +
            "  6: -1v-2v-3v-5\n" +
            "  7: -1v-2v-4v-5\n" +
            "  8: -1v-3v-4v-5\n" +
            "  9: -2v-3v-4v-5";

    private final int[] clause;
    private final int[] disjunction;

    public InfClauseToCNF(int[] clause, int[] disjunction) {
        this.clause = clause;
        this.disjunction = disjunction;
        Quantifier quantifier = Quantifier.getQuantifier(clause[1]);
        assert(quantifier != null);
        switch(quantifier) {
            case ATLEAST:
                rule = ruleAtleast;
                title = titleAtleast;
                break;
            case ATMOST:
                rule = ruleAtmost;
                title = titleAtmost;
                break;
            case EXACTLY:
                rule = ruleExactly;
                title = titleExactly;
                break;
            case INTERVAL:
                rule = ruleInterval;
                title = titleInterval;
        }
    }

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        return title+"\n"+ InputClauses.toString(0,clause,symboltable) + " -> " +
                InputClauses.toString(0,disjunction,symboltable);}

    @Override
    public IntArrayList inputClauseIds() {
        return IntArrayList.wrap(new int[]{clause[0]});}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        if(steps != null) {
            steps.add(new InfInputClause(clause[0]));
            steps.add(this);}}
}
