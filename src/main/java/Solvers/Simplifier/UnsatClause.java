package Solvers.Simplifier;

import Datastructures.Clauses.InputClauses;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import InferenceSteps.InfInputClause;
import InferenceSteps.InferenceStep;

/** This class describes an Unsatisfiablitiy caused by a non-empty clause.
 * Example: atleast 3 p,q
 */
public class UnsatClause extends Unsatisfiable {
    /** an unsatisfiable clause */
    private Clause clause = null;

    /** an unsatisfiable input clause */
    private int[] inputClause = null;

    /** constructs an Unsatisfiability from an unsatisfiable clause
     *
     * @param clause an unsatisfiable clause
     */
    public UnsatClause(String problemId, String solverId, Clause clause) {
        super(problemId, solverId);
        this.clause = clause;
        InferenceStep step = clause.inferenceStep;
        if(step != null) inferenceSteps.add(step);}

    /** constructs an Unsatisfiability from an unsatisfiable input clause
     *
     * @param inputClause an unsatisfiable clause
     */
    public UnsatClause(String problemId, String solverId,int[] inputClause) {
        super(problemId, solverId);
        this.inputClause = inputClause;
        inferenceSteps.add(new InfInputClause(inputClause[0]));}

    @Override
    public String description(Symboltable symboltable) {
        return "Unsatisfiable clause " +
                ((clause == null) ? InputClauses.toString(0,inputClause,symboltable) :
                        clause.toString(symboltable,0)+"\n");}


}
