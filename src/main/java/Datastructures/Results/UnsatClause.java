package Datastructures.Results;

import Datastructures.Clauses.InputClauses;
import Datastructures.Symboltable;
import InferenceSteps.InfInputClause;

/** This class describes an Unsatisfiablitiy caused by a non-empty clause.
 * Example: atleast 3 p,q
 */
public class UnsatClause extends Unsatisfiable {


    /** an unsatisfiable input clause */
    private int[] inputClause = null;


    /** constructs an Unsatisfiability from an unsatisfiable input clause
     *
     * @param inputClause an unsatisfiable clause
     */
    public UnsatClause(String problemId, String solverId, int[] inputClause) {
        super(problemId, solverId);
        this.inputClause = inputClause;
        inferenceSteps.add(new InfInputClause(inputClause[0]));}

    public UnsatClause(String problemId, String solverId, Solvers.Normalizer.Clause clause) {
        super(problemId,solverId);
        inferenceSteps.addAll(clause.inferenceSteps);
    }

    @Override
    public String description(Symboltable symboltable) {
        return "Unsatisfiable clause " +InputClauses.toString(0,inputClause,symboltable);}


}
