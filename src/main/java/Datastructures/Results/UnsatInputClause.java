package Datastructures.Results;

import Datastructures.Clauses.InputClauses;
import Datastructures.Symboltable;
import InferenceSteps.InfInputClause;

/** This signals an Unsatifiability which comes from a contradictory input clause,
 * for example an equivalence p == -p.
  */
public class UnsatInputClause extends Unsatisfiable {
    /** This is the contradictory input clause */
    private final int[] inputClause;

    /** constructs an Unsatisfiability from an unsatisfiable clause
     *
     * @param inputClause a contradictory InputClause
     */
    public UnsatInputClause(String problemId, String solverId, long startTime,int[] inputClause) {
        super(problemId,solverId,startTime);
        this.inputClause = inputClause;
        inferenceSteps.add(new InfInputClause(inputClause[0]));}

    /** returns a description of the contradiction.
     *
     * @param symboltable null or a symboltable
     * @return a description of the contradiction.
     */
    @Override
    public String description(Symboltable symboltable) {
        return "Contradictory clause " + InputClauses.toString(0, inputClause,symboltable);}


}
