package Datastructures.Results;

import Datastructures.Clause;
import Datastructures.Clauses.InputClauses;
import Datastructures.Symboltable;

/** This class describes an Unsatisfiablitiy caused by a non-empty clause.
 * Example: atleast 3 p,q
 */
public class UnsatClause extends Unsatisfiable {


    /** an unsatisfiable input clause */
    private int[] inputClause = null;

    private Clause clause = null;

    private Solvers.Normalizer.Clause normlizedClause = null;

    /** constructs an Unsatisfiability from an unsatisfiable input clause
     *
     * @param inputClause an unsatisfiable clause
     */
    public UnsatClause(String problemId, String solverId, int[] inputClause) {
        super(problemId, solverId);
        this.inputClause = inputClause;
       // inferenceSteps.add(new InfInputClause(inputClause[0]));
    }

    public UnsatClause(String problemId, String solverId, Clause clause) {
        super(problemId,solverId);
        this.clause = clause;}

    public UnsatClause(String problemId, String solverId, Solvers.Normalizer.Clause clause) {
        super(problemId,solverId);
        normlizedClause = clause;
    }


    @Override
    public String description(Symboltable symboltable) {
        if(inputClause != null)
            return "Unsatisfiable clause " +InputClauses.toString(0,inputClause,symboltable);
        if(clause != null)          return "Unsatisfiable clause " + clause.toString(symboltable,0);
        if(normlizedClause != null) return "Unsatisfiable clause " + normlizedClause.toString(symboltable,0);
        return "Unsatisfiable clause ";}

}
