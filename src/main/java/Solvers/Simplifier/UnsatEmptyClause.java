package Solvers.Simplifier;

import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;

/** represents an Unsatisfiability caused by the derivation of the empty clause. */
public class UnsatEmptyClause extends Unsatisfiable {

    /** the empty clause. */
    private final int emptyClauseId;

    /**  constructs the empty clause Unsatisfiability.
     *
     * @param  emptyClauseId; the empty clause id.
     */
    public UnsatEmptyClause(String problemId, String solverId, int emptyClauseId, InferenceStep step) {
        super(problemId,solverId);
        this.emptyClauseId = emptyClauseId;
        if(step != null) inferenceSteps.add(step);}

    /** a short description of the empty clause Unsatisfiability.
     *
     * @param symboltable  null or a symboltable.
     * @return the description of the empty clause Unsatisfiability.
     */
    @Override
    public String description(Symboltable symboltable) {
        return "Empty clause " + emptyClauseId + " derived";}

}
