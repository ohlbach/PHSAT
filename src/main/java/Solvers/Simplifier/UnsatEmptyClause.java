package Solvers.Simplifier;

import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;

/** represents an Unsatisfiability caused by the derivation of the empty clause. */
public class UnsatEmptyClause extends Unsatisfiable {

    /** the empty clause. */
    private final Clause emptyClause;

    /**  constructs the empty clause Unsatisfiability.
     *
     * @param emptyClause the empty clause.
     */
    public UnsatEmptyClause(Clause emptyClause) {
        super();
        this.emptyClause = emptyClause;
        InferenceStep step = emptyClause.inferenceStep;
        if(step != null) inferenceSteps.add(emptyClause.inferenceStep);}

    /** a short description of the empty clause Unsatisfiability.
     *
     * @param symboltable  null or a symboltable.
     * @return the description of the empty clause Unsatisfiability.
     */
    @Override
    public String description(Symboltable symboltable) {
        return "Empty clause " + emptyClause.id + " derived";}

}
