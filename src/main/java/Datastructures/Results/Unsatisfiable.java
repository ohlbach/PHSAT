package Datastructures.Results;

/** This class represents the final reason for an unsatisfiability in the clauses.
 * It must be subclassed for representing more specific reasons for the unsatisfiability.
 */
public abstract class Unsatisfiable extends Result {

    /** calls the super-constructor.
     *
     * @param problemId the problem where the unsatisfiability was discovered.
     * @param solverId  the solver which discovered the unsatisfiability.
     * @param startTime the time when the reasoning started.
     */
    public Unsatisfiable(String problemId, String solverId, long startTime) {
        super(problemId,solverId,"unsatisfiable",startTime);}

}
