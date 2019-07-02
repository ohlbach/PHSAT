package Coordinator.Tasks;

import Datastructures.Clauses.ClauseStructure;
import Datastructures.Results.Result;
import Management.Monitor;

import java.util.function.Function;

/** A clause set which has either only positive clauses and mixed clauses or only negative clauses and mixed clauses
 *  is satisfiable. The task causes a model to be generated.
 *
 */
public class Structure extends Task {
    /** POSITIVE or NEGATIVE */
    private ClauseStructure structure;
    private Function<ClauseStructure,Result> handler;


    /** constructs a task for processing positive or negative clause sets
     *
     * @param structure POSITIVE or NEGATIVE
     * @param handler which treats the structure.
     */
    public Structure(ClauseStructure structure, Monitor monitor, String sourceId, Function<ClauseStructure,Result> handler) {
        super(0,monitor,sourceId);
        this.structure = structure;
        this.handler = handler;}

    /** calls the processor to construct a model
     *
     * @return  Un/Satisfiable if this has been detected, otherwise null
     */
    public Result execute() {super.execute();  return handler.apply(structure);}

    public String toString() {
        return "Task: Structure: " + structure;}
}
