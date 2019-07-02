package Coordinator.Tasks;

import Datastructures.Results.Result;
import Datastructures.Results.Satisfiable;
import Management.Monitor;

/** The task contains an Satisfiable object.
 * Because of listPosition 0 it is moved to the front of the task queue
 */
public class Satisfiability  extends Task {

    private Satisfiable satisfiable;

    /** creates a task signalling a satisfiability.
     * It gets listPosition 0 and is therefore moved to the front of the listPosition queue.
     *
     * @param satisfiable a Satisfiability object
     */
    public Satisfiability(Satisfiable satisfiable, Monitor monitor, String sourceId) {
        super(0,monitor,sourceId);
        this.satisfiable = satisfiable;}

    /**
     * @return the Satisfiable object
     */
    public Result execute(){super.execute();  return satisfiable;}

    public String toString() {
        return "Task: Satisfiable " + satisfiable.toString();}
}
