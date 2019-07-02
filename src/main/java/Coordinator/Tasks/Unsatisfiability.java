package Coordinator.Tasks;

import Datastructures.Results.Result;
import Datastructures.Results.Unsatisfiable;
import Management.Monitor;


/** The task contains an Unsatisfiable object.
 * Because of listPosition 0 it is moved to the front of the task queue
 */

public class Unsatisfiability extends Task {

    private Unsatisfiable unsatisfiable;

    /** creates a task signalling an unsatisfiability.
     * It gets listPosition 0 and is therefore moved to the front of the listPosition queue.
     *
     * @param unsatisfiable an Unsatisfiability object
     */
    public Unsatisfiability(Unsatisfiable unsatisfiable, Monitor monitor, String sourceId) {
        super(0,monitor,sourceId);
        this.unsatisfiable = unsatisfiable;}

    /**
     * @return the Unsatisfiable object
     */
    public Result execute(){super.execute();  return unsatisfiable;}

    public String toString() {
        return "Task: " + unsatisfiable.toString();}
}
