package Solvers.Resolution;

import Coordinator.Tasks.Task;
import Datastructures.Results.Result;

/**
 * Created by ohlbach on 25.06.2019.
 */
public class ResolutionTask extends Task {
    /**
     * constructs a task
     * @param resolution the processor which executes the task
     */
    public ResolutionTask(Resolution resolution) {
        super(0, resolution);}


    public Result execute() {
        super.execute();
        return ((Resolution)processor).resolve();}

    public String toString() {
        return "Task: Resolution";}
    }


