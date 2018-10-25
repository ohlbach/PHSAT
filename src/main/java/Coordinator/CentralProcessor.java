package Coordinator;

import Datastructures.Results.Result;
import Datastructures.Statistics.CentralProcessorStatistics;

/** The CentralProcessor coordinates the work of several solvers.
 *  It waits for tasks to be executed, and executes them.
 *  All data structures are taken over from the preprocessor.
 *  All solvers may observe changes in the model, the implicationDAG and the clauses.
 * <p>
 * Created by ohlbach on 10.10.2018.
 */
public class CentralProcessor extends Processor {

    public CentralProcessor(PreProcessor preProcessor) {
        super(preProcessor.supervisor,preProcessor.globalParameters,preProcessor.applicationParameters,preProcessor.basicClauseList);
        clauses        = preProcessor.clauses;
        model          = preProcessor.model;
        implicationDAG = preProcessor.implicationDAG;
        equivalences   = preProcessor.equivalences;
        disjointnesses = preProcessor.disjointnesses;
        statistics     = new CentralProcessorStatistics(this);
    }

    /** This method waits for new tasks and executes them until the execution returns a result (unsatisfiable or satisfiable)
     * Tasks to be ignored are in fact ignored.
     *
     * @return the result (unsatisfiable or satisfiable)
     */
    public Result processTasks() {
        Task task = null;
        while((task = getTask()) != null) {
            if(task.ignore) {continue;}
            ++((CentralProcessorStatistics)statistics).CP_Tasks;
            Result result = task.execute();
            if(result != null) {return result;}}
        return null;}

    /** waits for a new task to be processed.
     *
     * @return a new task to be processed
     */
    private synchronized Task getTask() {
        while(taskQueue.isEmpty()) {
            try {wait();} catch (InterruptedException e) {continue;}}
        return taskQueue.poll();}

}
