package Coordinator;

import Datastructures.Results.Result;
import Datastructures.Statistics.CentralProcessorStatistics;

/**
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
     *
     * @return the result (unsatisfiable or satisfiable)
     */
    public Result processTasks() {
        Task task = null;
        while((task = getTask()) != null) {
            if(task.ignore) {continue;}
            Result result = task.execute();
            if(result != null) {return result;}}
        return null;}

    private synchronized Task getTask() {
        while(taskQueue.isEmpty()) {
            try {wait();} catch (InterruptedException e) {continue;}}
        return taskQueue.poll();}

}
