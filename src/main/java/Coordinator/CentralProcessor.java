package Coordinator;

import Coordinator.Tasks.Task;
import Datastructures.Results.Result;
import Datastructures.Statistics.CentralProcessorStatistics;
import Solvers.Solver;

/** The CentralProcessor coordinates the work of several solvers.
 *  It waits for tasks to be executed, and executes them.
 *  All data structures are taken over from the preprocessor.
 *  All solvers may observe changes in the model, the implicationDAG and the clauses.
 * <p>
 * Created by ohlbach on 10.10.2018.
 */
public class CentralProcessor extends Processor {
    public Solver[] solvers;

    public CentralProcessor(PreProcessor preProcessor) {
        super("CP",preProcessor.supervisor,preProcessor.applicationParameters,preProcessor.basicClauseList);
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
        long start = System.currentTimeMillis();
        try{
            while((task = getTask()) != null) {
                if(task.ignore) {continue;}
                ++((CentralProcessorStatistics)statistics).CP_Tasks;
                Result result = task.execute();
                if(result != null) {return result;}}}
        finally{statistics.elapsedTime = System.currentTimeMillis()-start;}
        return null;}

    /** waits for a new task to be processed.
     *
     * @return a new task to be processed
     */
    private synchronized Task getTask() {
        while(taskQueue.isEmpty()) {
            try {wait();} catch (InterruptedException e) {return null;}}
        return taskQueue.poll();}



    public synchronized void signalUnaryClause(int literal, Solver solver) {
        if(model.isTrue(literal)) {return;}
        if(model.isFalse(literal)) {signalContradiction(literal,solver);}
        model.add(literal);
        taskQueue.add(new Task.TrueLiteral(literal,this));}

    public synchronized void signalBinaryClause(int literal1, int literal2, Solver solver) {
        if(model.isTrue(literal1) || model.isTrue(literal2)) {return;}
        if(model.isFalse(literal1) && model.isFalse(literal2)) {signalContradiction(literal1,literal2,solver);}
        if(model.isFalse(literal1)) {
            model.add(literal2);
            taskQueue.add(new Task.TrueLiteral(literal2,this));
            return;}
        if(model.isFalse(literal2)) {
            model.add(literal1);
            taskQueue.add(new Task.TrueLiteral(literal1,this));
            return;}
        taskQueue.add(new Task.BinaryClause(literal1,literal2,this));}

    public Result processOneLiteralClause(int literal) {

    }

    public synchronized void signalContradiction(int literal, Solver solver) {

    }

    public synchronized void signalContradiction(int literal1, int literal2, Solver solver) {

    }

}
