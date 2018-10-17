package Coordinator;

import Datastructures.Results.Result;
import Datastructures.Results.Satisfiable;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Statistics.CentralProcessorStatistic;

/**
 * Created by ohlbach on 10.10.2018.
 */
public class CentralProcessor extends Processor {
    public CentralProcessorStatistic statistics;

    public CentralProcessor(PreProcessor preProcessor) {
        clauses        = preProcessor.clauses;
        model          = preProcessor.model;
        implicationDAG = preProcessor.implicationDAG;
        equivalences   = preProcessor.equivalences;
        disjointnesses = preProcessor.disjointnesses;
    }

    /** This method waits for new tasks and executes them until the execution returns a result (unsatisfiable or satisfiable)
     *
     * @return the result (unsatisfiable or satisfiable)
     */
    public Result processTasks() {
        Result result;
        while((result = getTask().execute()) != null) {}
        return result;}

    private synchronized Task getTask() {
        while(taskQueue.isEmpty()) {
            try {wait();} catch (InterruptedException e) {continue;}
            return taskQueue.poll();}
        return null;}

    public synchronized void newUnsatifiability(Unsatisfiable unsatisfiable) {
        addTask(new Task.Unsatisfiability(unsatisfiable,this)); notify();}

    public synchronized void newSatisfiability(Satisfiable satisfiable) {
        addTask(new Task.Satisfiability(satisfiable,this));  notify();}

    public synchronized void newUnitClause(int literal) {
        addTask(new Task.OneLiteral(literal,this)); notify();}

    public synchronized void newTwoLiteralClause(int literal1, int literal2) {
        addTask(new Task.TwoLiteral(literal1,literal2,this)); notify();}
}
