package Solvers.Backtracker;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

/** This is a thread which can propagate true literals found in a backtracker.
 * <p>
 * The propagator waits until a new propagator job is submitted, calls the propagate method,
 * and then waits for the next job. <br>
 * It can work for several Backtrackers.
 */
public class Propagator extends Thread {

    /** stores a job and waits until there is one. */
    final BlockingQueue<Object> queue = new LinkedBlockingQueue<>(3);

    /** the pool of propagator threads. */
    private final PropagatorPool propagatorPool;

    /** the backtracker which subitted the job. */
    Backtracker backtracker;

    /** the index in the propagatorPool */
    int poolIndex;

    /** constructs a new propagator.
     * @param propagatorPool the corresponding pool or propagators.*/
    public Propagator(PropagatorPool propagatorPool) {
        this.propagatorPool = propagatorPool;}

    /** waits for a propagator job, executes it and then waits again.*/
    @Override
    public void run() {
        try {
            while (!isInterrupted()) {
                backtracker = (Backtracker)queue.take();
                int literal = (Integer)queue.take();
                backtracker.propagate(literal);
                propagatorPool.deactivate(this);}
        } catch (InterruptedException e) {
        }}

    /** Adds a new propagate job to the queue.
     *
     * @param backtracker the backtracker which submitted the job.
     * @param literal the literal to propagate.
     */
    public void newPropagateJob(Backtracker backtracker, int literal) {
        queue.add(backtracker); queue.add(literal);}
}
