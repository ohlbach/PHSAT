package Solvers.Backtracker;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

/** This is a thread which can propagateInThread true predicates found in a backtracker.
 * <p>
 * The propagator waits until a new propagator job is submitted, calls the propagateInThread method,
 * and then waits for the next job. <br>
 * It can work for several Backtrackers.
 */
public class Propagator extends Thread {

    /** identifies the propagator */
    private final int identifier;

    /** indicates the status of the propagator */
    private boolean isActive = false;

    /** stores a job and waits until there is one. */
    final BlockingQueue<Object> queue = new LinkedBlockingQueue<>(3);

    /** the pool of propagator threads. */
    private final PropagatorPool propagatorPool;

    /** the backtracker which submitted the job. */
    Backtracker backtracker;

    /**  the literal submitted by the job. */
    private int literal = 0;

    /** the index in the propagatorPool */
    int poolIndex;

    /** constructs a new propagator.
     *
     * @param propagatorPool the corresponding pool or propagators.
     * @param identifier counts the propagators.
     * */
    public Propagator(PropagatorPool propagatorPool, int identifier) {
        this.propagatorPool = propagatorPool;
        this.identifier = identifier;}

    /** waits for a propagator job, executes it and then waits again.*/
    @Override
    public void run() {
        try {
            while (!isInterrupted()) {
                backtracker = (Backtracker)queue.take();
                literal = (Integer)queue.take();
                isActive = true;
                backtracker.propagateInThread(literal);
                propagatorPool.deactivate(this);
                isActive = false;}}
        catch (InterruptedException e) {
            propagatorPool.deactivate(this);}}

    /** Adds a new propagateInThread job to the queue.
     *
     * @param backtracker the backtracker which submitted the job.
     * @param literal the literal to propagateInThread.
     */
    public void newPropagateJob(Backtracker backtracker, int literal) {
        queue.add(backtracker); queue.add(literal);}

    /** a short description of the propagator
     *
     * @return  a short description of the propagator
     */
    public String toString() {
        String status = isActive ? "Active" : "Passive";
        return status + " propagator " + identifier + " for backtracker " + backtracker.solverId+
                ", literal " + literal;}
}
