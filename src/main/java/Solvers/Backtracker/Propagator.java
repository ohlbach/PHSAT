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
    protected final int identifier;

    /** indicates the status of the propagator */
    protected boolean isActive = false;

    /** stores a job and waits until there is one. */
    final BlockingQueue<Object> queue = new LinkedBlockingQueue<>();

    /** the pool of propagator threads. */
    private final PropagatorPool propagatorPool;

    /** the backtracker which submitted the job. */
    Backtracker backtracker;

    /**  the literal submitted by the job. */
    private int literal = 0;

    /** the index in the propagatorPool */
    int poolIndex;

    protected Thread myThread = null;

    /** constructs a new propagator.
     *
     * @param propagatorPool the corresponding pool or propagators.
     * @param identifier counts the propagators.
     * */
    public Propagator(PropagatorPool propagatorPool, int identifier) {
        this.propagatorPool = propagatorPool;
        this.identifier = identifier;
        myThread = Thread.currentThread();}

    /** waits for a propagator job, executes it and then waits again.*/
    @Override
    public void run() {
        try {
            while (true) {
                backtracker = (Backtracker)queue.take();
                literal = (Integer)queue.take();
                isActive = true;
                if(backtracker.propagateInThread(literal)) {// false clause found
                    propagatorPool.deactivate(this);
                    propagatorPool.jobFinished(backtracker);}
                else propagatorPool.deactivate(this);
            }}
        catch (InterruptedException e) {
            if(isActive) propagatorPool.deactivate(this);}}

    /** Adds a new propagateInThread job to the queue.
     *
     * @param backtracker the backtracker which submitted the job.
     * @param literal the literal to propagateInThread.
     */
    public void newPropagateJob(Backtracker backtracker, int literal) {
         this.backtracker = backtracker;
         this.literal = literal;
         queue.add(backtracker); queue.add(literal);}

    /** a short description of the propagator
     *
     * @return  a short description of the propagator
     */
    public String toString() {
        String status = isActive ? "Active" : "Passive";
        return status + " propagator " + identifier + " for backtracker " + backtracker.solverId+
                ", literal " + literal + " PoolIndex: " + poolIndex + " Thread: " +
                ((myThread == null) ? "null" : myThread.getName());}
}
