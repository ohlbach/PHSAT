package Solvers.Backtracker;

import java.util.ArrayList;

/**The PropagatorPool class represents a pool of propagators.
 * <p>
 * Propagators are responsible for propagating true literals in the backtracker.<br>
 * Each Propagator waits for a true literal and then calls propagateInThread-method of the backtracker.<br>
 * After the propagateInThread-method is finished, it becomes passive and waits for a new job. <br>
 * Propagators can work for different backtrackers.
 */
public class PropagatorPool {

    /** identifies a thread */
    private int identifier = 0;

    /** The list of propagators. Active propagators come first, and then the passive propagators.*/
    ArrayList<Propagator> propagators = new ArrayList<>();

    /** The index of the first passive propagator */
    int firstPassive = 0;

    /** constructs a new PropagatorPool */
    public PropagatorPool() {}

    /** Adds a new propagator job.
     * <p>
     * If there is a passive propagator,this one gets the job, otherwise a new Propagator is created.
     *
     * @param backtracker  the Backtracker which submitted the job.
     * @param literal      the true literal.
     */
    public void addPropagatorJob(Backtracker backtracker, int literal) {
        update(1,backtracker,null,literal);}

    /** deactivates the propagator.
     * <p>
     * The propagator becomes the first passive propagator.<br>
     * It may exchange its position in the propagators list.
     *
     * @param propagator an active propagator which becomes passive.
     */
    public void deactivate(Propagator propagator) {
        update(2,null,propagator,0);}

    /** The backtracker has finished (found a model, a contradiction or has been aborted).
     * <p>
     * All active propagators for this backtracker are deactivated.
     *
     * @param backtracker which finished the job.
     */
    public void jobFinished(Backtracker backtracker) {
        update(3,backtracker,null,0);}

    private synchronized void update(int action, Backtracker backtracker, Propagator propagator, int literal) {
        assert(firstPassive >= 0);
        switch (action){
            case 1: // addPropagatorJob
                if(firstPassive == propagators.size()) { // no passive propagator available.
                    propagator = new Propagator(this, ++identifier);
                    propagator.poolIndex = firstPassive++;
                    propagators.add(propagator);
                    propagator.newPropagateJob(backtracker,literal);
                    propagator.start();}
                else {
                    propagator = propagators.get(firstPassive++); // it becomes active.
                    assert(!propagator.isActive);
                    propagator.newPropagateJob(backtracker,literal);}
                break;
            case 2: //deactivate
                if(propagator.isActive) return;  // other threads found a false clause
                propagator.isActive = false;
                int lastActive = firstPassive-1;
                if(propagator.poolIndex < lastActive) {
                    Propagator lastActivePropagator = propagators.get(lastActive);
                    propagators.set(propagator.poolIndex, lastActivePropagator);
                    lastActivePropagator.poolIndex = propagator.poolIndex;
                    propagators.set(lastActive,propagator); // becomes the first passive propagator
                    propagator.poolIndex = lastActive;}
                --firstPassive; // active propagator was the first active propagator
                break;
            case 3: // jobFinished deactivate all active propagates for the backtracker
                for(int index = firstPassive-1; index >= 0; --index) {
                    propagator = propagators.get(index);
                    propagator.myThread.interrupt();
                    if(propagator.backtracker == backtracker) deactivate(propagator);}}
        }

    /** lists all the active and passive propagators.
     *
     * @return a short description of all the active and passive propagators.
     */
    public String toString() {
        assert(firstPassive >= 0);
        StringBuilder st = new StringBuilder();
        st.append("Propagator Pool: firstPassive ").append(firstPassive).append(", Size: ").append(propagators.size()).append("\n");
        st.append("Thread: ").append(Thread.currentThread().getName()).append("\n");
        if(firstPassive > 0) st.append("Active Propagators:\n");
        for(int i = 0; i < firstPassive; ++i)
            st.append(propagators.get(i).toString()).append("\n");
        if(firstPassive < propagators.size()) st.append("Passive Propagators:\n");
        for(int i = firstPassive; i < propagators.size(); ++i)
            st.append(propagators.get(i).toString()).append("\n");
        return st.toString();}
}
