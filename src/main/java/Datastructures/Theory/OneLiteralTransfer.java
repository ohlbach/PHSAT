package Datastructures.Theory;

import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.function.BiConsumer;

/** The instances of this class are used to transfer derived unit clauses between parallel modules.
 * A transfer is actually performed by a BiConsumer which accepts a literal and its origins
 * (basic clause ids of the clauses used to derive the literal).
 * The transferer itself carries the thread that generated the unit literal.
 *
 */
public class OneLiteralTransfer {
    /** the threadId of the thread that generates the unit literals */
    public Thread thread = null;
    /** the function that transfers the unit literals */
    public BiConsumer<Integer, IntArrayList> transferer = null;

    /** generates a transferer. The thread may be changed after the thread's start.
     *
     * @param thread the thread which generates the unit-literal
     * @param transferer a consumer (literal,origins)
     */
    public OneLiteralTransfer(Thread thread,BiConsumer<Integer, IntArrayList> transferer) {
        this.thread = thread;
        this.transferer = transferer;
    }

}
