package Management.Monitor;

/** This is an abstract class for implementing monitors.
 * A monitor can be used to print messages about the current state of the program.<br>
 * The messages are tagged with the elapsed time, counting from the start time.<br>
 * There are three types of monitors:<br>
 * - a monitor which prints to System.out<br>
 * - a monitor which prints to a file<br>
 * - a monitor which prints to a JFrame.
 */
public abstract class Monitor {

    /**  A title for the monitor.*/
    public String title = "Monitor";

    /** the start time of the job */
    protected long startTime;

    /** just sets the start time.
     *
     * @param title     a title for the monitor.
     * @param startTime the start time of the job.
     */
    public Monitor(String title, long startTime) {
        this.title = title;
        this.startTime = startTime;}

    /** prints the messages.
     *
     * @param id       an identifier for the message.
     * @param messages the messages themselves.
     */
    public abstract void print(String id, String... messages);

    /** prints the messages. A newline is appended at each message string.
     *
     * @param id       an identifier for the message.
     * @param messages the messages themselves.
     */
    public  abstract void println(String id, String... messages);

    /** prints the messages from a StringBuilder.
     *
     * @param id       an identifier for the message.
     * @param messages the messages themselves.
     */
    public abstract void print(String id, StringBuilder messages);

    /** closes files.
     *
     * @param close if true then files are closed.
     */
    public abstract void flush(boolean close);
}
