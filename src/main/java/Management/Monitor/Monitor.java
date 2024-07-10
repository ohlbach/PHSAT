package Management.Monitor;

import java.nio.file.Path;

/** This is an abstract class for implementing monitors.
 * <br>
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

    /** if true then a separate monitor is opened for each problem */
    protected boolean separate;

    /** just sets the start time.
     *
     * @param title     a title for the monitor.
     * @param separate if true then a separate monitor is opened for each problem.
     * @param startTime the start time of the job.
     */
    public Monitor(String title, boolean separate, long startTime) {
        this.title = title;
        this.separate = separate;
        this.startTime = startTime;}

    /** gets a monitor, dependent of the globalParameters.monitor value.
     * A 'life' monitor prints to System.out.<br>
     * A 'file' monitor prints to a file 'directory'/monitor.txt.<br>.
     * A 'frame' monitors prints to a newly generated frame.
     *
     * @param title  the identifier of the monitor.
     * @param type   none,life,file or frame
     * @param directory for file monitors
     * @param size  x- and y-size of the fram
     * @param offset for placing a frame
     * @param startTime in Nanoseconds
     * @return a monitor.
     */
    public static Monitor getMonitor(String title, String type, boolean separate, Path directory, int size, int offset, long startTime) {
        Monitor monitor = null;
        switch (type) {
            case "none":    return null;
            case "life":    monitor = new MonitorLife(title, startTime); break;
            case "file":    monitor = new MonitorFile(title, separate,startTime, directory); break;
            case "frame":   monitor = new MonitorFrame("Monitor for "+title, separate,startTime, size,size/2,
                    offset,offset);}
       return monitor;}

    /** initializes the monitor for a new problem.
     *
     * @param problemId
     */
    public abstract void initialize(String problemId);

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
    public abstract void println(String id, String... messages);

    /** prints just the message.
     *
     * @param message the messages themselves.
     */
    public abstract void println(String message);

    /** closes files.
     *
     * @param close if true then files are closed.
     */
    public abstract void flush(boolean close);
}
