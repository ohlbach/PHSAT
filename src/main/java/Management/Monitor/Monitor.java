package Management.Monitor;

/** This is an abstract class for implementing monitors.
 *
 * A monitor can be used to print messages, error messages, warnings or messages about the current state of the program.
 * The messages can be printed as they come, or they can be collected and printed later on.
 * A monitor can tag messages with an identifier.
 * The identifiers can help classifying the messages and printing the classes separately.
 */
public abstract class Monitor {

    /**  A title like "Messages", "Errors", "Warnings"*/
    public String title = "Monitor";

    /** becomes true after the first call to print pr println */
    public boolean filled = false;

    /** prints/collects the messages
     *
     * @param id       an identifier for the message
     * @param messages the messages themselves
     */
    public abstract void print(String id, String... messages);

    /** prints/collects the messages. A newline is appended at each message string.
     *
     * @param id       an identifier for the message
     * @param messages the messages themselves
     */
    public  abstract void println(String id, String... messages);

    /** prints/collects the messages from a StringBuilder.
     *
     * @param id       an identifier for the message
     * @param messages the messages themselves
     */
    public abstract void print(String id, StringBuilder messages);

    /** indicates whether messages have been printed/collected
     *
     * @return true if messages have been printed/collected
     */
    public boolean wasFilled() {
        return filled;}

    /** prints previously collected messages and clears buffers.
     *
     * @param close if true then files are closed.
     */
    public abstract void flush(boolean close);
}
