package Management.Monitor;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintStream;
import java.util.Date;

/** This monitor prints messages to files.
 */
public class MonitorFile extends Monitor {
    /** null or the file where to print the messages. */
     private File file;

    /** the output stream of the file. */
    private PrintStream out = System.out;

    /** creates a monitor which does nothing at all.*/
    public MonitorFile() {
        super("Monitor",System.nanoTime());
    }

    /** creates a monitor for printing messages to a file.
     * If the filename is not given then the messages are printed to System.out.<br>
     * If the file cannot be opened, an error message is printed to System.out.
     *
     * @param title for identifying the monitor.
     * @param file where to print the messages, or null if the messages are to be printed to System.out.
     */
    public MonitorFile(String title, long startTime, File file) {
        super(title,startTime);
        this.file = file;
        if(file != null) {
            try{out = new PrintStream(file);
            out.println("Monitor for " + title + ": " + (new Date()));}
            catch(FileNotFoundException ex) {
                System.out.println("MonitorFile: File not found " + file.getAbsolutePath());}}}

    /** prints the id and the elapsed time followed by the messages to a single line.
     *
     * @param id      for identifying and separating the messages.
     * @param messages to be printed.
     */
    @Override
    public void print(String id, String... messages) {
        double time = (double)(System.nanoTime() - startTime)/1000.0;
        out.print(id); out.print(" @ "); out.print(time); out.print(" μs: ");
        for(String message : messages)  out.printf(message);
        out.print("\n");}

    /** prints the id and the elapsed time followed by the messages one per line.
     *
     * @param id       an identifier for the message
     * @param messages the messages themselves
     */
    public void print(String id, StringBuilder messages) {
        double time = (double)(System.nanoTime() - startTime)/1000.0;
        out.print(id); out.print(" @ "); out.print(time); out.print(" μs: ");
        out.println(messages.toString());}

    /** prints the id and the elapsed time followed by the messages one per line.
     *
     * @param id      for identifying and separating the messages
     * @param messages to be printed.
     */
    @Override
    public void println(String id, String... messages) {
        double time = (double)(System.nanoTime() - startTime)/1000.0;
        out.print(id); out.print(" @ "); out.print(time); out.println(" μs: ");
        for (String message : messages) out.println(message);
    }

    /** prints just the message.
     *
     * @param message the messages themselves.
     */
    public void println(String message) {
        out.println(message);};


    /** The file is closed.
     *
     * @param close if true then the file is  closed.
     */
    @Override
    public synchronized void flush(boolean close) {
        if(close && out != System.out) out.close();}


    /** returns some information about the monitor
     *
     * @return some information about the monitor
     */
    public String toString() {
        return "MonitorFile: printing to " +
                ((file == null) ? "System.out" : file.getAbsolutePath());}

}
