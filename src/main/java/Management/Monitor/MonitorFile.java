package Management.Monitor;

import Utilities.Utilities;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintStream;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Date;

/** This monitor prints messages to files.
 * <br>
 * Either it prints the messages to a single file which is opened by the constructor,
 * or it prints the messages to separate files, which are opened in the initialize-method.<br>
 * In both cases the file is opened in the directory specified in the constructor.<br>
 * In the first case the constructor opens a file directory.&lt;title&gt;.<br>
 * In the second case the initialize-method opens a file directory.&lt;name&gt;.txt.
 * <br>
 * If opening the file causes an error, an error message is printed to System.err
 * and the monitoring is redirected to System.out.
 */
public class MonitorFile extends Monitor {

    /** the directory where to print the messages. */
    private final Path directory;

    /** the file where to print the messages. */
    private File file = null;

    /** the output stream of the file. */
    private PrintStream out = System.out;

    /** creates a monitor for printing messages to a file.
     * <br>
     * If separate = false then a file is immediatly opened in the constructor,
     * otherwise a new file is opened in the initialize-method.<br>
     * If the file cannot be opened, an error message is printed to System.err and the
     * output is redirected to System.out.
     *
     * @param title for identifying the monitor.
     * @param separate if true then a separate file is opened in the initialize-method.
     * @param startTime all print commands are tagged with the difference of the actual time and the startTime.
     * @param directory where to print the messages.
     */
    public MonitorFile(String title, boolean separate, long startTime, Path directory) {
        super(title,separate,startTime);
        this.directory = directory;
        if(!separate) {
            file = Paths.get(directory.toString() ,title+".txt").toFile();
            try{out = new PrintStream(file);
                out.println("Monitor for " + title + ": " + (new Date()));}
            catch(FileNotFoundException ex) {
                System.err.println("MonitorFile: Cannot open file " + file.getAbsolutePath());
                System.err.println("Redirecting monitor to System.out");
                file = null;
                out = System.out;}}}

    /** initializes the monitor for each new problem.
     * <br>
     * If separate = true then a new file is opened, otherwise the method does nothing.<br>
     * If the file cannot be opened, an error message is printed to System.err and the
     * output is redirected to System.out.
     *
     * @param name used to generate a new filename.
     */
    @Override
    public void initialize(String name) {
        if(separate) {
            if(file != null) out.close();
            file = Paths.get(directory.toString() ,name+".txt").toFile();
            try{out = new PrintStream(file);
                out.println("Monitor for " + title + "." + name + ": " + (new Date()));}
            catch(FileNotFoundException ex) {
                System.err.println("MonitorFile: Cannot open file " + file.getAbsolutePath());
                System.err.println("Redirecting monitor to System.out");
                file = null;
                out = System.out;}}}


    /** prints the id and the elapsed time followed by the messages to a single line.
     *
     * @param id      for identifying and separating the messages.
     * @param messages to be printed.
     */
    @Override
    public void print(String id, String... messages) {
        String time = Utilities.duration(System.nanoTime() - startTime);
        out.print(id +" @ " + time + ": ");
        for(String message : messages)  {out.print(message); out.print(" ");}
        out.print("\n");}

    /** prints the id and the elapsed time followed by the messages one per line.
     *
     * @param id      for identifying and separating the messages
     * @param messages to be printed.
     */
    @Override
    public void println(String id, String... messages) {
        String time = Utilities.duration(System.nanoTime() - startTime);
        out.println(id +" @ " + time);
        for (String message : messages) {out.println("  "); out.println(message);}}

    /** prints just the message.
     *
     * @param message the messages themselves.
     */
    public void println(String message) {
        out.println(message);}


    /** The file is closed.
     *
     * @param close if true then the file is  closed.
     */
    @Override
    public synchronized void flush(boolean close) {
        if(close && file != null) out.close();}


    /** returns some information about the monitor
     *
     * @return some information about the monitor
     */
    public String toString() {
        return (file == null) ? "MonitorFile " + title + ": printing to System.out" :
                                "MonitorFile " + title + ": printing to " + file.getAbsolutePath();}

}
