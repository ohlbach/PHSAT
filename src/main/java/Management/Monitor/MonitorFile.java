package Management.Monitor;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;

/** This monitor prints messages to files.
 * The messages are separated by their identifiers and collected in buffers.
 * The 'flush' method prints them, either to a given file, or to System.out.
 */
public class MonitorFile extends Monitor {
    private final HashMap<String, ArrayList<String>> buffers = new HashMap<>(); // collects the separated messages
    private File file; // null or the file where to print the messages

    /** the output stream of the file */
    private PrintStream out = System.out;

    /** creates a monitor which does nothing at all*/
    public MonitorFile() {}

    /** creates a monitor for later printing messages to a file.
     * If the filename is not given or cannot be opened then the messages are printed to System.out.
     *
     * @param title like "Messages", "Errors", "Warnings"
     * @param file where to print the messages, or null if the messages are to be printed to System.out
     */
    public MonitorFile(String title, File file) {
        this.title = title;
        this.file = file;
        if(file != null) {
            try{out = new PrintStream(file);
            out.println("Monitor for " + title + ": " + (new Date()).toString());}
            catch(FileNotFoundException ex) {
                System.out.println("MonitorFile: File not found " + file.getAbsolutePath());}}}

    /** collects the messages for later printing them as a single line to a file
     *
     * @param id      for identifying and separating the messages
     * @param messages to be printed.
     */
    @Override
    public void print(String id, String... messages) {
        out.printf(id); out.printf(": ");
        for(String message : messages)  out.printf(message);
        out.printf("\n");}

    /** prints the messages from a StringBuilder.
     *
     * @param id       an identifier for the message
     * @param messages the messages themselves
     */
    public void print(String id, StringBuilder messages) {
        out.printf(id); out.printf(": ");
        out.println(messages.toString());}

    /** collects the messages for later printing them one per line to a file
     *
     * @param id      for identifying and separating the messages
     * @param messages to be printed.
     */
    @Override
    public void println(String id, String... messages) {
        out.printf(id); out.printf(": ");
        for(int i = 0; i < messages.length; ++i) out.println(messages[i]);}


    /** The file is closed
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
        StringBuilder st = new StringBuilder();
        st.append("MonitorFile: printing to ");
        st.append((file == null) ? "System.out" : file.getAbsolutePath());
        return st.toString();}

}
