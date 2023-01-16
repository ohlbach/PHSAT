package Management.Monitor;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

/** This monitor prints messages to files.
 * The messages are separated by their identifiers and collected in buffers.
 * The 'flush' method prints them, either to a given file, or to System.out.
 */
public class MonitorFile extends Monitor {
    private final HashMap<String, ArrayList<String>> buffers = new HashMap<>(); // collects the separated messages
    private File file; // null or the file where to print the messages
    private String pathname = null; // the absolute pathname of the file.

    // If the monitor is flushed it can be reused and prints to a file whose name is extended with a number
    private int filenumber = 0;

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
        if(file != null) pathname = file.getAbsolutePath();
        monitoring = true;}

    /** collects the messages for later printing them as a single line to a file
     *
     * @param id      for identifying and separating the messages
     * @param messages to be printed.
     */
    @Override
    public void print(String id, String... messages) {
        filled = true;
        if(monitoring) {
            String string = "";
            for(String message : messages) string += message;
            buffers.computeIfAbsent(id, k -> new ArrayList<>()).add(string);}}

    /** fills the messages into the buffer
     *
     * @param id        an id for the message
     * @param messages messages
     */
    @Override
    public void print(String id, StringBuilder messages) {
        filled = true;
        if(monitoring) {
            buffers.computeIfAbsent(id, k -> new ArrayList<>()).add(messages.toString());}}


    /** collects the messages for later printing them one per line to a file
     *
     * @param id      for identifying and separating the messages
     * @param messages to be printed.
     */
    @Override
    public void println(String id, String... messages) {
        filled = true;
        if(monitoring) {
            String string = "";
            for(int i = 0; i < messages.length; ++i) {
                string += messages[i];
                if(i < messages.length-1) string += "\n";}
            buffers.computeIfAbsent(id, k -> new ArrayList<>()).add(string);}}


    /** The messages are now printed, either to System.out, or to the file
     * The buffers are cleared, and the monitor may be used again.
     *
     * @param close if true then the file (if != null) is closed and another file whose name is extended by a number is used.
     */
    @Override
    public synchronized void flush(boolean close) {
        if(monitoring && filled) {
            PrintStream out = System.out;
            if(file != null) {
                try{out = new PrintStream(file);}
                catch(FileNotFoundException ex) {
                    out.println("File not found " + file.getAbsolutePath());}}
            out.println(title);
            out.println("*******");
            for(Map.Entry<String,ArrayList<String>> entry : buffers.entrySet()) {
                out.println(entry.getKey()+":");
                for(String st : entry.getValue()) {out.printf(st);}
                out.println();}
            buffers.clear();
            filled = false;
            if(close && out != System.out) {
                out.close();
                file = new File(pathname + (filenumber++));}}}


    /** returns some information about the monitor
     *
     * @return some information about the monitor
     */
    public String toString() {
        if(!monitoring) {return title + ": monitoring deactivated.";}
            StringBuilder st = new StringBuilder();
            st.append(title).append(": ");
            st.append("separated printing");
            for(Map.Entry<String,ArrayList<String>> entry : buffers.entrySet()) {
                st.append(entry.getKey()).append(",");}
            st.append(" to ");
            st.append((file == null) ? "System.out" : file.getAbsolutePath());
            return st.toString();}


}
