package Management.Monitor;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

public class MonitorFile extends Monitor {
    private final HashMap<String, ArrayList<String>> buffers = new HashMap<>(); // collects the separated messages
    public String title;                   // a title for the messages
    public boolean filled = false;         // becomes true with the first call of print or println
    private File file;

    /** creates a monitor which does nothing at all*/
    public MonitorFile() {}

    /** creates a monitor according to the mode.
     * If the filename is not given or cannot be opened then the messages are printed to System.out.
     *
     * @param title like "Messages", "Errors", "Warnings"
     * @param file where to print the messages.
     */
    public MonitorFile(String title, File file) {
        this.title = title;
        this.file = file;
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

    /** returns true if the monitor was filled.
     *
     * @return true if the monitor was filled.
     */
    @Override
    public boolean wasFilled() {
        return filled;}


    /** In '!live' mode, the messages are now printed, either to System.out, or to the file
     * The buffers are cleared, and the monitor can be used again.
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
            if(close && out != System.out) out.close();}}


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
