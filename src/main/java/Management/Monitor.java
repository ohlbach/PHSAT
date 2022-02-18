package Management;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

/** This class allows one to collect and print monitoring messages for parallel threads.
 *  The messages can either be printed as they come, or they are internally collected and then printed separately.
 * Created by ohlbach on 12.10.2018.
 */
public class Monitor {
    public boolean monitoring  = false;   // if false then all messages are ignored
    public boolean live  = false;         // if true then the messages are collected and printed at flush
    public File file     = null;          // a file where to print the messages
    private PrintStream out = System.out; // a stream where to print the messages
    private final HashMap<String,ArrayList<String>> buffers = new HashMap<>(); // collects the separated messages
    public String title;                   // a title for the messages
    public boolean filled = false;         // becomes true with the first call of print or println

    /** creates a monitor which does nothing at all*/
    public Monitor() {}

    /** creates a monitor according to the mode.
     *  The mode may be:<br>
     *  - separated [filename]<br>
     *  - mixed     [filename]<br>
     * 'separated' causes the messages to be collected and printed separately for each thread. <br>
     * If the filename is not given or cannot be opened then the messages are printed to System.out.
     *
     * @param title like "Messages", "Errors", "Warnings"
     * @param file null or a file where to put the messages
     * @param live if true then the messages are printed as they come.
     */
    public Monitor(String title, File file, boolean live) {
        this.title = title;
        this.file = file;
        this.live = live;
        monitoring = true;
        if(file == null) return;
        try{out = new PrintStream(file);}
        catch(Exception ex) {
            out = System.out;
            System.out.println(title+ ": File '"+ file.getName() + "' cannot be opened. Printing to System.out\n");}}

    /** either prints or collects the messages
     *
     * @param id      for identifying and separating the messages
     * @param messages to be printed.
     */
    public void print(String id, String... messages) {
        filled = true;
        if(monitoring) {
            String string = "";
            for(String message : messages) string += message;
            printString(id,string);}}

    /** either prints or collects the messages with "\n" appended
     *
     * @param id      for identifying and separating the messages
     * @param messages to be printed.
     */
    public void println(String id, String... messages) {
        filled = true;
        if(monitoring) {
            String string = "";
            for(int i = 0; i < messages.length; ++i) {
                string += messages[i];
                if(i < messages.length-1) string += "\n";}
            printString(id,string);}}

    /** either prints the string to out, or collects it
     *
     * @param id     an identifier
     * @param string a string
     */
    private synchronized void printString(String id,String string) {
        if(live) {
            out.printf(title," ",id,": ");
            out.println(string);}
        else {buffers.computeIfAbsent(id, k -> new ArrayList<>()).add(string);}}


    /** In '!live' mode, the messages are now printed, either to System.out, or to the file
     * The buffers are cleared, and the monitor can be used again.
     */
    public synchronized void flush(boolean close) {
        if(monitoring && filled) {
            if(live) return;
            out = System.out;
            if(file != null) {
                try{out = new PrintStream(file);}
                catch(FileNotFoundException ex) {
                    out.println("File not found " + file.getAbsolutePath());}}
            out.println(title);
            out.println("*******");
            for(Map.Entry<String,ArrayList<String>> entry : buffers.entrySet()) {
                out.println(entry.getKey()+":");
                for(String st : entry.getValue()) {out.println(st);}}
            buffers.clear();
            filled = false;
            if(close && out != System.out) out.close();}}


    /** returns some information about the monitor
     *
     * @return some information about the monitor
     */
    public String toString() {
        if(!monitoring) {return title + ": monitoring deactivated.";}
        if(live) return title + ": Immediate printing to " + ((file == null) ? "System.out" : file.getAbsolutePath());
        else {
            StringBuilder st = new StringBuilder();
            st.append(title).append(": ");
            st.append("separated printing");
            for(Map.Entry<String,ArrayList<String>> entry : buffers.entrySet()) {
                st.append(entry.getKey()).append(",");}
            st.append(" to ");
            st.append((file == null) ? "System.out" : file.getAbsolutePath());
            return st.toString();}}


}
