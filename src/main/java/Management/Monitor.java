package Management;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

/** This class allows one to collect and print monitoring messages for parallel threads.
 *  The messages can either be printed as they come, or they are internally collected and then printed separately for each thread.
 * Created by ohlbach on 12.10.2018.
 */
public class Monitor {
    private boolean monitoring = true;
    private boolean separated  = true;
    private File file          = null;
    private PrintStream out   = System.out;
    private HashMap<String,ArrayList<String>> buffers = new HashMap<>();

    /** creates a monitor which does nothing at all*/
    public Monitor() {monitoring = false;}

    /** creates a monitor according to the specification.
     *  The specification may be:<br/>
     *  - separated [filename]<br/>
     *  - mixed     [filename]<br/>
     * 'separated' causes the messages to be collected and printed separately for each thread. <br/>
     * If the filename is not given or cannot be opened then the messages are printed to System.out.
     *
     * @param specification
     * @param errors for error messages.
     * @param warnings for warnings.
     */
    public Monitor(String specification, StringBuffer errors, StringBuffer warnings) {
        String[] parts = specification.split("\\s*(,| )\\s*");
        if(parts[0].equals("separated")) {separated = true;}
        else {if(parts[0].equals("mixed")) {separated = false;}
            else {monitoring = false; errors.append("Monitoring '" + parts[0] + "' is unknown. Use 'separated' or 'mixed'.\n");}}
        if(parts.length > 1) {
            file = new File(parts[1]);
            if(!separated){
                try{out = new PrintStream(file);}
                catch(Exception ex) {
                    out = System.out;
                    warnings.append("Monitorfile '"+ parts[1] + "' cannot be opened. Printing to System.out\n");}}}}


    /**
     * @return true if monitoring is activated.
     */
    public boolean monitoring() {return monitoring;}

    /** adds a thread for separated printing of messages
     *
     * @param id    for identifying the thread
     * @param info  some string.
     */
    public synchronized void addThread(String id, String info) {
        if(monitoring && separated) {
            ArrayList<String> buffer = new ArrayList<>();
            if(info != null) {buffer.add(info);}
            buffers.put(id,buffer);}}

    /** either prints or collects the message
     *
     * @param id      for identifying the thread
     * @param message to be printed.
     */
    public synchronized void print(String id, String message) {
        if(monitoring) {
            if(separated) {buffers.get(id).add(message);}
            else {out.printf(id);out.printf(": "); out.println(message);}}}

    /** In 'separated' mode, the messages are now printed.
     */
    public synchronized void flush() {
        if(monitoring) {
            if(separated) {
                PrintStream out = System.out;
                if(file != null) {
                    try{out = new PrintStream(file);}
                    catch(FileNotFoundException ex) {
                        out.println("File not found " + file.getAbsolutePath());}}
                for(Map.Entry<String,ArrayList<String>> entry : buffers.entrySet()) {
                    out.println(entry.getKey()+":");
                    for(String st : entry.getValue()) {out.println(st);}}}
            out.close();}}

    /** returns some information about the monitor
     *
     * @return some information about the monitor
     */
    public String toString() {
        if(!monitoring) {return "Monitoring deactivated.";}
        if(separated) {
            StringBuilder st = new StringBuilder();
            st.append("Separated printing for threads ");
            for(Map.Entry<String,ArrayList<String>> entry : buffers.entrySet()) {
                st.append(entry.getKey()+",");}
            st.append(" to ");
            st.append((file == null) ? "System.out" : file.getName());
            return st.toString();}
        else {return "Immediate printing to " + ((file == null) ? "System.out" : file.getAbsolutePath());}}


}
