package Management.Monitor;

import Management.GIU.ScrollableFrame;
import Utilities.Utilities;

import java.io.PrintStream;
import java.util.ArrayList;

/** This class implements a monitor which prints to a JFrame.
 * <br>
 * Either a single scrollable JFrame is opened, or separate JFrames are opened by the initialize-method.
 */
public class MonitorFrame extends Monitor {
    /** the width of the frame */
    private static int width = 500;
    /** the hight of the frame */
    private static int hight = 400;
    /** the initial horizontal offset */
    private static int offsetX = 100;
    /** the initial vertical offset */
    private static int offsetY = 100;
    /** where to print the messages */
    private PrintStream printStream;
    /** counts the frames and controls the offsets */
    private int index = 0;

    /** creates a monitor which does nothing at all*/
    public MonitorFrame() {
        super("Monitor",false,System.nanoTime());}

    /** creates a monitor that prints the messages into a JFrame
     *
     * @param title      a title for the monitor.
     * @param separate   if true then a separate frame is opened by each call of the initialize-method.
     * @param startTime  the start time or the job.
     */

    public MonitorFrame(String title, boolean separate, long startTime) {
        super(title, separate, startTime);
        if(!separate) printStream = ScrollableFrame.getPrintStream(width, hight, offsetX, offsetY,title);
    }

    /**
     * Sets the default values for the log-frame based on the "logframe" entry of the hashmap
     *
     * @param moduleValues the hashmap containing the module parameter values
     */
    public static void setDefaults(ArrayList<String> moduleValues) {
        if(moduleValues == null) {return;}
        try{
            for(String line : moduleValues) {
                String[] parts = line.split("\\s*=\\s*");
                if(parts.length != 2) {continue;}
                String variable = parts[0];
                String value = parts[1];
                switch(variable.toLowerCase()) {
                    case "width":   width   = Integer.parseInt(value); break;
                    case "hight":   hight   = Integer.parseInt(value); break;
                    case "offsetx": offsetX = Integer.parseInt(value); break;
                    case "offsety": offsetY = Integer.parseInt(value); break;
                }}}
        catch(NumberFormatException e) {
            System.err.println("Error parsing defaults for LogFrame:\n" + e);
            System.exit(1);}}

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
            printStream = ScrollableFrame.getPrintStream(width, hight, index*offsetX, index*offsetY,name);
            ++index;}}

    /** returns the sizes: with, hight, offsetY and offsetY
     *
     * @return the sizes: with, hight, offsetY and offsetY
     */
    public static int[] sizes() {
        return new int[]{width,hight,offsetX,offsetY};}


    /** prints the messages into a single line.
     *
     * @param id      for identifying and separating the messages.
     * @param messages to be printed.
     */
    @Override
    public void print(String id, String... messages) {
        String time = Utilities.duration(System.nanoTime() - startTime);
        printStream.printf(time);
        for(String message: messages) printStream.printf(message);}

    /** prints the messages one per line.
     *
     * @param id      for identifying and separating the messages.
     * @param messages to be printed.
     */
    @Override
    public void println(String id, String... messages) {
        String time = Utilities.duration(System.nanoTime() - startTime);
        printStream.printf(time);
        for(String message: messages) printStream.println(message);}


    /** prints just the message.
     *
     * @param message the messages themselves.
     */
    public void println(String message) {
        printStream.println(message);};

    /** does nothing*/
    @Override
    public synchronized void flush(boolean close) {}


    /** returns some information about the monitor.
     *
     * @return some information about the monitor.
     */
    public String toString() {
        return title + ": Immediate printing to frame";}

}
