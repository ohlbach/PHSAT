package Management.Monitor;

import Management.GIU.ScrollableFrame;
import Utilities.Utilities;

import java.io.PrintStream;

/** This class implements a monitor which prints to a JFrame.
 * <br>
 * Either a single scrollable JFrame is opened, or separate JFrames are opened by the initialize-method.
 */
public class MonitorFrame extends Monitor {
    /** the width of the frame */
    int width;
    /** the hight of the frame */
    int hight;
    /** the initial horizontal offset */
    int xOffset;
    /** the initial vertical offset */
    int yOffset;
    /** where to print the messages */
    PrintStream printStream;
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
     * @param width      of the frame
     * @param hight      of the frame
     * @param xOffset    x-offset of the left upper corner
     * @param yOffset    y-offset of the left upper corner
     */

    public MonitorFrame(String title, boolean separate, long startTime, int width, int hight, int xOffset, int yOffset) {
        super(title, separate, startTime);
        this.width = width;
        this.hight = hight;
        this.xOffset = xOffset;
        this.yOffset = yOffset;
        if(!separate) printStream = ScrollableFrame.getPrintStream(width, hight, xOffset, yOffset,title);
    }

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
            printStream = ScrollableFrame.getPrintStream(width, hight, index*xOffset, index*yOffset,name);
            ++index;}}


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
