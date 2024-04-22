package Management.Monitor;

import Management.GIU.ScrollableFrame;

import javax.swing.*;
import java.io.PrintStream;

/** This class implements a monitor which prints to a JFrame.
 */
public class MonitorFrame extends Monitor {

    PrintStream printStream;
    /** the JFrame. */
    private JFrame frame;

    /** its text ares */
    private JTextArea area;

    /** creates a monitor which does nothing at all*/
    public MonitorFrame() {
        super("Monitor",System.nanoTime());}

    /** creates a monitor that prints the messages into a JFrame
     *
     * @param title      a title for the monitor.
     * @param startTime  the start time or the job.
     * @param width      of the frame
     * @param hight     of the frame
     * @param xOffset    x-offset of the left upper corner
     * @param yOffset    y-offset of the left upper corner
     */

    public MonitorFrame(String title, long startTime, int width, int hight, int xOffset, int yOffset) {
        super(title, startTime);
        printStream = ScrollableFrame.getPrintStream(width, hight, xOffset, yOffset,title);
    }

    /** prints the messages into a single line.
     *
     * @param id      for identifying and separating the messages.
     * @param messages to be printed.
     */
    @Override
    public void print(String id, String... messages) {
        long time = System.nanoTime() - startTime;
        printStream.printf(time + " ns: ");
        for(String message: messages) printStream.printf(message);}

    /** prints the messages one per line.
     *
     * @param id      for identifying and separating the messages.
     * @param messages to be printed.
     */
    @Override
    public void println(String id, String... messages) {
        long time = System.nanoTime() - startTime;
        printStream.printf(time + " ns: ");
        for(String message: messages) printStream.println(message);}


    /** prints just the message.
     *
     * @param message the messages themselves.
     */
    public void println(String message) {
        printStream.println(message);};

    /** prints the messages one per line.
     *
     * @param id      for identifying and separating the messages.
     * @param messages to be printed.
     */
    @Override
    public void print(String id, StringBuilder messages) {
        print(id, messages.toString());}


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
