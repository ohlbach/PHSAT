package Management.Monitor;

import javax.swing.*;

/** This class implements a monitor which prints to a JFrame.
 */
public class MonitorFrame extends Monitor {
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
     * @param height     of the frame
     * @param xOffset    x-offset of the left upper corner
     * @param yOffset    y-offset of the left upper corner
     */

    public MonitorFrame(String title, long startTime, int width, int height, int xOffset, int yOffset) {
        super(title, startTime);
        frame = new JFrame(title);
        frame.setSize(width,height);
        frame.setVisible(true);
        frame.setLocation(xOffset,yOffset);
        frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        area = new JTextArea("",20,50);
        area.setLineWrap(true);
        JScrollPane scroll = new JScrollPane(area);
        scroll.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
        scroll.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
        frame.getContentPane().add(scroll);
        frame.pack();
        frame.setVisible(true);
        area.append("Monitor for " + title + ":\n");
    }

    /** prints the messages into a single line.
     *
     * @param id      for identifying and separating the messages.
     * @param messages to be printed.
     */
    @Override
    public void print(String id, String... messages) {
        double time = (double)(System.nanoTime() - startTime)/1000.0;
        frame.setVisible(true);
        area.append(id);  area.append(" @ "); area.append(Double.toString(time));   area.append(" μs: ");
        for(String message: messages) area.append(message);
        area.append("\n");}

    /** prints the messages one per line.
     *
     * @param id      for identifying and separating the messages.
     * @param messages to be printed.
     */
    @Override
    public void println(String id, String... messages) {
        double time = (double)(System.nanoTime() - startTime)/1000.0;
        frame.setVisible(true);
        area.append(id); area.append(" @ "); area.append(Double.toString(time));   area.append(" μs:\n");
        for(String message: messages) {area.append(message);area.append("\n");}
        }

    /** prints just the message.
     *
     * @param message the messages themselves.
     */
    public void println(String message) {
        area.append(message);area.append("\n");};

    /** prints the messages one per line.
     *
     * @param id      for identifying and separating the messages.
     * @param messages to be printed.
     */
    @Override
    public void print(String id, StringBuilder messages) {
        double time = (double)(System.nanoTime() - startTime)/1000.0;
        frame.setVisible(true);
        area.append(id); area.append(" @ "); area.append(Double.toString(time));   area.append(" μs: ");
        area.append(messages.toString());area.append("\n");}


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
