package Management.Monitor;

import Management.Monitor.Monitor;

import javax.swing.*;

/** implements a monitor which prints to a JFrame
 */
public class MonitorFrame extends Monitor {
    public String title;                 // a title for the messages
    public boolean filled = false;       // becomes true with the first call of print or println
    private JFrame frame;                // a frame
    private JTextArea area;              // its text area

    /** creates a monitor which does nothing at all*/
    public MonitorFrame() {}

    /** creates a monitor that prints the messages into a JFrame
     *
     * @param title like "Messages", "Errors", "Warnings"
     * @param width    of the frame
     * @param height    of the frame
     * @param xOffset  x-offset of the left upper corner
     * @param yOffset  y-offset of the left upper corner
     */

    public MonitorFrame(String title, int width, int height, int xOffset, int yOffset) {
        this.title = title;
        monitoring = true;
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
    }

    /** either prints or collects the messages into a single line
     *
     * @param id      for identifying and separating the messages
     * @param messages to be printed.
     */
    @Override
    public void print(String id, String... messages) {
        if(monitoring) {
            if(!filled) {frame.setVisible(true);}
            area.append(title); area.append(","); area.append(id); area.append(": ");
            for(String message: messages) area.append(message);
            area.append("\n");}
        filled = true;}

    /** either prints the messages one per line
     *
     * @param id      for identifying and separating the messages
     * @param messages to be printed.
     */
    @Override
    public void println(String id, String... messages) {
        if(monitoring) {
            if(!filled) {frame.setVisible(true);}
            area.append(title); area.append(","); area.append(id); area.append(": ");
            for(String message: messages) {area.append(message);area.append("\n");}}
        filled = true;}

    /** either prints the messages one per line
     *
     * @param id      for identifying and separating the messages
     * @param messages to be printed.
     */
    @Override
    public void print(String id, StringBuilder messages) {
        if(monitoring) {
            if(!filled) {frame.setVisible(true);}
            area.append(title); area.append(","); area.append(id); area.append(":\n");
            area.append(messages.toString());area.append("\n");}
        filled = true;}

    /** returns true if the frame was filled at least once
     *
     * @return true if the frame was filled at least once
     */
    @Override
    public boolean wasFilled() {
        return filled;}

    /** Sets 'filled' to false. The monitor can be reused.*/
    @Override
    public synchronized void flush(boolean close) {
        filled = false;
        if(close) {frame.dispose(); return;}
        frame.remove(area);
        new JTextArea();
        frame.add(area);}


    /** returns some information about the monitor
     *
     * @return some information about the monitor
     */
    public String toString() {
        if(!monitoring) {return title + ": monitoring deactivated.";}
        return title + ": Immediate printing to frame";}

}
