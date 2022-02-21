package Management.Monitor;

/** This class implements Monitor by printing the messages to System.out*/

public class MonitorLife implements Monitor {
    public boolean monitoring  = false;  // if false then all messages are ignored
    public String title;                 // a title for the messages
    public boolean filled = false;       // becomes true with the first call of print or println


    /** creates a monitor which does nothing at all*/
    public MonitorLife() {}

    /** creates a monitor that prints the messages to System.out
     *
     * @param title like "Messages", "Errors", "Warnings"
     */
    public MonitorLife(String title) {
        this.title = title;
        monitoring = true;}

    /** either prints or collects the messages into a single line
     *
     * @param id      for identifying and separating the messages
     * @param messages to be printed.
     */
    public void print(String id, String... messages) {
        filled = true;
        if(monitoring) {
            System.out.printf(title,",",id,": ");
            for(String message: messages) System.out.print(message);
            System.out.println();}}

    /** either prints the messages one per line
     *
     * @param id      for identifying and separating the messages
     * @param messages to be printed.
     */
    public void println(String id, String... messages) {
        filled = true;
        if(monitoring) {
            System.out.printf(title,",",id,": ");
            for(String message: messages) System.out.println(message);}}

    public boolean wasFilled() {
        return filled;}

    /** Sets 'filled' to false. The monitor can be reused.*/
    public synchronized void flush(boolean close) {
            filled = false;}


    /** returns some information about the monitor
     *
     * @return some information about the monitor
     */
    public String toString() {
        if(!monitoring) {return title + ": monitoring deactivated.";}
        return title + ": Immediate printing to System.out";}

}
