package Management.Monitor;

/** This class implements a monitor which prints messages directly to System.out*/

public class MonitorLife extends Monitor {

    /** creates a monitor which does nothing at all*/
    public MonitorLife() {}

    /** creates a monitor that prints the messages to System.out
     *
     * @param title like "Messages", "Errors", "Warnings"
     */
    public MonitorLife(String title) {
        this.title = title;
        monitoring = true;}

    /** prints the title, the id followed by the messages to a single line
     *
     * @param id      for identifying the messages
     * @param messages to be printed.
     */
    public void print(String id, String... messages) {
        filled = true;
        if(monitoring) {
            System.out.print(title+","+id+": ");
            for(String message: messages) System.out.print(message);
            System.out.println();}}

    /**  prints title, id and the messages one per line
     *
     * @param id      for identifying and separating the messages
     * @param messages to be printed.
     */
    public void println(String id, String... messages) {
        filled = true;
        if(monitoring) {
            System.out.println(title+","+id+":");
            for(String message: messages) System.out.println(message);}}

    /** prints title, id and the messages one per line
     *
     * @param id        an id for the message
     * @param messages messages
     */
    public void print(String id, StringBuilder messages) {
        filled = true;
        if(monitoring) {
            System.out.println(title+","+id+":");
            System.out.println(messages);}}


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
