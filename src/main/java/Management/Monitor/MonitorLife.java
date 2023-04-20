package Management.Monitor;

/** This class implements a monitor which prints messages directly to System.out.*/

public class MonitorLife extends Monitor {

    /** creates a monitor which does nothing at all.*/
    public MonitorLife() {
        super("Monitor",System.nanoTime());}

    /** creates a monitor that prints the messages to System.out.
     *
     * @param title for identifying the monitor.
     */
    public MonitorLife(String title, long startTime) {
        super(title,startTime);}

    /** prints the id and the elapsed time followed by the messages to a single line.
     *
     * @param id      for identifying the messages.
     * @param messages to be printed.
     */
    public void print(String id, String... messages) {
        double time = (double)(System.nanoTime() - startTime)/1000.0;
        System.out.print(id+","+title+"@" + time + "μs: ");
        for(String message: messages) System.out.print(message);
        System.out.println();}

    /**  prints the id and the elapsed time followed by the messages one per line.
     *
     * @param id      for identifying and separating the messages
     * @param messages to be printed.
     */
    public void println(String id, String... messages) {
        double time = ((double)(System.nanoTime() - startTime))/1000.0;
        System.out.println(id+","+title+"@" + time + "μs: ");
        for(String message: messages) System.out.println("  "+message);}

    /** prints just the message.
     *
     * @param message the messages themselves.
     */
    public void println(String message) {
        System.out.println(message);};

    /** prints the id and the elapsed time followed by the messages one per line.
     *
     * @param id        an id for the message.
     * @param messages messages.
     */
    public void print(String id, StringBuilder messages) {
        double time = (double)(System.nanoTime() - startTime)/1000.0;
        System.out.println(id+","+title+"@" + time + "μs: ");
        System.out.println(messages);}


    /** does nothing.*/
    public synchronized void flush(boolean close) {}


    /** returns some information about the monitor.
     *
     * @return some information about the monitor.
     */
    public String toString() {
        return title + ": Immediate printing to System.out";}

}
