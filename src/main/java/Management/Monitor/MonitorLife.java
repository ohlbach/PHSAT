package Management.Monitor;

import Utilities.Utilities;

import java.util.Date;

/** This class implements a monitor which prints messages directly to System.out.*/

public class MonitorLife extends Monitor {

    /** creates a monitor which does nothing at all.*/
    public MonitorLife() {
        super("Monitor",false,System.nanoTime());}

    /** creates a monitor that prints the messages to System.out.
     *
     * @param title for identifying the monitor.
     * @param startTime all print commands are tagged with the difference of the actual time and the startTime.
     */
    public MonitorLife(String title, long startTime) {
        super(title,false,startTime);
        System.out.println("Monitor for " + title + " @ " + (new Date()));}

    /** initializes the monitor for a new problem by printing a message to System.out.
     *
     * @param problemId the problem's identifier.
     */
    public void initialize(String problemId) {
        System.out.println("Monitor for problem" + problemId + " @ " + Utilities.duration(System.nanoTime() - startTime));
    }

    /** prints the id and the elapsed time followed by the messages to a single line.
     *
     * @param id      for identifying the messages.
     * @param messages to be printed.
     */
    public void print(String id, String... messages) {
        String time = Utilities.duration(System.nanoTime() - startTime);
        System.out.print(id+"@ " + time + ":");
        for(String message: messages) {System.out.print(" "); System.out.print(message);}
        System.out.println();}

    /**  prints the id and the elapsed time followed by the messages one per line.
     *
     * @param id      for identifying and separating the messages
     * @param messages to be printed.
     */
    public void println(String id, String... messages) {
        String time = Utilities.duration(System.nanoTime() - startTime);
        System.out.println(id+"@ " + time);
        for(String message: messages) {System.out.print(" "); System.out.println(message);}}

    /** prints just the message.
     *
     * @param message the messages themselves.
     */
    public void println(String message) {
        System.out.println(message);};

    /** does nothing.*/
    public synchronized void flush(boolean close) {}


    /** returns some information about the monitor.
     *
     * @return some information about the monitor.
     */
    public String toString() {
        return "MonitorLife " + title + ": printing to System.out";}

}
