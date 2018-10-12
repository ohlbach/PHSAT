package Management;

import Utilities.Utilities;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintStream;
import java.util.HashMap;
import java.util.Map;

/** This class maintains the global parameters for controlling the PHSat system
 * Created by ohlbach on 12.10.2018.
 */
public class GlobalParameters {
    /** number of parallel threads for solving several problems. 0 (default) means sequential processing. */
    public int parallel             = 0;
    /** for printing information about the working of the system */
    public PrintStream logstream    = System.out;
    /** just for the toString method */
    private String logFile = "System.out";
    /** for printing the results of the solvers */
    public PrintStream statisticsStream = System.out;
    /** just for the toString method */
    private String statisticsFile = "System.out";
    /** for printing the results of the solvers */
    public PrintStream resultStream = System.out;
    /** just for the toString method */
    private String resultFile = "System.out";
    /** for monitoring the actions of the solvers */
    public Monitor monitor          = new Monitor();
    /** supervises the solution of the problem */
    public ProblemSupervisor supervisor = null;

    /** returns a help-string which describes the parameters */
    public static String help() {
        return "Global Parameters:\n" +
                " - parallel   (default: 0)          controls how many problems are processed in parallel threads\n" +
                "                                    'true' means to use the number of available processors\n" +
                " - logFile    (default: System.out) for logging the actions\n" +
                " - monitor   'separated file': collect them for all threads separated, \n" +
                "             'mixed file':     just print out as the messages come\n" +
                "              If the file is not specified then the messages are printed to System.out\n" +
                "              default: no monitoring\n" +
                " - results    (default: System.out) for printing the results to the given file." +
                " - statistics (default: System.out) for printing statistics information to the given file.";}


    /** generates the default parameters */
    public GlobalParameters() {}

    /** parses the parameters and generates the internal data */
    public GlobalParameters(HashMap<String,String> parameters, StringBuffer errors, StringBuffer warnings) {
        for(Map.Entry<String,String> entry : parameters.entrySet()) {
            String key = entry.getKey();
            if(key.equals("global")) {continue;}
            String value = entry.getValue();
            switch(key) {
                case "parallel":
                    if(value.equals("true")) {parallel = Runtime.getRuntime().availableProcessors(); break;}
                    Integer par = Utilities.parseInteger("GlobalParameter parallel", value,errors);
                    if(par != null) {parallel = par;}
                    break;
                case "logFile":
                    logFile = value;
                    try {logstream = new PrintStream(new File(value));}
                    catch(FileNotFoundException ex) {
                        errors.append("Logfile "+ value + " cannot be opened.\n");}
                    break;
                case "monitor":
                    monitor = new Monitor(value,errors,warnings);
                    break;
                case "results":
                    resultFile = value;
                    try {
                        resultStream = new PrintStream(new File(value));}
                    catch(FileNotFoundException ex) {
                        errors.append("Resultfile "+ value + " cannot be opened");}
                    break;
                case "statistics":
                    statisticsFile = value;
                    try {
                        statisticsStream = new PrintStream(new File(value));}
                    catch(FileNotFoundException ex) {
                        errors.append("Statisticsfile "+ value + " cannot be opened");}
                    break;
                default: warnings.append("Unknown global parameter: " + key);
            }}}

    /** prints a message to the logstream */
    public void log(String message) {
        logstream.println(message);}

    /** closes log- and resultStream */
    public void close() {
        logstream.close();
        resultStream.close();
        statisticsStream.close();}

    /** returns a description of the parameters */
    public String toString() {
        return "Global Parameters:\n" +
                "  parallel threads: " + Integer.toString(parallel)+"\n" +
                "  logFile:          " + logFile +"\n" +
                "  resultFile:       " + resultFile +"\n" +
                "  statisticsFile:   " + statisticsFile +"\n" +
                "  monitor:          " + monitor.toString();
    }

}

