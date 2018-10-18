package Management;

import Utilities.Utilities;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintStream;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;

/** This class maintains the global parameters for controlling the PHSat system
 * Created by ohlbach on 12.10.2018.
 */
public class GlobalParameters {
    public String home = System.getenv("USERPROFILE");
    public File directory = new File(home);
    /** number of parallel threads for solving several problems. 0 (default) means sequential processing. */
    public int parallel             = 0;
    /** for printing information about the working of the system */
    public PrintStream logstream    = null;
    /** just for the toString method */
    private String logFile = "";
    /** for printing the results of the solvers */
    public String statisticsTextFile = null;
    public String statisticsCSVFile = null;
    public boolean statisticsText = true;
    public String resultFile = null;
    /** for monitoring the actions of the solvers */
    public Monitor monitor          = new Monitor();
    /** supervises the solution of the problem */
    public ProblemSupervisor supervisor = null;



    /** returns a help-string which describes the parameters */
    public static String help() {
        return "Global Parameters:\n" +
                " - directory  (default home) relative to the homedirectory\n"+
                " - parallel   (default: 0)  controls how many problems are processed in parallel threads\n" +
                "                            'true' means to use the number of available processors\n" +
                " - logging    filename (default: System.out) for logging the actions\n" +
                " - monitor   'separated file': collect them for all threads separated, \n" +
                "             'mixed file':     just print out as the messages come\n" +
                "              If the file is not specified then the messages are printed to System.out\n" +
                "              default: no monitoring\n" +
                " - results    (default: System.out) for printing the results to the given file.\n" +
                " - statistics text [file] csv file (default: text System.out) for printing statistics information to the given files.";}


    /** generates the default parameters */
    public GlobalParameters() {}

    /** parses the parameters and generates the internal data */
    public GlobalParameters(HashMap<String,String> parameters, StringBuffer errors, StringBuffer warnings) {
        for(Map.Entry<String,String> entry : parameters.entrySet()) {
            String key = entry.getKey();
            if(key.equals("global")) {continue;}
            String value = entry.getValue();
            switch(key) {
                case "directory" :
                    directory = Paths.get(home,value).toFile();
                    if(!directory.isDirectory()) {
                        if(directory.exists()) {errors.append("Directory " + value + " is not a directory");}
                        else {directory.mkdirs();}}
                    break;
                case "parallel":
                    if(value.equals("true")) {parallel = Runtime.getRuntime().availableProcessors(); break;}
                    Integer par = Utilities.parseInteger("GlobalParameter parallel", value,errors);
                    if(par != null) {parallel = par;}
                    break;
                case "logging":
                    logFile = value;
                    if(value.equals("true")) {logstream = System.out; logFile = "System.out"; break;}
                    try {logstream = new PrintStream(new File(value));}
                    catch(FileNotFoundException ex) {
                        warnings.append("Logfile "+ value + " cannot be opened.\n");
                        warnings.append("Printing to System.out instead.");
                        logFile = "System.out";
                        logstream = System.out;}
                    break;
                case "monitor":
                    monitor = new Monitor(value,errors,warnings);
                    break;
                case "results":
                    resultFile = value;
                    break;
                case "statistics":
                    if(!value.equals("true")) { // otherwise System.out
                        String[] parts = value.split("\\s*[=, ]\\s*");
                        int length = parts.length;
                        int start = 0;
                        while(start < length) {
                        if (parts[start].equals("text")) {
                            statisticsText = true;
                            if(length > 1 && !parts[1].equals("csv")) {
                                statisticsTextFile = parts[start+1]; start += 2;}}
                        if(parts[start].equals("csv")) {
                            if(length > start) {statisticsCSVFile = parts[start+1]; start += 2;}
                            else {errors.append("No csv-File for the statistics defined"); break;}}
                        }}
                    break;
                default: warnings.append("Unknown global parameter: " + key);
            }}}

    /** prints a message to the logstream */
    public void log(String message) {
        if(logstream != null) logstream.println(message);}

    /** closes log- and resultStream */
    public void close() {
        if(logstream != null && logstream != System.out) logstream.close();}

    /** returns a description of the parameters */
    public String toString() {
        String result =
                "Global Parameters:\n" +
                "  directory:          " + directory.getAbsolutePath()+"\n"+
                "  parallel threads:   " + Integer.toString(parallel)+"\n" +
                "  logFile:            " + logFile +"\n" +
                "  resultFile:         " + resultFile +"\n" +
                "  monitor:            " + monitor.toString() + "\n" +
                "  statisticsText:     " + Boolean.toString(statisticsText) +"\n";
        if(statisticsTextFile != null) {result +=
                "  statisticsTextFile: " + statisticsTextFile + "\n";}
        if(statisticsCSVFile != null) {result +=
                "  statisticsCSVFile: " + statisticsCSVFile;}
    return result;}

}

