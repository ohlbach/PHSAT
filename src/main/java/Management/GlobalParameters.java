package Management;

import Management.Monitor.Monitor;
import Management.Monitor.MonitorFile;
import Management.Monitor.MonitorFrame;
import Management.Monitor.MonitorLife;
import Utilities.Utilities;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintStream;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

/** This class maintains the global parameters for controlling the QUSat system
 * Created by ohlbach on 12.10.2018.
 */
public class GlobalParameters {
    /** the homedirectory */
    public String homeDirectory = System.getenv("USERPROFILE");
    /** the name of the current job. It is used as prefix for all generated files */
    public String jobname = "Test";

    public String basicDirectory = System.getenv("USERPROFILE");

    /** the directory where to print the files. Default: null (System.out) */
    public File directory = null;
    /** number of parallel threads for solving several problems. 0 (default) means sequential processing. */
    public int parallel = 1;
    /** for printing information about the working of the system */
    public PrintStream logstream = System.out;
    /** just for the toString method */
    private String logFile = "System.out";
    /** print errors and warnings to a file */
    public boolean errors2File = false;
    /** for monitoring the actions of the solvers */
    public String monitorMode = null;
    /** if false then many things are done destructively. Only the final results are needed. */
    public boolean trackReasoning = false;

    public boolean simplifier = true;

    public MonitorLife monitor;
    public File monitorFile;

    public Monitor getMonitor(String title) {
        switch (monitorMode) {
            case "collect": return new MonitorFile(title,monitorFile);
            case "live":    return new MonitorLife(title);
            case "frame":   return new MonitorFrame(title, 1000,1000,100,100);}
        return null;}

    /** @return a help-string which describes the parameters */
    public static String help() {
        return "Global Parameters:\n" +
                " - directory  (default home) relative to the homedirectory.\n"+
                " - parallel   (default: 1)  controls how many problems are processed in parallel threads.\n" +
                "                            'true' means to use the number of available processors.\n" +
                " - simplifier (default true) a simplifier in a separate thread triers to deduce true literals and equivalences\n" +
                " - logging    (default: System.out) 'file' for logging the actions\n" +
                " -            if 'file' is specified then logging information is printed to [jobname]-logfile.txt"+
                " - errors2File (default false) print errors and warnings to files\n" +
                " - monitor   'collect': collect them for all keys separated and print them at the end.\n" +
                "             'live':    just print out as the messages come.\n" +
                "              If the file is not specified then the messages are printed to System.out\n" +
                "              default: no monitoring.\n" +
                " - trackReasoning (default false) tracks reasons for all derivations.\n";}


    /** generates the default parameters */
    public GlobalParameters() {}

    /** parses the parameters and generates the internal data
     *
     * @param parameters the raw parameters (as strings)
     * @param errors    for error messages
     * @param warnings  for warnings
     */
    public GlobalParameters(ArrayList<HashMap<String,String>> parameters, StringBuilder errors, StringBuilder warnings) {
        String title = "Global Parameters";
        for(HashMap<String,String> pars :parameters) {
            for(Map.Entry<String,String> entry : pars.entrySet()) {
                String key = entry.getKey();
                String value = entry.getValue();
            switch(key) {
                case "jobname": jobname = value;
                break;
                case "directory" :
                    directory = Paths.get(homeDirectory,value).toFile();
                    if(directory.exists()) {
                        if(!directory.isDirectory()) {
                            errors.append(title+"Directory " + directory + " is not a directory");}}
                    else {
                        if(!directory.mkdirs()){
                            errors.append(title+"Directory " + directory + " cannot be created");}}
                    break;
                case "parallel":
                    if(value.equals("true")) {parallel = Runtime.getRuntime().availableProcessors(); break;}
                    StringBuilder err = new StringBuilder();
                    Integer par = Utilities.parseInteger(title, value,err);
                    if(par == null) {errors.append(title+err);}   // überarbeiten
                    if(par != null) {parallel = par;}
                    break;
                case "trackReasoning" : trackReasoning = value.equals("true");
                    break;
                case "logging":
                    logFile = value;
                    if(!value.equals("file")) {logstream = System.out; logFile = "System.out"; break;}
                    try {Path path = Paths.get(directory.getAbsolutePath(),jobname+"-logfile.txt");
                        logFile =  path.toFile().toString();
                        logstream = new PrintStream(path.toFile());}
                    catch(FileNotFoundException ex) {
                        warnings.append(title+"Logfile "+ logFile + " cannot be opened."+
                                "Printing to System.out instead.");
                        logFile = "System.out";
                        logstream = System.out;}
                    break;
                case "monitor":
                    monitorMode = value;
                    if(!(value.equals("live") || value.equals("collect"))) {
                        errors.append(title+"monitor '" + value + "' is not one of 'live','collect");}
                    break;
                case "errors2File":  errors2File = true;
                    break;
                case "simplifier": simplifier = value.equals("true");
                    break;
                default: warnings.append(title+"Unknown parameter: " + key);
            }}}}


    /** returns a description of the parameters */
    public String toString() {
        return "Global Parameters:\n" +
                "  name                " + jobname +"\n"+
                "  homedirectory       " + homeDirectory + "\n"+
                "  directory:          " + directory.getAbsolutePath()+"\n"+
                "  parallel threads:   " + parallel +"\n" +
                "  logFile:            " + logFile +"\n" +
                "  errors2File         " + errors2File + "\n" +
                "  monitor:            " + (monitorMode == null ? "null" : monitorMode) +"\n"+
                "  trackReasoning:     " + trackReasoning;}

}

