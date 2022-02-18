package Management;

import Utilities.Utilities;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintStream;
import java.nio.file.Path;
import java.nio.file.Paths;
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


    /** @return a help-string which describes the parameters */
    public static String help() {
        return "Global Parameters:\n" +
                " - directory  (default home) relative to the homedirectory.\n"+
                " - parallel   (default: 1)  controls how many problems are processed in parallel threads.\n" +
                "                            'true' means to use the number of available processors.\n" +
                " - logging    (default: System.out) 'file' for logging the actions\n" +
                " -            if 'file' is specified then logging information is printed to [jobname]-logfile.txt"+
                " - errors2File (default false) print errors and warnings to files\n" +
                " - monitor   'collect': collect them for all keys separated and print them at the end.\n" +
                "             'live':    just print out as the messages come.\n" +
                "              If the file is not specified then the messages are printed to System.out\n" +
                "              default: no monitoring.\n" +
                " - trackReasoning (default false) tracks reasons for all derivations.";}


    /** generates the default parameters */
    public GlobalParameters() {}

    /** parses the parameters and generates the internal data
     *
     * @param parameters the raw parameters (as strings)
     * @param errors    for error messages
     * @param warnings  for warnings
     */
    public GlobalParameters(HashMap<String,String> parameters, Monitor errors, Monitor warnings) {
        String title = "Global Parameters";
        for(Map.Entry<String,String> entry : parameters.entrySet()) {
            String key = entry.getKey();
            if(key.equals("global")) {continue;}
            String value = entry.getValue();
            switch(key) {
                case "directory" :
                    directory = Paths.get(homeDirectory,value).toFile();
                    if(directory.exists()) {
                        if(!directory.isDirectory()) {
                            errors.print(title,"Directory " + directory + " is not a directory");}}
                    else {
                        if(!directory.mkdirs()){
                            errors.print(title,"Directory " + directory + " cannot be created");}}
                    break;
                case "parallel":
                    if(value.equals("true")) {parallel = Runtime.getRuntime().availableProcessors(); break;}
                    Integer par = Utilities.parseInteger(title, "parallel", value,errors);
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
                        warnings.println(title,"Logfile "+ logFile + " cannot be opened.",
                                "Printing to System.out instead.");
                        logFile = "System.out";
                        logstream = System.out;}
                    break;
                case "monitor":
                    monitorMode = value;
                    if(!(value.equals("live") || value.equals("collect"))) {
                        errors.print(title,"monitor '" + value + "' is not one of 'live','collect");}
                    break;
                case "errors2File": errors2File = true;
                    break;
                default: warnings.print(title,"Unknown parameter: " + key);
            }}}


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
                "  trackReasoning:     " + trackReasoning +"\n";}

}

