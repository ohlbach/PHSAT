package Management;

import Utilities.Utilities;

import java.io.PrintStream;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

/** This class maintains the global parameters for controlling the QuSat system.
 */
public class GlobalParameters {

    /** the homedirectory. */
    public String homeDirectory = System.getenv("USERPROFILE");

    /** the name of the current job. It is used as prefix for all generated files. */
    public String jobname = "Test";

    /** the directory where to print the files. Default: system temporal directory. */
    public Path directory = Paths.get(System.getenv("TEMP"));

    /** number of parallel threads for solving several problems. 0 (default) means sequential processing. */
    public int parallel = 1;

    /** life, none or file. */
    public String logging = "life";

    /** for printing information about the working of the system, set by the QuSat instance. */
    public PrintStream logstream;

    /** just for the toString method, set by the QuSat instance. */
    public String logFile = "System.out";

    /** print errors and warnings to a file */
    public boolean errors2File = false;

    /** if false then many things are done destructively. Only the final results are needed. */
    public boolean trackReasoning = false;

    /** activates the Simplifier as separate thread. */
    public boolean simplifier = true;

    /** the monitor mode: life, file, frame. */
    public String monitor = null;

    /** @return a help-string which describes the parameters */
    public static String help() {
        return "Global Parameters:\n" +
                " - directory  the output goes to directory/'jobname'/... \n"+
                "              default: the TEMP directory.\n"+
                " - parallel   (default: 1)  controls how many problems are processed in parallel threads.\n" +
                "                            'true' means to use the number of available processors.\n" +
                " - simplifier (default true) a simplifier in a separate thread triers to deduce true literals and equivalences\n" +
                " - logging    (default: life) 'life', 'none' or 'file' for logging the actions.\n" +
                " -            if 'file' is specified then logging information is printed to logfile.txt"+
                " - errors2File (default false) print errors and warnings to files\n" +
                " - monitor   'life': print all the messages to System.out.\n" +
                "             'file': just print the messages to a file, separate for each problem.\n" +
                "             'frame': print the messages to a frame, separate for each problem.\n" +
                "             default: no monitoring.\n" +
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
                case "jobname": jobname = value; break;
                case "directory" :
                    directory = Utilities.pathWithHome(value);
                    if(!directory.toFile().exists()) {errors.append("Directory " + directory.toString() + " does not exist.");}
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
                    logging = value;
                    if(!(value.equals("life") || value.equals("file") || value.equals("none"))) {
                        errors.append(title+"logging '" + value + "' is not one of 'life','none','file'");}
                    break;
                case "monitor":
                    monitor = value;
                    if(!(value.equals("life") || value.equals("file") || value.equals("frame"))) {
                        errors.append(title+"monitor '" + value + "' is not one of 'life','file', 'frame'");}
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
                "  directory:          " + directory+"\n"+
                "  parallel threads:   " + parallel +"\n" +
                "  logging:            " + logging +"\n" +
                "  errors2File         " + errors2File + "\n" +
                "  monitor:            " + (monitor == null ? "null" : monitor) +"\n"+
                "  trackReasoning:     " + trackReasoning;}

}

