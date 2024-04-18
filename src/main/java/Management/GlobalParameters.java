package Management;

import Utilities.Utilities;

import java.io.File;
import java.io.PrintStream;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

/** This class maintains the global parameters for controlling the QuSat system.
 */
public class GlobalParameters {
    public Parameters parameters;

    /** the homedirectory. */
    public static Path homeDirectory = Path.of(System.getenv("USERPROFILE"),"QuSat");

    public Path jobDirectory = homeDirectory;

    /** the name of the current job. It is used as prefix for all generated files. */
    public String jobname;

    public String getJobname(){return jobname;}

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

    /** if true then the clauses are printed to the logfile */
    public boolean showClauses = false;

    /** enables printing the clauses to a cnf-file */
    public String cnfFile = "none";

    /** print errors and warnings to a file */
    public boolean errors2File = false;

    /** if false then many things are done destructively. Only the final results are needed. */
    public boolean trackReasoning = false;

    /** activates the Simplifier as separate thread. */
    public boolean simplifier = true;

    /** the monitor mode: life, file, frame. */
    public String monitor = null;

    /** specifies how the statistics are printed. */
    public String statistic = "life";


    /** @return a help-string which describes the parameters */
    public static String help() {
        return "Global Parameters:\n" +
                " - jobname    the name of the job (default: Test)\n"+
                " - directory  the output goes to directory/'jobname'/... \n"+
                "              default: the TEMP directory.\n"+
                " - parallel   (default: 1)  controls how many problems are processed in parallel threads.\n" +
                "                            'true' means to use the number of available processors.\n" +
                " - simplifier (default true) a simplifier in a separate thread tries to deduce true literals and equivalences.\n" +
                " - logging    'life', 'none' or 'file' (default: life) for logging the actions.\n" +
                "              if 'file' is specified then logging information is printed to logfile.txt\n"+
                " - showClauses (default: false) if true then the clauses are printed to the logfile. \n"+
                " - cnfFile    'none', 'symboltable' or 'numbers' (default none) enables printing the clauses to a cnf-file.\n"+
                " - errors2File (default false) print errors and warnings to files\n" +
                " - monitor   'life': print all the messages to System.out.\n" +
                "             'file': just print the messages to a file, separate for each problem.\n" +
                "             'frame': print the messages to a frame, separate for each problem.\n" +
                "             default: no monitoring.\n" +
                " - statistic 'life': print the statistics to System.out (default)\n"+
                "             'none': ignore the statistics\n"+
                "             'text': print the statistics as text-files\n"+
                "             'csv':  print the statistics as csv-files\n"+
                "             'both:  print the statistics both as text-files and as csv-files\n"+
                " - trackReasoning (default false) tracks reasons for all derivations.\n";}


    /** generates the default parameters */
    public static Parameters makeGlobalParameters() {
        Parameters parameters = new Parameters("Global Parameters");
        Parameter jobname = new Parameter("jobname",Parameter.Type.String, "TestJob","TestJob",
                "Identifier for the job\n" +
                        "It is also used as directoryname.");
        parameters.add(jobname);
        Parameter jobDirectory = new Parameter("jobdirectory",Parameter.Type.Directory, homeDirectory.toString(),homeDirectory,
                "Directory where to place the produced files.");
        jobDirectory.parser = (text,ignore) -> new File(text).getAbsolutePath();
        parameters.add(jobDirectory);

        Parameter monitor = new Parameter("monitor",Parameter.Type.OneOf,"none",
                "Specifies the target for monitoring.\n"+
                " None:  No monitoring.\n"+
                " Life:  Print the monitor text to System.out.\n"+
                " File:  Print the monitor text to a File.\n"+
                " Frame: Print the monitor text into a frame.");
        monitor.parameters = new Parameters("monitor");
        monitor.parameters.add(new Parameter("None",Parameter.Type.Label,null, null));
        monitor.parameters.add(new Parameter("Life",Parameter.Type.Label,null, null));
        monitor.parameters.add(new Parameter("File",Parameter.Type.File,null, null));
        monitor.parameters.add(new Parameter("Frame",Parameter.Type.Frame,null, null));
        parameters.add(monitor);
        
        Parameter logging = new Parameter("logging",Parameter.Type.OneOf,"none",
                "Specifies the target for logging.\n"+
                        " None:  No logging.\n"+
                        " Life:  Print the logging text to System.out.\n"+
                        " File:  Print the logging text to a File.\n"+
                        " Frame: Print the logging text into a frame.");
        logging.parameters = new Parameters("logging");
        logging.parameters.add(new Parameter("None",Parameter.Type.Label,null, null));
        logging.parameters.add(new Parameter("Life",Parameter.Type.Label,null, null));
        logging.parameters.add(new Parameter("File",Parameter.Type.File,null, null));
        logging.parameters.add(new Parameter("Frame",Parameter.Type.Frame,null, null));
        parameters.add(logging);

        Parameter CNFFile = new Parameter("CNFFile",Parameter.Type.OneOf,"none",
                "Specifies the form of the CNFFile for the clauses.\n"+
                        " None:         No output to a CNFFile.\n"+
                        " Symboltable:  Output using a symboltable, if available.\n"+
                        " Numbers:      Output using numbers for the predicates.");
        CNFFile.parameters = new Parameters("CNFFile");
        CNFFile.parameters.add(new Parameter("None",Parameter.Type.Label,null, null));
        CNFFile.parameters.add(new Parameter("Symboltable",Parameter.Type.Label,null, null));
        CNFFile.parameters.add(new Parameter("Numbers",Parameter.Type.File,null, null));
        parameters.add(CNFFile);

        Parameter statistics = new Parameter("statistics",Parameter.Type.OneOf,"none",
                "Specifies the target for statistics.\n"+
                        " None:  No statistics.\n"+
                        " Life:  Print the statistics text to System.out.\n"+
                        " File:  Print the statistics text to a File.\n"+
                        " Frame: Print the statistics text into a frame.");
        statistics.parameters = new Parameters("statistics");
        statistics.parameters.add(new Parameter("None",Parameter.Type.Label,null, null));
        statistics.parameters.add(new Parameter("Life",Parameter.Type.Label,null, null));
        statistics.parameters.add(new Parameter("File",Parameter.Type.File,null, null));
        statistics.parameters.add(new Parameter("Frame",Parameter.Type.Frame,null, null));
        parameters.add(statistics);

        parameters.add(new Parameter("TrackReasoning",Parameter.Type.Boolean,"false",false,
                "If true then the inference steps are tracked and verified."));
        return parameters;}

    public GlobalParameters(Parameters globalParams) {
        jobname = (String)(globalParams.parameters.get(0).value);
        jobDirectory = (Path)(globalParams.parameters.get(1).value);
        monitor = (String)(globalParams.parameters.get(2).value);
        logging = (String)(globalParams.parameters.get(3).value);
        cnfFile = (String)(globalParams.parameters.get(4).value);
        statistic = (String)(globalParams.parameters.get(5).value);
        trackReasoning = (Boolean)(globalParams.parameters.get(6).value);
    }

    /** parses the parameters and generates the internal data
     *
     * @param parameters the raw parameters (as strings)
     * @param errors    for error messages
     * @param warnings  for warnings
     */
    public GlobalParameters(ArrayList<HashMap<String,String>> parameters, StringBuilder errors, StringBuilder warnings) {
        String title = "Global Parameters: ";
        for(HashMap<String,String> pars :parameters) {
            for(Map.Entry<String,String> entry : pars.entrySet()) {
                String key = entry.getKey();
                String value = entry.getValue();
            switch(key) {
               // case "jobname": jobname = value; break;
                case "directory" :
                    directory = Utilities.pathWithHome(value);
                    if(!directory.toFile().exists()) {errors.append(title + "Directory " + directory.toString() + " does not exist.");}
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
                case "showClauses":
                    showClauses = value.equals("true");
                    if(!(value.equals("true") || value.equals("false"))) {
                        errors.append(title+"showClauses '" + value + "' is not 'true' or 'false'");}
                    break;
                case "cnfFile": cnfFile = value;
                    if(!(value.equals("none") || value.equals("symboltable") || value.equals("numbers"))) {
                        errors.append(title+"showClauses '" + value + "' is not 'none', 'symboltable' or 'numbers'");}
                    break;
                case "monitor":
                    monitor = value;
                    if(!(value.equals("life") || value.equals("file") || value.equals("frame"))) {
                        errors.append(title+"monitor '" + value + "' is not one of 'life','file', 'frame'");}
                    break;
                case "statistic":
                    statistic = value;
                    if(!(value.equals("life") || value.equals("none") || value.equals("text")|| value.equals("csv")|| value.equals("both"))) {
                        errors.append(title+"  statistic '" + value + "' is not one of 'life','none', 'text', 'csv', 'both'");}
                case "errors2File":  errors2File = true;
                    break;
                case "simplifier": simplifier = value.equals("true"); break;
                case "global": break;
                default: warnings.append(title+" Unknown parameter: " + key);
            }}}}


    /** returns a description of the parameters */
    public String toString() {
        return "Global Parameters:\n" +
                "  jobname             " + jobname +"\n"+
                "  homedirectory       " + homeDirectory + "\n"+
                "  directory:          " + directory+"\n"+
                "  jobdirecory:        " + ((jobDirectory == null) ? "null" : jobDirectory.toString()) + "\n"+
                "  parallel threads:   " + parallel +"\n" +
                "  logging:            " + logging +"\n" +
                "  errors2File         " + errors2File + "\n" +
                "  monitor:            " + (monitor == null ? "null" : monitor) +"\n"+
                "  showClauses:        " + showClauses+"\n"+
                "  cnfFile:            " + cnfFile+"\n"+
                "  statistic:          " + statistic + "\n" +
                "  trackReasoning:     " + trackReasoning;}

}

