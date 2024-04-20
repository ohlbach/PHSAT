package Management;

import java.io.File;
import java.io.PrintStream;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;

import static java.lang.Boolean.parseBoolean;

/** This class maintains the global parameters for controlling the QuSat system.
 */
public class GlobalParameters {

    /** the homedirectory. */
    public static Path homeDirectory = Path.of(System.getenv("USERPROFILE"),"QuSat");

    /** the parameters as specified by the GUI */
    public Parameters parameters;

    private static String jobNameDefault = "TestJob";
    /** the name of the current job.*/
    public String jobName;

    /** to distinguish different versions of the same Job.
     * Example: If a jobname is 'myJob' then the files are actually put into myJob&lt;Version&gt;
     */
    public int version = 0;

    /** the directory where to create the subdirectory for the generated files */
    public Path jobDirectory = homeDirectory;

    /**  @return the jobname (without version) */
    public String getJobName(){return jobName;}

    private static String loggingDefault = "life";
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

    private static boolean trackReasoningDefault = false;

    /** if false then many things are done destructively. Only the final results are needed. */
    public boolean trackReasoning = false;

    private static String monitorDefault = "frame";

    /** the monitor mode: life, file, frame. */
    public String monitor = null;

    private static String statisticDefault = "frame";

    /** specifies how the statistics are printed. */
    public String statistic = "life";

    public static void setDefaults(HashMap<String, ArrayList<String>> moduleValues) {
        ArrayList<String> globalDefaults = moduleValues.get("global");
        System.out.println("GL " + globalDefaults);
        if(globalDefaults == null) {return;}
        for(String line : globalDefaults) {
            String[] parts = line.split("\\s*=\\s*");
            if(parts.length != 2) {continue;}
            String variable = parts[0];
            String value = parts[1];
            switch(variable.toLowerCase()) {
                case "jobname": jobNameDefault = value; break;
                case "logging": loggingDefault = value; break;
                case "monitor": monitorDefault = value; break;
                case "statistic": statisticDefault = value; break;
                case "tracking": trackReasoningDefault = parseBoolean(value); break;
                case "directory": if(value.startsWith("home"))
                    homeDirectory = Path.of(System.getenv("USERPROFILE"),value.substring(5));
                    else homeDirectory = Path.of(value);
                break;
            }
            }}





    /**Creates a Parameters object with predefined set of global parameters.
     * - jobname (name of the job, without version)<br>
     * - jobDirectory<br>
     * - monitor<br>
     * - logging<br>
     * - CNFFile<br>
     * - statistics<br>
     * - trackReasoning
     *
     * @return The Parameters object containing the global parameters.
     */
    public static Parameters makeGlobalParameters() {
        Parameters parameters = new Parameters("Global Parameters");
        Parameter jobName = new Parameter("jobname",Parameter.Type.String, jobNameDefault,jobNameDefault,
                "Identifier for the job.");
        parameters.add(jobName);
        Parameter jobDirectory = new Parameter("jobdirectory",Parameter.Type.Directory, homeDirectory.toString(),homeDirectory,
                "Directory where to place the produced files.");
        jobDirectory.parser = (text,ignore) -> new File(text).getAbsolutePath();
        parameters.add(jobDirectory);

        Parameter monitor = new Parameter("monitor",Parameter.Type.OneOf,monitorDefault,
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
        
        Parameter logging = new Parameter("logging",Parameter.Type.OneOf,loggingDefault,
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

        Parameter statistics = new Parameter("statistics",Parameter.Type.OneOf,statisticDefault,
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

        parameters.add(new Parameter("TrackReasoning",Parameter.Type.Boolean,""+trackReasoningDefault,trackReasoningDefault,
                "If true then the inference steps are tracked and verified."));
        return parameters;}

    public GlobalParameters(Parameters globalParams) {
        jobName      = (String)(globalParams.parameters.get(0).value);
        jobDirectory = (Path)(globalParams.parameters.get(1).value);
        monitor      = (String)(globalParams.parameters.get(2).value);
        logging      = (String)(globalParams.parameters.get(3).value);
        cnfFile      = (String)(globalParams.parameters.get(4).value);
        statistic    = (String)(globalParams.parameters.get(5).value);
        trackReasoning = (Boolean)(globalParams.parameters.get(6).value);
        version      = findVersion();
        jobDirectory = Paths.get(jobDirectory.toString(),jobName+"_"+version);
    }

    private int findVersion() {
        int version = 0;
        for(File file: jobDirectory.toFile().listFiles()) {
            if(file.isDirectory() && file.getName().startsWith(jobName)) {
                String[] parts = file.getName().split("_",2);
                if(parts.length == 2) {
                    try{version = Math.max(version,Integer.parseInt(parts[1]));}catch(NumberFormatException e){}}}}
        return version+1;}






    /** returns a description of the parameters */
    public String toString() {
        return "Global Parameters:\n" +
                "  jobname             " + jobName +"\n"+
                "  version             " + version +"\n"+
                "  homedirectory       " + homeDirectory + "\n"+
                "  jobdirecory:        " + ((jobDirectory == null) ? "null" : jobDirectory.toString()) + "\n"+
                "  logging:            " + logging +"\n" +
                "  monitor:            " + (monitor == null ? "null" : monitor) +"\n"+
                "  showClauses:        " + showClauses+"\n"+
                "  cnfFile:            " + cnfFile+"\n"+
                "  statistic:          " + statistic + "\n" +
                "  trackReasoning:     " + trackReasoning;}

}

