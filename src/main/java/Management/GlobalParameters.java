package Management;

import java.io.File;
import java.io.PrintStream;
import java.nio.file.Path;
import java.nio.file.Paths;

/** This class maintains the global parameters for controlling the QuSat system.
 */
public class GlobalParameters {

    /** the homedirectory. */
    public static Path homeDirectory = Path.of(System.getenv("USERPROFILE"),"QuSat");

    /** the parameters as specified by the GUI */
    public Parameters parameters;

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
        Parameter jobName = new Parameter("jobname",Parameter.Type.String, "TestJob","TestJob",
                "Identifier for the job\n" +
                        "It is also used as directoryname.");
        parameters.add(jobName);
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

