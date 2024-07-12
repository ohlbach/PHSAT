package Management;

import Management.GIU.ScrollableFrame;
import Management.Monitor.Monitor;
import Management.Monitor.MonitorFrame;
import Utilities.Utilities;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.PrintStream;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Date;
import java.util.Objects;

import static java.lang.Boolean.parseBoolean;

/** This class maintains the global parameters for controlling the QuSat system.
 */
public class GlobalParameters {

    /** the homedirectory. */
    public static Path homeDirectory = Path.of(System.getenv("USERPROFILE"),"QuSat");

    /** the date when the job has been processed. */
    public Date jobDate = new Date();

    /** the default jobname */
    private static String jobNameDefault = "TestJob";

    /** the name of the current job, extended by _version.*/
    public String jobName;

    /** the directory where to create the subdirectory for the generated files. Default: homeDirectory
     * <br>
     * In order to distinguish different versions of the same job,
     * jobName and jobDirectory are extended by a version number (starting from 0).
     * Example: If a jobname is 'myJob' then the files are actually put into myJob_&lt;Version&gt;<br>
     * The extendJobDirectory-method analyses the directories and finds the first free directory.
     * */
    public Path jobDirectory = homeDirectory;

    /** the time (in ns) when the job started. */
    public long jobStartTime = System.nanoTime();

    /** the parameters as specified by the GUI */
    public Parameters parameters;

    /** the default value for the logging: none, life, or file */
    private static String loggingDefault = "life";

    /** for printing information about the working of the system. */
    public PrintStream logstream = null;

    /** if true then the clauses are printed to the logfile */
    public boolean showClauses = false;

    /** enables printing the clauses to a cnf-file */
    public String cnfFileSymbols = "none";

    /** the default value for trackReasoning */
    private static boolean trackReasoningDefault = false;

    /** If true then reasoning steps are tracked and can be verified.
     * <br>
     * If false then many things are done destructively. Only the final results are needed. */
    public boolean trackReasoning = false;

    /** if true then inference steps are verified */
    public static boolean verifyDefault = false;

    /** If true then inference steps are verified.
     * <br>
     * If verify is set to true then trackReasoning is also set to true.*/
    public boolean verify = false;

    /** the default value for the monitor type:  none, life, file, frame */
    private static String monitorTypeDefault = "frame";

    /** the default value for monitorSeparate. */
    private static boolean monitorSeparateDefault = false;

    /** if true then a separate monitor is opened for each problem. */
    public boolean monitorSeparate = false;

    /** the monitor */
    public Monitor monitor = null;

    /** specifies how the statistics are printed. */
    private static String statisticPrintTypeDefault = "life";

    /** specifies how the statistics are printed: none,life,file,frame. */
    public String statisticPrintType = "life";

    /** This method can be used to change the default values for various global parameters.
     * <br>
     * - jobname: any name without ending _&lt;number&gt;<br>
     * - logging: none,life,file,frame<br>
     * - monitor: none,life,file,frame<br>
     * - monitorSeparate: true,false<br>
     * - statistic: none,life,file,frame<br>
     * - tracking: true,false<br>
     * - verify: true,false<br>
     * - directory: either a full pathname, or a pathname starting with home/
     *
     * @param globalDefaults an ArrayList of strings: name = value.
     */
    public static void setDefaults(ArrayList<String> globalDefaults, StringBuilder errors) {
        if(globalDefaults == null) {return;}
        for(String line : globalDefaults) {
            String[] parts = line.split("\\s*=\\s*");
            if(parts.length != 2) {continue;}
            String variable = parts[0];
            String value = parts[1].trim();
            switch(variable.toLowerCase()) {
                case "jobname":         jobNameDefault            = value; checkJobname(value,errors); break;
                case "logging":         loggingDefault            = value; checkType("logging",value,errors); break;
                case "monitor":         monitorTypeDefault        = value; checkType("monitor",value,errors); break;
                case "monitorseparate": monitorSeparateDefault    = parseBoolean(value); checkBoolean("monitorSeparate",value,errors); break;
                case "statistic":       statisticPrintTypeDefault = value; checkType("statistic",value,errors); break;
                case "tracking":        trackReasoningDefault     = parseBoolean(value); checkBoolean("tracking",value,errors); break;
                case "verify" :         verifyDefault             = parseBoolean(value); checkBoolean("verify",value,errors); break;
                case "directory": if(value.startsWith("home"))
                    homeDirectory = Path.of(System.getenv("USERPROFILE"),value.substring(5));
                    else homeDirectory = Path.of(value); break;
                default: errors.append("Unknown parameter: ").append(variable);}
            }}

    /** checks if the jobname ends with _&lt;number&gt; and in this case adds an error message.
     *
     * @param jobname the default jobname.
     * @param errors for appending an error message.
     */
    protected static void checkJobname(String jobname, StringBuilder errors) {
        String[] parts = jobname.split("_");
        if(parts.length > 1) {
            try {Integer.parseInt(parts[1]);
                errors.append("Jobname ").append(jobname).append(" should not end with _<number>\n");}
            catch(NumberFormatException ignore) {}}}

    /** checks the value. It must be one of none,life,file,frame
     *
     * @param name a parameter name
     * @param value must be one of none,life,file,frame
     * @param errors for appending an error message.
     */
    protected static void checkType(String name, String value, StringBuilder errors) {
        if(!(value.equals("none") || value.equals("life") || value.equals("file") || value.equals("frame")))
            errors.append(name).append(" = ").append(value).append(" is not one of none,life,file,frame.\n");}


    /** checks the value. It must be true or false.
     *
     * @param name a parameter name
     * @param value must be one of none,life,file,frame
     * @param errors for appending an error message.
     */
    protected static void checkBoolean(String name, String value, StringBuilder errors) {
        if(!(value.equals("true") || value.equals("false")))
            errors.append(name).append(" = ").append(value).append(" is not one of true,false.\n");}



    /** Creates a Parameters object with predefined set of global parameters.
     * - jobname (name of the job, without version)<br>
     * - jobDirectory<br>
     * - monitor<br>
     * - monitorSeparate<br>
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
                """
                        Directory where to place a subdirectory to contain the produced files.
                        The actual subdirectoy is then <jobdirectory>/<jobname>_<version>, the version counting from 0.
                        The jobdirectory must exists, whereas the subdirectory will be created.""");
        jobDirectory.parser = (text,ignore) -> new File(text).getAbsolutePath();
        parameters.add(jobDirectory);

        Parameter monitor = new Parameter("monitor",Parameter.Type.OneOf, monitorTypeDefault,
                """
                        Specifies the target for monitoring.
                         None:  No monitoring.
                         Life:  Print the monitor text to System.out.
                         File:  Print the monitor text to a File.
                         Frame: Print the monitor text into a frame.""");
        monitor.parameters = new Parameters("monitor");
        monitor.parameters.add(new Parameter("None",Parameter.Type.Label,null, null));
        monitor.parameters.add(new Parameter("Life",Parameter.Type.Label,null, null));
        monitor.parameters.add(new Parameter("File",Parameter.Type.File,null, null));
        monitor.parameters.add(new Parameter("Frame",Parameter.Type.Frame,null, null));
        parameters.add(monitor);

        parameters.add(new Parameter("monitorSeparate",Parameter.Type.Boolean,""+monitorSeparateDefault,monitorSeparateDefault,
                "If true then a separate monitor is opened for each problem."));
        
        Parameter logging = new Parameter("logging",Parameter.Type.OneOf,loggingDefault,
                """
                        Specifies the target for logging.
                         None:  No logging.
                         Life:  Print the logging text to System.out.
                         File:  Print the logging text to a File.
                         Frame: Print the logging text into a frame.""");
        logging.parameters = new Parameters("logging");
        logging.parameters.add(new Parameter("None",Parameter.Type.Label,null, null));
        logging.parameters.add(new Parameter("Life",Parameter.Type.Label,null, null));
        logging.parameters.add(new Parameter("File",Parameter.Type.File,null, null));
        logging.parameters.add(new Parameter("Frame",Parameter.Type.Frame,null, null));
        parameters.add(logging);

        Parameter cnfFileSymbols = new Parameter("CNFFile",Parameter.Type.OneOf,"none",
                """
                        Specifies the form of the CNFFile for the clauses.
                         None:         No output to a CNFFile.
                         Symboltable:  Output using a symboltable, if available.
                         Numbers:      Output using numbers for the predicates.""");
        cnfFileSymbols.parameters = new Parameters("CNFFile");
        cnfFileSymbols.parameters.add(new Parameter("None",Parameter.Type.Label,null, null));
        cnfFileSymbols.parameters.add(new Parameter("Symboltable",Parameter.Type.Label,null, null));
        cnfFileSymbols.parameters.add(new Parameter("Numbers",Parameter.Type.File,null, null));
        parameters.add(cnfFileSymbols);

        Parameter statistics = new Parameter("statistics",Parameter.Type.OneOf, statisticPrintTypeDefault,
                """
                        Specifies the target for statistics.
                         None:  No statistics.
                         Life:  Print the statistics text to System.out.
                         File:  Print the statistics text to a File.
                         Frame: Print the statistics text into a frame.""");
        statistics.parameters = new Parameters("statistics");
        statistics.parameters.add(new Parameter("None",Parameter.Type.Label,null, null));
        statistics.parameters.add(new Parameter("Life",Parameter.Type.Label,null, null));
        statistics.parameters.add(new Parameter("File",Parameter.Type.File,null, null));
        statistics.parameters.add(new Parameter("Frame",Parameter.Type.Frame,null, null));
        parameters.add(statistics);

        parameters.add(new Parameter("TrackReasoning",Parameter.Type.Boolean,""+trackReasoningDefault,trackReasoningDefault,
                "If true then the inference steps are tracked."));
        parameters.add(new Parameter("Verify",Parameter.Type.Boolean,""+verifyDefault,verifyDefault,
                "If true then the inference steps are verified."));
        return parameters;}

    /** creates an instance of GlobalParameters based on the parameters provided by the GUI.
     *
     * @param globalParams the parameters provided by the GUI.
     */
    public GlobalParameters(Parameters globalParams) {
        int index = -1;
        jobName            = (String) (globalParams.parameters.get(++index).value);
        jobDirectory       = (Path)   (globalParams.parameters.get(++index).value);
        extendJobDirectory();
        String monitorType = (String) (globalParams.parameters.get(++index).value);
        monitorSeparate    = (Boolean)(globalParams.parameters.get(++index).value);
        String logging     = (String) (globalParams.parameters.get(++index).value);
        cnfFileSymbols     = (String) (globalParams.parameters.get(++index).value);
        statisticPrintType = (String) (globalParams.parameters.get(++index).value);
        trackReasoning     = (Boolean)(globalParams.parameters.get(++index).value);
        verify             = (Boolean)(globalParams.parameters.get(++index).value);
        if(verify) trackReasoning = true;
        jobStartTime = System.nanoTime();
        monitor = Monitor.getMonitor(jobName, monitorType.toLowerCase(),monitorSeparate,jobDirectory, jobStartTime);
        logstream = openLogging(logging);
    }


    /** A subdirectory of jobDirectory is determined as follows: jobDirectory/jobName_version
     * <br>
     * The jobDirectory is checked for the first subdirectory jobDirectory/jobName_number,
     * and the version is number + 1.<br>
     * The subdirectory is new and will be created.<br>
     * The jobName is extended by _version (counting from 0).
     */
    protected void extendJobDirectory() {
        int version = -1;
        for(File file: Objects.requireNonNull(jobDirectory.toFile().listFiles())) {
            if(file.isDirectory() && file.getName().startsWith(jobName)) {
                String[] parts = file.getName().split("_",2);
                if(parts.length == 2) {
                    try{version = Math.max(version,Integer.parseInt(parts[1]));}catch(NumberFormatException ignore){}}}}
        jobName += "_"+ (version+1);
        jobDirectory = Path.of(jobDirectory.toString(),jobName);
        jobDirectory.toFile().mkdirs();}


    /**Opens logging for the QuSat job using globalParameters.logging.
     * <br>
     * Depending on this either no logging at all is opened, or a file is opened, or System.out is used, or a frame is opened.
     */
    public PrintStream openLogging(String logging) {
        PrintStream logstream = null;
        switch(logging.toLowerCase()) {
            case "none": return null;
            case "life":  logstream = System.out; break;
            case "file":
                try {
                    File jobdir = Paths.get(jobDirectory.toString(),"Logfile").toFile();
                    logstream = new PrintStream(new FileOutputStream(jobdir)); break;
                } catch (FileNotFoundException e) {e.printStackTrace(); System.exit(1);}
            case "frame":
                int[] sizes = MonitorFrame.sizes();
                logstream = ScrollableFrame.getPrintStream(sizes[0], sizes[1], sizes[2], sizes[3],"Logging");}
        if(logstream != null) {
            logstream.println("Starting Logging for QuSat job " + jobName + " at " + jobDate);
            return logstream;}
        return null;}

    /** A final message is printed to the logstream and the logstream is closed.
     *
     */
    public void closeLogging() {
        if(logstream == null) return;
        String duration = Utilities.duration(System.nanoTime()- jobStartTime);
        logstream.println("Ending Logging for QuSat job " + jobName +" Duration: " + duration);
        logstream.close();}



    /** returns a description of the parameters
     *
     * @return a description of the parameters */
    public String toString() {
        return "Global Parameters:\n" +
                "  jobname             " + jobName +"\n"+
                "  homedirectory       " + homeDirectory + "\n"+
                "  jobdirecory:        " + ((jobDirectory == null) ? "null" : jobDirectory.toString()) + "\n"+
                ((logstream == null) ? "" :
                "  logging:            " + logstream + "\n")+
                "  monitor:            " + (monitor == null ? "null" : monitor) +"\n"+
                "  monitorSeparate:    " + monitorSeparate + "\n"+
                "  showClauses:        " + showClauses+"\n"+
                "  cnfFile:            " + cnfFileSymbols +"\n"+
                "  statistic:          " + statisticPrintType + "\n" +
                "  trackReasoning:     " + trackReasoning+"\n"+
                "  verify:             " + verify;}

}

