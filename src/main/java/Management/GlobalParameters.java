package Management;

import Datastructures.ValueType;
import Management.GIU.OutputType;
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
    public static Parameters parameters = makeGlobalParameters();

    /** the default value for the logging: none, life, or file */
    private static OutputType loggingDefault = OutputType.LIFE;

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
    private static OutputType monitorTypeDefault = OutputType.LIFE;

    /** the default value for monitorSeparate. */
    private static boolean monitorSeparateDefault = false;

    /** if true then a separate monitor is opened for each problem. */
    public boolean monitorSeparate = false;

    /** the monitor */
    public Monitor monitor = null;

    /** specifies how the statistics are printed. */
    private static OutputType statisticPrintTypeDefault = OutputType.LIFE;

    /** specifies how the statistics are printed: none,life,file,frame. */
    public OutputType statisticPrintType = OutputType.LIFE;

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
            try{
                switch(variable.toLowerCase()) {
                    case "jobname":         jobNameDefault            = (String)parameters.getParameter("jobname").valueType.parseValue(value,errors);  break;
                    case "logging":         loggingDefault            = (OutputType) parameters.getParameter("logging").valueType.parseValue(value,errors);  break;
                    case "monitor":         monitorTypeDefault        = (OutputType) parameters.getParameter("monitor").valueType.parseValue(value,errors); break;
                    case "monitorseparate": monitorSeparateDefault    = (boolean) parameters.getParameter("monitorSeparate").valueType.parseValue(value,errors); break;
                    case "statistic":       statisticPrintTypeDefault = (OutputType) parameters.getParameter("statistics").valueType.parseValue(value,errors); break;
                    case "tracking":        trackReasoningDefault     = (boolean) parameters.getParameter("trackReasoning").valueType.parseValue(value,errors); break;
                    case "verify" :         verifyDefault             = (boolean) parameters.getParameter("verify").valueType.parseValue(value,errors); break;
                    case "directory": if(value.startsWith("home"))
                        homeDirectory = Path.of(System.getenv("USERPROFILE"),value.substring(5));
                        else homeDirectory = Path.of(value); break;
                    default: errors.append("Unknown parameter: ").append(variable).append("\n");}
                checkJobname(jobNameDefault,errors);}
            catch(NullPointerException ignore) {errors.append("Unknown parameter: ").append(variable).append("\n");}
            }}

    /** checks if the jobname ends with _&lt;number&gt; and in this case adds an error message.
     *
     * @param jobname the default jobname.
     * @param errors for appending an error message.
     * @result true if the name is okay.
     */
    protected static boolean checkJobname(String jobname, StringBuilder errors) {
        String[] parts = jobname.split("_");
        if(parts.length > 1) {
            try {Integer.parseInt(parts[1]);
                errors.append("Jobname ").append(jobname).append(" should not end with _<number>\n");
                return false;}
            catch(NumberFormatException ignore) {}}
        return true;}


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
        StringBuilder errors = new StringBuilder();
        Parameters parameters = new Parameters("Global Parameters");
        ValueType.Strings jobType = new ValueType.Strings();
        jobType.setCheck((jobname) -> {return checkJobname((String)jobname,errors);});
        Parameter jobName = new Parameter("jobname", Parameter.DisplayType.String, jobType,
                jobNameDefault, "Identifier for the job.\n"+ "The jobname must not end with '_'!");
        parameters.add(jobName);
        Parameter jobDirectory = new Parameter("jobdirectory", Parameter.DisplayType.Directory,
                new ValueType.Paths(),homeDirectory,
                """
                        Directory where to place a subdirectory to contain the produced files.
                        The actual subdirectoy is then <jobdirectory>/<jobname>_<version>, the version counting from 0.
                        The jobdirectory must exists, whereas the subdirectory will be created.""");
        parameters.add(jobDirectory);

        ValueType outputType = new ValueType.Enums(OutputType.class);
        Parameter monitor = new Parameter("monitor", Parameter.DisplayType.OneOf,outputType,monitorTypeDefault,
                """
                        Specifies the target for monitoring.
                         None:  No monitoring.
                         Life:  Print the monitor text to System.out.
                         File:  Print the monitor text to a File.
                         Frame: Print the monitor text into a frame.""");
        monitor.parameters = new Parameters("monitor");
        monitor.parameters.add(new Parameter("None", Parameter.DisplayType.Label,outputType, null,null));
        monitor.parameters.add(new Parameter("Life", Parameter.DisplayType.Label,outputType, null,null));
        monitor.parameters.add(new Parameter("File", Parameter.DisplayType.File,outputType, null,null));
        monitor.parameters.add(new Parameter("Frame", Parameter.DisplayType.Frame,outputType, null,null));
        parameters.add(monitor);

        parameters.add(new Parameter("monitorSeparate", Parameter.DisplayType.Boolean,
                new ValueType.Booleans(),monitorSeparateDefault,
                "If true then a separate monitor is opened for each problem."));
        
        Parameter logging = new Parameter("logging", Parameter.DisplayType.OneOf,outputType, loggingDefault,
                """
                        Specifies the target for logging.
                         None:  No logging.
                         Life:  Print the logging text to System.out.
                         File:  Print the logging text to a File.
                         Frame: Print the logging text into a frame.""");
        logging.parameters = new Parameters("logging");
        logging.parameters.add(new Parameter("None", Parameter.DisplayType.Label,outputType,null,null));
        logging.parameters.add(new Parameter("Life", Parameter.DisplayType.Label,outputType,null, null));
        logging.parameters.add(new Parameter("File", Parameter.DisplayType.File,outputType,null, null));
        logging.parameters.add(new Parameter("Frame", Parameter.DisplayType.Frame,outputType,null, null));
        parameters.add(logging);

        ValueType cnfType = new ValueType.Strings("None","Symboltable","Numbers");
        Parameter cnfFileSymbols = new Parameter("CNFFile", Parameter.DisplayType.OneOf,cnfType,"None",
                """
                        Specifies the form of the CNFFile for the clauses.
                         None:         No output to a CNFFile.
                         Symboltable:  Output using a symboltable, if available.
                         Numbers:      Output using numbers for the predicates.""");
        cnfFileSymbols.parameters = new Parameters("CNFFile");
        cnfFileSymbols.parameters.add(new Parameter("None", Parameter.DisplayType.Label,cnfType,null,null));
        cnfFileSymbols.parameters.add(new Parameter("Symboltable", Parameter.DisplayType.Label,cnfType,null,null));
        cnfFileSymbols.parameters.add(new Parameter("Numbers", Parameter.DisplayType.File,cnfType,null,null));
        parameters.add(cnfFileSymbols);

        Parameter statistics = new Parameter("statistics", Parameter.DisplayType.OneOf, outputType, statisticPrintTypeDefault,
                """
                        Specifies the target for statistics.
                         None:  No statistics.
                         Life:  Print the statistics text to System.out.
                         File:  Print the statistics text to a File.
                         Frame: Print the statistics text into a frame.""");
        statistics.parameters = new Parameters("statistics");
        statistics.parameters.add(new Parameter("None", Parameter.DisplayType.Label,outputType,null, null));
        statistics.parameters.add(new Parameter("Life", Parameter.DisplayType.Label,outputType,null, null));
        statistics.parameters.add(new Parameter("File", Parameter.DisplayType.File,outputType,null,null));
        statistics.parameters.add(new Parameter("Frame", Parameter.DisplayType.Frame,outputType,null,null));
        parameters.add(statistics);

        ValueType choiceType = new ValueType.Booleans();

        parameters.add(new Parameter("TrackReasoning", Parameter.DisplayType.Boolean,choiceType,trackReasoningDefault,
                "If true then the inference steps are tracked."));
        parameters.add(new Parameter("Verify", Parameter.DisplayType.Boolean,choiceType,verifyDefault,
                "If true then the inference steps are verified."));

        return parameters;}

    /** creates an instance of GlobalParameters based on the parameters provided by the GUI.
     *
     * @param globalParams the parameters provided by the GUI.
     */
    public GlobalParameters(Parameters globalParams) {
        int index = -1;
        jobName                = (String)     (globalParams.parameters.get(++index).value);
        jobDirectory           = (Path)       (globalParams.parameters.get(++index).value);
        extendJobDirectory();
        OutputType monitorType = (OutputType) (globalParams.parameters.get(++index).value);
        monitorSeparate        = (Boolean)    (globalParams.parameters.get(++index).value);
        OutputType logging     = (OutputType) (globalParams.parameters.get(++index).value);
        cnfFileSymbols         = (String)     (globalParams.parameters.get(++index).value);
        statisticPrintType     = (OutputType) (globalParams.parameters.get(++index).value);
        trackReasoning         = (Boolean)    (globalParams.parameters.get(++index).value);
        verify                 = (Boolean)    (globalParams.parameters.get(++index).value);
        if(verify) trackReasoning = true;
        jobStartTime = System.nanoTime();
        monitor = Monitor.getMonitor(jobName, monitorType,monitorSeparate,jobDirectory, jobStartTime);
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
    public PrintStream openLogging(OutputType logging) {
        PrintStream logstream = null;
        switch(logging) {
            case NONE: return null;
            case LIFE:  logstream = System.out; break;
            case FILE:
                try {
                    File jobdir = Paths.get(jobDirectory.toString(),"Logfile").toFile();
                    logstream = new PrintStream(new FileOutputStream(jobdir)); break;
                } catch (FileNotFoundException e) {e.printStackTrace(); System.exit(1);}
            case FRAME:
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

