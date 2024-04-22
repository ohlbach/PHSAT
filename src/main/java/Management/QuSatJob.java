package Management;

import Management.GIU.ScrollableFrame;
import Management.Monitor.Monitor;
import Management.Monitor.MonitorFile;
import Management.Monitor.MonitorFrame;
import Management.Monitor.MonitorLife;
import ProblemGenerators.ProblemGenerator;
import Solvers.Solver;
import Utilities.Utilities;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.PrintStream;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;

import static org.apache.commons.lang3.StringUtils.split;

/** The class initiates the solution of QuSat problems.
 * <br>
 * Several QuSat-problems can be processed, one by one. <br>
 * Each QuSat-problem can be processed by several solvers in parallel. <br>
 * They may exchange information, such as derived unit clauses.
 */
public class QuSatJob {

    /** the global control parameters. */
    public GlobalParameters globalParameters;

    /** the list of parameters for the clause generators */
    ArrayList<Parameters> generatorParams;

    /** the list of parameters for the QuSat Solvers */
    ArrayList<Parameters> solverParams;

    /** the date when the job has been processed. */
    public Date jobDate = new Date();

    /** the start time in nanoseconds. */
    public long startTime;

    /** the end time in nanoseconds. */
    public long endTime;

    ArrayList<ProblemGenerator> generators = null;
    ArrayList<Solver> solvers = null;

    private static int frameWidth = 500;
    private static int frameHight = 400;
    private static int offsetX = 100;
    private static int offsetY = 100;



    /** the list of problem supervisors,one for each problem. */
    public ArrayList<ProblemSupervisor> problemSupervisors = new ArrayList<>();

    /**
     * Sets the default values for the log-frame based on the "logframe" entry of the hashmap
     *
     * @param moduleValues the hashmap containing the module parameter values
     */
    public static void setDefaults(HashMap<String, ArrayList<String>> moduleValues) {
        ArrayList<String> globalDefaults = moduleValues.get("logframe");
        if(globalDefaults == null) {return;}
        try{
        for(String line : globalDefaults) {
            String[] parts = line.split("\\s*=\\s*");
            if(parts.length != 2) {continue;}
            String variable = parts[0];
            String value = parts[1];
            switch(variable.toLowerCase()) {
                case "width":   frameWidth = Integer.parseInt(value); break;
                case "hight":   frameHight = Integer.parseInt(value); break;
                case "offsetx": offsetX    = Integer.parseInt(value); break;
                case "offsety": offsetY    = Integer.parseInt(value); break;
            }}}
        catch(NumberFormatException e) {
            System.err.println("Error parsing defaults for LogFrame:\n" + e);
            System.exit(1);}}


    /**QuSatJob class represents a job in the QuSat solver system.
     *
     * It just stores the parameters as specified in the Frame.
     *
     * @param globalParameters     global parameters.
     * @param generatorParameters  parameters for the clause generators.
     * @param solverParameters     parameter for the solvers.
     */
    public QuSatJob(Parameters globalParameters, ArrayList<Parameters> generatorParameters, ArrayList<Parameters> solverParameters) {
        this.globalParameters = new GlobalParameters(globalParameters);
        this.generatorParams  = generatorParameters;
        this.solverParams     = solverParameters;
        makeJobDirectory(this.globalParameters);
    }

    /** solves the QuSat-problems.
     *
     * The generatorParams tiggers the construction of one or more clause generators.<br>
     * Each clause generator represents a QuSat problem. The clauses themselves are generated in the ProblemSupervisors.<br>
     * The ProblemSupervisors then activate one or more Solvers which work in parallel at the problems.
     */
    public void solveProblems() {
        startTime = System.nanoTime();
        generators = ProblemGenerator.makeGenerators(generatorParams);
        solvers = Solver.makeSolvers(solverParams);
       // System.out.println(toString());System.exit(0);
        if(solvers.isEmpty()) {
            System.err.println("System Error: No solver found");
            new Exception().printStackTrace();
            System.exit(1);}
        openLogging(globalParameters);
        for(ProblemGenerator problemGenerator : generators) {
            problemSupervisors.add(new ProblemSupervisor(this, globalParameters,problemGenerator,solvers));}
        for(ProblemSupervisor problemSupervisor : problemSupervisors) {problemSupervisor.solveProblem();}
        //analyseResults();
        //finalizeSystem();
        endTime = System.nanoTime();
        closeLogging(globalParameters);
    }

    /**Opens logging for the QuSat job using globalParameters.logging.
     *
     * Depending on this either no logging at all is opened, or a file is opened, or System.out is used, or a frame is opened.
     *
     * @param globalParameters the global parameters for the job
     */
    private void openLogging(GlobalParameters globalParameters) {
        PrintStream logstream = null;
        System.out.println("LOGGING");
        switch(globalParameters.logging) {
            case "None": return;
            case "Life":  logstream = System.out; break;
            case "File":
                try {
                    File jobdir = Paths.get(globalParameters.jobDirectory.toString(),"Logfile").toFile();
                    logstream = new PrintStream(new FileOutputStream(jobdir)); break;
                } catch (FileNotFoundException e) {e.printStackTrace(); System.exit(1);}
            case "Frame":
                 logstream = ScrollableFrame.getPrintStream(frameWidth, frameHight, offsetX, offsetY,"Logfile");}
        if(logstream != null) {
            logstream.println("Starting Logging for QuSat job " + globalParameters.jobName + "_" + globalParameters.version+
                    " at " + jobDate);
            globalParameters.logstream = logstream;}}

    /** A final message is printed to the logstream and the logstream is closed.
     *
     * @param globalParameters the global parameters.
     */
    public void closeLogging(GlobalParameters globalParameters) {
        PrintStream logstream = globalParameters.logstream;
        if(logstream == null) return;
        String duration = Utilities.duration(endTime-startTime);
        logstream.println("Ending Logging for QuSat job " + globalParameters.jobName() +" Duration: " + duration);
        logstream.close();}

    /** Creates a job directory using the provided global parameters.
     *
     * If this directory already exists, it is used as it is.<br>
     * All generated files are put into this directory.
     *
     * @param globalParameters the global parameters for the job
     * @return the created job directory
     */
    public File makeJobDirectory(GlobalParameters globalParameters) {
        File jobDir = globalParameters.jobDirectory.toFile();
        if(!jobDir.exists()) jobDir.mkdirs();
        return jobDir;}

    public String toString() {
        StringBuilder st = new StringBuilder();
        st.append("QuSatJob:   ").append(globalParameters.jobName()).append("\n");
        st.append("Start Time: ").append(jobDate).append("\n");
        st.append(globalParameters.toString()).append("\n");
        if(generators != null) {
            for(ProblemGenerator generator : generators) st.append(generator.toString()).append("\n");}
        if(solvers != null) {
            for (Solver solver : solvers) st.append(solver.toString()).append("\n");}
        return st.toString();
    }


    public void finalizeSystem() {
        for(Monitor monitor : monitors) monitor.flush(true); // close the files.
        printlog("QuSat job" + globalParameters.jobName + " finished.");
        if(globalParameters.logstream != null && globalParameters.logstream != System.out)
            globalParameters.logstream.close();}

    /** counts the problems */
    int problemCounter = 0;

    /** collects the monitors */
    ArrayList<Monitor> monitors = new ArrayList<>();

    /** gets a monitor, dependent of the globalParameters.monitor value.
     * A 'life' monitor prints to System.out.<br>
     * A 'file' monitor prints to a file 'problemId'_monitor.txt.<br>.
     * A 'frame' monitors prints to a newly generated frame.
     *
     * @param problemId  the identifier of the problem.
     * @return a monitor.
     */
    public Monitor getMonitor(String problemId) {
        if(globalParameters.monitor == null) return null;
        ++problemCounter;
        Monitor monitor = null;
        switch (globalParameters.monitor) {
            case "life":    monitor = new MonitorLife(problemId, startTime); break;
            case "file":    File file = Paths.get(globalParameters.jobDirectory.toString(),problemId+"_monitor.txt").toFile();
                            monitor =  new MonitorFile(problemId, startTime, file); break;
            case "frame":   monitor =  new MonitorFrame(problemId, startTime, 1000,1000,
                    100*problemCounter,100*problemCounter);}
        if(monitor != null) monitors.add(monitor);
        return monitor;}

    /** creates a new directory in the basicDirectory.
     * The name of the new directory is basicDirectory/'jobname'_'number'
     * where the number starts with 1 and is increased every time the method is called.
     *
     * @param basicDirectory  a directory.
     * @param jobname         the name of the job.
     * @return                the path of the newly created directory.
     */
    public static Path makeJobDirectory(Path basicDirectory, String jobname) {
        File directory = basicDirectory.toFile();
        File [] files = directory.listFiles((dir, name) -> name.contains(jobname));
        Path path;
        if(files == null) path = Paths.get(basicDirectory.toString(),jobname+"_1");
        else{int version = 1;
            for(File file : files) {
                String[] parts = split(file.getName(),"_");
                if(parts.length > 1) version = Math.max(version,Integer.parseInt(parts[1]));}
            version++;
            path = Paths.get(basicDirectory.toString(),jobname+"_"+version);}
        path.toFile().mkdirs();
        return path;}

    /** prints the elapsed time followed by the message to globalParameters.logstream.
     *
     * @param message a string to be printed.
     */
    public void printlog(String message) {
        if(globalParameters.logstream == null) return;
        globalParameters.logstream.print(Utilities.duration(System.nanoTime() - startTime) + " ");
        globalParameters.logstream.println(message);
    }

    protected void analyseResults() {
        ProblemSupervisor.printStatistics(globalParameters,problemSupervisors);

    }


}
