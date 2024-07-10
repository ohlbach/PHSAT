package Management;

import Datastructures.ClauseList;
import Management.GIU.ScrollableFrame;
import Management.Monitor.Monitor;
import Management.Monitor.MonitorFrame;
import ProblemGenerators.ProblemGenerator;
import Solvers.Normalizer.Normalizer;
import Solvers.Solver;
import Utilities.Utilities;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.PrintStream;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Date;

/** The class initiates the solution of QuSat problems.
 * <br>
 * Several QuSat-problems can be processed, one by one. <br>
 * Each QuSat-problem can be processed by several solvers in parallel. <br>
 * They may exchange information, such as derived unit clauses.
 */
public class QuSatJob {

    /** the global control parameters. */
    public GlobalParameters globalParameters;

    Monitor monitor;

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

    /** stores clauses and a literal index.
     * The datastructures can be created once and then reused for a sequence of problems.*/
    public ClauseList clauseList;

    /** is used to normalize and simplifiy the input clauses.
     * The datastructures can be created once and then reused for a sequence of problems.*/
    public Normalizer normalizer;

    /** the list of problem supervisors, one for each problem. */
    public ArrayList<ProblemSupervisor> problemSupervisors = new ArrayList<>();



    /** QuSatJob class represents a job in the QuSat solver system.
     * <br>
     * It turns the paramters provided by JFrame into internal datastructures:<br>
     * - global parameters <br>
     * - parameters for generating a sequence of QSat problems generate Generators<br>
     * - parameters for generating a set of solvers generate Solvers. <br>
     * - if desired a monitor is generated. <br>
     * The solvers can work in parallel and exchange intermediate results.
     *
     * @param globalParams         global parameters.
     * @param generatorParameters  parameters for the clause generators.
     * @param solverParameters     parameter for the solvers.
     */
    public QuSatJob(Parameters globalParams, ArrayList<Parameters> generatorParameters, ArrayList<Parameters> solverParameters) {
        globalParameters = new GlobalParameters(globalParams);
        makeJobDirectory();
        generators = ProblemGenerator.makeGenerators(generatorParameters);
        solvers    = Solver.makeSolvers(solverParameters);
        startTime  = System.nanoTime();
        monitor = Monitor.getMonitor(globalParameters.jobName, globalParameters.monitor.toLowerCase(),
                globalParameters.monitorSeparate,globalParameters.jobDirectory,startTime);}

    /** solves the QuSat-problems.
     *
     * The generatorParams triggers the construction of one or more clause generators.<br>
     * Each clause generator represents a QuSat problem. The clauses themselves are generated in the ProblemSupervisors.<br>
     * The ProblemSupervisors then activate one or more Solvers which work in parallel at the problems.
     */
    public void solveProblems() {
        clauseList = new ClauseList(globalParameters.trackReasoning,globalParameters.verify,monitor);
        normalizer = new Normalizer(clauseList, globalParameters.trackReasoning,globalParameters.verify,monitor);
        openLogging(globalParameters);
        for(ProblemGenerator problemGenerator : generators) {
            ProblemSupervisor problemSupervisor = new ProblemSupervisor(this, globalParameters,problemGenerator,solvers);
            problemSupervisors.add(problemSupervisor);
            problemSupervisor.solveProblem();}
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
        switch(globalParameters.logging.toLowerCase()) {
            case "none": return;
            case "life":  logstream = System.out; break;
            case "file":
                try {
                    File jobdir = Paths.get(globalParameters.jobDirectory.toString(),"Logfile").toFile();
                    logstream = new PrintStream(new FileOutputStream(jobdir)); break;
                } catch (FileNotFoundException e) {e.printStackTrace(); System.exit(1);}
            case "frame":
                int[] sizes = MonitorFrame.sizes();
                 logstream = ScrollableFrame.getPrintStream(sizes[0], sizes[1], sizes[2], sizes[3],"Logging");}
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
     * @return the created job directory
     */
    public File makeJobDirectory() {
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

/*
    public void finalizeSystem() {
        for(Monitor monitor : monitors) monitor.flush(true); // close the files.
        printlog("QuSat job" + globalParameters.jobName + " finished.");
        if(globalParameters.logstream != null && globalParameters.logstream != System.out)
            globalParameters.logstream.close();}
*/
    /** counts the problems */
    int problemCounter = 0;





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
