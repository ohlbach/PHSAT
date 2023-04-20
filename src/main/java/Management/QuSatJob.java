package Management;

import Management.Monitor.Monitor;
import Management.Monitor.MonitorFile;
import Management.Monitor.MonitorFrame;
import Management.Monitor.MonitorLife;
import ProblemGenerators.ProblemGenerator;
import Solvers.Solver;
import Utilities.KVParser;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintStream;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Date;

import static org.apache.commons.lang3.StringUtils.split;

/** The class initiates the solution of QuSat problems.
 * <br>
 * Several QuSat-problems can be processed, one by one. <br>
 * Each QuSat-problem can be processed by several solvers in parallel. <br>
 * They may exchange information, such as derived unit clauses.
 */
public class QuSatJob {

    /** the date when the job has been processed. */
    public Date jobDate = new Date();

    /** the start time in nanoseconds. */
    public long startTime;

    /** the end time in nanoseconds. */
    public long endTime;

    /** key-value parser for parsing the problem and solver parameters. */
    public KVParser kvParser;

    /** the global control parameters. */
    public GlobalParameters globalParameters;

    /** the list of problem supervisors,one for each problem. */
    public ArrayList<ProblemSupervisor> problemSupervisors = new ArrayList<>();

    /** the class which distributes the problems to the problem supervisors. */
    public ProblemDistributor problemDistributor;

    /** creates the QuSatJob.
     *
     * @param kvParser the key-value parser which has already parsed the parameters.
     */
    public QuSatJob(KVParser kvParser) {
        this.kvParser = kvParser;}

    /** solves the QuSat-problems.
     */
    public void solveProblems() {
        startTime = System.nanoTime();
        StringBuilder errors   = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        globalParameters= new GlobalParameters(kvParser.get("global"),errors,warnings);
        ArrayList<ProblemGenerator> generators = ProblemGenerator.makeProblemGenerators(kvParser.get("problem"),errors,warnings);
        ArrayList<Solver> solvers = Solver.makeSolvers(kvParser.get("solver"),errors,warnings);
        if(warnings.length() > 0)  System.out.println(warnings);
        if(errors.length() > 0) {
            System.out.println(errors);
            System.out.println("QuSat solver stops!");
            return;}
        prepareMonitorAndLogstream();
        for(ProblemGenerator problemGenerator : generators) {
            problemSupervisors.add(new ProblemSupervisor(this,globalParameters,problemGenerator,solvers));}
        problemDistributor = new ProblemDistributor(this,globalParameters,problemSupervisors);
        problemDistributor.solveProblems();
        endTime = System.nanoTime();
        analyseResults();
        finalizeSystem();
    }

    /** pepares the monitor and the logstream
     */
    private void prepareMonitorAndLogstream() {
        if((globalParameters.monitor != null && globalParameters.monitor.equals("file"))  ||
                globalParameters.logging.equals("file") || !globalParameters.cnfFile.equals("none") ||
                !(globalParameters.statistic.equals("none") || globalParameters.statistic.equals("text"))){
            globalParameters.jobDirectory = makeJobDirectory(globalParameters.directory, globalParameters.jobname);
        }

        switch(globalParameters.logging) {
            case "file":
                File file = Paths.get(globalParameters.jobDirectory.toString(),"logging.txt").toFile();
                try{globalParameters.logstream = new PrintStream(file);
                    globalParameters.logFile = file.getAbsolutePath();
                    globalParameters.logstream.println("QuSat Job " + globalParameters.jobname + " logfile @ " + (new Date()));}
                catch(FileNotFoundException exception) {
                    System.out.println("Cannot open logfile: "+ file.getAbsolutePath());
                    System.exit(0);}
            break;
            case "none": globalParameters.logstream = null;       globalParameters.logFile = "none";       break;
            case "life": globalParameters.logstream = System.out; globalParameters.logFile = "System.out"; break;}
        if(globalParameters.logstream != null)
            globalParameters.logstream.println(jobDate.toString() + ": Starting QuSat-job " + globalParameters.jobname);
    }

    public void finalizeSystem() {
        for(Monitor monitor : monitors) monitor.flush(true); // close the files.
        printlog("QuSat job" + globalParameters.jobname + " finished.");
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
        double time = (double)(System.nanoTime() - startTime)/1000.0;
        globalParameters.logstream.print(time+" μs: ");
        globalParameters.logstream.println(message);
    }

    public static void main(String[] args) {
        QuSatJob quSatJob = new QuSatJob(null);
        quSatJob.globalParameters = new GlobalParameters();
        quSatJob.globalParameters.monitor = "life";
        quSatJob.globalParameters.jobname = "MyJob";
        quSatJob.globalParameters.logging = "life";
        quSatJob.globalParameters.directory = Utilities.Utilities.pathWithHome("home/TEST");
        quSatJob.prepareMonitorAndLogstream();
        System.out.println(quSatJob.globalParameters.toString());

        //Path path = Utilities.pathWithHome("home/TEST");
        //quSatJob.jobDirectory = makeJobDirectory(Utilities.pathWithHome("home/TEST"), quSatJob.globalParameters.jobname);

        /*Monitor monitor1 = quSatJob.getMonitor("TestProblem1");
        quSatJob.startTime = System.nanoTime();
        monitor1.println("Simplifier","hahahaha", "flkgdflkgjd");
        monitor1.println("equviv","jajajaja", "GGGGGGGGG");
        Monitor monitor2 = quSatJob.getMonitor("TestProblem2");
        monitor2.print("Simplifier 2 ","hahahaha", "HHHHHHHHH");
        monitor2.print("Simplifier 3 ","xxxxx","XXXXXXXX");
        monitor2.print("equviv 2","jajajaja","YYYYYYYY");
        for(int i = 0; i < 10 ; ++i) monitor2.println("III ", ""+i);

        //monitor1.flush(true);
        //monitor2.flush(true);
        System.out.println(monitor1.toString());
        */

}
    protected void analyseResults() {
        ProblemSupervisor.printStatistics(globalParameters,problemSupervisors);

    }


}
