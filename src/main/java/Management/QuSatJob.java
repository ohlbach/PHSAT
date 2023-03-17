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

public class QuSatJob {

    public Date jobDate = new Date();

    public KVParser kvParser;

    public long startTime;

    public long endTime;

    public GlobalParameters globalParameters;

    public ArrayList<ProblemSupervisor> problemSupervisors = new ArrayList<>();

    public ProblemDistributor problemDistributor;

    public Path jobDirectory;

    public QuSatJob(KVParser kvParser) {
        startTime = System.nanoTime();
        this.kvParser = kvParser;}

    public void solveProblems() {
        startTime = System.nanoTime();
        StringBuilder errors   = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        globalParameters= new GlobalParameters(kvParser.get("global"),errors,warnings);
        ArrayList<ProblemGenerator> generators = ProblemGenerator.makeProblemGenerators(kvParser.get("generator"),errors,warnings);
        ArrayList<Solver> solvers = Solver.makeSolvers(kvParser.get("solver"),errors,warnings);
        if(warnings.length() > 0)  System.out.println(warnings);
        if(errors.length() > 0) {
            System.out.println(errors);
            System.out.println("QuSat solver stops!");
            return;}
        prepareSystem();
        for(ProblemGenerator problemGenerator : generators) {
            problemSupervisors.add(new ProblemSupervisor(globalParameters,problemGenerator,solvers));}
        problemDistributor = new ProblemDistributor(this,globalParameters,problemSupervisors);
        problemDistributor.solveProblems();
        endTime = System.nanoTime();
        analyseResults();
        finalizeSystem();
    }

    private void prepareSystem() {
        if((globalParameters.monitor != null && globalParameters.monitor.equals("file"))  ||
                globalParameters.logging.equals("file")){
            jobDirectory = makeJobDirectory(globalParameters.directory, globalParameters.jobname);
        }
        switch(globalParameters.logging) {
            case "file":
                File file = Paths.get(jobDirectory.toString(),"logging.txt").toFile();
                try{globalParameters.logstream = new PrintStream(file);
                    globalParameters.logFile = file.getAbsolutePath();
                    globalParameters.logstream.println("QuSat Job " + globalParameters.jobname + " logfile @ " + (new Date()));}
                catch(FileNotFoundException exception) {
                    System.out.println("Cannot open logfile: "+ file.getAbsolutePath());
                    System.exit(0);}
            break;
            case "none": globalParameters.logstream = null;       globalParameters.logFile = "none";       break;
            case "life": globalParameters.logstream = System.out; globalParameters.logFile = "System.out"; break;}
    }

    public void finalizeSystem() {
        for(Monitor monitor : monitors) monitor.flush(true); // close the files.
        if(globalParameters.logstream != null && globalParameters.logstream != System.out)
            globalParameters.logstream.close();
    }

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
        ++problemCounter;
        Monitor monitor = null;
        switch (globalParameters.monitor) {
            case "life":    monitor = new MonitorLife(problemId, startTime); break;
            case "file":    File file = Paths.get(jobDirectory.toString(),problemId+"_monitor.txt").toFile();
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
        quSatJob.prepareSystem();
        quSatJob.printlog("TEESTTT");
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
    }


}
/*
    String[] list = dir.list(new FilenameFilter() {
        @Override
        public boolean accept(File dir, String name) {
            return name.matches("[0-9]+");
        }

 */