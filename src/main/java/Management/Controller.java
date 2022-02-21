package Management;

import Datastructures.Statistics.Statistic;
import Management.Monitor.Monitor;
import Management.Monitor.MonitorLife;
import Utilities.Utilities;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintStream;
import java.nio.file.Paths;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;

/**
 * Created by Ohlbach on 03.09.2018.<br>
 *
 * This class controls the processing of the problems. <br>
 * It <br>
 * - analyses the input parameters <br>
 * - reads or generates the QUSAT-problems<br>
 * - distributes them over several threads <br>
 * - activates the solvers <br>
 * - collects the results and statistics
 */
public class Controller {
    public String jobname;

    public final GlobalParameters                  globalParameters;
    public final HashMap<String,Object>            initializeParameters;
    public final ArrayList<HashMap<String,Object>> problemParameters;
    public final ArrayList<HashMap<String,Object>> solverParameters;

    public ArrayList<ProblemSupervisor> problemSupervisors = null;
    public Thread[] threads = null;
    public final Monitor errors;
    public final Monitor warnings;


    /** generates a new controller
     *
     * @param globalParameters   for global control
     * @param problemParameters  the problem specifications
     * @param solverParameters   the solver specifications
     */
    public Controller(String jobname,
                      GlobalParameters globalParameters,
                      HashMap<String,Object> initializeParameters,
                      ArrayList<HashMap<String,Object>> problemParameters,
                      ArrayList<HashMap<String,Object>> solverParameters,
                      Monitor errors, Monitor warnings) {
        this.jobname = jobname;
        this.globalParameters  = globalParameters;
        this.initializeParameters = initializeParameters;
        this.problemParameters = problemParameters;
        this.solverParameters  = solverParameters;
        this.errors            = errors;
        this.warnings          = warnings;}


    /** controls the solution of the problems.
     * A ProblemSupervisor is generated for each problem.
     * The problemSupervisors are distributed among some threads.
     * Each problemSupervisor generates the problem and invokes the different solvers.
     * The results are printed to the various streams.
     */
    public void solveProblems() {
        globalParameters.logstream.println("Starting job " + jobname + " at " + LocalDateTime.now());
        problemSupervisors = new ArrayList<>();
        for (HashMap<String, Object> parameters : problemParameters) {
            problemSupervisors.add(
                    new ProblemSupervisor(this, globalParameters, parameters, solverParameters));}
        long start = System.nanoTime();
        distributeProblems();
        errors.flush(true);
        warnings.flush(true);
        long end = System.nanoTime(); // only the elapsed solution time is reported.
        reportResults();
        globalParameters.logstream.println("Ending job   " + jobname + " at " + LocalDateTime.now());
        globalParameters.logstream.println("Elapsed time" + (float)(end-start)/1000.0 + " ms");
        globalParameters.logstream.close();
        }


    /** distributes the problems to different threads.
     */
    private void distributeProblems(){
        int nthreads = globalParameters.parallel; // parallel = 5 means: 5 threads work on the problems in parallel
        if(nthreads <= 1) {
            Thread.currentThread().setName("T0");
            solveProblems(0,problemSupervisors.size()); // sequential processing
            return;}
        int nproblems = problemSupervisors.size();
        int groupSize = nproblems / nthreads;
        if(nproblems % nthreads != 0) {++groupSize;}
        int groupsize = groupSize;
        threads = new Thread[nthreads];
        int group = -1;
        for(int thread = 0; thread < nthreads; ++thread) {
            int start = group++ * groupsize;
            threads[thread] = new Thread(()->solveProblems(start,groupsize),"T"+thread);}
        for(int n = 0; n < nthreads; ++n) {threads[n].start();}
        for(int n = 0; n < nthreads; ++n) {
            try {threads[n].join();} catch (InterruptedException e) {}}}


    /** solves a group of problems
     *
     * @param start the index of the first problem to be solved.
     * @param size the number of problems to be solved sequentially
     */
     private void solveProblems(int start, int size)  {
        for(int i = start; i < start+size; ++i) {
            if(i < problemSupervisors.size())  problemSupervisors.get(i).solveProblem(errors,warnings);}}

    /** collects and prints all statistics
     */
    public void printStatistics() {
        /*
        ArrayList<Statistic[]> statistics = new ArrayList<>();  // problem,preProcessor,centralProcessor,solvers...
        for(ProblemSupervisor supervisor : problemSupervisors) {statistics.add(supervisor.collectStatistics());}
        PrintStream textStream = null;
        if(globalParameters.statisticsTextFile != null) {
            try{textStream = new PrintStream(globalParameters.statisticsTextFile);}
            catch(Exception ex) {
                System.out.println("Statistics file " + globalParameters.statisticsTextFile.getAbsolutePath() + " cannot be openend.\nUsing System.out instead");
                textStream = System.out;}}
        if(textStream == null && globalParameters.statisticsText) {textStream = System.out;}
        if(textStream != null) {
            printIndividualTextStatistics(textStream,statistics);
            if(statistics.size() > 1) {printCombinedTextStatistics(textStream,statistics);}
            if(textStream != System.out) {textStream.close();}}
            */

     }

    /** prints for each problem separately all statistics
     *
     * @param out        where to print.
     * @param statistics with entries: statistics of [problem,preprocessor,central processor, solvers ...]
     */
     private void printIndividualTextStatistics(PrintStream out, ArrayList<Statistic[]> statistics) {
         out.println("\n\nIndividual Problem Statistics");
         out.println("****************************");
         int indent = 3;
         for(int i = 0; i < problemSupervisors.size(); ++i) {
             ProblemSupervisor supervisor = problemSupervisors.get(i);
             out.println("Problem " + supervisor.problemId);
             Statistic[] statistic =  statistics.get(i);
             out.println("Problem:");
             Utilities.printIndented(out,indent,statistic[0].toString(false));
             out.println("\n\nPreprocessor");
             Utilities.printIndented(out,indent,statistic[1].toString(false));}}

    /** combines the processor statistics and prints them
     *
     * @param out        where to print.
     * @param statistics with entries: statistics of [problem,preprocessor,central processor, solvers ...]
     */
    public void printCombinedTextStatistics(PrintStream out, ArrayList<Statistic[]> statistics) {
        out.println("\n\nCombined statisics;");
        int indent = 3;
        out.println("\n\nPreprocessors");
        Utilities.printIndented(out,indent,
                Statistic.statisticToString(Statistic.combineDifferentStatistics(
                        Utilities.extract(statistics,1),false)));
        out.println("\n\nCentral Processors");
        Utilities.printIndented(out,indent,
                Statistic.statisticToString(Statistic.combineDifferentStatistics(
                        Utilities.extract(statistics,2),false)));
        out.println("\n\nSolvers");
        ArrayList<Statistic> st = new ArrayList<>();
        for(Statistic[] statistic : statistics) {
            for(int i = 3; i < statistic.length; ++i) {st.add(statistic[i]);}}
        Statistic[] sts = new Statistic[st.size()];
        st.toArray(sts);
        Utilities.printIndented(out,indent,
                Statistic.statisticToString(Statistic.combineDifferentStatistics(sts,false)));}


    /** prints all results to the resultfile.*/
    private void reportResults() {
        PrintStream stream = System.out;
        if(globalParameters.directory != null) {
            File file = Paths.get(globalParameters.directory.toString(),jobname+"-results.txt").toFile();
            try {stream = new PrintStream(file);}
            catch(FileNotFoundException ex) {
                System.out.println("Resultfile "+ file + " cannot be opened. Printing to System.out");}}
        try{
            stream.println("\n\nResults");
            stream.println("*******");
            for(ProblemSupervisor supervisor : problemSupervisors) {
                supervisor.reportResult(stream);}}
        finally{if(stream != System.out) {stream.close();}}}
}
