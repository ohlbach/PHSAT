package Management;

import Datastructures.Statistics.Statistic;
import Utilities.Utilities;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintStream;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Date;

/**
 * Created by Ohlbach on 03.09.2018.<br>
 *
 * This class controls the processing of the problems. <br>
 * It <br>
 * - reads or generates the QUSAT-problems.<br>
 * - distributes them over several threads. <br>
 * - activates the solvers. <br>
 * - collects the results and statistics.
 */
public class ProblemDistributor {

    /** The global parameters */
    public final GlobalParameters globalParameters;

    /** collects the problem supervisors */
    public ArrayList<ProblemSupervisor> problemSupervisors;

    /** index of the next problem supervisor */
    private int nextProblemSupervisor = 0;
    private QuSatJob quSatJob;

    /** generates a new controller
     *
     * @param quSatJob           the overall manager.
     * @param globalParameters   for global control parameters.
     * @param problemSupervisors  the problem supervisors.
     */
    public ProblemDistributor(QuSatJob quSatJob, GlobalParameters globalParameters, ArrayList<ProblemSupervisor> problemSupervisors) {
        this.quSatJob = quSatJob;
        this.globalParameters  = globalParameters;
        this.problemSupervisors = problemSupervisors;}


    /** The method can be called from different threads to get the next unprocessed ProblemSupervisor.
     *
     * @return null or the next ProblemSupervisor which has not yet been processed.
     */
    private synchronized ProblemSupervisor getNextProblemSupervisor() {
        return (nextProblemSupervisor == problemSupervisors.size()) ?
                null : problemSupervisors.get(nextProblemSupervisor++);}

    /** gets and processes the unprocessed ProblemSupervisors.
     * The method can be called from different threads to get then next unprocessed ProblemSupervisor and calls
     * its solveProblem() method.
     */
    private void processProblems() {
        ProblemSupervisor problemSupervisor;
        while ((problemSupervisor = getNextProblemSupervisor()) != null) problemSupervisor.solveProblem();}

    /** creates threads which call the solveProblem() methods of the ProblemSupervisors.
     * The number of threads is determined such that all but one core gets busy
     * and there are free cores for each solver.
     * Each thread then fetches the next unprocessed ProblemSupervisor and calls its solveProblem() method.
     */
    public void solveProblems() {
        String jobname = globalParameters.jobname;
        globalParameters.logstream.println("Starting job " + jobname + " at " + (new Date()));
        long start = System.nanoTime();
        int numberOfCores = Runtime.getRuntime().availableProcessors() - 1; // one core as spare
        int numberOfSolvers = problemSupervisors.get(0).numberOfSolvers;
        int simultaneousProblems = (numberOfSolvers == 0) ? 1 : Math.max(1, numberOfCores / numberOfSolvers);
        Thread[] threads = new Thread[simultaneousProblems];
        for (int i = 0; i < simultaneousProblems; ++i) {
            Thread thread = new Thread(this::processProblems);
            threads[i] = thread;
            thread.start();}
        try {for (int i = 0; i < simultaneousProblems; ++i) threads[i].join();}
        catch (InterruptedException ex) {
            ErrorReporter.reportErrorAndStop(ex.toString());}
        long end = System.nanoTime(); // only the elapsed solution time is reported.
        reportResults();
        globalParameters.logstream.println("Ending job    " + jobname + " at " + (new Date()));
        globalParameters.logstream.println("Elapsed time " + (float)(end-start)/1000.0 + " μs");
        globalParameters.logstream.close();
    }






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
        String jobname = globalParameters.jobname;
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
