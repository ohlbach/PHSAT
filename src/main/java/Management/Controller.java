package Management;

import Datastructures.Statistics.Statistic;
import Generators.Generator;
import Solvers.Solver;
import Utilities.Utilities;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.HashMap;

/**
 * Created by Ohlbach on 03.09.2018.<br>
 *
 * This class controls the processing of the problems. <br>
 * It <br>
 * - analyses the input parameters <br>
 * - reads or generates the SAT-problems<br>
 * - distributes them over several threads <br>
 * - activates the solvers <br>
 * - collects the results and statistics
 */
public class Controller {
    public HashMap<String,String>            globalInputParameters = null;
    public ArrayList<HashMap<String,String>> problemInputParameters = null;
    public ArrayList<HashMap<String,String>> solverInputParameters = null;

    public GlobalParameters                  globalParameters  = null;
    public ArrayList<HashMap<String,Object>> problemParameters = null;
    public ArrayList<HashMap<String,Object>> solverParameters  = null;

    public ArrayList<ProblemSupervisor> problemSupervisors = null;
    public Thread[] threads = null;

    private StringBuffer errors   = new StringBuffer();
    private StringBuffer warnings = new StringBuffer();

    /** generates a new controller
     *
     * @param globalInputParameters   for global control
     * @param problemInputParameters  the problem specifications
     * @param solverInputParameters   the solver specifications
     */
    public Controller(HashMap<String,String> globalInputParameters, ArrayList<HashMap<String,String>> problemInputParameters,
                      ArrayList<HashMap<String,String>> solverInputParameters) {
        this.globalInputParameters  = globalInputParameters;
        this.problemInputParameters = problemInputParameters;
        this.solverInputParameters  = solverInputParameters;}


    /** analyses the input specifications and turns them into internal data structures.
     *
     * @return true if the processing can continue, false if it should stop.
     */
    public boolean analyseParameters() {
        if(globalInputParameters == null) {globalParameters = new GlobalParameters();}
        else{globalParameters = new GlobalParameters(globalInputParameters,errors,warnings);}
        if(problemInputParameters == null) {errors.append("No problems specified.\n");}
        else {analyseProblemParameters();}
        if(solverInputParameters == null) {errors.append("No solvers specified.\n");}
        else {analyseSolverParameters();}
        return reportErrors();}



    /** analyses the problemParameters and turns them into sequences of objectParameters.
     * Since the input parameters may specify ranges, each single input parameter may expand to a sequence of parsed parameters*/
    private void analyseProblemParameters() {
        problemParameters = new ArrayList<>();
        for(HashMap<String,String> parameters : problemInputParameters) {
            String type = parameters.get("type");
            if(type == null) {errors.append("No problem type specified.\n"); return;}
            ArrayList<HashMap<String,Object>> pars = Generator.parseParameters(type,parameters,errors,warnings);
            if(pars != null) {
                for(HashMap<String,Object> map :pars) {map.put("type",type);}
                problemParameters.addAll(pars);}}}

    /** analyses the solverParameters and turns them into sequences of objectParameters.
     * Since the input parameters may specify ranges, each single input parameter may expand to a sequence of parsed parameters*/
    private void analyseSolverParameters() {
        solverParameters = new ArrayList<>();
        for(HashMap<String,String> parameters : solverInputParameters) {
            String type = parameters.get("type");
            if(type == null) {errors.append("No solver type specified.\n"); return;}
            ArrayList<HashMap<String,Object>> pars = Solver.parseParameters(type,parameters,errors,warnings);
            if(pars != null) {
                for(HashMap<String,Object> map :pars) {map.put("type",type);}
                if(pars.size() > 1) {
                    for(int i = 0; i < pars.size(); ++i) {pars.get(i).put("solverId",type+"_"+i);}}
                else {pars.get(0).put("solverId",type);}
                solverParameters.addAll(pars);}}}


    /** prints errors and warnings
     *
     * @return true if the processing can continue, false if it should stop
     */
    private boolean reportErrors() {
        if(errors.length() != 0) {
            System.out.println("Errors:");
            System.out.println(errors.toString());
            if(warnings.length() != 0) {
                System.out.println("Warnings:");
                System.out.println(warnings.toString());}
            System.out.println("System stops");
            return false;}
        if(warnings.length() != 0) {
            System.out.println("Warnings:");
            System.out.println(warnings.toString());
            System.out.println("Processing continues anyway.");}
        errors = new StringBuffer();
        warnings = new StringBuffer();
        return true;}

    /** prints all results to the resultfile.*/
    private void reportResults() {
        File file = globalParameters.resultFile;
        PrintStream stream = System.out;
        if(file != null) {
            try {stream = new PrintStream(file);}
            catch(FileNotFoundException ex) {
                System.out.println("Resultfile "+ file.getAbsolutePath() + " cannot be opened. Printing to System.out");}}
        try{
            stream.println("\n\nResults");
            stream.println("*******");
            for(ProblemSupervisor supervisor : problemSupervisors) {
                supervisor.reportResult(stream);}}
        finally{if(stream != System.out) {stream.close();}}}


    /** initiates the solution of the problems.
     * A ProblemSupervisor is generated for each problem.
     * It takes care of invoking the solvers
     *
     * @return true if the problem was solved
     */
    public boolean solve() {
        problemSupervisors = new ArrayList<>();
        for(int i = 0; i < problemParameters.size(); ++i) {
            HashMap<String,Object> parameters = problemParameters.get(i);
            problemSupervisors.add(new ProblemSupervisor(this,globalParameters, parameters,solverParameters));}
        distributeProblems();
        if(globalParameters.monitor.monitoring) {globalParameters.monitor.flush();}
        if(reportErrors()) {reportResults(); return true;}
        return false;}


    /** distributes the problems to different threads.
     */
    private void distributeProblems() {
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
     private void solveProblems(int start, int size) {
        for(int i = start; i < start+size; ++i) {
            if(i < problemSupervisors.size())  {
                ProblemSupervisor supervisor = problemSupervisors.get(i);
                if(supervisor.generateProblem()) {
                    supervisor.solveProblem();}}}}

    /** collects and prints all statistics
     */
    public void printStatistics() {
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

    public void close() {
         globalParameters.close();}

    public synchronized void addError(String error) {
        errors.append(error).append("\n");}

    public synchronized void addWarning(String warning) {
        warnings.append(warning).append("\n");
    }

    public synchronized void addError(StringBuffer error) {
        errors.append(error);}

    public synchronized void addWarning(StringBuffer warning) {
        warnings.append(warning);
    }
}
