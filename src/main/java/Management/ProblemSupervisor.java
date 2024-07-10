package Management;

import Datastructures.ClauseList;
import Datastructures.Clauses.InputClauses;
import Datastructures.Results.Aborted;
import Datastructures.Results.Erraneous;
import Datastructures.Results.Result;
import Datastructures.Results.Satisfiable;
import Datastructures.Theory.Model;
import Management.Monitor.Monitor;
import ProblemGenerators.ProblemGenerator;
import Solvers.Normalizer.Normalizer;
import Solvers.Solver;
import Utilities.Utilities;

import java.io.PrintStream;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.function.Consumer;
import java.util.function.Supplier;

/** A problem supervisor solves a single problem by using several cooperating solvers in parallel threads.
 * Created by ohlbach on 09.10.2018.
 */
public class ProblemSupervisor {
    /** the name of the job. */
    public String jobname;
    /** the name of the problem. */
    public String problemId;
    /** the input clauses. */
    public InputClauses inputClauses;
    /** the global parameters. */
    public GlobalParameters globalParameters;

    /** the final result */
    public Result result = null;

    private long startTime;

    /** stores all the solver threads */
    Thread[] threads;
    /** the number of solvers */
    int numberOfSolvers;

    /** the global model */
    public Model model;
    private ArrayList<Solver> solvers;

    public Monitor monitor;

    public SupervisorStatistics statistics = null;
    private ProblemGenerator problemGenerator;

    public QuSatJob quSatJob;

    public Normalizer normalizer;
    public ClauseList clauseList;

    public ProblemSupervisor(QuSatJob quSatJob, GlobalParameters globalParameters, ProblemGenerator problemGenerator,
                             ArrayList<Solver> solvers) {
        this.quSatJob         = quSatJob;
        this.globalParameters = globalParameters;
        jobname               = globalParameters.getJobName();
        this.problemGenerator = problemGenerator;
        this.solvers          = solvers;
        statistics            = new SupervisorStatistics("Supervisor");
        normalizer            = quSatJob.normalizer;
        clauseList            = quSatJob.clauseList;
    }


    /** for enumerating the clauses */
    public int clauseCounter = 0;

    /** for enumerating the clauses
     *
     * @return the next free clause identifier
     */
    public synchronized int nextClauseId() {
        return ++clauseCounter;}

    /** starts the solvers in parallel threads and waits for their results.
     */
    public void solveProblem()  {
        startTime = System.nanoTime();
        StringBuilder errors = new StringBuilder();
        try {
            inputClauses = problemGenerator.generateProblem(errors);
            System.out.println(inputClauses.toString());
            problemId = inputClauses.problemId;
            clauseCounter = inputClauses.nextId-1;
            if(errors.length() > 1) {
                System.err.println("Error when reading/generating problem '" + problemId + "'");
                System.err.println(errors);
                System.err.println(problemGenerator.toString());
                System.err.println("System is aborted.");
                System.exit(1);}

            Consumer<String> mon = (monitor != null) ? (string -> monitor.println(problemId + string)) : null;
            if(!globalParameters.cnfFileSymbols.equals("None")) {
                Path path = inputClauses.makeCNFFile(globalParameters.jobDirectory,true);
                if(globalParameters.logstream != null) {globalParameters.logstream.println("Clauses printed to file " + path.toString()); }
            }
            model = new Model(inputClauses.predicates);
            clauseList.initialize(problemId,model,inputClauses.symboltable);
            normalizer.initialize(inputClauses,model);
            normalizer.normalizeClauses();
            System.out.println(result == null ? "no result " :result.toString());
           // System.out.println(normalizer.toString(null));
            System.out.println(normalizer.statistics.toString());

           // Utilities.wait(15000);System.exit(1);
            if(result != null)  {finished(result); return;}
            numberOfSolvers = solvers.size();
            threads = new Thread[numberOfSolvers];
            for(int i = 0; i < numberOfSolvers; ++i) {int j = i;
                threads[i] = new Thread(() -> finished(solvers.get(j).solveProblem()));
                solvers.get(i).initialize(threads[i],this);}
            for(int i = 0; i < numberOfSolvers; ++i) {threads[i].start();}
            for(int i = 0; i < numberOfSolvers; ++i) {threads[i].join();}}
        catch(Exception ex) {
            System.out.println(ex);
            ex.printStackTrace();
            System.exit(0);}
        if(globalParameters.logstream != null) {
            long time = System.nanoTime() - startTime;
            globalParameters.logstream.println("Solvers finished the problem " + problemId +" in " + Utilities.duration(time));}}



    /** This method is called to indicate that they have done their job or gave up.
     * If the solver succeeded (satisfiable or unsatisfiable) then all other solvers are interrupted. <br>
     * Some messages are logged.
     */
    public synchronized void finished(Result result) {
        if(result == null) return;
        if(result instanceof Aborted) {
            if(result.message != null && !result.message.isEmpty()) {quSatJob.printlog(result.message);}
            ++statistics.aborted; return;}
        if(result instanceof Erraneous ) {
            if(result.message != null && !result.message.isEmpty()) {quSatJob.printlog(result.message);}
            ++statistics.erraneous; return;}
        if(result instanceof Satisfiable) checkModel((Satisfiable) result);
        this.result = result;
        quSatJob.printlog("Solver " + result.solverId + " finished  work at problem " + problemId);
        quSatJob.printlog("Result:\n"+result.toString(inputClauses.symboltable, globalParameters.trackReasoning));
        if(threads != null) {for(Thread thread : threads) {thread.interrupt();}}}

    /** checks the model against the input clauses.
     * If some clauses are false in this model, they are printed and the system exits.
     *
     * @param satisfiable the result of a SAT-search.
     */
    protected void checkModel(Satisfiable satisfiable) {
        ArrayList<int[]>[] criticalClauses = inputClauses.criticalClausesInModel(satisfiable.model);
        if(criticalClauses == null) {
            globalParameters.logstream.println("Problem " + problemId + ": model successfully checked.");}
        else {
            ArrayList<int[]> falseClauses = criticalClauses[0];
            ArrayList<int[]> undefinedClauses = criticalClauses[1];
                System.out.println("Wrong or incomplete model derived by " + satisfiable.solverId +
                        " for problem " + problemId + "\n  " + model.toString());
            if(!falseClauses.isEmpty()) {
                System.out.println("False Clauses:\n");
                System.out.println(InputClauses.toString(falseClauses,inputClauses.symboltable));}
            if(!undefinedClauses.isEmpty()) {
                System.out.println("Undefined Clauses:\n");
                System.out.println(InputClauses.toString(undefinedClauses,inputClauses.symboltable));}
            System.out.println("Process Terminates");
            System.exit(1);}}

    public void intermediateModelCheck(Model model, Supplier supplier) {
        ArrayList<int[]>[] criticalClauses = inputClauses.criticalClausesInModel(model);
        ArrayList<int[]> falseClauses = criticalClauses[0];
        if(!falseClauses.isEmpty()) {
            System.out.println("False Clauses for model: " + model.toString()+"\n");
            System.out.println(InputClauses.toString(falseClauses,inputClauses.symboltable));
            if(supplier != null) supplier.get();
            System.exit(1);}}

    /** The method collects the individual solver statistics into an array of Statistic-objects:
     * statistics[0]    = supervisor statistics <br>
     * statistics[1...] = solver statistics
     *
     * @return the array of Statistics objects.
     */
    public static void printStatistics(GlobalParameters globalParameters, ArrayList<ProblemSupervisor> problemSupervisors) {
       String statistics = globalParameters.statistic;

    }

    /** prints the result to the PrintStream
     *
     * @param out a PrintStream
     */
    public void reportResult(PrintStream out) {
        out.println("Result for problem " + problemId + ":");
        if(result == null) {out.println("   no result");}
        else {out.println("  "+result.toString());}}


}
