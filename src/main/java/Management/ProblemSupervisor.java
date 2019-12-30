package Management;

import Datastructures.Clauses.BasicClauseList;
import Datastructures.Results.Result;
import Datastructures.Statistics.Statistic;
import Datastructures.Results.*;
import Generators.Generator;
import Solvers.Solver;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.HashMap;

/** A problem supervisor solves a single problem by using several cooperating solvers in parallel threads.
 * Created by ohlbach on 09.10.2018.
 */
public class ProblemSupervisor {
    public String problemId;
    public String threadId;

    public BasicClauseList basicClauseList;
    public GlobalParameters globalParameters;
    public HashMap<String,Object> problemParameters;
    public ArrayList<HashMap<String,Object>> solverParameters;
    Result result;
    Thread[] threads;
    Solver[] solvers;
    Result[] results;
    int numberOfSolvers;
    Controller controller;

    public SupervisorStatistics statistics = null;

    public ProblemSupervisor(Controller controller,GlobalParameters globalParameters,
                             HashMap<String,Object> problemParameters,
                             ArrayList<HashMap<String,Object>> solverParameters) {
        this.controller             = controller;
        this.problemId              = (String)problemParameters.get("name");
        this.globalParameters       = globalParameters;
        this.problemParameters      = problemParameters;
        this.solverParameters       = solverParameters;
        statistics                  = new SupervisorStatistics(problemId);
    }

    /** reads or generates the SAT-clauses
     *
     * @return true if method succeeded, false if an error has occurred
     */
    public boolean generateProblem() {
        String type = (String)problemParameters.get("type");
        StringBuffer errors = new StringBuffer(); StringBuffer warnings = new StringBuffer();
        basicClauseList = Generator.generate(type,problemParameters,errors,warnings);
        controller.addError(errors); controller.addWarning(warnings);
        return basicClauseList != null;}


    public void solveProblem(String threadId) {
        this.threadId = threadId;
        numberOfSolvers = solverParameters.size();
        solvers = new Solver[numberOfSolvers];
        statistics.solvers = numberOfSolvers;
        Statistic[] solverStatistics = new Statistic[numberOfSolvers];
        for(int i = 0; i < numberOfSolvers; ++i) {
            HashMap<String,Object> solverParameter = solverParameters.get(i);
            solvers[i] = Solver.construct((String)solverParameter.get("type"),i,solverParameter,this);}
        threads = new Thread[numberOfSolvers];
        results = new Result[numberOfSolvers];
        for(int i = 0; i < numberOfSolvers; ++i) {
            int j = i;
            threads[i] = new Thread(() -> {results[j] = solvers[j].solve();});}
        for(int i = 0; i < numberOfSolvers; ++i) {threads[i].start();}
        try{for(int i = 0; i < numberOfSolvers; ++i) {threads[i].join();}}
        catch (InterruptedException e) {}
        for(Solver solver : solvers) {
            System.out.println(solver.getStatistics().toString(false));
        }
        globalParameters.log("Solvers finished for problem " + problemId);}

    /** This method is called when a solver has found a new true literal.
     *  It forwards the literal to all other solvers.
     *
     * @param solver   which found the literal
     * @param literal  the new true literal.
     */
    public synchronized void forwardTrueLiteral(Solver solver,int literal) {
        for(Solver solv : solvers) {if(solv != solver) solv.newTrueLiteral(literal);}}

    /** This method is called when a solver found a new binary clause.
     * It forwards the clause to all other solvers.
     *
     * @param solver    which found the clause
     * @param literal1  the first literal of the clause
     * @param literal2  the second literal of the clause
     */
    public synchronized void forwardBinaryClause(Solver solver, int literal1,int literal2) {
        for(Solver solv : solvers) {if(solv != solver) solv.newBinaryClause(literal1,literal2);}}

    /** This method is called by the solvers to indicate that they have done their job or gave up.
     * If the solver succeeded (satisfiable or unsatisfiable) then all other solvers are interrupted. <br>
     * Some messages are logged.
     *
     * @param solver    which finished its work.
     * @param result    the result of the solver's work.
     * @param message   an extra message to explain the result.
     */
    public synchronized void finished(Solver solver, Result result, String message) {
        this.result = result;
        globalParameters.log("Solver " + solver.solverId + " finished  work at problem " + problemId);
        if(message != null && !message.isEmpty()) {globalParameters.log(message);}
        if(result instanceof Satisfiable || result instanceof Unsatisfiable) {
            for(Thread thread : threads) {thread.interrupt();}
            return;}
        if(result instanceof Aborted)    {++statistics.aborted;}
        if(result instanceof Erraneous ) {++statistics.erraneous;}}


    /** The method collects the individual solver statistics into an array of Statistic-objects:
     * statistics[0]    = supervisor statistics <br>
     * statistics[1...] = solver statistics
     *
     * @return the array of Statistics objects.
     */
    public Statistic[] collectStatistics() {
        Statistic[] statistics = new Statistic[2+solvers.length];
        statistics[0] = this.statistics;
        statistics[1] = basicClauseList.getStatistics(problemId);
        for(int i = 0; i < solvers.length; ++i) {statistics[i+2] = solvers[i].getStatistics();}
        return statistics;
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
