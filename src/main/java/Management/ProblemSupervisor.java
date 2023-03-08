package Management;

import Datastructures.Clauses.AllClauses.InitializerSimplifier;
import Datastructures.Clauses.Connective;
import Datastructures.Clauses.InputClauses;
import Datastructures.Clauses.SimplifiersOld.ClauseSimplifier;
import Datastructures.Results.*;
import Datastructures.Statistics.Statistic;
import Datastructures.Theory.EquivalenceClasses.EquivalenceClasses;
import Datastructures.Theory.Model;
import Datastructures.TwoLiteral.TwoLitClauses;
import InferenceSteps.InfInputClause;
import Management.Monitor.Monitor;
import ProblemGenerators.ProblemGenerator;
import Solvers.Solver;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.HashMap;

/** A problem supervisor solves a single problem by using several cooperating solvers in parallel threads.
 * Created by ohlbach on 09.10.2018.
 */
public class ProblemSupervisor {
    public String jobname;
    public String problemId;

    public InputClauses inputClauses;
    public GlobalParameters globalParameters;
    public HashMap<String,Object> problemParameters;
    public HashMap<String,Object> initializeParameters;
    public ArrayList<HashMap<String,Object>> solverParameters;
    public InitializerSimplifier initializer;
    public Result result = null;
    private String solver = null;
    Thread[] threads;
    Result[] results;
    int numberOfSolvers;
    public ProblemDistributor problemDistributor;

    public Model model;
    public EquivalenceClasses equivalenceClasses;
    public ClauseSimplifier clauseSimplifier;
    public Thread equivalenceThread;

    public TwoLitClauses twoLitClauses;
    public Thread twoLitThread;

    public InitializerSimplifier clauses;
    public Thread allClausesThread;

    public Thread supervisorThread;

    private ArrayList<Solver> solvers;

    public SupervisorStatistics statistics = null;
    private ProblemGenerator problemGenerator;

    private boolean trackReasoning;

    public Monitor monitor;

    public ProblemSupervisor(GlobalParameters globalParameters, ProblemGenerator problemGenerator,
                             ArrayList<Solver> solvers) {
        this.globalParameters = globalParameters;
        jobname               = globalParameters.jobname;
        trackReasoning        = globalParameters.trackReasoning;
        problemId             = (String)problemParameters.get("name");
        this.problemGenerator = problemGenerator;
        this.solvers          = solvers;
    }


    int predicates;
    public int clauseCounter = 0;

    public synchronized int nextClauseId() {
        return ++clauseCounter;}

    public void solveProblem()  {
        monitor = globalParameters.getMonitor(problemId);
        try {
            inputClauses = problemGenerator.generateProblem(null);
            model = new Model(inputClauses.predicates);
            conjunctions2Model(inputClauses.conjunctions);
            equivalenceClasses = new EquivalenceClasses(this,monitor);
            equivalenceClasses.integrateEQUIVClauses(inputClauses.equivalences);
            equivalenceThread = new Thread(()->equivalenceClasses.run(false));
            equivalenceThread.start();
            numberOfSolvers = solvers.size();
            statistics.solvers = numberOfSolvers;
            threads = new Thread[numberOfSolvers];
            results = new Result[numberOfSolvers];
            for(int i = 0; i < numberOfSolvers; ++i) {
                int j = i;
                threads[i] = new Thread(() -> {results[j] = solvers.get(j).solveProblem(inputClauses);});} // ???
            for(int i = 0; i < numberOfSolvers; ++i) {threads[i].start();}
            for(int i = 0; i < numberOfSolvers; ++i) {threads[i].join();}
            equivalenceThread.interrupt();}
        catch(Unsatisfiable uns) {}
        catch(Exception ex) {}
        globalParameters.logstream.println("Solvers finished for problem " + problemId);}

    /** inserts the initial conjunctions (if any) into the model
     *
     * @param conjunctions a list of input clauses
     * @throws Unsatisfiable if the conjunctions are contradictory
     */
    private void conjunctions2Model(ArrayList<int[]> conjunctions) throws Unsatisfiable {
        for(int[] inputClause : conjunctions) {
            assert inputClause[1] == Connective.AND.ordinal();
            for(int i = 2; i < inputClause.length; ++i) {
                model.add(inputClause[i],trackReasoning ? new InfInputClause(inputClause[0]) : null);}}}

    /** This method is called by the solvers to indicate that they have done their job or gave up.
     * If the solver succeeded (satisfiable or unsatisfiable) then all other solvers are interrupted. <br>
     * Some messages are logged.
     *
     * @param solver    which finished its work.
     * @param result    the result of the solver's work.
     * @param message   an extra message to explain the result.
     */
    public synchronized void finished(String solver, Result result, String message) {
        this.result = result;
        globalParameters.logstream.println("Solver " + solver + " finished  work at problem " + problemId);
        if(message != null && !message.isEmpty()) {globalParameters.logstream.println(message);}
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
        Statistic[] statistics = new Statistic[2+solvers.size()];
        statistics[0] = this.statistics;
        statistics[1] = inputClauses.getStatistics(problemId);
        for(int i = 0; i < solvers.size(); ++i) {statistics[i+2] = solvers.get(i).getStatistics();}
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


    public void announceResult(Result unsatisfiable,String source) {};

}
