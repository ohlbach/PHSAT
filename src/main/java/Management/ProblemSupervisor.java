package Management;

import Datastructures.Clauses.BasicClauseList;
import Datastructures.Results.Result;
import Datastructures.Statistics.Statistic;
import Datastructures.Results.*;
import Datastructures.Theory.DisjointnessClasses;
import Datastructures.Theory.EquivalenceClasses;
import Datastructures.Theory.Model;
import Datastructures.TwoLiteral.TwoLitClauses;
import Generators.Generator;
import Solvers.Resolution.Preparer;
import Solvers.Solver;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.HashMap;

/** A problem supervisor solves a single problem by using several cooperating solvers in parallel threads.
 * Created by ohlbach on 09.10.2018.
 */
public class ProblemSupervisor {
    public String problemId;

    public BasicClauseList basicClauseList;
    public GlobalParameters globalParameters;
    public HashMap<String,Object> problemParameters;
    public ArrayList<HashMap<String,Object>> solverParameters;
    Result result;
    Thread[] threads;
    Solver[] solvers;
    Result[] results;
    int numberOfSolvers;
    public Controller controller;
    Preparer preparer;

    public Model model;
    public EquivalenceClasses equivalenceClasses;
    public Thread equivalenceThread;

    public DisjointnessClasses disjointnessClasses;
    public Thread disjointnessThread;

    public TwoLitClauses twoLitClauses;
    public Thread twoLitThread;

    public Thread supervisorThread;

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
        supervisorThread            = Thread.currentThread();
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


    public void solveProblem() throws Result {
        initializeClasses();
        initializeAndEqv();
        try{
            equivalenceThread = new Thread(()-> equivalenceClasses.run());
            equivalenceThread.start();
            disjointnessThread = new Thread(()-> equivalenceClasses.run());
            disjointnessThread.start();
            twoLitThread = new Thread(() -> twoLitClauses.run());
            twoLitThread.start();
            initializeDisjoints();

        preparer =  new Preparer(this);
        preparer.prepare();
        if(result != null) {return;}
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
        for(int i = 0; i < numberOfSolvers; ++i) {threads[i].join();}}
        catch (InterruptedException e) {}

        for(Solver solver : solvers) {
            System.out.println(solver.getStatistics().toString(false));
        }
        globalParameters.log("Solvers finished for problem " + problemId);}


    /** initializes the model, the equivalenceClasses, the disjointnessClasses and the twoLitClauses
     */
    private void initializeClasses() {
        model = new Model(basicClauseList.predicates,basicClauseList.symboltable);
        equivalenceClasses  = new EquivalenceClasses(model,problemId, globalParameters.monitor,supervisorThread);
        disjointnessClasses = new DisjointnessClasses(model, equivalenceClasses, problemId, globalParameters.monitor,supervisorThread);
        twoLitClauses = new TwoLitClauses(model,equivalenceClasses,disjointnessClasses,problemId, globalParameters.monitor,supervisorThread);
    }

    /** This method initially fills up the model and the equivalenceClasses.
     * The initial model comes from the conjunctions and the disjunctions with one literal.
     * The initial equivalence classes come from the basic equivalence clauses.
     * At this stage there is no further interaction with other parts.
     *
     * @throws Unsatisfiable if a contradiction occurs.
     */
    private void initializeAndEqv() throws Unsatisfiable {
        for(int[] basicClause : basicClauseList.conjunctions) {
            for(int i = 2; i < basicClause.length; ++i) {
                IntArrayList origin = new IntArrayList(); origin.add(basicClause[0]);
                model.add(basicClause[i],origin,supervisorThread);}}

        for(int[] clause : basicClauseList.equivalences) {
            equivalenceClasses.addBasicEquivalenceClause(clause);}}

    /** sends the disjoints and xors into the corresponding class queues
     */
    private void initializeDisjoints() {
        for(int[] basicClause : basicClauseList.disjoints) {
            disjointnessClasses.addDisjointnessClause(basicClause);}
        for(int[] basicClause : basicClauseList.xors) {
            disjointnessClasses.addDisjointnessClause(basicClause);} }


    /** This method is called when a solver has found a new true literal.
     *  It forwards the literal to all other solvers.
     *
     * @param solver   which found the literal
     * @param literal  the new true literal.
     */
    public Result forwardTrueLiteral(Solver solver,int literal) {
        Result result = null;
        for(Solver solv : solvers) {
            if(solv != solver) {
                result = solv.importTrueLiteral(literal);}
                if(result.getClass() == Unsatisfiable.class || result.getClass() == Satisfiable.class) {return result;}}
        return null;}

    /** This method is called when a solver found a new equivalence p == q
     * It forwards the clause to all other solvers.
     *
     * @param solver    which found the equivalence
     * @param literal1  the first literal of the equivalence
     * @param literal2  the second literal of the equivalence
     * @param origins    null or the ids of the basicClauses which imply the equivalence
     */
    public void forwardEquivalence(Solver solver, int literal1, int literal2, IntArrayList origins) {
        for(Solver solv : solvers) {if(solv != solver) solv.importEquivalence(literal1,literal2,origins);}}

    /** This method is called when a solver found a new disjointness p != q
     * It forwards the clause to all other solvers.
     *
     * @param solver    which found the disjointness
     * @param predicate1  the first literal of the equivalence
     * @param predicate2  the second literal of the equivalence
     * @param origins    null or the ids of the basicClauses which imply the disjointness
     */
    public void forwardDisjointness(Solver solver, int predicate1, int predicate2, IntArrayList origins) {
        for(Solver solv : solvers) {if(solv != solver) solv.importDisjointness(predicate1,predicate2,origins);}}



    /** This method is called when a solver found a new binary clause.
     * It forwards the clause to all other solvers.
     *
     * @param solver    which found the clause
     * @param literal1  the first literal of the clause
     * @param literal2  the second literal of the clause
     */
    public void forwardBinaryClause(Solver solver, int literal1,int literal2) {
        for(Solver solv : solvers) {if(solv != solver) solv.importBinaryClause(literal1,literal2);}}

    /** This method is called when a solver found a new clause.
     * It forwards the clause to all other solvers.
     *
     * @param solver    which found the clause
     * @param literals  the literals of the clause
     */
    public void forwardClause(Solver solver, int[] literals) {
        for(Solver solv : solvers) {if(solv != solver) solv.importClause(literals);}}

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
