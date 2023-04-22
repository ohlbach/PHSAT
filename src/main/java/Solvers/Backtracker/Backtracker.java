package Solvers.Backtracker;

import Datastructures.Clauses.InputClauses;
import Datastructures.Results.Result;
import Datastructures.Results.Satisfiable;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Statistics.Statistic;
import Datastructures.Task;
import Management.ErrorReporter;
import Management.ProblemSupervisor;
import Solvers.Simplifier.InfExactlyToAtleast;
import Solvers.Simplifier.InfIntervalToAtleast;
import Solvers.Simplifier.UnsatClause;
import Solvers.Solver;
import Utilities.Utilities;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.function.IntSupplier;

public class Backtracker extends Solver {

    /** provides a help text about the parameters of the solver.
     * @return a help text.
     * */
    public static String help() {
        return "Solver Backtracker: a kind of Davis-Putnam Procedure.\n"+
                "parameters:\n" +
                "  seeds:  for the random number generator (default: 0)\n";}

    /** contains the allowed keys in the specification. */
    private static final HashSet<String> keys = new HashSet<>();

    static { // these are the allowed keys in the specification.
        Collections.addAll(keys, "seeds", "solver");}

    /** parses a HashMap with key-value pairs and creates corresponding Walkers.
     *
     * @param parameters  the parameters with the keys "seed", "flips", "jumps".
     * @param solvers     for adding the newly created Walkers.
     * @param errors      for error messages.
     * @param warnings    for warnings (not used here).
     */
    public static void makeSolvers(HashMap<String,String> parameters, ArrayList<Solver> solvers,
                                   StringBuilder errors, StringBuilder warnings){
        for(String key : parameters.keySet()) {
            if(!keys.contains(key)) {
                ErrorReporter.reportWarning("Walker: unknown key in parameters: " + key + "\n" +
                        "        allowed keys: seed, flips, jumps.\n");}}
        String seeds = parameters.get("seed");
        if(seeds == null) seeds = "0";
        String place = "Backtracker: ";
        ArrayList seedA = Utilities.parseIntRange(place+"seed: ",seeds,errors);
        if(errors.length() > 0) ErrorReporter.reportErrorAndStop("Check Backtracker parameters!");
        ArrayList<ArrayList> pars = Utilities.crossProduct(seedA);
        int solverNumber = 1;
        for(ArrayList<Object> p : pars ) {
            int seedV  = (int)p.get(0);
            if(seedV < 0)   errors.append("Backtracker: seed < 0: ").append(seedV).append("\n");
            HashMap<String,Object> solverParameters = new HashMap<>();
            solverParameters.put("seed",seedV);
            solverParameters.put("name",(pars.size() == 1 ? "Backtracker" : "Backtracker_"+solverNumber));
            solvers.add(new Backtracker(solverNumber++, solverParameters, seedV));}}

    /** The seed for the random number generator */
    private int seed;

    /** the thread which executes the solver */
    private Thread myThread;

    /** for generating an identifier for the new clauses. */
    private IntSupplier nextId;

    private Clauses clauses;

    /** literals in or- and atleast- clauses */
    private Literals literalIndexPositive;

    /** literals in atmost-clauses */
    private Literals literalIndexNegative;

    /** literals in all other clause types */
    private Literals literalIndexMixed;

    private BacktrackerStatistics statistics;

    /** constructs a new Backtracker.
     *
     * @param solverNumber  for enumerating the walkers.
     * @param solverParameters which specified the walker (for documentation only).
     * @param seed          for starting the random number generator.
     */
    public Backtracker(int solverNumber, HashMap<String,Object> solverParameters, int seed) {
        super(solverNumber,solverParameters);
        this.seed = seed;
        monitorId = "Backtracker_"+solverNumber;}

    /** adds the literals which are already true in the model to the task queue.
     * Installs the observer in the model.
     */
    public void initialize(Thread myThread, ProblemSupervisor problemSupervisor) {
        super.initialize(myThread,problemSupervisor);
        problemSupervisor.model.addObserver(myThread, this::addTrueLiteralToQueue);
        for(int literal: model.model) {
            addTrueLiteralToQueue(literal,model.getInferenceStep(literal));}}

    @Override
    public Result solveProblem(ProblemSupervisor problemSupervisor) {

        long startTime         = System.nanoTime();
        model                  = problemSupervisor.model;
        inputClauses           = problemSupervisor.inputClauses;
        predicates             = inputClauses.predicates;
        monitor                = problemSupervisor.monitor;
        monitoring             = monitor != null;
        monitorId              = "Backtracker";
        problemId              = problemSupervisor.problemId;
        literalIndexPositive   = new Literals(predicates);
        literalIndexNegative   = new Literals(predicates);
        literalIndexMixed      = new Literals(predicates);
        clauses                = new Clauses();
        statistics             = new BacktrackerStatistics(solverId);
        nextId                 = problemSupervisor::nextClauseId;
        myThread               = Thread.currentThread();
        try{
            readInputClauses();
            processTasks(0);}
        catch(Result result) {
            result.statistic = statistics;
            result.solverId = "Backtracker";
            result.problemId = problemId;
            result.startTime = startTime;
            System.out.println(statistics);
            return result;}
        System.out.println(statistics);
        return null;
    }

    /** reads the disjunctions, the atleast, atmost, exactly and interval clauses from inputClauses and transforms them to atleast-clauses.
     * The clauses themselves are simplified as far as possible.<br>
     * New unit clauses are put into the model. <br>
     * Two-literal clauses generate a corresponding task.
     *
     * @throws Result if a contradiction or the empty clause is derived.
     */
    public void readInputClauses() throws Result{
        try{
            for(int[] inputClause : inputClauses.disjunctions) {
                Clause clause = new Clause(inputClause);
                if(clause.removeComplementaryLiterals((n -> statistics.complementaryLiterals += n),null)) {
                    continue;}
                if(clause.size() == 1) {
                    ++statistics.derivedUnitClauses;
                    ++statistics.notInternalizedInputClauses;
                    addTrueLiteralTask(clause.literals.get(0).literal,clause.inferenceStep);
                    continue;}
                insertClause(clause);}

            for(int[] inputClause : inputClauses.atleasts) {
                insertNewClause(inputClause,new Solvers.Simplifier.Clause(inputClause));}

            for(int[] atmostClause : inputClauses.atmosts) {
                int[] atleastClause = InputClauses.atmostToAtleast(atmostClause);
                Solvers.Simplifier.Clause clause = new Solvers.Simplifier.Clause(atleastClause);
                if(insertNewClause(atmostClause,clause)) {
                    if(monitoring) {
                        monitor.println(monitorId,"Atmost-clause: " +
                                InputClauses.toString(0,atmostClause,symboltable) + " turned to atleast-clause " +
                                clause.toString(symboltable,0));}}}

            for(int[] exactlyClause : inputClauses.exactlys) {
                int[][] atleastClauses = InputClauses.exactlyToAtleast(exactlyClause,nextId);
                for(int i = 0; i < 2; ++i) {
                    int[] atleastClause = atleastClauses[i];
                    Solvers.Simplifier.Clause clause = new Solvers.Simplifier.Clause(atleastClause);
                    if(insertNewClause(exactlyClause,clause)){
                        if(monitoring) {
                            monitor.println(monitorId,"Exactly-clause: " +
                                    InputClauses.toString(0,exactlyClause,symboltable) + " turned to atleast-clause " +
                                    clause.toString(symboltable,0));}}}}

            for(int[] intervalClause : inputClauses.intervals) {
                int[][] atleastClauses = InputClauses.intervalToAtleast(intervalClause,nextId);
                for(int i = 0; i < 2; ++i) {
                    int[] atleastClause = atleastClauses[i];
                    Solvers.Simplifier.Clause clause = new Solvers.Simplifier.Clause(atleastClause);
                    if(insertNewClause(intervalClause,clause)){
                        if(monitoring) {
                            monitor.println(monitorId,"Interval-clause: " +
                                    InputClauses.toString(0,intervalClause,symboltable) + " turned to atleast-clause " +
                                    clause.toString(symboltable,0));}}}}
            if(clauses.isEmpty()) throw new Satisfiable(problemId,solverId, model);
            synchronized(this){queue.add(new Task<>(Simplifier.TaskType.ProcessClauseFirstTime,clauses.firstClause));}
        }
        catch(Result result) {
            result.solverId  = solverId;
            result.problemId = problemId;
            result.statistic = statistics;
            throw result;}
    }

    /** simplifies and inserts an atleast-clause derived from input clauses.
     *
     * @param inputClause the original input-clause.
     * @param clause      the clause to be inserted.
     * @return            true if the clause survived the simplifications.
     * @throws Unsatisfiable     if a contradiction is encountered.
     */
    private boolean insertNewClause(int[] inputClause, Clause clause) throws Unsatisfiable {
        if(clause.isTrue())  {return false;}
        if(clause.isFalse()) {throw new UnsatClause(problemId,solverId,inputClause);}
        int size = clause.size();
        if(clause.removeComplementaryLiterals((n -> {statistics.complementaryLiterals += n;}),null))   {return false;}
        if(clause.size() != size) {
            if(monitoring)
                monitor.println(monitorId,"Complementary literals removed in clause " +
                        InputClauses.toString(0,inputClause,symboltable) + " -> " + clause.toString(symboltable,0));}
        if(!simplifyClause(clause, false)) {return false;}
        insertClause(clause);
        return true;}

    /** inserts a clause into the internal lists.
     *
     * @param clause a clause.
     */
    protected void insertClause(Clause clause) {
        Literals literalIndex;
        switch(clause.quantifier) {
            case OR:
            case ATLEAST: literalIndex = literalIndexPositive; break;
            case ATMOST:  literalIndex = literalIndexNegative; break;
            default:      literalIndex = literalIndexMixed;}
        for(Literal literalObject : clause.literals) literalIndex.addLiteral(literalObject);
        clauses.addClause(clause);}
    @Override
    public Statistic getStatistics() {
        return statistics;}
}
