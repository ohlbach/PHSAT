package Solvers.Backtracker;

import Datastructures.Results.Result;
import Datastructures.Results.Satisfiable;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Statistics.Statistic;
import Management.ErrorReporter;
import Management.ProblemSupervisor;
import Solvers.Solver;
import Utilities.Utilities;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.*;
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
    private Literals literalIndexPositive = new Literals();

    /** literals in atmost-clauses */
    private Literals literalIndexNegative = new Literals();

    /** literals in all other clause types */
    private Literals literalIndexMixed = new Literals();

    /** keeps the local candidate model */
    private short[] localModel;

    private int[] predicateIndex;

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
       // problemSupervisor.model.addObserver(myThread, this::addTrueLiteralToQueue);
       // for(int literal: model.model) {
       //     addTrueLiteralToQueue(literal,model.getInferenceStep(literal));}
    }

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
        clauses                = new Clauses();
        statistics             = new BacktrackerStatistics(solverId);
        nextId                 = problemSupervisor::nextClauseId;
        myThread               = Thread.currentThread();
        literalIndexPositive.reset(predicates);
        literalIndexNegative.reset(predicates);
        literalIndexMixed.reset(predicates);
        initializeLocalModel();
        initializePredicateIndex();
        try{
            readInputClauses();}
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
        IntArrayList trueLiterals = new IntArrayList();
        Clause clause;
        try{
            for(int[] inputClause : inputClauses.disjunctions) {
                clause = new Clause(inputClause,problemId,solverId,trueLiterals);
                addTrueLiterals(trueLiterals);
                if(clause.isEmpty()) continue;
                insertClause(clause);}

            for(int[] inputClause : inputClauses.atleasts) {
                clause = new Clause(inputClause,problemId,solverId,trueLiterals);
                addTrueLiterals(trueLiterals);
                if(clause.isEmpty()) continue;
                insertClause(clause);}

            for(int[] inputClause : inputClauses.atmosts) {
                clause = new Clause(inputClause,problemId,solverId,trueLiterals);
                addTrueLiterals(trueLiterals);
                if(clause.isEmpty()) continue;
                insertClause(clause);}

            for(int[] inputClause : inputClauses.exactlys) {
                clause = new Clause(inputClause,problemId,solverId,trueLiterals);
                addTrueLiterals(trueLiterals);
                if(clause.isEmpty()) continue;
                insertClause(clause);}

            for(int[] inputClause : inputClauses.intervals) {
                clause = new Clause(inputClause,problemId,solverId,trueLiterals);
                addTrueLiterals(trueLiterals);
                if(clause.isEmpty()) continue;
                insertClause(clause);}

            for(int[] inputClause : inputClauses.equivalences) {
                clause = new Clause(inputClause,problemId,solverId,trueLiterals);
                if(clause.isEmpty()) continue;
                insertClause(clause);}

            if(clauses.isEmpty()) throw new Satisfiable(problemId,solverId, model);
        }
        catch(Result result) {
            result.solverId  = solverId;
            result.problemId = problemId;
            result.statistic = statistics;
            throw result;}}


    void addTrueLiterals(IntArrayList trueLiterals) throws Unsatisfiable {
        for(int literal : trueLiterals) {
            model.add(myThread,literal,null);
            localModel[Math.abs(literal)] = (short)((literal > 0) ? 1: -1);}}

    /** inserts a clause into the internal lists.
     *
     * @param clause a clause.
     */
    void insertClause(Clause clause) {
        Literals literalIndex;
        switch(clause.quantifier) {
            case OR:
            case ATLEAST: literalIndex = literalIndexPositive; break;
            case ATMOST:  literalIndex = literalIndexNegative; break;
            default:      literalIndex = literalIndexMixed;}
        for(Literal literalObject : clause.literals) literalIndex.addLiteral(literalObject);
        clauses.addClause(clause);}



    /** initializes the predicate index.
     * If seed = 0 then the sequence of predicates is just that natural order.<br>
     * If seed != 0 then sequence of predicates is randomly changed. <br>
     * This way one can have different Backtracker solvers searching in parallel in completely different order.
     */
    void initializePredicateIndex(){
        if(predicateIndex == null || predicateIndex.length < predicates+1) predicateIndex = new int[predicates+1];
        for(int predicate = 1; predicate <= predicates; ++predicate) predicateIndex[predicate] = predicate;
        if(seed ==  0) return;
        Random rnd = new Random(seed);
        int position1 = rnd.nextInt(predicates)+1;
        predicateIndex[1] = position1;
        predicateIndex[position1] = 1;
        for(int i = 0; i < predicates/2; ++i) {
            position1 = rnd.nextInt(predicates)+1;
            int position2 = rnd.nextInt(predicates) + 1;
            int predicate = predicateIndex[position1];
            predicateIndex[position1] = predicateIndex[position2];
            predicateIndex[position2] = predicate;}}

    /** initializes the local to be synchronous to the global model.
     */
    void initializeLocalModel() {
        if(localModel == null || localModel.length < predicates+1) localModel = new short[predicates+1];
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            localModel[predicate] = (short)model.status(predicate);}}


    @Override
    public Statistic getStatistics() {
        return statistics;}
}
