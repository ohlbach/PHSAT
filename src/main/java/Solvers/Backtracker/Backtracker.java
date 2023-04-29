package Solvers.Backtracker;

import Datastructures.Results.Result;
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
                ErrorReporter.reportWarning("Backtracker: unknown key in parameters: " + key + "\n" +
                        "        allowed keys: seed.\n");}}
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

    /** literals  */
    private Literals literalIndex = new Literals();

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
        literalIndex.reset(predicates);
        initializeLocalModel();
        initializePredicateIndex();
        try{
            readInputClauses();
            searchModel();
        }
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

    /** integrates the normalized input clauses into the clauses-list.
     */
    public void readInputClauses() {
        Clause clause;
        for(IntArrayList normalizedClause: problemSupervisor.normalizer.clauses) {
            clause = new Clause(normalizedClause);
            insertClause(clause);}}


    private IntArrayList derivedTrueLiterals = new IntArrayList();

    private HashMap<Integer,Integer> selectedTrueLiterals = new HashMap<>();
    public void searchModel() throws Result{
        for(int index = 1; index <= predicates; ++ index) {
            int predicate = predicateIndex[index];
            if(localModel[predicate] != 0) continue;
            localModel[predicate] = 1;
            derivedTrueLiterals.add(predicate);
            for(int i = 0; i < derivedTrueLiterals.size(); ++i) {
                predicate = derivedTrueLiterals.getInt(i);
                if(checkClauses(predicate,predicate)) { // contradiction found
                    break;
                };
            }
        }

    }

    private boolean checkClauses(int predicate, int selectedPredicate) {
        for(int sign = 1; sign >= -1; sign -= 2) {
            Literal literalObject = literalIndex.getFirstLiteralObject(sign*predicate);
            while(literalObject != null) {
                if(deriveTrueLiterals(literalObject.clause,selectedPredicate)) return true; // contradiction
                literalObject = literalObject.nextLiteral;}}
        return false;}

    boolean deriveTrueLiterals(Clause clause, int selectedPredicate) {
        switch(clause.quantifier) {
            case OR:      return deriveTrueLiteralsOr(clause, selectedPredicate);
            case ATLEAST: return deriveTrueLiteralsAtleast(clause, selectedPredicate);
            case ATMOST:  return deriveTrueLiteralsAtmost(clause, selectedPredicate);
            case EXACTLY: return deriveTrueLiteralsExactly(clause, selectedPredicate);
            default: deriveTrueLiteralsInterval(clause, selectedPredicate);}
        return false;}

    boolean deriveTrueLiteralsOr(Clause clause, int selectedPredicate) {
        int negativeLiteralsFound = 0;
        int unassignedLiteral = 0;
        for(Literal literalObject : clause.literals) {
            int literal = literalObject.literal;
            int sign = getLocalTruth(literal);
            if(sign == -1) ++negativeLiteralsFound;
            else {if (sign != 1) unassignedLiteral = literal;}}
        int expandedSize = clause.expandedSize;
        if(negativeLiteralsFound == expandedSize) return true; // contradiction
        if(negativeLiteralsFound == expandedSize - 1) {setLocalTruth(unassignedLiteral,selectedPredicate);}
        return false;}

    boolean deriveTrueLiteralsAtleast(Clause clause, int selectedPredicate) {
        int negativeLiteralsFound = 0;
        for(Literal literalObject : clause.literals) {
            if(getLocalTruth(literalObject.literal) == -1) {negativeLiteralsFound += literalObject.multiplicity;}}
        int expandedSize = clause.expandedSize;
        if(negativeLiteralsFound == expandedSize) return true; // contradiction
        if(negativeLiteralsFound >= expandedSize - clause.min) {
            for(Literal literalObject : clause.literals) {
                int literal = literalObject.literal;
                if(getLocalTruth(literal) != 1) {setLocalTruth(literal,selectedPredicate);}}}
        return false;}

    boolean deriveTrueLiteralsAtmost(Clause clause, int selectedPredicate) {
        int positiveLiteralsFound = 0;
        for(Literal literalObject : clause.literals) {
            if(getLocalTruth(literalObject.literal) == 1) {positiveLiteralsFound += literalObject.multiplicity;}}
        int max = clause.max;
        if(positiveLiteralsFound > max) return true; // contradiction
        if(positiveLiteralsFound == max) {
            for(Literal literalObject : clause.literals) {
                int literal = literalObject.literal;
                if(getLocalTruth(literal) != -1) {setLocalTruth(-literal,selectedPredicate);}}}
        return false;}

    boolean deriveTrueLiteralsExactly(Clause clause, int selectedPredicate) {
        int positiveLiteralsFound = 0;
        int negativeLiteralsFound = 0;
        int exactlyPostive = clause.min;
        int exactlyNegative = clause.expandedSize - exactlyPostive;
        for(Literal literalObject : clause.literals) {
            int sign = getLocalTruth(literalObject.literal);
            if(sign == 1) {
                positiveLiteralsFound += literalObject.multiplicity;
                if(positiveLiteralsFound > exactlyPostive) return true;} // contradiction; too many positive literals.
            else {
                if(sign == -1) negativeLiteralsFound += literalObject.multiplicity;
                if(negativeLiteralsFound > exactlyNegative) return true; // contradiction: too many negative literals.
            }}
        if(positiveLiteralsFound == exactlyPostive) { // all other literals must be false.
            for(Literal literalObject : clause.literals) {
                int literal = literalObject.literal;
                if(getLocalTruth(literal) != -1) {setLocalTruth(-literal,selectedPredicate);}}
            return false;}
        if(negativeLiteralsFound == exactlyNegative) { // all other literals must be true.
            for(Literal literalObject : clause.literals) {
                int literal = literalObject.literal;
                if(getLocalTruth(literal) != 1) {setLocalTruth(literal,selectedPredicate);}}}
        return false;}

    boolean deriveTrueLiteralsInterval(Clause clause, int selectedPredicate) {
        int positiveLiteralsFound = 0;
        int negativeLiteralsFound = 0;
        int maxPositive = clause.max;
        int maxNegative = clause.expandedSize - clause.min;
        for(Literal literalObject : clause.literals) {
            int sign = getLocalTruth(literalObject.literal);
            if(sign == 1) {
                positiveLiteralsFound += literalObject.multiplicity;
                if(positiveLiteralsFound > maxPositive) return true;} // contradiction; too many positive literals.
            else {
                if(sign == -1) negativeLiteralsFound += literalObject.multiplicity;
                if(negativeLiteralsFound > maxNegative) return true; // contradiction: too many negative literals.
            }}
        if(positiveLiteralsFound == maxPositive) { // all other literals must be false.
            for(Literal literalObject : clause.literals) {
                int literal = literalObject.literal;
                if(getLocalTruth(literal) != -1) {setLocalTruth(-literal,selectedPredicate);}}
            return false;}
        if(negativeLiteralsFound == maxNegative) { // all other literals must be true.
            for(Literal literalObject : clause.literals) {
                int literal = literalObject.literal;
                if(getLocalTruth(literal) != 1) {setLocalTruth(literal,selectedPredicate);}}}
        return false;}



    private void setLocalTruth(int literal, int selectedPredicate) {
        if(literal > 0) localModel[literal] = 1; else localModel[-literal] = -1;
        derivedTrueLiterals.add(literal);
        selectedTrueLiterals.put(literal,selectedPredicate);}

    private int getLocalTruth(int literal) {
        return literal > 0 ? localModel[literal] : -localModel[-literal]; }


    void addTrueLiterals(IntArrayList trueLiterals) throws Unsatisfiable {
        for(int literal : trueLiterals) {
            model.add(myThread,literal,null);
            localModel[Math.abs(literal)] = (short)((literal > 0) ? 1: -1);}}

    /** inserts a clause into the internal lists.
     *
     * @param clause a clause.
     */
    void insertClause(Clause clause) {
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
