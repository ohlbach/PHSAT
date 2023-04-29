package Solvers.Backtracker;

import Datastructures.Results.Result;
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
    short[] localModel;

    int[] predicateIndex;

    BacktrackerStatistics statistics;

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
        predicates             = inputClauses.predicates;
        monitor                = problemSupervisor.monitor;
        monitoring             = monitor != null;
        monitorId              = "Backtracker";
        problemId              = problemSupervisor.problemId;
        clauses                = new Clauses();
        statistics             = new BacktrackerStatistics(solverId);
        myThread               = Thread.currentThread();
        literalIndex.reset(predicates);
        initializeLocalModel();
        initializePredicateIndex();
        if(derivedTrueLiteralArray.length < predicates+1) derivedTrueLiteralArray = new IntArrayList[predicates +1];
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            IntArrayList derivedTrueLiterals = derivedTrueLiteralArray[predicate];
            if (derivedTrueLiterals == null) derivedTrueLiteralArray[predicate] = new IntArrayList(10);
            else derivedTrueLiterals.clear();}
        if(trueLiteralIndex.length < predicates+1) trueLiteralIndex = new int[predicates+1];
        readInputClauses();
        boolean satisfiable = searchModel();
        if(satisfiable) System.out.println(localModel);
        else System.out.println("Unsatisfiable");
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


    IntArrayList[] derivedTrueLiteralArray = new IntArrayList[predicates+1];
    IntArrayList derivedTrueLiterals;

    int[] trueLiteralIndex = new int[predicates+1];

    boolean positiveLiteral = true;
    public boolean searchModel(){
        int firstIndex = 0;
        for(firstIndex = 1; firstIndex <= predicates; ++firstIndex) {
            if(model.status(predicateIndex[firstIndex]) == 0) break;}

        for(int index = firstIndex; index <= predicates; ++index) {
            int selectedLiteral = predicateIndex[index];
            int status = model.status(selectedLiteral);
            if(status != 0) {
                localModel[selectedLiteral] = (short)status;
                continue;}

            if(localModel[selectedLiteral] != 0) continue;

            trueLiteralIndex[selectedLiteral] = index;

            int minIndex = -1;
            derivedTrueLiterals = derivedTrueLiteralArray[index];
            derivedTrueLiterals.clear();
            if(positiveLiteral) {
                localModel[selectedLiteral] = 1;
                derivedTrueLiterals.add(selectedLiteral);
                for(int i = 0; i < derivedTrueLiterals.size(); ++i) {
                    if((minIndex = checkClauses(derivedTrueLiterals.getInt(i))) >= 0) break;};}

            if(minIndex >= 0) { // positive literal caused a contradiction.
                positiveLiteral = true;
                for(int literal : derivedTrueLiterals) localModel[Math.abs(literal)] = 0;
                derivedTrueLiterals.clear();
                localModel[selectedLiteral] = -1;
                selectedLiteral *= -1;
                derivedTrueLiterals.add(selectedLiteral);
                for(int i = 0; i < derivedTrueLiterals.size(); ++i) {
                    if((minIndex = checkClauses(derivedTrueLiterals.getInt(i))) >= 0) break;}}

            if(minIndex >= 0) { // negative literal also caused a contradiction. Backtracking.
                if(minIndex < firstIndex) return false;
                ++statistics.backtrackings;
                positiveLiteral = false;
                while(index >= minIndex) {
                    for(int literal : derivedTrueLiteralArray[index]) {
                        int predicate = Math.abs(literal);
                        localModel[predicate] = (short)model.status(predicate);
                        trueLiteralIndex[predicate] = 0;}
                    --index;}}}
        return true;}

    private int checkClauses(int predicate) {
        for(int sign = 1; sign >= -1; sign -= 2) {
            Literal literalObject = literalIndex.getFirstLiteralObject(sign*predicate);
            while(literalObject != null) {
                int minIndex = deriveTrueLiterals(literalObject.clause);
                if(minIndex >= 0) return minIndex; // contradiction
                literalObject = literalObject.nextLiteral;}}
        return -1;}

    int deriveTrueLiterals(Clause clause) {
        switch(clause.quantifier) {
            case OR:      return deriveTrueLiteralsOr(clause);
            case ATLEAST: return deriveTrueLiteralsAtleast(clause);
            case ATMOST:  return deriveTrueLiteralsAtmost(clause);
            case EXACTLY: return deriveTrueLiteralsExactly(clause);
            default:      return deriveTrueLiteralsInterval(clause);}}

    int minTruthIndex(Clause clause) {
        int minIndex = Integer.MAX_VALUE;
        for(Literal literalObject : clause.literals) {
            int predicate = Math.abs(literalObject.literal);
            if(localModel[predicate] != 0) minIndex = Math.min(minIndex, trueLiteralIndex[predicate]);}
        return minIndex;}

    /** if all except one literal are locally false then the remaining literal is derived as true literal.
     * <br>
     * If all literals are locally false then the smallest index in the predicateIndex which was responsible for the
     * contradiction is returned.
     *
     * @param clause a clause to be checked.
     * @return -1 or the smallest index in the predicateIndex whose selection as true literal was responsible for the truth of the literal.
     */
    int deriveTrueLiteralsOr(Clause clause) {
        int negativeLiteralsFound = 0;
        int unassignedLiteral = 0;
        for(Literal literalObject : clause.literals) {
            int literal = literalObject.literal;
            int sign = getLocalTruth(literal);
            if(sign == -1) ++negativeLiteralsFound;
            else {if (sign != 1) unassignedLiteral = literal;}}
        int expandedSize = clause.expandedSize;
        if(negativeLiteralsFound == expandedSize) return minTruthIndex(clause); // contradiction
        if(unassignedLiteral != 0 && negativeLiteralsFound == expandedSize - 1) {
            setLocalTruth(unassignedLiteral,minTruthIndex(clause));}
        return -1;}

    /** checks the clause for local contradiction or derives new locally true literals.
     * <br>
     * If too many literals are false then the clause is contradictory and the smallest predicate index is returned.<br>
     * If enough literals are true then the clause is satisfied and -1 is returned.<br>
     * If expandedSize-min literals are false then the remaining literals can be made true, and -1 is returned.
     *
     * @param clause a clause to be checked.
     * @return -1 or the smallest predicate index if the clause is locally contradictory.
     */
    int deriveTrueLiteralsAtleast(Clause clause) {
        int negativeLiteralsFound = 0;
        int positiveLiteralsFound = 0;
        for(Literal literalObject : clause.literals) {
            int literal = literalObject.literal;
            int status =  getLocalTruth(literal);
            if(status == -1)      {negativeLiteralsFound += literalObject.multiplicity;}
            else {if(status == 1) {positiveLiteralsFound += literalObject.multiplicity;}}}
        if(positiveLiteralsFound > clause.min) return -1; // clause is satisfied.
        int expandedSize = clause.expandedSize;
        if(negativeLiteralsFound > expandedSize - clause.min) return minTruthIndex(clause); // not enough positive literals left
        if(negativeLiteralsFound == expandedSize - clause.min) { // the remaining literals must become true.
            for(Literal literalObject : clause.literals) {
                int literal = literalObject.literal;
                if(getLocalTruth(literal) == 0) {setLocalTruth(literal,minTruthIndex(clause));}}}
        return -1;}

    int deriveTrueLiteralsAtmost(Clause clause) {
        int positiveLiteralsFound = 0;
        for(Literal literalObject : clause.literals) {
            if(getLocalTruth(literalObject.literal) == 1) {positiveLiteralsFound += literalObject.multiplicity;}}
        int max = clause.max;
        if(positiveLiteralsFound > max) return minTruthIndex(clause); // contradiction
        if(positiveLiteralsFound == max) {
            for(Literal literalObject : clause.literals) {
                int literal = literalObject.literal;
                if(getLocalTruth(literal) != -1) {setLocalTruth(-literal,minTruthIndex(clause));}}}
        return -1;}

    int deriveTrueLiteralsExactly(Clause clause) {
        int positiveLiteralsFound = 0;
        int negativeLiteralsFound = 0;
        int exactlyPostive = clause.min;
        int exactlyNegative = clause.expandedSize - exactlyPostive;
        for(Literal literalObject : clause.literals) {
            int sign = getLocalTruth(literalObject.literal);
            if(sign == 1) {
                positiveLiteralsFound += literalObject.multiplicity;
                if(positiveLiteralsFound > exactlyPostive) return minTruthIndex(clause);} // contradiction; too many positive literals.
            else {
                if(sign == -1) negativeLiteralsFound += literalObject.multiplicity;
                if(negativeLiteralsFound > exactlyNegative) return minTruthIndex(clause); // contradiction: too many negative literals.
            }}
        if(positiveLiteralsFound == exactlyPostive) { // all other literals must be false.
            for(Literal literalObject : clause.literals) {
                int literal = literalObject.literal;
                if(getLocalTruth(literal) != -1) {setLocalTruth(-literal,minTruthIndex(clause));}}
            return -1;}
        if(negativeLiteralsFound == exactlyNegative) { // all other literals must be true.
            for(Literal literalObject : clause.literals) {
                int literal = literalObject.literal;
                if(getLocalTruth(literal) != 1) {setLocalTruth(literal,minTruthIndex(clause));}}}
        return -1;}

    int deriveTrueLiteralsInterval(Clause clause) {
        int positiveLiteralsFound = 0;
        int negativeLiteralsFound = 0;
        int maxPositive = clause.max;
        int maxNegative = clause.expandedSize - clause.min;
        for(Literal literalObject : clause.literals) {
            int sign = getLocalTruth(literalObject.literal);
            if(sign == 1) {
                positiveLiteralsFound += literalObject.multiplicity;
                if(positiveLiteralsFound > maxPositive) return minTruthIndex(clause);} // contradiction; too many positive literals.
            else {
                if(sign == -1) negativeLiteralsFound += literalObject.multiplicity;
                if(negativeLiteralsFound > maxNegative) return minTruthIndex(clause); // contradiction: too many negative literals.
            }}
        if(positiveLiteralsFound == maxPositive) { // all other literals must be false.
            for(Literal literalObject : clause.literals) {
                int literal = literalObject.literal;
                if(getLocalTruth(literal) != -1) {setLocalTruth(-literal,minTruthIndex(clause));}}
            return -1;}
        if(negativeLiteralsFound == maxNegative) { // all other literals must be true.
            for(Literal literalObject : clause.literals) {
                int literal = literalObject.literal;
                if(getLocalTruth(literal) != 1) {setLocalTruth(literal,minTruthIndex(clause));}}}
        return -1;}


    /** adds the literal to the derivedTrueLiterals list and puts minTruthIndex into the trueLiteralIndex;
     *
     * @param literal       a derived true literal.
     * @param minTruthIndex the smallest index in the predicateIndex whose selection as true literal was responsible for the truth of the literal.
     */
    private void setLocalTruth(int literal, int minTruthIndex) {
        if(literal > 0) localModel[literal] = 1; else localModel[-literal] = -1;
        derivedTrueLiterals.add(literal);
        trueLiteralIndex[Math.abs(literal)] = minTruthIndex;}

    private int getLocalTruth(int literal) {
        return literal > 0 ? localModel[literal] : -localModel[-literal]; }


    /** inserts a clause into the internal lists.
     *
     * @param clause a clause.
     */
    void insertClause(Clause clause) {
        for(Literal literalObject : clause.literals) literalIndex.addLiteral(literalObject);
        clauses.addClause(clause);}



    /** initializes the predicate index. The first predicate starts with index 1.
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
