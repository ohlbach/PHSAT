package Solvers.Backtracker;

import Datastructures.Clauses.Quantifier;
import Datastructures.Results.Result;
import Datastructures.Results.Satisfiable;
import Datastructures.Results.UnsatClauses;
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
                "  seeds:           for the random number generator (default: 0)\n"+
                "  mergeResolution: for activating merge-resolution (default true)";}

    /** contains the allowed keys in the specification. */
    private static final HashSet<String> keys = new HashSet<>();

    static { // these are the allowed keys in the specification.
        Collections.addAll(keys, "seeds", "solver", "mergeResolution");}

    /** parses a HashMap with key-value pairs and creates corresponding Walkers.
     *
     * @param parameters  the parameters with the keys "seed", "flips", "jumps".
     * @param solvers     for adding the newly created Backtrackers.
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
        String mergeResolutions = parameters.get("mergeResolution");
        if(mergeResolutions == null) mergeResolutions = "true";
        String place = "Backtracker: ";
        ArrayList seedA = Utilities.parseIntRange(place+"seed: ",seeds,errors);
        ArrayList mergeResolutionA = Utilities.parseBoolean("mergeResolution: ", mergeResolutions,errors);
        if(errors.length() > 0) ErrorReporter.reportErrorAndStop("Check Backtracker parameters!");
        ArrayList<ArrayList> pars = Utilities.crossProduct(seedA,mergeResolutionA);
        int solverNumber = 1;
        for(ArrayList<Object> p : pars ) {
            int seedV  = (int)p.get(0);
            boolean mergeResolutionV = (boolean)p.get(1);
            if(seedV < 0)   errors.append("Backtracker: seed < 0: ").append(seedV).append("\n");
            HashMap<String,Object> solverParameters = new HashMap<>();
            solverParameters.put("seed",seedV);
            solverParameters.put("mergeResolution",mergeResolutionV);
            solverParameters.put("name",(pars.size() == 1 ? "Backtracker" : "Backtracker_"+solverNumber));
            solvers.add(new Backtracker(solverNumber++, solverParameters, seedV, mergeResolutionV));}}

    /** The seed for the random number generator */
    private int seed;

    boolean mergeResolution = false;

    /** the thread which executes the solver */
    private Thread myThread;

    /** for generating an identifier for the new clauses. */
    private IntSupplier nextId;

    Clauses clauses;

    /** literals  */
    Literals literalIndex = new Literals();

    /** keeps the local candidate model */
    byte[] localModel;

    int[] predicateIndex;

    BacktrackerStatistics statistics;

    /** constructs a new Backtracker.
     *
     * @param solverNumber  for enumerating the walkers.
     * @param solverParameters which specified the walker (for documentation only).
     * @param seed          for starting the random number generator.
     */
    public Backtracker(int solverNumber, HashMap<String,Object> solverParameters, int seed, boolean mergeResolution) {
        super(solverNumber,solverParameters);
        this.seed = seed;
        this.mergeResolution = mergeResolution;
        monitorId = "Backtracker_"+solverNumber;}

    /** adds the literals which are already true in the model to the task queue.
     * Installs the observer in the model.
     */
    public void initialize(Thread myThread, ProblemSupervisor problemSupervisor) {
        super.initialize(myThread,problemSupervisor);
       // problemSupervisor.model.addObserver(myThread, this::addExternalTrueLiteralTask);

    }

    @Override
    public Result solveProblem() {
        long startTime         = System.nanoTime();
        model                  = problemSupervisor.model;
        predicates             = problemSupervisor.inputClauses.predicates;
        monitor                = problemSupervisor.monitor;
        monitoring             = monitor != null;
        monitorId              = "Backtracker";
        problemId              = problemSupervisor.problemId;
        clauses                = new Clauses();
        statistics             = new BacktrackerStatistics(solverId);
        myThread               = Thread.currentThread();
        literalIndex.reset(predicates);
        readInputClauses();
        initializeLocalModel();
        initializePredicateIndex();
        if(derivedTrueLiteralArray.length < predicates+1) derivedTrueLiteralArray = new IntArrayList[predicates +1];
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            IntArrayList derivedTrueLiterals = derivedTrueLiteralArray[predicate];
            if (derivedTrueLiterals == null) derivedTrueLiteralArray[predicate] = new IntArrayList(10);
            else derivedTrueLiterals.clear();}
        if(trueLiteralIndex.length < predicates+1) trueLiteralIndex = new int[predicates+1];

        //System.out.println(clauses.toString(null));
        Result result = searchModel();
        System.out.println(statistics);
        return result;
    }

    /** integrates the normalized input clauses into the clauses-list.
     */
    public void readInputClauses() {
        Clause clause;
        Solvers.Normalizer.Clause normalizedClause = problemSupervisor.normalizer.clauses.firstLinkedItem;
        while(normalizedClause != null) {
            clause = new Clause(normalizedClause);
            insertClause(clause);
            normalizedClause = (Solvers.Normalizer.Clause)normalizedClause.nextItem;}}


    IntArrayList[] derivedTrueLiteralArray = new IntArrayList[predicates+1];
    IntArrayList derivedTrueLiterals;

    int[] trueLiteralIndex = new int[predicates+1];

    boolean positiveLiteral = true;
    public Result searchModel(){
        int firstIndex;
        for(firstIndex = 1; firstIndex <= predicates; ++firstIndex) {
            if(model.status(predicateIndex[firstIndex]) == 0) break;}

        for(int index = firstIndex; index <= predicates; ++index) {
            derivedTrueLiterals = derivedTrueLiteralArray[index];
            int selectedLiteral = predicateIndex[index];

            if(selectedLiteral > 0) {
                byte status = model.status(selectedLiteral);
                if (status != 0) { localModel[selectedLiteral] = status; continue;}
                if (localModel[selectedLiteral] != 0) continue;
                int maxIndex = -1;
                clearDerivedLiterals(derivedTrueLiterals);
                localModel[selectedLiteral] = 1;
                derivedTrueLiterals.add(selectedLiteral);

                for (int i = 0; i < derivedTrueLiterals.size(); ++i) {
                    maxIndex = checkClauses(derivedTrueLiterals.getInt(i));
                    if (maxIndex >= 0) break;}
                statistics.addDerivedLiteralLength(derivedTrueLiterals.size());
                  if (maxIndex == -1) { // no contradiction
                    trueLiteralIndex[selectedLiteral] = index; continue;}
                else selectedLiteral *= -1;}

            clearDerivedLiterals(derivedTrueLiterals);
            localModel[-selectedLiteral] = -1;
            trueLiteralIndex[-selectedLiteral] = 0;
            predicateIndex[index] = selectedLiteral;
            derivedTrueLiterals.add(selectedLiteral);
            int maxIndex = -1;
            for(int i = 0; i < derivedTrueLiterals.size(); ++i) {
                int literal = derivedTrueLiterals.getInt(i);
                maxIndex = checkClauses(literal);
                if(maxIndex >= 0) {break;}}
            statistics.addDerivedLiteralLength(derivedTrueLiterals.size());
             if(maxIndex == -1) { // no contradiction
                trueLiteralIndex[-selectedLiteral] = index;
                continue;}

            if(maxIndex == 0) ++maxIndex;
            // negative literal also caused a contradiction. Backtracking.

            if((index - maxIndex) > 1) ++statistics.backjumps;
            while(maxIndex > firstIndex && predicateIndex[maxIndex] < 0) --maxIndex;
            if(maxIndex == firstIndex && predicateIndex[maxIndex] < 0)
                return new UnsatClauses(problemId,solverId, startTime);

            // now predicateIndex[maxIndex] > 0 or maxIndex is at firstIndex

            ++statistics.backtrackings;
            while(index > maxIndex) {
                if(predicateIndex[index] < 0) predicateIndex[index] *= -1;
                clearDerivedLiterals(derivedTrueLiteralArray[index--]);
            }
            if(predicateIndex[index] > 0) predicateIndex[index] *= -1;
            clearDerivedLiterals(derivedTrueLiteralArray[index--]);
            }
        model.exchangeModel(localModel);
        return new Satisfiable(problemId,solverId, startTime, model);}

    private void clearDerivedLiterals(IntArrayList derivedTrueLiterals) {
        for(int literal : derivedTrueLiterals) {
            int predicate = Math.abs(literal);
            localModel[predicate] = model.status(predicate);
            trueLiteralIndex[predicate] = 0;}
        derivedTrueLiterals.clear();}

    void printStatus(String position, int index, int literal, int maxIndex) {
        System.out.println("\nStatus: " + position + " Index: " + index + " Sel.Lit: " + literal + " Max Ind: " + maxIndex +
                " BT: " + statistics.backtrackings);
        System.out.println("Model:   " + Arrays.toString(localModel));
       if(index > 0) System.out.println("Derived: " + derivedTrueLiteralArray[index].toString());
        System.out.println("TLI: "  + Arrays.toString(trueLiteralIndex));
    }

    /** checks the clauses with the given predicate: they may be locally unsatisfiable, or new true literals may be derivable.
     *
     * @param literal the predicate to be checked.
     * @return -1 if none of the clauses is locally unsatisfiable, otherwise the largest predicate index for the predicates which causes a clause to be unsatisfiable.
     */
    private int checkClauses(int literal) {
        Literal literalObject = literalIndex.getFirstLiteralObject(-literal);
        while(literalObject != null) {
            int maxIndex = deriveTrueLiterals(literalObject.clause);
            if(maxIndex >= 0) return maxIndex; // contradiction
            literalObject = literalObject.nextLiteral;}
        return -1;}

    /** checks the clause: it may be locally unsatisfiable, or new true literals may be derivable.
     *
     * @param clause the clause to be checked.
     * @return -1 if the clause is not locally unsatisfiable, otherwise the largest predicate index for the predicates which causes the clause to be unsatisfiable.
     */
    int deriveTrueLiterals(Clause clause) {
        switch(clause.quantifier) {
            case OR:      return deriveTrueLiteralsOr(clause);
            case ATLEAST: return deriveTrueLiteralsAtleast(clause);
            case ATMOST:  return deriveTrueLiteralsAtmost(clause);
            default:      return deriveTrueLiteralsInterval(clause);}}


    /** if all except one literal are locally false then the remaining literal is derived as true literal.
     * <br>
     * If all literals are locally false then the largest index in the predicateIndex which was responsible for the
     * contradiction is returned.
     *
     * @param clause a clause to be checked.
     * @return -1 or the largest index in the predicateIndex whose selection as true literal was responsible for the truth of the literal.
     */
    int deriveTrueLiteralsOr(Clause clause) {
        //System.out.println("DT " + clause);
        int maxIndex = -1;
        int falseLiterals = 0;
        int unassignedLiteral1 = 0;
        int unassignedLiteral2 = 0;
        for(Literal literalObject : clause.literals) {
            int literal = literalObject.literal;
            int status = getLocalTruth(literal);
            if(status == 1) return -1; // clause is satisfied.
            if(status == 0) {
                if(unassignedLiteral1 == 0) unassignedLiteral1 = literal; else unassignedLiteral2 = literal;}
            else {
                maxIndex = Math.max(maxIndex,trueLiteralIndex[Math.abs(literal)]);
                ++falseLiterals;}}
        int expandedSize = clause.expandedSize;
        if(falseLiterals == expandedSize) return maxIndex; // contradiction
        if(falseLiterals == expandedSize - 1) setLocalTruth(unassignedLiteral1,maxIndex);
        if(mergeResolution && falseLiterals == expandedSize - 2) {
            if(!mergeResolution(unassignedLiteral1,unassignedLiteral2,maxIndex))
                mergeResolution(unassignedLiteral2,unassignedLiteral1,maxIndex);}
        return -1;}

    /** performs merge-resolution between (locally) binary clauses.
     * <br>
     * If all other literals are locally false then merge-resolution between two
     * binary clauses p,q and -p,q yields q.
     *
     * @param resolutionLiteral an unassigned literal.
     * @param mergeLiteral another unassigned literal.
     * @param maxIndexOriginal the largest predicate index which caused the other literals to be false.
     * @return true if the merge-resolution succeeded.
     */
    boolean mergeResolution1(int resolutionLiteral, int mergeLiteral, int maxIndexOriginal) {
        Literal literalObject = literalIndex.getFirstLiteralObject(-resolutionLiteral);
        while(literalObject != null) {
            Clause clause = literalObject.clause;
            if(clause.quantifier == Quantifier.OR) {
                int maxIndex = maxIndexOriginal;
                boolean mergeLiteralFound = false;
                int falseLiterals = 0;
                for(Literal litObject : clause.literals) {
                    int literal = litObject.literal;
                    if(literal == -resolutionLiteral) continue;
                    if(literal == mergeLiteral) {mergeLiteralFound = true; continue;}
                    if(getLocalTruth(literal) == -1) {
                        ++falseLiterals;
                        maxIndex = Math.max(maxIndex,trueLiteralIndex[Math.abs(literal)]);}
                    else break;}
                if(mergeLiteralFound && falseLiterals == clause.expandedSize - 2) {
                    setLocalTruth(mergeLiteral,maxIndex);
                    ++statistics.mergeResolutions;
                    return true;}}
            literalObject = literalObject.nextLiteral;}
        return false;}

    private int timestamp = 1;

    boolean mergeResolution(int resolutionLiteral, int mergeLiteral, int maxIndexOriginal) {
        int negResolutionLiteral = -resolutionLiteral;
        Literal literalObject = literalIndex.getFirstLiteralObject(negResolutionLiteral);
        while(literalObject != null) {
            Clause clause = literalObject.clause;
            if(clause.quantifier == Quantifier.OR)  clause.timestamp = timestamp;
            literalObject = literalObject.nextLiteral;}

        literalObject = literalIndex.getFirstLiteralObject(mergeLiteral);
        while(literalObject != null) {
            Clause clause = literalObject.clause;
            if(clause.timestamp == timestamp) {
                int maxIndex = maxIndexOriginal;
                int falseLiterals = 0;
                ArrayList<Literal> literals = clause.literals;
                int size = literals.size();
                for(int i = 0; i < size; ++i){
                    int literal = literals.get(i).literal;
                    if(literal == negResolutionLiteral || literal == mergeLiteral) continue;
                    if(getLocalTruth(literal) == -1) {
                        ++falseLiterals;
                        maxIndex = Math.max(maxIndex,trueLiteralIndex[Math.abs(literal)]);}
                    else break;}
                if(falseLiterals == clause.expandedSize-2) {
                    setLocalTruth(mergeLiteral,maxIndex);
                    ++statistics.mergeResolutions;
                    ++timestamp;
                    return true;}}
            literalObject = literalObject.nextLiteral;}
        ++timestamp;
        return false;}

                /** checks the clause for local unsatisfiability or derives new locally true literals.
                 * <br>
                 * The clause is locally unsatisfiable iff<br>
                 * - |false literals| &gt; expandedSize - min
                 * <br>
                 * True literals can be derived iff<br>
                 * - |false literals| = expandedSize - min; all other literals must be true.
                 * <br>
                 * If nothing can be concluded, -1 is returned. <br>
                 * If the clause is unsatisfiable then the largest predicate index is returned.<br>
                 *
                 * @param clause a clause to be checked.
                 * @return -1 or the largest predicate index if the clause is locally contradictory.
                 */
    int deriveTrueLiteralsAtleast(Clause clause) {
        int maxIndex = Integer.MIN_VALUE;
        int maxFalse = clause.expandedSize - clause.min;
        int falseLiterals = 0;
        for(Literal literalObject : clause.literals) {
            int literal = literalObject.literal;
            if(getLocalTruth(literal) == -1)  {
                falseLiterals += literalObject.multiplicity;
                maxIndex = Math.max(maxIndex,trueLiteralIndex[Math.abs(literal)]);
                if(falseLiterals > maxFalse) return maxIndex;}}
        if(falseLiterals == maxFalse) { // the remaining literals must become true.
             for(Literal literalObject : clause.literals) {
                int literal = literalObject.literal;
                if(getLocalTruth(literal) == 0) {setLocalTruth(literal,maxIndex);}}}
        return -1;}

    /** checks the clause for local unsatisfiability or derives new locally true literals.
     * <br>
     * An atmost-clause is locally unsatisfiable iff<br>
     * - |true literals| &gt; max
     * <br>
     * New literals can be derived iff <br>
     * - |true literals| = max. All other literals must be false.
     *
     * @param clause a clause to be checked.
     * @return -1 or the largest predicate index if the clause is locally unsatisfiable.
     */
    int deriveTrueLiteralsAtmost(Clause clause) {
        int maxIndex = Integer.MIN_VALUE;
        int max = clause.max;
        int trueLiterals = 0;
        for(Literal literalObject : clause.literals) {
            int literal = literalObject.literal;
            if(getLocalTruth(literal) == 1)  {
                trueLiterals += literalObject.multiplicity;
                maxIndex = Math.max(maxIndex,trueLiteralIndex[Math.abs(literal)]);
                if(trueLiterals > max) return maxIndex;}} // unsatisfiable
        if(trueLiterals == max) {
            for(Literal literalObject : clause.literals) {
                int literal = literalObject.literal;
                if(getLocalTruth(literal) == 0) {setLocalTruth(-literal,maxIndex);}}}
        return -1;}


    /** checks the clause for local contradiction or derives new locally true literals.
     * <br>
     * The clause is locally unsatisfiable iff<br>
     * - |true literals| &gt; max or <br>
     * - |false literals| &gt; expandedSize - min
     * <br>
     * New literals can be derived iff <br>
     * - |true literals| = max; the remaining literals must be false
     * - |false literals| &gt; expandedSize - min; the remaining literals must be true.
     *
     * @param clause an interval-clause
     * @return -1 if nothing can be concluded, otherwise the largest predicate index which caused the contradiction.
     */
    int deriveTrueLiteralsInterval(Clause clause) {
        int maxIndexTrue  = Integer.MIN_VALUE;
        int maxIndexFalse = Integer.MIN_VALUE;
        int trueLiterals  = 0;
        int falseLiterals = 0;
        int maxTrue  = clause.max;
        int maxFalse = clause.expandedSize - clause.min;
        for(Literal literalObject : clause.literals) {
            int literal = literalObject.literal;
            int status = getLocalTruth(literal);
            if(status == 0) continue;
            if(status == 1) {
                maxIndexTrue = Math.max(maxIndexTrue,trueLiteralIndex[Math.abs(literal)]);
                trueLiterals += literalObject.multiplicity;
                if(trueLiterals > maxTrue) return maxIndexTrue;} // contradiction; too many positive literals.
            else {falseLiterals += literalObject.multiplicity;
                  maxIndexFalse = Math.max(maxIndexFalse,trueLiteralIndex[Math.abs(literal)]);
                  if(falseLiterals > maxFalse) return maxIndexFalse;}} // contradiction: too many negative literals.

         if(trueLiterals == maxTrue) { // all other literals must be false.
            for(Literal literalObject : clause.literals) {
                int literal = literalObject.literal;
                if(getLocalTruth(literal) == 0) {setLocalTruth(-literal,maxIndexTrue);}}
            return -1;}
        if(falseLiterals == maxFalse) { // all other literals must be true.
            for(Literal literalObject : clause.literals) {
                int literal = literalObject.literal;
                if(getLocalTruth(literal) == 0) {setLocalTruth(literal,maxIndexFalse);}}}
        return -1;}


    /** adds the literal to the derivedTrueLiterals list and puts minTruthIndex into the trueLiteralIndex;
     *
     * @param literal       a derived true literal.
     * @param minTruthIndex the largest index in the predicateIndex whose selection as true literal was responsible for the truth of the literal.
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



    /** initializes the predicate index.
     * <br>
     * The predicates are sorted according to the number of literal occurrences.
     * The higher numbers of literal occurrences come earlier.
     */
    void initializePredicateIndex() {
        Integer[] preds = new Integer[predicates+1];
        for (int predicate = 1; predicate <= predicates; ++predicate) preds[predicate] = predicate;
        Arrays.sort(preds,1,predicates+1, Comparator.comparingInt(i -> -literalIndex.sizeTotal(i)));
        if(predicateIndex == null || predicateIndex.length < predicates+1) predicateIndex = new int[predicates+1];
        for (int predicate = 1; predicate <= predicates; ++predicate) predicateIndex[predicate] = preds[predicate];
    }


        /** initializes the local to be synchronous to the global model.
         */
    void initializeLocalModel() {
        if(localModel == null || localModel.length < predicates+1) localModel = new byte[predicates+1];
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            localModel[predicate] = model.status(predicate);}}


    @Override
    public Statistic getStatistics() {
        return statistics;}
}
