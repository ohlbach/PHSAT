package Solvers.Backtracker;

import Datastructures.Clauses.Quantifier;
import Datastructures.LinkedItemList;
import Datastructures.LiteralIndex;
import Datastructures.Results.Result;
import Datastructures.Results.Satisfiable;
import Datastructures.Statistics.Statistic;
import Management.Parameter;
import Management.Parameters;
import Management.ProblemSupervisor;
import Solvers.Solver;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;

public class Backtracker extends Solver {


    public static Parameters makeParameter() {
        Parameters parameters = new Parameters("Backtracker");
        Parameter selected = new Parameter("Select",Parameter.Type.Button,"false",false,
                "Select the Backtracker");
        parameters.add(selected);
        parameters.setDescription("Backtracking search (kind of Davis-Putnam Procedure)");
        return parameters;
    }

    /**
     * Generates and adds new problem generators based on the provided parameters.
     *
     * @param parameters The parameters containing the values necessary to create the problem generators.
     * @param backtrackers The list of backtrackers to add the newly created walkers to.
     */
    public static void makeSolvers(Parameters parameters,ArrayList<Solver> backtrackers) {
        backtrackers.add(new Backtracker(1));}

    private int predicateArrangement = 1;
    int[] predicateSequence;
    int[] predicatePositions;
    int firstSign = 1;

    private LinkedItemList<Clause> clauses;

    private LiteralIndex<Literal> literalIndex;

    /** keeps the local candidate model */
    byte[] localModel;

    IntArrayList[] dependencies;
    IntArrayList[] derivedLiterals;

    BacktrackerStatistics statistics;

    public ThreadPool threadPool;

    /** constructs a new Backtracker.
     *
     * @param solverNumber  for enumerating the walkers.
     */
    public Backtracker(int solverNumber) {
        super(solverNumber);}

    /** adds the literals which are already true in the model to the task queue.
     * Installs the observer in the model.
     */
    public void initialize(Thread myThread, ProblemSupervisor problemSupervisor) {
        super.initialize(myThread,problemSupervisor);
       // problemSupervisor.model.addObserver(myThread, this::addExternalTrueLiteralTask);

    }

    @Override
    public Result solveProblem() {
        startTime    = System.nanoTime();
        clauses      = new LinkedItemList<>("Clauses");
        statistics   = new BacktrackerStatistics(solverId);
        literalIndex = new LiteralIndex<>(predicates);
        dependencies = new IntArrayList[predicates+1];
        derivedLiterals = new IntArrayList[predicates+1];
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            derivedLiterals[predicate] = new IntArrayList();}
        readInputClauses();
        initializeLocalModel();
        initializePredicateSequence();

        //System.out.println(clauses.toString(null));
        Result result = searchModel();
        System.out.println(statistics);
        return result;
    }

    /** integrates the normalized input clauses into the clauses-list.
     */
    public void readInputClauses() {
        Clause clause;
        Solvers.Normalizer.Clause normalizedClause = normalizer.clauses.firstLinkedItem;
        while(normalizedClause != null) {
            clause = new Clause(normalizedClause);
            insertClause(clause);
            normalizedClause = (Solvers.Normalizer.Clause)normalizedClause.nextItem;}}

    /** inserts a clause into the internal lists.
     *
     * @param clause a clause.
     */
    void insertClause(Clause clause) {
        for(Literal literalObject : clause.literals) literalIndex.addToBack(literalObject);
        clauses.addToBack(clause);}


    public Result searchModel() throws Result{
        Thread myThread = Thread.currentThread();
        IntArrayList selectedPredicateIndices = new IntArrayList();
        int topPredicateIndex = 1;
        int literal = 0;
        while(!myThread.isInterrupted() && clauses.size() != 0 &&
                ((topPredicateIndex = findNextPredicateIndex(topPredicateIndex)) != 0)){
            literal = firstSign * predicateSequence[topPredicateIndex];
            if(tryTopPredicate(literal) != null) {
                literal *= -1;
                model.add(myThread,literal);
                setLocalTruth(literal);
                removeClauses(literal);}
            else {
                selectedPredicateIndices.clear();
                selectedPredicateIndices.add(topPredicateIndex);
                int nextPredicateIndex = findNextPredicateIndex(topPredicateIndex+1);
                while(!myThread.isInterrupted() && clauses.size() != 0 && (nextPredicateIndex != 0)) {
                    literal = firstSign * predicateSequence[nextPredicateIndex];
                    setLocalTruth(literal);
                    selectedPredicateIndices.add(nextPredicateIndex);
                    IntArrayList dependencies = tryLiteral(literal);
                    if(dependencies == null) continue;
                    int lastLiteral = dependencies.getInt(dependencies.size()-1);
                    int lastIndex = predicatePositions[Math.abs(lastLiteral)];
                    for(int index = nextPredicateIndex; index >= lastIndex; --index) {
                        int predicate = predicateSequence[index];
                        clearLocalTruth(predicate);}
                    setLocalTruth(-literal);
                    selectedPredicateIndices.removeLast();
                    nextPredicateIndex = findNextPredicateIndex(lastIndex+1);}}

        }
        model.exchangeModel(localModel);
        return new Satisfiable(problemId,solverId, startTime, model);}

    int findNextPredicateIndex(int predicateIndex) throws Result {
        for(; predicateIndex <= predicates; ++predicateIndex) {
            int predicate = predicateSequence[predicateIndex];
            if (literalIndex.getFirstLiteral(predicate) == null &&
                    literalIndex.getFirstLiteral(-predicate) == null) continue;
            byte status = model.status(predicate);
            if (status != 0) {removeClauses(predicate*status); continue;}
            return predicateIndex;}
        return 0;}

    IntArrayList tryTopPredicate(int predicate) {
         dependencies[predicate] = IntArrayList.wrap(new int[]{predicate});
         setLocalTruth(predicate);
         return propagate(predicate);
    }

    IntArrayList tryLiteral(int literal) {

    }


    void removeClauses(int trueLiteral) throws Result {
        for(int sign = 1; sign >= -1; --sign) {
            int literal = sign*trueLiteral;
            Literal literalObject = literalIndex.getFirstLiteral(literal);
            while(literalObject != null) {
                Clause clause = literalObject.clause;
                clause.removeLiteral(literalObject,(sign == 1));
                Clause falseClause = analyseClause(literalObject.clause);
                if(falseClause != null)
                literalObject = (Literal)literalObject.nextItem;
            }
        }
    }



    /**Analyzes a clause given the current local model.
     * <p>
     * The following cases are possible:<br>
     * - the clause is already true: return null; <br>
     * - the clause is already false: return the clause; <br>
     * - making an unsigned literal true causes the clause to become false: make the literal false;<br>
     * - making an unsigned literal false causes the to become false: make the literal true.
     *
     * @param clause The clause to be analyzed.
     * @return the clause if it is false alsready, otherwise null.
     */
    Clause analyseClause(Clause clause) {
        // since disjunctions are frequent,
        // and only one passage through the literals is sufficient,
        // it is worth treating this case separately.
        if(clause.quantifier == Quantifier.OR) {
            Literal unsignedLiteral = null;
            for(Literal literalObject : clause.literals) {
                switch(getLocalTruth(literalObject.literal)) {
                    case 0:
                        if(unsignedLiteral != null) return null; // two unsigned literals: nothing to be done
                        unsignedLiteral = literalObject; break;
                    case 1: return null;}}                   // clause is true;
                if(unsignedLiteral == null) {return clause;} // all literals are false
                makeLiteralLocallyTrue(clause,unsignedLiteral,true);
               return null;}

        // all other clause types.
        int trueLiterals = 0;
        int unsignedLiterals = 0;
        for(Literal literalObject : clause.literals) {
            switch(getLocalTruth(literalObject.literal)) {
                case 0: unsignedLiterals += literalObject.multiplicity; break;
                case 1: trueLiterals += literalObject.multiplicity;}}
        int max = clause.max; int min = clause.min;
        if(trueLiterals > max || trueLiterals + unsignedLiterals < min) return clause;
                        // too many or not enough true literals.
        if(min <= trueLiterals && trueLiterals <= max) return null; // clause is true.
                        // try to derive new true or false literals
        for(Literal literalObject : clause.literals) {
            if(getLocalTruth(literalObject.literal) == 0) {
                 int trueLits = trueLiterals + literalObject.multiplicity;
                 if(!(min <= trueLits && trueLits <= max)) { // making it true causes a contradiction
                     makeLiteralLocallyTrue(clause,literalObject,false);
                     continue;}
                 trueLits = trueLiterals + unsignedLiterals - literalObject.multiplicity;
                 if(!(min <= trueLits && trueLits <= max)) { // making it false causes a contradiction
                    makeLiteralLocallyTrue(clause,literalObject,true);}}}
    return null;}




    synchronized void makeLiteralLocallyTrue(Clause clause, Literal literalObject, boolean truth) {
        int sign = truth? 1:-1;
        int literal = sign*literalObject.literal;
        dependencies[Math.abs(literal)] = joinDependencies(clause);
        setLocalTruth(literal);
        threadPool.addPropagatorJob(this,literal);}

    IntArrayList joinDependencies(Clause clause) {
        IntArrayList joinedDependencies = new IntArrayList();
        for(Literal literalObject : clause.literals) {
            int literal = literalObject.literal;
            if(getLocalTruth(literal) != 0)
                joinDependencies(joinedDependencies,dependencies[Math.abs(literal)]);}
        joinedDependencies.sort(Comparator.comparingInt(predicate -> predicatePositions[predicate]));
        return joinedDependencies;}

    void joinDependencies(IntArrayList dep1, IntArrayList dep2){
        if(dep1.isEmpty()) {dep1.addAll(dep2); return;}
        for(int predicate : dep2) {if(!dep1.contains(predicate)) dep1.add(predicate);}}

    IntArrayList propagate(int literal) {
        Thread currentThread = Thread.currentThread();
        for(int sign = 1; sign >= -1; sign -= 2) {
            Literal literalObject = literalIndex.getFirstLiteral(sign*literal);
            while(literalObject != null && !currentThread.isInterrupted()) {
                Clause clause = literalObject.clause;
                if(clause.quantifier != Quantifier.OR || sign == -1) {
                    Clause falseClause = analyseClause(clause);
                    if(falseClause != null) {return joinDependencies(falseClause);}}
                literalObject = (Literal)literalObject.nextItem;}}
        return null;}

    void backtrack(IntArrayList dependencies) {

    }





    /** adds the literal to the derivedTrueLiterals list and puts minTruthIndex into the trueLiteralIndex;
     *
     * @param literal       a derived true literal.
     */
    private synchronized void setLocalTruth(int literal) {
        if(literal > 0) localModel[literal] = 1; else localModel[-literal] = -1;
        }

    /**
     * Retrieves the truth value of a literal in the local model.
     *
     * @param literal The literal to check.
     * @return The truth value of the literal in the local model.
     */
    private synchronized byte getLocalTruth(int literal) {
        return literal > 0 ? localModel[literal] : (byte)-localModel[-literal]; }

    private synchronized void clearLocalTruth(int predicate) {
        localModel[predicate] = 0;}

    /** initializes the predicate sequence.
     * <br>
     * The predicates are sorted as follows:<br>
     * - predicateArrangement == 1: just the sequence of natural numbers: 1,2,...<br>
     * - predicateArrangement == 2: just the inverse sequence of natural numbers: n,n-1,...<br>
     * - predicateArrangement == 3: predicates with more literal occurrences first<br>
     * - predicateArrangement == 4: predicates with less literal occurrences first.
     */
    void initializePredicateSequence() {
        predicateSequence  = new int[predicates+1];
        predicatePositions = new int[predicates+1];
        switch(predicateArrangement) {
            case 0:
                for (int predicate = 1; predicate <= predicates; ++predicate) {
                    predicateSequence[predicate] = predicate;
                    predicatePositions[predicate] = predicate;}
                return;
            case 1:
                for (int predicate = 1; predicate <= predicates; ++predicate) {
                    predicateSequence[predicates-predicate+1] = predicate;
                    predicatePositions[predicate] = predicates-predicate+1;}
                return;}
        int sign = (predicateArrangement == 3) ? -1 : 1;
        Integer[] predicateIndex = new Integer[predicates+1];
        for (int predicate = 1; predicate <= predicates; predicate++) {predicateIndex[predicate] = predicate;}
        Arrays.sort(predicateIndex,1,predicates+1,
                Comparator.comparingInt(i -> sign * (literalIndex.size(i) + literalIndex.size(-i))));
        for(int predicate = 1; predicate <= predicates; ++ predicate) {
            predicateSequence[predicate] = predicateIndex[predicate];
            predicatePositions[predicateSequence[predicate]] = predicate;
        }}

    /** initializes the local to be synchronous to the global model.
     */
    void initializeLocalModel() {
        localModel = new byte[predicates+1];
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            localModel[predicate] = model.status(predicate);}}


    @Override
    public Statistic getStatistics() {
        return statistics;}
}
