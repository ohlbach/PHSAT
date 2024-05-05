package Solvers.Backtracker;

import Datastructures.LinkedItemList;
import Datastructures.LiteralIndex;
import Datastructures.Results.Result;
import Datastructures.Results.Satisfiable;
import Datastructures.Results.UnsatClauses;
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

    private LinkedItemList<Clause> clauses;

    private LiteralIndex<Literal> literalIndex;

    /** keeps the local candidate model */
    byte[] localModel;

    BacktrackerStatistics statistics;

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
        readInputClauses();
        initializeLocalModel();
        initializePredicateSequence();
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


    IntArrayList[] derivedTrueLiteralArray = new IntArrayList[predicates+1];
    IntArrayList derivedTrueLiterals;

    int[] trueLiteralIndex = new int[predicates+1];

    boolean positiveLiteral = true;
    public Result searchModel(){
        int firstIndex;
        for(firstIndex = 1; firstIndex <= predicates; ++firstIndex) {
            if(model.status(predicateSequence[firstIndex]) == 0) break;}

        for(int index = firstIndex; index <= predicates; ++index) {
            derivedTrueLiterals = derivedTrueLiteralArray[index];
            int selectedLiteral = predicateSequence[index];

            if(selectedLiteral > 0) {
                byte status = model.status(selectedLiteral);
                if (status != 0) { localModel[selectedLiteral] = status; continue;}
                if (localModel[selectedLiteral] != 0) continue;
                int maxIndex = -1;
                clearDerivedLiterals(derivedTrueLiterals);
                localModel[selectedLiteral] = 1;
                derivedTrueLiterals.add(selectedLiteral);

                for (int i = 0; i < derivedTrueLiterals.size(); ++i) {
                    //maxIndex = checkClauses(derivedTrueLiterals.getInt(i));
                    if (maxIndex >= 0) break;}
                statistics.addDerivedLiteralLength(derivedTrueLiterals.size());
                  if (maxIndex == -1) { // no contradiction
                    trueLiteralIndex[selectedLiteral] = index; continue;}
                else selectedLiteral *= -1;}

            clearDerivedLiterals(derivedTrueLiterals);
            localModel[-selectedLiteral] = -1;
            trueLiteralIndex[-selectedLiteral] = 0;
            predicateSequence[index] = selectedLiteral;
            derivedTrueLiterals.add(selectedLiteral);
            int maxIndex = -1;
            for(int i = 0; i < derivedTrueLiterals.size(); ++i) {
                int literal = derivedTrueLiterals.getInt(i);
                //maxIndex = checkClauses(literal);
                if(maxIndex >= 0) {break;}}
            statistics.addDerivedLiteralLength(derivedTrueLiterals.size());
             if(maxIndex == -1) { // no contradiction
                trueLiteralIndex[-selectedLiteral] = index;
                continue;}

            if(maxIndex == 0) ++maxIndex;
            // negative literal also caused a contradiction. Backtracking.

            if((index - maxIndex) > 1) ++statistics.backjumps;
            while(maxIndex > firstIndex && predicateSequence[maxIndex] < 0) --maxIndex;
            if(maxIndex == firstIndex && predicateSequence[maxIndex] < 0)
                return new UnsatClauses(problemId,solverId, startTime);

            // now predicateIndex[maxIndex] > 0 or maxIndex is at firstIndex

            ++statistics.backtrackings;
            while(index > maxIndex) {
                if(predicateSequence[index] < 0) predicateSequence[index] *= -1;
                clearDerivedLiterals(derivedTrueLiteralArray[index--]);
            }
            if(predicateSequence[index] > 0) predicateSequence[index] *= -1;
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
        if(falseLiterals == expandedSize - 2) {
            }
        return -1;}



    private int timestamp = 1;





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


    /** initializes the predicate sequence.
     * <br>
     * The predicates are sorted as follows:<br>
     * - predicateArrangement == 1: just the sequence of natural numbers: 1,2,...<br>
     * - predicateArrangement == 2: predicates with more literal occurrences first<br>
     * - predicateArrangement == 3: predicates with less literal occurrences first.
     */
    void initializePredicateSequence() {
        Integer[] predicateIndex = new Integer[predicates+1];
        for (int predicate = 1; predicate <= predicates; ++predicate)
            predicateIndex[predicate] = predicate;
        if(predicateArrangement == 1) return;
        int sign = predicateArrangement == 2 ? -1 : 1;
        Arrays.sort(predicateIndex,1,predicates+1,
                Comparator.comparingInt(i -> sign * (literalIndex.size(i) + literalIndex.size(-i))));}


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
