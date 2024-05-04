package Solvers.Backtracker;

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
import java.util.function.IntSupplier;

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
                    //maxIndex = checkClauses(derivedTrueLiterals.getInt(i));
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
                //maxIndex = checkClauses(literal);
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
