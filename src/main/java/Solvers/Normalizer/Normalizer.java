package Solvers.Normalizer;

import Datastructures.Clauses.InputClauses;
import Datastructures.Clauses.Quantifier;
import Datastructures.LinkedItemList;
import Datastructures.Results.Result;
import Datastructures.Results.UnsatClause;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Datastructures.Theory.Model;
import InferenceSteps.InfInputClause;
import InferenceSteps.InferenceStep;
import Management.Monitor.Monitor;
import Management.ProblemSupervisor;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.concurrent.PriorityBlockingQueue;

public class Normalizer {

    ProblemSupervisor problemSupervisor;

    String problemId;
    String solverId = "Normalizer";

    /**
     * the global model.
     */
    public Model model;

    public Monitor monitor;

    public boolean monitoring;
    public String monitorId;

    public Symboltable symboltable;

    public boolean trackReasoning;

    private ArrayList<Clause>[] positiveOccAtleast;
    private ArrayList<Clause>[] positiveOccAtmost;
    private ArrayList<Clause>[] positiveOccInterval;

    private ArrayList<Clause>[] negativeOccAtleast;
    private ArrayList<Clause>[] negativeOccAtmost;
    private ArrayList<Clause>[] negativeOccInterval;

    private ArrayList<Clause>[][] posOccurrences = new ArrayList[3][];
    private ArrayList<Clause>[][] negOccurrences = new ArrayList[3][];

    public Thread myThread;

    /** A queue of newly derived unit literals and binary equivalences.
     * The unit literals are automatically put at the beginning of the queue.
     */
    private final PriorityBlockingQueue<Task> queue =
            new PriorityBlockingQueue<>(100, Comparator.comparingInt(task->task.priority));


    /**
     * the normalizer statistics
     */
    NormalizerStatistics statistics;

    public LinkedItemList<Clause> clauses = new LinkedItemList<>("Normalized Clauses");

    private void addClause(Clause clause) {
        clauses.addToBack(clause);
    }

    private void removeClause(Clause clause) {
        clauses.remove(clause);
        removeClauseFromIndex(clause);}

    public Normalizer(ProblemSupervisor problemSupervisor) {
        problemId = problemSupervisor.problemId;
        model = problemSupervisor.model;
        statistics = new NormalizerStatistics(null);
        monitor = problemSupervisor.monitor;
        monitoring = monitor != null;
        symboltable = problemSupervisor.inputClauses.symboltable;
        trackReasoning = problemSupervisor.globalParameters.trackReasoning;
        monitorId = "Normalizer_"+ problemSupervisor.problemId;
        myThread = Thread.currentThread();
    }

    /** reads the next task from the task queue and processes it.
     *
     * @param n 0 or the maximum number of task before stopping the loop (0: unlimited) (> 0 for test purposes).
     * @throws Result if a contradiction is encountered or the clause set became empty.
     */
    public void processTasks(final int n) throws Result {
        Task task;
        int counter = 0;
        while(!myThread.isInterrupted()) {
            try{
            task = queue.take(); // waits if the queue is empty
                if(task.trueLiteral != 0) applyTrueLiteral(task.trueLiteral,task.inferenceStep);
                else {applyEquivalence(task.eqLiteral1,task.eqLiteral2,task.inferenceStep); }

        }catch(InterruptedException ex) {
            ex.printStackTrace();
            return;}
        }}


    public Result normalizeClauses() {
        InputClauses inputClauses = problemSupervisor.inputClauses;
        try {
            for (int[] inputClause : inputClauses.conjunctions) makeTrueLiteralTask(inputClause);
            for (int[] inputClause : inputClauses.equivalences) makeEquivalenceTask(inputClause);
            for (int[] inputClause : inputClauses.disjunctions) transformAndSimplify(inputClause);
            for (int[] inputClause : inputClauses.atleasts) transformAndSimplify(inputClause);
            for (int[] inputClause : inputClauses.atmosts) transformAndSimplify(inputClause);
            for (int[] inputClause : inputClauses.exactlys) transformAndSimplify(inputClause);
            for (int[] inputClause : inputClauses.intervals) transformAndSimplify(inputClause);}
        catch (Result result) {}
        // from now on the equivalence classes are no longer needed.
        // while (newTrueLiterals) { // fixpoint iteration until no new true literals are derived.
        //     newTrueLiterals = false;
        //    for (int i = clauses.size() - 1; i >= 0; --i) replaceTrueLiterals(i);
        statistics.survivedClauses = clauses.size();
        //return clauses.isEmpty() ? new Satisfiable(problemId, solverId, model) : null;
        return null;
    }

    /**
     *
     * @param inputClause
     * @throws Unsatisfiable
     */
    private void makeTrueLiteralTask(int[] inputClause) throws Unsatisfiable {
        for(int i = Quantifier.AND.firstLiteralIndex; i < inputClause.length; ++i) {
            int literal = inputClause[i];
            if(monitoring) monitor.println(monitorId, "adding true literal " +
                    Symboltable.toString(literal,symboltable) + " to the model.");
            ++statistics.initialTrueLiterals;
            addTrueLiteralTask(literal,trackReasoning ? new InfInputClause(inputClause[0]) : null);}}

    private void makeEquivalenceTask(int[] inputClause) {
        int sign = 1;
        int representative = inputClause[Quantifier.EQUIV.firstLiteralIndex];
        if(representative < 0) {sign = -1; representative *= -1;}
        for(int i = Quantifier.EQUIV.firstLiteralIndex+1; i < inputClause.length; ++i) {
            addEquivalenceTask(representative,sign*inputClause[i],
                    trackReasoning ? new InfInputClause(inputClause[0]) : null);}}

    private void transformAndSimplify(int[] inputClause) throws Unsatisfiable {
        Clause clause = new Clause(inputClause,trackReasoning,monitor,symboltable);
        if(clause.quantifier == Quantifier.AND) {addTrueLiteralTask(clause); return;} // unit clause
        Clause conjunction = clause.simplify(trackReasoning,monitor,symboltable);
        if(conjunction != null) {
            for(int i = 0; i < conjunction.literals.size()-1; i +=2) {
                addTrueLiteralTask(conjunction.literals.getInt(i),new InfInputClause(conjunction.id));}
            return;}
        if(clause.isTrue) return;
        if(clause.isFalse) throw new UnsatClause(null,null,clause.inputClause);
        insertClauseToIndex(clause);
        clauses.addToBack(clause);}

    private void insertClauseToIndex(Clause clause) {
        ArrayList<Clause>[] positiveArrayList = null;
        ArrayList<Clause>[] negativeArrayList = null;
        switch(clause.quantifier) {
            case OR:
            case ATLEAST:
                if(positiveOccAtleast == null) {
                    positiveOccAtleast = new ArrayList[model.predicates+1];
                    posOccurrences[0] = positiveOccAtleast;}
                if(negativeOccAtleast == null) {
                    negativeOccAtleast = new ArrayList[model.predicates+1];
                    negOccurrences[0] = negativeOccAtleast;}
                positiveArrayList = positiveOccAtleast;
                negativeArrayList = negativeOccAtleast;
                break;
            case ATMOST:
                if(positiveOccAtmost == null) {
                    positiveOccAtmost = new ArrayList[model.predicates+1];
                    posOccurrences[1] = positiveOccAtmost;}
                if(negativeOccAtmost == null) {
                    negativeOccAtmost = new ArrayList[model.predicates+1];
                    negOccurrences[1] = negativeOccAtmost;}
                positiveArrayList = positiveOccAtmost;
                negativeArrayList = negativeOccAtmost;
                break;
            case EXACTLY:
            case INTERVAL:
                if (positiveOccInterval == null) {
                    positiveOccInterval = new ArrayList[model.predicates + 1];
                    posOccurrences[2] = positiveOccInterval;}
                if (negativeOccInterval == null) {
                    negativeOccInterval = new ArrayList[model.predicates + 1];
                    negOccurrences[2] = negativeOccInterval;}
                positiveArrayList = positiveOccInterval;
                negativeArrayList = negativeOccInterval;}

        for(int i = 0; i < clause.literals.size()-1; i += 2) {
            int literal = clause.literals.getInt(i);
            int predicate = Math.abs(literal);
            ArrayList<Clause> clausesArray = null;
            if(literal > 0) {
                clausesArray = positiveArrayList[predicate];
                if(clausesArray == null) {clausesArray = new ArrayList<>(); positiveArrayList[predicate] = clausesArray;}}
            else {
                clausesArray = negativeArrayList[predicate];
                if(clausesArray == null) {clausesArray = new ArrayList<>(); negativeArrayList[predicate] = clausesArray;}}
            clausesArray.add(clause);}}

    private void removeClauseFromIndex(Clause clause) {
        ArrayList<Clause>[] positiveArrayList = null;
        ArrayList<Clause>[] negativeArrayList = null;
        switch(clause.quantifier) {
            case OR:
            case ATLEAST:
                positiveArrayList = positiveOccAtleast;
                negativeArrayList = negativeOccAtleast;
                break;
            case ATMOST:
                positiveArrayList = positiveOccAtmost;
                negativeArrayList = negativeOccAtmost;
                break;
            case EXACTLY:
            case INTERVAL:
                positiveArrayList = positiveOccInterval;
                negativeArrayList = negativeOccInterval;}

        for(int i = 0; i < clause.literals.size()-1; i += 2) {
            int literal = clause.literals.getInt(i);
            int predicate = Math.abs(literal);
            ArrayList<Clause> clausesArray = (literal > 0) ? positiveArrayList[predicate] : negativeArrayList[predicate];
            clausesArray.remove(clause);}}




    private void applyTrueLiteral(int literal, InferenceStep inferenceStep) throws Unsatisfiable {
        int predicate = Math.abs(literal);
        for(ArrayList<Clause>[] clausesList : (literal > 0) ? posOccurrences : negOccurrences) {
            if(clausesList != null) {
                ArrayList<Clause> clausesArray = clausesList[predicate];
                if(clausesArray != null) {
                    for(Clause clause : clausesArray) {
                        if(clause.isDisjunction()) {removeClause(clause); continue;}
                        removeClauseFromIndex(clause);
                        if(clause.applyTrueLiteral(literal,inferenceStep,trackReasoning,monitor,symboltable)) {
                            Clause conjunction = clause.simplify(trackReasoning,monitor,symboltable);
                            if (conjunction != null) {addTrueLiteralTask(conjunction);}
                            if(clause.isTrue) {removeClause(clause); continue;}
                            if(clause.isFalse) throw new UnsatClause(problemId,solverId, clause);
                            insertClauseToIndex(clause);}}}}}}

    private void addTrueLiteralTask(Clause clause) throws Unsatisfiable {}

    private void addTrueLiteralTask(int literal, InferenceStep step) throws Unsatisfiable{
            model.add(myThread,literal,step);
            queue.add(new Task(literal,step));
    }

    private void addEquivalenceTask(int representative, int equivalentLiteral, InferenceStep step){
        queue.add(new Task(representative, equivalentLiteral,step));
    }

    private void applyEquivalence(int representative, int equivalentLiteral, InferenceStep step) throws Result {
        int predicate = Math.abs(equivalentLiteral);
        for(ArrayList<Clause>[] clausesList : (equivalentLiteral > 0) ? posOccurrences : negOccurrences) {
            if(clausesList != null) {
                ArrayList<Clause> clausesArray = clausesList[predicate];
                if(clausesArray != null) {
                    for(Clause clause : clausesArray) {
                        if(clause.isDisjunction()) {removeClause(clause); continue;}
                        removeClauseFromIndex(clause);
                        Clause conjunction =clause.replaceEquivalentLiterals(equivalentLiteral,representative, step,trackReasoning,monitor,symboltable);
                        if (conjunction != null) {addTrueLiteralTask(conjunction);}
                        if(clause.isTrue) {removeClause(clause); continue;}
                        if(clause.isFalse) throw new UnsatClause(problemId,solverId, clause);
                        insertClauseToIndex(clause);}}}}}



}