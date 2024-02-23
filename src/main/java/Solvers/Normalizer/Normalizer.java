package Solvers.Normalizer;

import Datastructures.Clauses.InputClauses;
import Datastructures.Clauses.Quantifier;
import Datastructures.LinkedItemList;
import Datastructures.Results.Result;
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

    /**
     * the global model.
     */
    public Model model;

    public Monitor monitor;

    public boolean monitoring;
    public String monitorId;

    public Symboltable symboltable;

    public boolean trackReasoning;

    private ArrayList<Clause>[] positiveOccurrences;

    private ArrayList<Clause>[] negativeOccurrences;
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
    }

    public Normalizer(ProblemSupervisor problemSupervisor) {
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

        }catch(InterruptedException ex) {
            ex.printStackTrace();
            return;}
        }}


    public Result normalizeClauses() {
        InputClauses inputClauses = problemSupervisor.inputClauses;
        try {
            for (int[] inputClause : inputClauses.conjunctions) normalizeConjunction(inputClause);
            for (int[] inputClause : inputClauses.equivalences) normalizeEquivalence(inputClause);
            // if (equivalences.size() > 1) joinEquivalences();
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
    private void normalizeConjunction(int[] inputClause) throws Unsatisfiable {
        for(int i = Quantifier.AND.firstLiteralIndex; i < inputClause.length; ++i) {
            int literal = inputClause[i];
            if(monitoring) monitor.println(monitorId, "adding literal " +
                    Symboltable.toString(literal,symboltable) + " to the model.");
            ++statistics.initialTrueLiterals;
            addTrueLiteral(literal,trackReasoning ? new InfInputClause(inputClause[0]) : null);}}

    private void normalizeEquivalence(int[] inputClause) {}

    private void transformAndSimplify(int[] inputClause) throws Unsatisfiable {
        Clause clause = new Clause(inputClause);
        if(clause.quantifier == Quantifier.AND) {addTrueLiteral(clause); return;} // unit clause
        if (clause.removeComplementaries(trackReasoning, monitor, symboltable)) return; // true clause
        if (clause.isDisjunction()) {addClause(clause); return;} // no further simplifications are possible here.
        if(!clause.reduceNumbers(this::addTrueLiteral,trackReasoning, monitor, symboltable)) return;

    }

    private void insertClauseToIndex(Clause clause) {
        for(int literal : clause.literals) {
            int predicate = Math.abs(literal);
            ArrayList<Clause>[] clausesArray = (literal > 0) ? positiveOccurrences : negativeOccurrences;
            ArrayList<Clause> clauses = clausesArray[predicate];
            if(clauses == null) {clauses = new ArrayList<>(clauses); clausesArray[predicate] = clauses;}
            clauses.add(clause);}}

    private void removeClauseFromIndex(Clause clause) {
        for(int literal : clause.literals) {
            int predicate = Math.abs(literal);
            ArrayList<Clause>[] clausesArray = (literal > 0) ? positiveOccurrences : negativeOccurrences;
            ArrayList<Clause> clauses = clausesArray[predicate];
            if(clauses != null) clauses.remove(clause);}}




    private void applyTrueLiteral(int literal, InferenceStep inferenceStep) throws Unsatisfiable {
        int predicate = Math.abs(literal);
        ArrayList<Clause>[] clausesArray = (literal > 0) ? positiveOccurrences : negativeOccurrences;
        for(int i = 1; i >= -1; i -= 2) { // i = 1: true(literal), i = -1: false(-literal)
            ArrayList<Clause> clauses = clausesArray[predicate];
            if(clauses != null) {
                for(int j = 0; j < clauses.size(); ++j) {
                    Clause clause = clauses.get(j);
                    clauses.remove(j--);
                    if(clause.applyTrueLiteral(literal,inferenceStep, trackReasoning,monitor, symboltable)) {
                        removeClauseFromIndex(clause);
                    };

            }}
            clausesArray = (literal > 0) ? negativeOccurrences : positiveOccurrences;}
    }

    private void addTrueLiteral(Clause clause) throws Unsatisfiable {}

    private void addTrueLiteral(int literal, InferenceStep step) throws Unsatisfiable{
            model.add(myThread,literal,step);
            queue.add(new Task(literal,step));
    }



}