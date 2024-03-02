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
import Solvers.Normalizer.NMInferenceSteps.NMISClause;

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

    public int predicates;

    private ArrayList<Clause>[] positiveOccAtleast;
    private ArrayList<Clause>[] positiveOccAtmost;
    private ArrayList<Clause>[] positiveOccInterval;

    private ArrayList<Clause>[] negativeOccAtleast;
    private ArrayList<Clause>[] negativeOccAtmost;
    private ArrayList<Clause>[] negativeOccInterval;

    private final ArrayList<Clause>[][] posOccurrences = new ArrayList[3][];
    private final ArrayList<Clause>[][] negOccurrences = new ArrayList[3][];

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

    LinkedItemList<Clause> clauses = new LinkedItemList<>("Normalized Clauses");

    void removeClause(Clause clause) {
        clauses.remove(clause);
        removeClauseFromIndex(clause);}

    public Normalizer(ProblemSupervisor problemSupervisor) {
        this.problemSupervisor = problemSupervisor;
        problemId = problemSupervisor.problemId;
        model = problemSupervisor.model;
        statistics = new NormalizerStatistics(null);
        monitor = problemSupervisor.monitor;
        monitoring = monitor != null;
        symboltable = problemSupervisor.inputClauses.symboltable;
        trackReasoning = problemSupervisor.globalParameters.trackReasoning;
        predicates = problemSupervisor.inputClauses.predicates;
        monitorId = "Normalizer_"+ problemSupervisor.problemId;
        myThread = Thread.currentThread();
    }

    /** reads the next task from the task queue and processes it.
     *
     * @param n 0 or the maximum number of task before stopping the loop (0: unlimited) (> 0 for test purposes).
     * @throws Result if a contradiction is encountered or the clause set became empty.
     */
    void processTasks(final int n) throws Result {
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


    Result normalizeClauses() {
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

    /** Adds all literals in the clause (a conjunction) as true literal task to the queue and inserts them into the model.
     *
     * @param inputClause a conjunction.
     * @throws Unsatisfiable if the model discovers an inconsistency.
     */
    void makeTrueLiteralTask(int[] inputClause) throws Unsatisfiable {
        InferenceStep step = trackReasoning ? new InfInputClause(inputClause[0]) : null;
        for(int i = Quantifier.AND.firstLiteralIndex; i < inputClause.length; ++i) {
            int literal = inputClause[i];
            if(monitoring) monitor.println(monitorId, "adding true literal " +
                    Symboltable.toString(literal,symboltable) + " to the model.");
            ++statistics.initialTrueLiterals;
            addTrueLiteralTask(literal,step);}}

    /** Adds all literals in the clause (a conjunction) as true literal task to the queue and inserts them into the model.
     *
     * @param clause a conjunction.
     * @throws Unsatisfiable if the model discovers an inconsistency.
     */
    void makeTrueLiteralTask(Clause clause) throws Unsatisfiable{
        InferenceStep step = trackReasoning ? new NMISClause(clause) : null;
        for(int i = 0; i < clause.literals.size()-1; i += 2) {
            int literal = clause.literals.getInt(i);
            if(monitoring) monitor.println(monitorId, "adding true literal " +
                    Symboltable.toString(literal,symboltable) + " to the model.");
            ++statistics.initialTrueLiterals;
            addTrueLiteralTask(literal,step);}}

    /**Adds the literal as true literal to the model and to the queue,
     *
     * @param literal a true literal
     * @param step    which caused the literal to be true.
     * @throws Unsatisfiable if the model discovers an inconsistency.
     */
    void addTrueLiteralTask(int literal, InferenceStep step) throws Unsatisfiable{
        model.add(myThread,literal,step);
        queue.add(new Task(literal,step));}

    void makeEquivalenceTask(int[] inputClause) {
        int sign = 1;
        int representative = inputClause[Quantifier.EQUIV.firstLiteralIndex];
        if(representative < 0) {sign = -1; representative *= -1;}
        for(int i = Quantifier.EQUIV.firstLiteralIndex+1; i < inputClause.length; ++i) {
            addEquivalenceTask(representative,sign*inputClause[i],
                    trackReasoning ? new InfInputClause(inputClause[0]) : null);}}

    void transformAndSimplify(int[] inputClause) throws Unsatisfiable {
        Clause clause = new Clause(inputClause,trackReasoning,monitor,symboltable);
        if(clause.quantifier == Quantifier.AND) {makeTrueLiteralTask(clause); return;} // unit clause
        if(clause.isTrue) return;
        if(clause.isFalse) throw new UnsatClause(null,null,clause.inputClause);
        Clause conjunction = clause.simplify(trackReasoning,monitor,symboltable);
        if(conjunction != null) makeTrueLiteralTask(conjunction);
        if(clause.isTrue) return;
        if(clause.isFalse) throw new UnsatClause(null,null,clause.inputClause);
        addClauseToIndex(clause);
        clauses.addToBack(clause);}



    /** add the given clause to the corresponding index (occurrence lists).
     * <br>
     * The lists map literals to the clauses containing the literal.<br>
     * or- and atleast clauses are added to positiveOccAtleast and negativeOccAtleast.<br>
     * atmost-clauses are added to positiveOccAtmost and negativeOccAtmost.<br>
     * exactly- and interval-clauses are added to positiveOccInterval and negativeOccInterval.<br>
     * If the lists do not yet exist, they are created.<br>
     * The difference lists support the identification of pure and singleton literals.
     *
     * @param clause the clause to be added.
     */
    void addClauseToIndex(Clause clause) {
        ArrayList<Clause>[] positiveArrayList = null;
        ArrayList<Clause>[] negativeArrayList = null;
        switch(clause.quantifier) {
            case OR:
            case ATLEAST:
                if(positiveOccAtleast == null) {
                    positiveOccAtleast = new ArrayList[predicates+1];
                    negativeOccAtleast = new ArrayList[predicates+1];}
                positiveArrayList = positiveOccAtleast;
                negativeArrayList = negativeOccAtleast;
                break;
            case ATMOST:
                if(positiveOccAtmost == null) {
                    positiveOccAtmost = new ArrayList[predicates+1];
                    negativeOccAtmost = new ArrayList[predicates+1];}
                positiveArrayList = positiveOccAtmost;
                negativeArrayList = negativeOccAtmost;
                break;
            case EXACTLY:
            case INTERVAL:
                if(positiveOccInterval == null) {
                    positiveOccInterval = new ArrayList[predicates+1];
                    negativeOccInterval = new ArrayList[predicates+1];}
                positiveArrayList = positiveOccInterval;
                negativeArrayList = negativeOccInterval;}

        for(int i = 0; i < clause.literals.size()-1; i += 2) {
            int literal = clause.literals.getInt(i);
            int predicate = Math.abs(literal);
            ArrayList<Clause> clausesArray = (literal > 0) ? positiveArrayList[predicate] : negativeArrayList[predicate];
            if(clausesArray == null) {
                clausesArray = new ArrayList<>();
                if (literal > 0) {positiveArrayList[predicate] = clausesArray;}
                else             {negativeArrayList[predicate] = clausesArray;}}
            clausesArray.add(clause);}}

    /** removes the clause from the corresponding index lists.
     * Empty list are entirely removed.
     *
     * @param clause the clause to be removed from the list.
     */
     void removeClauseFromIndex(Clause clause) {
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
            clausesArray.remove(clause);
            if (clausesArray.isEmpty()) {
                if (literal > 0) {positiveArrayList[predicate] = null;}
                else             {negativeArrayList[predicate] = null;}}}}




    void applyTrueLiteral(int literal, InferenceStep inferenceStep) throws Unsatisfiable {
        int predicate = Math.abs(literal);
        for(ArrayList<Clause>[] clausesList : (literal > 0) ? posOccurrences : negOccurrences) {
            if(clausesList != null) {
                ArrayList<Clause> clausesArray = clausesList[predicate];
                if(clausesArray != null) {
                    for(Clause clause : clausesArray) {
                        if(clause.isDisjunction()) {removeClause(clause); continue;}
                        removeClauseFromIndex(clause);
                        Clause conjunction = clause.applyTrueLiteral(literal,inferenceStep,trackReasoning,monitor,symboltable);
                        if (conjunction != null) {makeTrueLiteralTask(conjunction);}
                        if(clause.isTrue) {removeClause(clause); continue;}
                        if(clause.isFalse) throw new UnsatClause(problemId,solverId, clause);
                        addClauseToIndex(clause);}}}}}



    void addEquivalenceTask(int representative, int equivalentLiteral, InferenceStep step){
        queue.add(new Task(representative, equivalentLiteral,step));
    }

    void applyEquivalence(int representative, int equivalentLiteral, InferenceStep step) throws Result {
        int predicate = Math.abs(equivalentLiteral);
        for(ArrayList<Clause>[] clausesList : (equivalentLiteral > 0) ? posOccurrences : negOccurrences) {
            if(clausesList != null) {
                ArrayList<Clause> clausesArray = clausesList[predicate];
                if(clausesArray != null) {
                    for(Clause clause : clausesArray) {
                        if(clause.isDisjunction()) {removeClause(clause); continue;}
                        removeClauseFromIndex(clause);
                        Clause conjunction =clause.replaceEquivalentLiterals(equivalentLiteral,representative, step,trackReasoning,monitor,symboltable);
                        if (conjunction != null) {makeTrueLiteralTask(conjunction);}
                        if(clause.isTrue) {removeClause(clause); continue;}
                        if(clause.isFalse) throw new UnsatClause(problemId,solverId, clause);
                        addClauseToIndex(clause);}}}}}


    void processPurity()  throws Unsatisfiable{
        boolean purityFound = true;
        while(purityFound) {
            purityFound = false;
            for(int predicate = 1; predicate <= predicates; ++predicate) {
                int literal = isPositivelyPure(predicate);
                if(literal != 0) {
                    purityFound = true;
                    ArrayList<Clause> causeList = (literal > 0) ? positiveOccAtleast[predicate] : negativeOccAtleast[predicate];
                    addTrueLiteralTask(literal,null);
                    for(int i = causeList.size()-1; i >= 0; --i) {
                        Clause clause = causeList.get(i);
                        removeClauseFromIndex(clause);
                        Clause conjunct = clause.applyTrueLiteral(literal,null,trackReasoning,monitor,symboltable);
                        if(conjunct != null) makeTrueLiteralTask(conjunct);
                        if(clause.isTrue) {clauses.remove(clause); continue;}
                        if(clause.isFalse) throw new UnsatClause(problemId,solverId,clause);
                        addClauseToIndex(clause);}
                    continue;}

                literal = isNegativelyPure(predicate);
                if(literal != 0) {
                    purityFound = true;
                    ArrayList<Clause> causeList = (literal > 0) ? positiveOccAtmost[predicate] : negativeOccAtmost[predicate];
                    addTrueLiteralTask(-literal,null);
                    for(int i = causeList.size()-1; i >= 0; --i) {
                        Clause clause = causeList.get(i);
                        removeClauseFromIndex(clause);
                        Clause conjunct = clause.applyTrueLiteral(-literal,null,trackReasoning,monitor,symboltable);
                        if(conjunct != null) makeTrueLiteralTask(conjunct);
                        if(clause.isTrue) {clauses.remove(clause); continue;}
                        if(clause.isFalse) throw new UnsatClause(problemId,solverId,clause);
                        addClauseToIndex(clause);}
                    continue;}

                literal = isSingletonPure(predicate);
                if(literal != 0) {
                    purityFound = true;
                    Clause clause = (literal > 0) ? positiveOccInterval[predicate].get(0) : negativeOccInterval[predicate].get(0);
                    singletons.add(literal); singletons.add(clause.clone());
                    removeClauseFromIndex(clause);
                    Clause conjunct = clause.removeLiteral(literal,trackReasoning,monitor,symboltable);
                    if(conjunct != null) makeTrueLiteralTask(conjunct);
                    if(clause.isTrue) {clauses.remove(clause); continue;}
                    if(clause.isFalse) throw new UnsatClause(problemId,solverId,clause);
                    addClauseToIndex(clause);}}}
    }

    private ArrayList<Object> singletons = new ArrayList<>();

    /** Checks if the given predicate is positively pure, i.e. it occurs only in atleast- or or-clauses, either positively or negatively
     *
     * @param predicate The predicate to check.
     * @return +predicate if it occurs only positively in atleast or or-clauses, -predicate the other way round, otherwise 0.
     */
    int isPositivelyPure(int predicate) {
        if(positiveOccAtmost == null || positiveOccAtmost[predicate] != null ||
                negativeOccAtmost == null || negativeOccAtmost[predicate] != null ||
                positiveOccInterval == null || positiveOccInterval[predicate] == null ||
                negativeOccInterval == null || negativeOccInterval != null) return 0;
        if((negativeOccAtleast == null || negativeOccAtleast[predicate] == null) &&
                positiveOccAtleast != null && positiveOccAtleast[predicate] != null) return predicate;
        if((positiveOccAtleast == null || positiveOccAtleast[predicate] == null) &&
                negativeOccAtleast != null && negativeOccAtleast[predicate] != null) return -predicate;
        return 0;}

    /** Checks if the given predicate is negatively pure, i.e. it occurs only in atmost-clauses, either positively or negatively.
     *
     * @param predicate The predicate to check.
     * @return +predicate if it occurs only positively in atmost-clauses, -predicate the other way round, otherwise 0.
     */
    int isNegativelyPure(int predicate) {
        if(positiveOccAtleast == null || positiveOccAtleast[predicate] != null ||
                negativeOccAtleast == null || negativeOccAtleast[predicate] != null ||
                positiveOccInterval == null || positiveOccInterval[predicate] == null ||
                negativeOccInterval == null || negativeOccInterval != null) return 0;
        if((negativeOccAtmost == null || negativeOccAtmost[predicate] == null) &&
                positiveOccAtmost != null && positiveOccAtmost[predicate] != null) return predicate;
        if((positiveOccAtmost == null || positiveOccAtmost[predicate] == null) &&
                negativeOccAtmost != null && negativeOccAtmost[predicate] != null) return -predicate;
        return 0;}

    /** checks if the given predicate occurrs only in a single interval clause.
     *
     * @param predicate the predicate to check.
     * @return +predicate if it occurs positively in a single interval clause, -predicate the other way round, otherwise 0.
     */
    int isSingletonPure(int predicate) {
        if(positiveOccAtleast == null || positiveOccAtleast[predicate] != null ||
                negativeOccAtleast == null || negativeOccAtleast[predicate] != null ||
                positiveOccAtmost == null || positiveOccAtmost[predicate] == null ||
                negativeOccAtmost == null || negativeOccAtmost != null) return 0;
        if((negativeOccInterval == null || negativeOccInterval[predicate] == null) &&
                positiveOccInterval != null && positiveOccInterval[predicate].size() == 1) return predicate;
        if((positiveOccInterval == null || positiveOccInterval[predicate] == null) &&
                negativeOccInterval != null && negativeOccInterval[predicate].size() == 1) return -predicate;
        return 0;}

    String toString(Symboltable symboltable) {
         if(clauses.isEmpty()) return "";
         int size = (clauses.lastLinkedItem.id + "." + clauses.lastLinkedItem.version).length();
         StringBuilder sb = new StringBuilder();
         Clause clause = clauses.firstLinkedItem;
         while(clause != null) {
            sb.append(clause.toString(symboltable,size));
            if(clause != clauses.lastLinkedItem) sb.append("\n");
            clause = (Clause)clause.nextItem;}
        return sb.toString();
    }

}