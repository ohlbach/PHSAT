package Solvers.Resolution;

import Coordinator.Tasks.Task;
import Coordinator.Tasks.TaskQueue;
import Datastructures.Clauses.Clause;
import Datastructures.Literals.CLiteral;
import Datastructures.Literals.LitAlgorithms;
import Datastructures.Results.Aborted;
import Datastructures.Results.Result;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Statistics.Statistic;
import Datastructures.Symboltable;
import Datastructures.Theory.EquivalenceClasses;
import Datastructures.Theory.Transformers;
import Management.ProblemSupervisor;
import Solvers.Solver;
import Utilities.BucketSortedIndex;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.function.Consumer;

/**
 * Created by ohlbach on 01.01.2020.
 */
public abstract class ResolutionReduction extends Solver {

    protected TaskQueue taskQueue = null;

    /** collects statistical information */
    public ResolutionStatistics statistics;

    /** for forming the ids of the new clauses */
    protected int[] id = new int[]{0};

    /** constructs a new Resolution solver.
     *
     * @param solverNumber         for distinguishing different solvers of the same type, but different parameters
     * @param solverParameters     contains the parameters for controlling the solver
     * @param problemSupervisor    coordinates several solvers.
     */
    public ResolutionReduction(Integer solverNumber, HashMap<String,Object> solverParameters, ProblemSupervisor problemSupervisor) {
        super(solverNumber,solverParameters, problemSupervisor);}

    protected boolean checkConsistency = true;

    /** maps literals (numbers) to their occurrences in clauses */
    protected BucketSortedIndex<CLiteral<Clause>> literalIndex;

    /** is set false after all initial clauses are integrated */
    protected boolean initializing = true;

    /** for optimizing subsumption and replacement resolution operations */
    protected int timestamp = 1;

    protected int maxClauseLength = 3;

    /** EquivalenceClasses manage equivalent literals.
     *  In each equivalence class the literals are mapped to their representatives,
     *  which is always the predicate with the smallest number.
     */
    protected EquivalenceClasses equivalenceClasses = null;

    /** If an equivalence p = -p occurs or can be derived, this function is called.
     *  It adds an Unsatisfiable task to the task queue.
     */
    protected Consumer<String> contradictionHandler = ((reason)->{
        taskQueue.add(new Task(0,(()-> new Unsatisfiable(reason)), (()->reason)));});

    protected int maxInputId = 0;

    protected abstract void initializeData();
    protected abstract Result doTheWork() throws InterruptedException;
    protected abstract void simplifyBackwards(Clause clause);
    protected abstract void simplifyForward(Clause clause);
    protected abstract void insertClause(Clause clause, String reason);
    protected abstract void removeClause(Clause clause, int ignoreLiteral);
    protected abstract void check(String info);

    protected abstract Result processTrueLiteral(int literal);

    /** This method controls the entire resolution sequence
     * 1. All local data are initialized <br>
     * 2. All basic clauses are transformed to clauses and distributed to the primary and secondary clause lists. <br>
     *    Equivalences are turned to equivalence classes <br>
     *    All literals are mapped to the representatives in their classes.<br>
     * 3. The initial causes are put into the task queue for simplifications.<br>
     * 4. Resolution and simplification is started until <br>
     *     - a contradiction is formed or <br>
     *     - the clauses became empty or<br>
     *     - the resolution limit is exceeded or <br>
     *     - the thread is interrupted.<br>
     *
     * @return the result of the resolution sequence.
     */
    public Result solve() {
        super.initialize();
        globalParameters.log(solverId + " for problem " + problemId + " started");
        long time = System.currentTimeMillis();
        initializeData();
        Result result = null;
        try{result = initializeClauses();
            if(result == null) {result = doTheWork();}}
        catch(InterruptedException ex) {
            globalParameters.log("Resolution " + combinedId + " interrupted.\n");
            result = new Aborted("Resolution aborted after ");}
        statistics.elapsedTime = System.currentTimeMillis() - time;
        System.out.println("RESULT " + result.toString());
        problemSupervisor.finished(this, result, "done");
        return result;}


    /** This function is called when a new disjunction is to be inserted.
     *  It generates a simplifyBackwards task.
     */
    private Consumer<Clause> insertHandler = (
            clause -> {insertClause(clause,"Initial clause");
                if(clause.size() > 1) {
                    taskQueue.add(new Task(basicClauseList.maxClauseLength-clause.size()+3, // longer clauses should be
                            (()-> {simplifyBackwards(clause); return null;}),    // checked first for subsumption and replacement resolution
                            (()-> "Simplify initial clause " + clause.toString())));}});


    /** This method translates all basic clauses into Clause data structures.
     *  Equivalent literals are replaced by their representatives.
     *
     * @return possibly Unsatisfiable
     * @throws InterruptedException
     */
    protected Result initializeClauses() throws InterruptedException {
        if(basicClauseList.equivalences != null) {
            equivalenceClasses = Transformers.prepareEquivalences(basicClauseList,contradictionHandler,symboltable);
            if(!taskQueue.isEmpty()) {Result result = taskQueue.run(); if(result != null) {return result;}}}

        Transformers.prepareConjunctions(basicClauseList,equivalenceClasses,
                (literal-> addTrueLiteralTask(literal, true, "Initial Conjunction")));
        if(Thread.interrupted()) {throw new InterruptedException();}
        Transformers.prepareDisjunctions(basicClauseList,id,equivalenceClasses,insertHandler);
        Transformers.prepareXors     (basicClauseList,id,equivalenceClasses,insertHandler);
        Transformers.prepareDisjoints(basicClauseList,id,equivalenceClasses,insertHandler);
        initializing = false;
        if(Thread.interrupted()) {throw new InterruptedException();}
        if(checkConsistency) {check("initializeClauses");}
        maxInputId = id[0];
        purityAndElimination();
        return taskQueue.run();}


    private ArrayList<Integer> zeros = new ArrayList<>();
    private ArrayList<Integer> ones = new ArrayList<>();

    /** This method checks all predicates for purity and elimination.
     * A literal p is pure if there are no clauses with p any more. <br>
     * In this case -p can be made true. <br>
     * A literal p can be eliminated if it occurs only once in the clauses, say in clause C.
     * In this case all clauses with -p can be replaced with their resolvent with C.
     */
    protected void purityAndElimination() {
        while(literalIndex.size01(predicates,zeros,ones)) {
            for(int literal : zeros) {
                int sizep = literalIndex.size01(literal);
                if(sizep == 0) {
                    if(monitoring) {monitor.print(combinedId, "Eliminating pure literal " + -literal);}
                    processTrueLiteral(-literal);}}
            for(int literal : ones)  {processElimination(literal);}}}

    protected ArrayList<Object[]> eliminatedLiterals = new ArrayList<>();

    protected void processElimination(int eliminateLiteral) {
        int size01p = literalIndex.size01(eliminateLiteral);
        int size01n = literalIndex.size01(-eliminateLiteral);
        if(size01p != 1 || size01n == 0) {return;}
        //System.out.println("Start Elimination " + eliminateLiteral );
        //System.out.println(toString());
        Clause clause  = literalIndex.getAllItems(eliminateLiteral).get(0).clause;
        ArrayList<CLiteral<Clause>> literals = clause.cliterals;
        for(CLiteral<Clause> otherCliteral : literalIndex.getAllItems(-eliminateLiteral)) {
            boolean tautology = false;
            Clause otherClause = otherCliteral.clause;
            Clause newClause = new Clause(++id[0]);
            ArrayList<CLiteral<Clause>> newLiterals = new ArrayList<>();
            for(CLiteral<Clause> literal : literals) {
                if(literal.literal != eliminateLiteral) {newLiterals.add(new CLiteral(literal.literal,newClause,newLiterals.size()));}}
            for(CLiteral<Clause> literal : otherClause.cliterals) {
                if(literal.literal == -eliminateLiteral) {continue;}
                int contained = LitAlgorithms.contains(newLiterals,literal.literal);
                if(contained > 0) {continue;}
                if(contained < 0) {tautology = true; break;}
                newLiterals.add(new CLiteral(literal.literal,newClause,newLiterals.size()));}
            if(tautology) {removeClause(otherClause,0); continue;}
            newClause.cliterals = newLiterals;
            newClause.setStructure();
            simplifyBackwards(newClause);
            if(newClause.removed) {removeClause(otherClause,0); continue;}
            removeClause(otherClause,0);
            insertClause(newClause,"Literal " + eliminateLiteral + " eliminated");}
        eliminatedLiterals.add(new Object[]{clause.cliterals,eliminateLiteral});
        removeClause(clause,0);
        literalIndex.clearBoth(Math.abs(eliminateLiteral));
        if(checkConsistency) {check("processElimination");}
        //System.out.println("End Elimination " + eliminateLiteral + "@" + clause.toString());
        //System.out.println(toString());
    }

    /** turns the literal into a trueLiteralTask.
     * If it is a unit resolvent then it is forwarded to the problem supervisor.
     *
     * @reason  for monitoring the tasks
     * @param literal a unit literal.
     */
    protected void addTrueLiteralTask(int literal, boolean forward, String reason) {
        taskQueue.add(new Task(1,
                (()->processTrueLiteral(literal)),
                (()->reason + ": " + (symboltable == null ? literal : symboltable.getLiteralName(literal)))));
        ++statistics.derivedUnitClauses;
        if(forward && !initializing) problemSupervisor.forwardTrueLiteral(this,literal);}

    public void newBinaryClause(int literal1, int literal2) {
        ++statistics.importedBinaryClauses;
        taskQueue.add(new Task(2,
                (()-> {importBinaryClause(literal1,literal2); return null;}),
                (()-> "importing binary clause " + literal1 + " " + literal2)));}


    protected void importBinaryClause(int literal1, int literal2) {
        literal1 = equivalenceClasses.mapToRepresentative(literal1);
        literal2 = equivalenceClasses.mapToRepresentative(literal2);
        if(literal1 == literal2) {newTrueLiteral(literal1); return;}
        if(literal1 == -literal2 || model.isTrue(literal1) || model.isTrue(literal2)) {return;}
        if(model.isFalse(literal1)) {newTrueLiteral(-literal2); return;}
        if(model.isFalse(literal2)) {newTrueLiteral(-literal1); return;}
        Clause clause = new Clause(++id[0],2);
        clause.add(new CLiteral(literal1));
        clause.add(new CLiteral(literal2));
        clause.setStructure();
        simplifyBackwards(clause);
        if(clause.removed) {return;}
        insertClause(clause, "imported clause");
        simplifyForward(clause);}

    public void newClause(int[] literals) {
        ++statistics.importedOtherClauses;
        taskQueue.add(new Task(literals.length,
                (()-> {importClause(literals); return null;}),
                (()-> "importing clause " + Arrays.toString(literals))));}


    protected void importClause(int[] literals) {
        int size = literals.length;
        Clause clause = new Clause(++id[0],literals.length);
        for(int i = 0; i < size; ++i) {
            int literal = equivalenceClasses.mapToRepresentative(literals[i]);
            if(model.isTrue(literal)) {return;}
            if(!model.isFalse(literal)) {clause.add(new CLiteral(literal));}}
        if(clause.hasComplementaries()) {return;}
        clause.removeDoubles();
        if(clause.size() == 1) {newTrueLiteral(clause.getLiteral(0)); return;}
        clause.setStructure();
        simplifyBackwards(clause);
        if(clause.removed) {return;}
        insertClause(clause, "imported clause");
        simplifyForward(clause);}


    /** checks if the clause is part of an equivalence like (p,q) and (-p,-q)
     *
     * @param clause a clause to be checked (p,q)
     * @return true if there is another clause (-p,-q)
     */
    private boolean isEquivalence(Clause clause) {
        if(clause.size() != 2) {return false;}
        ArrayList<CLiteral<Clause>> clits1 = literalIndex.getItems(-clause.getCLiteral(0).literal,2);
        ArrayList<CLiteral<Clause>> clits2 = literalIndex.getItems(-clause.getCLiteral(1).literal,2);
        if(clits1 == null || clits2 == null) {return false;}
        timestamp += maxClauseLength +1;
        for(CLiteral<Clause> clit : clits1) {clit.clause.timestamp = timestamp;}
        for(CLiteral<Clause> clit : clits2) {if(clit.clause.timestamp == timestamp) {return true;}}
        return false;}

    /** This method checks if the clause is part of an equivalence (p,q) (-p,-q)
     * If this is the case: <br>
     *     - -p == q is inserted into the equivalence classes <br>
     *     - a processEquivalence task is generated
     *
     * @param clause the clause to be checked
     * @return true if an equivalence has been found.
     */
    protected boolean findEquivalence(Clause clause) {
        if(!isEquivalence(clause)) {return false;}
        int literal1 = -clause.getCLiteral(0).literal;
        int literal2 =  clause.getCLiteral(1).literal;
        if(literal1 < 0) {literal1 = -literal1; literal2 = -literal2;}
        int fromliteral = literal1; int toliteral = literal2;
        if(!equivalenceClasses.addEquivalence(fromliteral,toliteral)) {return false;}
        taskQueue.add(new Task(2,(()->processEquivalence(fromliteral,toliteral)),
                (()-> "Replacing equivalent literal: "+ fromliteral + " -> " + toliteral)));
        ++statistics.equivalences;
        return true;}

    protected ArrayList<Clause> replacedClauses = new ArrayList<>();

    /** This method replaces all occurrences of fromLiteral by toLiteral.
     *  Generated tautologies are ignored.<br>
     *  Double literals are avoided. <br>
     *  The new clauses are backwards simplified.
     *
     * @param fromLiteral antecedent
     * @param toLiteral   succedent
     * @return            null
     */
    protected Result processEquivalence(int fromLiteral, int toLiteral) {
        //System.out.println("START EQUIVALENCE " + fromLiteral + " -> " + toLiteral);
        //System.out.println(toString());
        int fromStatus = model.status(fromLiteral);
        int toStatus   = model.status(toLiteral);
        if(fromStatus != 0 && toStatus != 0 && fromStatus != toStatus) {
            return new Unsatisfiable(model,toLiteral);}
        if(fromStatus != 0) {
            addTrueLiteralTask((fromStatus == 1 ? toLiteral : -toLiteral),true,
                    "equivalent literals " + fromLiteral + " " + toLiteral);
            return null;}
        if(toStatus != 0) {
            addTrueLiteralTask((toStatus == 1 ? fromLiteral : -fromLiteral),true,
                    "equivalent literals " + fromLiteral + " " + toLiteral);
            return null;}
        replacedClauses.clear();
        for(CLiteral<Clause> cliteral : literalIndex.getAllItems(fromLiteral)) {
            Clause clause = cliteral.clause;
            Clause newClause = new Clause(++id[0],clause.size());
            boolean tautology = false;
            for(CLiteral cLiteral : clause.cliterals) {
                int literal = cLiteral.literal;
                if(literal == toLiteral) {continue;}
                if(literal == -toLiteral) {tautology = true; break;}
                if(literal == fromLiteral) {literal = toLiteral;}
                newClause.add(new CLiteral(literal));}
            removeClause(clause,0);
            if(!tautology) {replacedClauses.add(newClause);}}

        for(CLiteral<Clause> cliteral : literalIndex.getAllItems(-fromLiteral)) {
            Clause clause = cliteral.clause;
            Clause newClause = new Clause(++id[0],clause.size());
            boolean tautology = false;
            for(CLiteral cLiteral : clause.cliterals) {
                int literal = cLiteral.literal;
                if(literal == -toLiteral) {continue;}
                if(literal == toLiteral) {tautology = true; break;}
                if(literal == -fromLiteral) {literal = -toLiteral;}
                newClause.add(new CLiteral(literal));}
            removeClause(clause,0);
            if(!tautology) {replacedClauses.add(newClause);}}

        literalIndex.clearBoth(fromLiteral);
        for(Clause clause : replacedClauses) {
            simplifyBackwards(clause);
            if(!clause.removed) {
                insertClause(clause,"Literal " + fromLiteral + " -> " + toLiteral);}}
        for(Clause clause : replacedClauses) {if(!clause.removed) {simplifyForward(clause);}}

        //System.out.println("END EQUIVALENCE " + fromLiteral + " -> " + toLiteral);
        //System.out.println(toString());
        return null;}



    /** return the entire statistics information
     *
     * @return the entire statistics information for the resolution solver.
     */
    public Statistic getStatistics() {return statistics;}

    public abstract String toString(Symboltable symboltable);

    /** lists the clauses and the literal index as a string.
     *
     * @return the clauses and the literal index as a string.
     */
    public String toString() {return toString(null);}

}
