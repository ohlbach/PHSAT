package Solvers.Resolution;

import Coordinator.Tasks.Task;
import Coordinator.Tasks.TaskQueue;
import Datastructures.Clauses.Clause;
import Datastructures.Literals.CLiteral;
import Datastructures.Literals.LitAlgorithms;
import Datastructures.Results.Aborted;
import Datastructures.Results.Result;
import Datastructures.Results.Satisfiable;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Statistics.Statistic;
import Datastructures.Symboltable;
import Datastructures.Theory.EquivalenceClasses;
import Datastructures.Theory.Transformers;
import Management.ProblemSupervisor;
import Solvers.Solver;
import Utilities.BucketSortedIndex;
import Utilities.BucketSortedList;

import java.util.*;
import java.util.function.Consumer;

/** This is the superclass for the Resolution and Reduction classes.
 *  Resolution is as in the textbooks.
 *  <br>
 *  Reduction is a generalisation of replacement resolution. <br>
 *  Examples: <br>
 *      p,q,r <br>
 *      -p,q,r <br>
 *      -----  <br>
 *      q,r     (p is 'resolved away')<br>
 *      <br>
 *      p,q,r <br>
 *      -p,a,b <br>
 *      -a,q <br>
 *      -b,r <br>
 *      ---- <br>
 *      q,r   (a sequence of resolutions eliminates the p)
 *
 * Created by ohlbach on 01.01.2020.
 */
public abstract class ResolutionReduction extends Solver {

    /** This is just for debugging purposes */
    boolean checkConsistency = true;

    /** orders tasks according to a priority */
    TaskQueue taskQueue = null;

    /** collects statistical information */
    public ResolutionStatistics statistics;

    /** for forming the ids of the new clauses */
    int[] id = new int[]{0};

    /** constructs a new ResolutionReduction solver.
     *
     * @param solverNumber         for distinguishing different solvers of the same type, but different parameters
     * @param solverParameters     contains the parameters for controlling the solver
     * @param problemSupervisor    coordinates several solvers.
     */
    public ResolutionReduction(Integer solverNumber, HashMap<String,Object> solverParameters, ProblemSupervisor problemSupervisor) {
        super(solverNumber,solverParameters, problemSupervisor);}

    /** maps literals (numbers) to their occurrences in clauses */
    BucketSortedIndex<CLiteral<Clause>> literalIndex;

    /** is set false after all initial clauses are integrated */
    boolean initializing = true;

    /** for optimizing subsumption and replacement resolution operations */
    int timestamp = 1;

    /** for incrementing the timestamp */
    int maxClauseLength = 3;

    /** counts the number of clauses in the solver */
    int clauseCounter = 0;

    /** EquivalenceClasses manage equivalent literals.
     *  In each equivalence class the literals are mapped to their representatives,
     *  which is always the predicate with the smallest number.
     */
    EquivalenceClasses equivalenceClasses = null;

    /** If an equivalence p = -p occurs or can be derived, this function is called.
     *  It adds an Unsatisfiable task to the task queue.
     */
    Consumer<String> contradictionHandler = ((reason)->{
        taskQueue.add(new Task(0,(()-> new Unsatisfiable(reason)), (()->reason)));});

    /** the id of the last input clause */
    int maxInputId = 0;

    /** initializes algorithm specific data structures*/
    abstract void initializeData();
    /** does resolution or reduction */
    abstract Result doTheWork() throws InterruptedException;
    /** gets the clause list containing the given clause */
    abstract BucketSortedList<Clause> getClauseList(Clause clause);
    /** replaces a subsumed clause by its subsumer */
    abstract void replaceClause(Clause clause, Clause subsumer);
    /** checks the consistency of the data structures */
    abstract void check(String info);

    /** This method controls the entire operation sequence
     * 1. All local data are initialized <br>
     * 2. All basic clauses are transformed to clauses and distributed to the primary and secondary clause lists. <br>
     *    Equivalences are turned to equivalence classes <br>
     *    All literals are mapped to the representatives in their classes.<br>
     * 3. The initial causes are put into the task queue for simplifications.<br>
     * 4. Resolution/Reduction and simplification is started until <br>
     *     - a contradiction is formed or <br>
     *     - the clauses became empty or<br>
     *     - in case of resolution, the resolution limit is exceeded or <br>
     *     - the thread is interrupted.<br>
     *
     * @return the result of the operation sequence.
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
            globalParameters.log(combinedId + " interrupted.\n");
            result = new Aborted(combinedId + " aborted after ");}
        statistics.elapsedTime = System.currentTimeMillis() - time;
        System.out.println("RESULT " + result.toString());
        problemSupervisor.finished(this, result, "done");
        return result;}

    /** This function is called when a new disjunction is to be inserted.
     *  It generates a simplifyBackwards task.
     */
    Consumer<Clause> insertHandler = (
            clause -> {
                switch(clause.size()) {
                    case 1: addTrueLiteralTask(clause.getLiteral(0),"Initial clause"); return;
                    case 2: findEquivalence(clause);
                    default: insertClause(clause);
                        taskQueue.add(new Task(clause.size()+3,        // shorter clauses should be
                            (()-> {simplifyForward(clause); return null;}),    // checked first for subsumption and replacement resolution
                            (()-> "Simplify initial clause " + clause.toString())));}});

    /** This method translates all basic clauses into Clause data structures.
     *  Equivalent literals are replaced by their representatives.
     *
     * @return possibly Unsatisfiable
     * @throws InterruptedException
     */
    Result initializeClauses() throws InterruptedException {
        if(basicClauseList.equivalences != null) {
            equivalenceClasses = Transformers.prepareEquivalences(basicClauseList,contradictionHandler,symboltable);
            if(!taskQueue.isEmpty()) {Result result = taskQueue.run(); if(result != null) {return result;}}}

        Transformers.prepareConjunctions(basicClauseList,equivalenceClasses,
                (literal-> addTrueLiteralTask(literal, "Initial Conjunction")));
        if(Thread.interrupted()) {throw new InterruptedException();}
        Transformers.prepareDisjunctions(basicClauseList,id,equivalenceClasses,insertHandler);
        Transformers.prepareXors     (basicClauseList,id,equivalenceClasses,(clause -> insertClause(clause)));
        Transformers.prepareDisjoints(basicClauseList,id,equivalenceClasses,(clause -> insertClause(clause)));
        if(Thread.interrupted()) {throw new InterruptedException();}
        if(checkConsistency) {check("initializeClauses");}
        maxInputId = id[0];
        Result result = taskQueue.run();
        if(result != null) {return result;}
        purityAndElimination();
        result = taskQueue.run();
        initializing = false;
        if(Thread.interrupted()) {throw new InterruptedException();}
        return result;}


    /** checks if the clause is subsumed or some of its literals can be resolved away by replacement resolution.
     * The clause is assumed not to be in the lists and index. <br>
     * It might be a new resolvent.
     * If the clause is subsumed, it is removed from the clause lists <br>
     *    <br>
     *  - Example for replacement resolution:<br>
     *      p,q,r<br>
     *      -p,r <br>
     *    p in the first clause can be removed. <br>
     *    In more complex examples, several literals can be removed at once.<br>
     *    The resulting clause may be a unit clause
     *
     * @param clause  a new clause, which is not yet integrated into the lists and the literal index.
     * @return true if the clause still exists
     */
     boolean simplifyBackwards(Clause clause) {
        if(clause.removed) {return false;}
        Clause subsumer = backwardSubsumption(clause);
        if(subsumer != null) {replaceClause(clause,subsumer); return false;}
        if(backwardReplacementResolution(clause)) {analyseShortenedClause(clause);}
         return !clause.removed;}

    /** checks if the clause is subsumed by some other clause.
     *
     * @param clause the clause to be checked.
     * @return null or the subsumer.
     */
    Clause backwardSubsumption(Clause clause) {
        Clause subsumer = LitAlgorithms.isSubsumed(clause,literalIndex,timestamp);
        timestamp += maxClauseLength +1;
        if(subsumer != null) {
            ++statistics.backwardSubsumptions;
            if(monitoring) {
                monitor.print(combinedId,"Clause \n  " + clause.toString() + " is subsumed by \n  " + subsumer.toString());}}
        return subsumer;}

    /** checks if the clause can be simplified by replacement resolution.
     *
     * @param clause the clause to be checked
     * @return true if the clause has been simplified.
     */
    boolean backwardReplacementResolution(Clause clause) {
        Object[] replacements = LitAlgorithms.replacementResolutionBackwards(clause,literalIndex,timestamp);
        timestamp += maxClauseLength +1;
        boolean changes = false;
        while(replacements != null) { // several literals may become resolved away
            changes = true;
            CLiteral<Clause> cLiteral = (CLiteral<Clause>)replacements[0];
            ++statistics.backwardReplacementResolutions;
            if(monitoring) {
                monitor.print(combinedId,
                        "Literal " + cLiteral.toString(symboltable) + " in clause \n  " + clause.toString(symboltable) + " resolved away by clause \n  "
                                + ((Clause)replacements[1]).toString(symboltable));}
            clause.remove(cLiteral);
            replacements = (clause.size() == 1) ? null : LitAlgorithms.replacementResolutionBackwards(clause,literalIndex,timestamp);
            timestamp += maxClauseLength +1;}
        return changes;}


    /** does forward subsumption and replacement resolution
     *
     * @param clause the clause which may cause subsumptions and replacement resolutions
     */
    void simplifyForward(Clause clause) {
        if(clause.removed) {return;}
        forwardSubsumption(clause);
        forwardReplacementResolution(clause);}


    /** just used in simplifyForward */
    private ArrayList<Clause> subsumedClauses = new ArrayList<>();

    void forwardSubsumption(Clause clause) {
        subsumedClauses.clear();
        LitAlgorithms.subsumes(clause,literalIndex,timestamp, subsumedClauses);
        timestamp += maxClauseLength +1;
        for(Clause subsumedClause : subsumedClauses) {
            ++statistics.forwardSubsumptions;
            if(monitoring) {
                monitor.print(combinedId,"Clause \n  " + clause.toString(symboltable) +
                    "  subsumes \n  " + subsumedClause.toString(symboltable));}
            removeClause(subsumedClause,0);}}

    /** just used in simplifyForward */
    private ArrayList<CLiteral<Clause>> resolvedLiterals = new ArrayList<>();

    void forwardReplacementResolution(Clause clause) {
        resolvedLiterals.clear();
        LitAlgorithms.replacementResolutionForward(clause,literalIndex,timestamp, resolvedLiterals);
        timestamp += maxClauseLength +1;
        for(CLiteral<Clause> cLiteral : resolvedLiterals) {
            ++statistics.forwardReplacementResolutions;
            Clause literalClause = cLiteral.clause;
            if(monitoring) {
                monitor.print(combinedId,"Literal " + cLiteral.toString(symboltable) + " in clause \n  " +
                        literalClause.toString(symboltable) +
                        " resolved away by clause\n  " +clause.toString(symboltable));}
            removeLiteral(cLiteral);
            if(monitoring) {monitor.print(combinedId,"Shortened CLause:\n  " + literalClause.toString());}
            analyseShortenedClause(literalClause);}}



    /** This method deals with shortened clauses.
     * If the clause became a unit clause, a trueLiteralTask is inserted into the task queue, <br>
     *     the clause is removed and the literal is forwarded to other solvers. <br>
     * If the clause is a binary clause, it is also forwarded to other solvers. <br>
     * If the clause is longer then forwardClause is called, which does nothing, but may be overwritten in the solvers.<br>
     * In both cases a new simplifyForward task is inserted into the task queue.
     *
     * @param clause a shortened clause.
     */
    void analyseShortenedClause(Clause clause) {
        switch(clause.size()) {
            case 1:
                CLiteral<Clause> cliteral = clause.getCLiteral(0);
                int literal = cliteral.literal;
                addTrueLiteralTask(literal,
                        "Clause " + clause.toString(symboltable) + " shortened to unit literal " + ": " + literalName(literal));
                exportUnitClause(literal);
                literalIndex.remove(cliteral);
                clause.removed = true;
                return;
            case 2: exportBinaryClause(clause);
                    findEquivalence(clause); break;
            default: exportClause(clause);}

        taskQueue.add(new Task(clause.size()+3,
                (()->{simplifyForward(clause);return null;}),
                (()->"Forward Simplification for shortened clause " + clause.toString(symboltable))));}


    private ArrayList<Integer> zeros = new ArrayList<>();
    private ArrayList<Integer> ones = new ArrayList<>();

    /** This method checks all predicates for purity and elimination.
     * A literal p is pure if there are no clauses with p any more. <br>
     * In this case -p can be made true. <br>
     * A literal p can be eliminated if it occurs only once in the clauses, say in clause C. <br>
     * In this case all clauses with -p can be replaced with their resolvent with C.
     *
     * @result true if some simplification was performed.
     */
    boolean purityAndElimination() {
        boolean reduced = false;
        while(literalIndex.size01(predicates,zeros,ones)) {
            reduced = true;
            for(int literal : zeros) {
                int sizep = literalIndex.size01(literal);
                if(sizep == 0) {
                    if(monitoring) {monitor.print(combinedId, "Eliminating pure literal " + literalName(-literal));}
                    processTrueLiteral(-literal);}}
            for(int literal : ones)  {processElimination(literal);}}
        return reduced;}

    /** This list collects the eliminated literals for completing the model at the end.
     *  For each clause (p,q,r,...) where p occurs only once it contains <br>
     *  (the clause, the eliminated literal)
     */
    ArrayList<Object[]> eliminatedLiterals = new ArrayList<>();

    private ArrayList<Clause> replacedClauses = new ArrayList<>();

    /** The method eliminates all occurrences of a literal p and -p, where p occurs only once.<br>
     *  The clauses with -p are replaced by their resolvents with the clause with p.
     *  The resolvents are backward-simplified
     *  If the replaced clause reduces to a unit clause, it is forwarded to the other solvers.
     *  Longer resolvents are not forwarded.
     *
     * @param eliminateLiteral the literal which occurs only once.
     */
    void processElimination(int eliminateLiteral) {
        int size01p = literalIndex.size01(eliminateLiteral);
        int size01n = literalIndex.size01(-eliminateLiteral);
        if(size01p != 1 || size01n == 0) {return;} // the situation may have changed when this method is called.
        CLiteral<Clause> parentCLiteral = literalIndex.getAllItems(eliminateLiteral).get(0);
        Clause clause  = literalIndex.getAllItems(eliminateLiteral).get(0).clause;  // the clause with the literal to be eliminated
        replacedClauses.clear();
        for(CLiteral<Clause> otherCliteral : literalIndex.getAllItems(-eliminateLiteral)) { // all literals with -p
            Clause otherClause = otherCliteral.clause;
            Clause resolvent = LitAlgorithms.resolve(id,parentCLiteral,otherCliteral); // double literals and tautologies tested
            removeClause(otherClause,0);
            if(resolvent != null) {replacedClauses.add(resolvent);}}

        removeClause(clause,0);
        eliminatedLiterals.add(new Object[]{clause.cliterals,eliminateLiteral});
        literalIndex.clearBoth(Math.abs(eliminateLiteral));
        // all original clauses are removed

        for(Clause resolvent : replacedClauses) { // subsumption within the replaced clauses may not be recognized
            if(simplifyBackwards(resolvent)) {insertClause(resolvent);}}
        if(checkConsistency) {check("processElimination");}
    }

    // Methods called by the ProblemSupervisor.

    /** This method is called by the problemSupervisor, when another solver derives a true literal.
     * It generates a trueLiteral task.
     *
     * @param literal a new true literal
     */
    public void importTrueLiteral(int literal) {
        ++statistics.importedUnitClauses;
        taskQueue.add(new Task(1,
                (()->processTrueLiteral(literal)),
                (()->"Literal " + literalName(literal) + " is imported from another solver ")));}

    /** This method is called by the problemSupervisor to announce a binary clause found by another solver
     *
     * @param literal1 the first literal of the clause
     * @param literal2 the second literal of the clause
     */
    public void importBinaryClause(int literal1, int literal2) {
        ++statistics.importedBinaryClauses;
        taskQueue.add(new Task(2,
                (()-> {
                    processBinaryClause(literal1,literal2); return null;}),
                (()-> "Importing binary clause " + literal1 + " " + literal2)));}

    /** This method is called by the problemSupervisor to announce a longer clause.
     * An application might be a Reducer which was able to shorten an input clause.
     * This might be useful information for a Resolver.
     *
     * @param literals the literals of the clause
     */
    public void importClause(int[] literals) {
        ++statistics.importedOtherClauses;
        taskQueue.add(new Task(literals.length,
                (()-> {
                    processClause(literals); return null;}),
                (()-> "Importing clause " + Arrays.toString(literals))));}


    /** except in the initializing phase, a unit clause is forwarded to the other solvers.
     *
     * @param clause a unit clause
     */
    void exportUnitClause(Clause clause) {
        if(!initializing) {problemSupervisor.forwardTrueLiteral(this,clause.getLiteral(0));}}


    /** except in the initializing phase, a true literal is forwarded to the other solvers.
     *
     * @param literal a true literal
     */
    void exportUnitClause(int literal) {
        if(!initializing) {problemSupervisor.forwardTrueLiteral(this,literal);}}


    /** except in the initializing phase, a binary clause is forwarded to the other solvers.
     *
     * @param clause a binary clause
     */
    void exportBinaryClause(Clause clause) {
        if(!initializing) {
            problemSupervisor.forwardBinaryClause(this,
                    clause.getLiteral(0),
                    clause.getLiteral(1));}}

    /** except in the initializing phase, a longer clause is forwarded to the other solvers.
     *
     * @param clause a longer clause
     */
    void exportLongerClause(Clause clause) {
        if(!initializing) {
            int size = clause.size();
            int [] literals = new int[size];
            for(int i = 0; i < size; ++i) {literals[i] = clause.getLiteral(i);}
            problemSupervisor.forwardClause(this,literals);}}

    /** except in the initializing phase, a clause is forwarded to the other solvers.
     *
     * @param clause a clause
     */
    void exportClause(Clause clause) {
        if(!initializing) {
            switch(clause.size()) {
                case 1:  exportUnitClause(clause); break;
                case 2:  exportBinaryClause(clause); break;
                default: exportLongerClause(clause);}}}

    /** turns the literal into a trueLiteralTask.
     *
     * @reason  for monitoring the tasks
     * @param literal a unit literal.
     */
    void addTrueLiteralTask(int literal, String reason) {
        ++statistics.derivedUnitClauses;
        taskQueue.add(new Task(1,
                (()->processTrueLiteral(literal)),
                (()->reason + ": " + literalName(literal))));}

    /** computes the consequences of a new true literal
     * - all clauses with this literal are removed <br>
     * - all occurrences of the negated literal are removed<br>
     * - for each shortened clause which became a unit clause, a new task is created.<br>
     * - if the primary clauses became empty, the model is completed
     *
     * @param literal a new true literal
     * @return the result of a model completion or null
     */
    Result processTrueLiteral(int literal) {
        literal = equivalenceClasses.mapToRepresentative(literal);
        switch(model.status(literal)) {
            case -1: return new Unsatisfiable(model,literal);
            case +1: return null;}
        model.add(literal);
        Iterator<CLiteral<Clause>> iterator = literalIndex.popIterator(literal);
        while(iterator.hasNext()) {
            Clause clause = iterator.next().clause;
            removeClause(clause,literal);}
        literalIndex.pushIterator(literal,iterator);
        for(CLiteral<Clause> cLiteral : literalIndex.getAllItems(-literal)) {
            removeLiteral(cLiteral);
            analyseShortenedClause(cLiteral.clause);}
        literalIndex.clearBoth(Math.abs(literal));
        return null;}



    /** This method imports an external binary clause into the local data structures.
     *
     * @param literal1 the first literal of the clause
     * @param literal2 the second literal of the clause
     */
    void processBinaryClause(int literal1, int literal2) {
        literal1 = equivalenceClasses.mapToRepresentative(literal1);
        literal2 = equivalenceClasses.mapToRepresentative(literal2);
        if(literal1 == literal2) {
            addTrueLiteralTask(literal1,"Imported binary clause merged to" + literalName(literal1)); return;}
        if(literal1 == -literal2 || model.isTrue(literal1) || model.isTrue(literal2)) {return;}
        if(model.isFalse(literal1)) {
            addTrueLiteralTask(literal2,"Imported binary clause " + literalName(literal1) + "," +
                literalName(literal2) + " reduced to " + literalName(literal2)); return;}
        if(model.isFalse(literal2)) {
            addTrueLiteralTask(literal2,"Imported binary clause " + literalName(literal1) + "," +
                literalName(literal2) + " reduced to " + literalName(literal1)); return;}
        Clause clause = new Clause(++id[0],2);
        clause.add(new CLiteral(literal1));
        clause.add(new CLiteral(literal2));
        clause.setStructure();
        simplifyBackwards(clause);
        if(clause.removed) {return;}
        insertClause(clause);
        simplifyForward(clause);}


    /** This method imports an external clause into the local data structures.
     *
     * @param literals the clause sent by the ProblemSupervisor
     */
    void processClause(int[] literals) {
        int size = literals.length;
        Clause clause = new Clause(++id[0],literals.length);
        for(int i = 0; i < size; ++i) {
            int literal = equivalenceClasses.mapToRepresentative(literals[i]);
            if(model.isTrue(literal)) {return;}
            if(!model.isFalse(literal)) {clause.add(new CLiteral(literal));}}
        if(clause.hasComplementaries()) {return;}
        clause.removeDoubles();
        if(clause.size() == 1) {
            int literal = clause.getLiteral(0);
            addTrueLiteralTask(literal,"Imported clause merged to " + literalName(literal)); return;}
        clause.setStructure();
        simplifyBackwards(clause);
        if(clause.removed) {return;}
        switch(clause.size()) {
            case 1:
                int literal = clause.getLiteral(0);
                addTrueLiteralTask(literal,"Imported clause merged to " + literalName(literal)); return;
            case 2: findEquivalence(clause);}
        insertClause(clause);
        simplifyForward(clause);}


    /** This method checks if the clause is part of an equivalence (p,q) (-p,-q)
     * If this is the case: <br>
     *     - -p == q is inserted into the equivalence classes <br>
     *     - a processEquivalence task is generated
     *
     * @param clause the clause to be checked
     * @return true if an equivalence has been found.
     */
    boolean findEquivalence(Clause clause) {
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




    /** This method replaces all occurrences of fromLiteral by toLiteral.
     *  Generated tautologies are ignored.<br>
     *  Double literals are avoided. <br>
     *  The new clauses are backwards simplified.
     *
     * @param fromLiteral antecedent
     * @param toLiteral   succedent
     * @return            null
     */
    Result processEquivalence(int fromLiteral, int toLiteral) {
        int fromStatus = model.status(fromLiteral);
        int toStatus   = model.status(toLiteral);
        if(fromStatus != 0 && toStatus != 0 && fromStatus != toStatus) {
            return new Unsatisfiable(model,toLiteral);}
        if(fromStatus != 0) {
            addTrueLiteralTask((fromStatus == 1 ? toLiteral : -toLiteral),
                    "equivalent literals " + fromLiteral + " " + toLiteral);
            return null;}
        if(toStatus != 0) {
            addTrueLiteralTask((toStatus == 1 ? fromLiteral : -fromLiteral),
                    "equivalent literals " + fromLiteral + " " + toLiteral);
            return null;}
        replacedClauses.clear();
        replaceLiteralInAllClauses(fromLiteral,toLiteral);
        replaceLiteralInAllClauses(-fromLiteral,-toLiteral);
        literalIndex.clearBoth(fromLiteral);

        for(Clause clause : replacedClauses) {
            if(clause.size() > 1) simplifyBackwards(clause);
            if(!clause.removed) {
                analyseShortenedClause(clause);
                if(!clause.removed) {insertClause(clause);}}}
        return null;}

    /** replaces fromLiteral by toLiteral in all clauses and inserts the new clause in replacedClauses
     * Double literals are ignored.<br>
     * A tautology is not inserted into replacedClauses.
     *
     * @param fromLiteral  the old literal
     * @param toLiteral    the new literal
     */
    void replaceLiteralInAllClauses(int fromLiteral, int toLiteral) {
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
            if(!tautology) {replacedClauses.add(newClause);}}}


    /** inserts the clause into the local data structures.
     *
     * @param clause  the clause to be inserted.
     */
    void insertClause(Clause clause) {
        if(clause.size() > 1) {
            ++clauseCounter;
            maxClauseLength = Math.max(maxClauseLength,clause.size());
            getClauseList(clause).add(clause);
            insertIntoIndex(clause);
            if(checkConsistency) {check("insertClause");}}}


    /** removes a clause from primary/secondary clauses and from the literal index (except ignoreLiteral)
     *
     * @param clause        the clause to be removed
     * @param ignoreLiteral the literal not to remove from the index
     */
    void removeClause(Clause clause, int ignoreLiteral) {
        if(clause.removed) {return;}
        --clauseCounter;
        getClauseList(clause).remove(clause);
        for(CLiteral<Clause> cLiteral : clause) {
            if(cLiteral.literal != ignoreLiteral) {
                literalIndex.remove(cLiteral);}}
        clause.removed = true;
        if(checkConsistency) {check("removeClause");}}

    /** removes the literal from its clause and the literal index.
     * - If the clause is already marked 'removed' nothing happens. <br>
     * - If the shortened clause is a unit clause, it is entirely removed from the lists and the index.<br>
     *   The clause itself becomes a unit clause.
     *
     * @param cLiteral the literal to be removed
     * @return true if the clause became a unit clause
     */
    boolean removeLiteral(CLiteral<Clause> cLiteral) {
        Clause clause = cLiteral.clause;
        if(clause.removed) {return false;}
        if(clause.size() == 2) {
            removeClause(clause,0);
            clause.remove(cLiteral);
            return true;}
        getClauseList(clause).remove(clause);
        removeFromIndex(clause);
        clause.remove(cLiteral);
        getClauseList(clause).add(clause);
        insertIntoIndex(clause);
        if(checkConsistency) {check("removeLiteral");}
        return false;}

    /** completes a model after resolution has finished.
     * Strategy INPUT or SOS: all remaining clauses should be true <br>
     * Strategy POSITIVE: all remaining clauses are negative of mixed. <br>
     * Choose an unassigned negative literal. <br>
     *
     * Strategy NEGATIVE: all remaining clauses are positive of mixed. <br>
     * Choose an unassigned positive literal. <br>
     *
     * @return Satisfiable or Erraneous (if something went wrong).
     */
    Result completeModel() {
        System.out.println("Completing Model\n"+toString());
        if(model.size() == predicates) {return new Satisfiable(model);}
        boolean isPositive = true;

        completeEliminationsInModel();
        Result result = equivalenceClasses.completeModel(model);
        if(result != null) {return result;}
        result = checkModel();
        if(result != null) {return result;}
        return new Satisfiable(model);}

    /** completes a partial model by inserting the value for eliminated literals */
    void completeEliminationsInModel() {
        for(int i = eliminatedLiterals.size()-1; i >= 0; --i) {
            Object[] els = eliminatedLiterals.get(i);
            ArrayList<CLiteral<Clause>> literals = (ArrayList<CLiteral<Clause>>)els[0];
            int literal = (int)els[1];
            if(model.status(literal) != 0) {continue;}
            boolean satisfied = false;
            for(CLiteral<Clause>cliteral : literals) {
                int lit = cliteral.literal;
                if(lit != literal && model.status(lit) == 1) {satisfied = true; break;}}
            model.add(satisfied ? -literal : literal);}}


    /** inserts a clause into the literal index
     *
     * @param clause  the clause to be inserted
     */
    void insertIntoIndex(Clause clause) {
        for(CLiteral<Clause> cliteral : clause) {literalIndex.add(cliteral);}}

    /** removes a clause from the literal index
     *
     * @param clause  the clause to be inserted
     */
    void removeFromIndex(Clause clause) {
        for(CLiteral<Clause> cliteral : clause) {literalIndex.remove(cliteral);}}




    /** turns the literal into a string, either via the symboltable, or directly
     *
     * @param literal the literal
     * @return its name
     */
    String literalName(int literal) {
        return (symboltable == null) ? Integer.toString(literal) :
                symboltable.getLiteralName(literal);}

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
