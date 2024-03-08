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
import Management.ErrorReporter;
import Management.Monitor.Monitor;
import Management.ProblemSupervisor;
import Solvers.Normalizer.NMInferenceSteps.NMISClause;
import Solvers.Normalizer.NMInferenceSteps.NMISTrueLiteralToEquivalence;
import Solvers.Normalizer.NMInferenceSteps.NMTrueSingletonLiteral;
import Solvers.Normalizer.NMInferenceSteps.NMUnsatEquivalence;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.concurrent.PriorityBlockingQueue;

/** The normalizer takes the inputClauses provided by the supervisor and removes any immediately recognizable redundancies.<br>
 * In particular: <br>
 * - superfluous multiplicities of literals<br>
 * - complementary literals<br>
 * - immediately recognizable true or false clauses<br>
 * - multiplicities dividable by the greatest common divisor<br>
 * - extractable true or false literals (e.g. atmost 2 p^3,q,r: p must be false)<br>
 * <br>
 * The quantifiers of the clauses are optimized as far as possible (e.g. atleast 1 p,... =&gt; or p,...)<br>
 * Pure literals are identified and elminated.<br>
 * True literals are put into the model.<br>
 * The simplification operations can be accompanied by inference steps. The soundness of these steps can be verified.<br>
 * The result is a list of simplified clauses which can be submitted to the solvers.
 */
public class Normalizer {

    /** the supervisor for the given problem.*/
    final ProblemSupervisor problemSupervisor;

    /** the problem's identifier */
    final String problemId;

    /** just 'Normalizer'*/
    final static String solverId = "Normalizer";

    /** the global model.*/
    public Model model;

    /** the number of predicates in the problem */
    public int predicates;

    /** null or a monitor */
    public Monitor monitor;

    /** true if there is a monitor */
    public boolean monitoring;
    /** the monitor's identifier */
    public String monitorId;

    /** null or a symboltable */
    public final Symboltable symboltable;

    /** if true then inference steps are generated */
    public boolean trackReasoning;

    /**the normalizer statistics */
    NormalizerStatistics statistics;

    /** the current thread*/
    public final Thread myThread;

    /** maps predicates to or- and atleast-clauses containing the predicate */
    ArrayList<Clause>[] positiveOccAtleast;

    /** maps predicates to atmost-clauses containing the predicate */
    ArrayList<Clause>[] positiveOccAtmost;

    /** maps predicates to interval-clauses containing the predicate */
    ArrayList<Clause>[] positiveOccInterval;

    /** maps predicates to or- and atleast-clauses containing the negated predicate */
    ArrayList<Clause>[] negativeOccAtleast;

    /** maps predicates to atmost-clauses containing the negated predicate */
    ArrayList<Clause>[] negativeOccAtmost;

    /** maps predicates to interval-clauses containing the negated predicate */
    ArrayList<Clause>[] negativeOccInterval;

    /** contains pointers to all six occurrence lists.*/
    final ArrayList[][] indexLists = new ArrayList[6][];


    /** A queue of newly derived unit literals and binary equivalences.
     * The unit literals are automatically put at the beginning of the queue.
     */
    private final PriorityBlockingQueue<Task> queue =
            new PriorityBlockingQueue<>(100, Comparator.comparingInt(task->task.priority));


    /** contains all the clauses. */
    LinkedItemList<Clause> clauses = new LinkedItemList<>("Normalized Clauses");


    /**Creates a Normalizer object with the given ProblemSupervisor.
     *
     * @param problemSupervisor the ProblemSupervisor object that controls the solution of the problem.
     */
    public Normalizer(ProblemSupervisor problemSupervisor) {
        this.problemSupervisor = problemSupervisor;
        problemId              = problemSupervisor.problemId;
        model                  = problemSupervisor.model;
        statistics             = new NormalizerStatistics(null);
        monitor                = problemSupervisor.monitor;
        monitoring             = monitor != null;
        symboltable            = problemSupervisor.inputClauses.symboltable;
        trackReasoning         = problemSupervisor.globalParameters.trackReasoning;
        predicates             = problemSupervisor.inputClauses.predicates;
        monitorId              = "Normalizer_"+ problemId;
        myThread               = Thread.currentThread();
    }

    /** reads the next task from the task queue and processes it.
     * <br>
     * This method is called after all clauses are simplified.
     * The resulting true literals and the equivalences are now processed.
     *
     * @param maxLoop 0 or the maximum number of task before stopping the loop (0: unlimited) (> 0 for test purposes).
     * @throws Result if a contradiction is encountered or the clause set became empty.
     */
    void processTasks(final int maxLoop) throws Result {
        queue.add(new Task());
        boolean purityIsInQueue = true;
        Task task;
        int counter = 0;
        while((maxLoop == 0 || counter <= maxLoop) && !queue.isEmpty()) {
            ++counter;
            try{
                task = queue.take(); // waits if the queue is empty
                switch(task.taskType) {
                    case EQUIVALENCE: applyEquivalence(task.eqLiteral1,task.eqLiteral2,task.inferenceStep); break;
                    case TRUELITERAL:
                        applyTrueLiteral(task.trueLiteral);
                        if(!purityIsInQueue) {queue.add(new Task()); purityIsInQueue = true;} // there can be new purities.
                        break;
                    case PURITY:      processPurity(); purityIsInQueue = false;}}
            catch(InterruptedException ex) {
                if(monitoring) monitor.println("Normalizer for problem "+ problemId + " interrupted.");
                return;}}}


    /** reads the clauses from the problemSupervisor, turns them into Clause datastructures, and simplifies them as far as possible.
     *
     * @param maxLoop 0 or the maximum number of task before stopping the loop (0: unlimited) (> 0 for test purposes).
     * @return null or Unsatisfiable
     */
    public Result normalizeClauses(int maxLoop) {
        InputClauses inputClauses = problemSupervisor.inputClauses;
        try {
            for (int[] inputClause : inputClauses.conjunctions) makeTrueLiteralTask(inputClause);
            for (int[] inputClause : inputClauses.equivalences) makeEquivalenceTask(inputClause);
            for (int[] inputClause : inputClauses.disjunctions) transformAndSimplify(inputClause);
            for (int[] inputClause : inputClauses.atleasts)     transformAndSimplify(inputClause);
            for (int[] inputClause : inputClauses.atmosts)      transformAndSimplify(inputClause);
            for (int[] inputClause : inputClauses.exactlys)     transformAndSimplify(inputClause);
            for (int[] inputClause : inputClauses.intervals)    transformAndSimplify(inputClause);
            processTasks(maxLoop);
        }
        catch (Result result) {
            statistics.survivedClauses = clauses.size();
            return result;}
        statistics.survivedClauses = clauses.size();
        return null;}

    /** Adds all literals in the clause (a conjunction) as true literal task to the queue and inserts them into the model.
     *
     * @param inputClause a conjunction.
     * @throws Unsatisfiable if the model discovers an inconsistency.
     */
    void makeTrueLiteralTask(int[] inputClause) throws Unsatisfiable {
        assert(inputClause[1] == Quantifier.AND.ordinal());
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
        assert(clause.quantifier == Quantifier.AND);
        InferenceStep step = trackReasoning ? new NMISClause(clause) : null;
        for(int i = 0; i < clause.literals.size()-1; i += 2) {
            int literal = clause.literals.getInt(i);
            if(monitoring) monitor.println(monitorId, "adding true literal " +
                    Symboltable.toString(literal,symboltable) + " to the model.");
            ++statistics.derivedTrueLiterals;
            addTrueLiteralTask(literal,step);}}

    /**Adds the literal as true literal to the model and to the queue,
     *
     * @param literal a true literal
     * @param step    which caused the literal to be true.
     * @throws Unsatisfiable if the model discovers an inconsistency.
     */
    void addTrueLiteralTask(int literal, InferenceStep step) throws Unsatisfiable {
        model.add(myThread,literal,step);
        queue.add(new Task(literal,step));}

    /** turns the equivalence clauses in the input clause into an equivalence task and adds it to the queue.
     * <br>
     * The first literal in the equivalence clause becomes the representative literal.
     * Each further literal in the clause yields a separate equivalence task.
     *
     * @param inputClause an equivalence input clause.
     */
    void makeEquivalenceTask(int[] inputClause) {
        assert(inputClause[1] == Quantifier.EQUIV.ordinal());
        int sign = 1;
        int representative = inputClause[Quantifier.EQUIV.firstLiteralIndex];
        if(representative < 0) {sign = -1; representative *= -1;}
        for(int i = Quantifier.EQUIV.firstLiteralIndex+1; i < inputClause.length; ++i) {
            queue.add(new Task(representative, sign*inputClause[i],
                    trackReasoning ? new InfInputClause(inputClause[0]) : null));
            ++statistics.initialEquivalences;}}

    /** The method turns an inputClause into a Clause data structure and simplifies the clause.
     *
     * @param inputClause   an input clause (not AND- and not EQUIV-type)
     * @throws Unsatisfiable if a contradiction is discovered.
     */
    void transformAndSimplify(int[] inputClause) throws Unsatisfiable {
        Clause clause = new Clause(inputClause,trackReasoning,monitor,symboltable);
        if(clause.quantifier == Quantifier.AND) {makeTrueLiteralTask(clause); return;} // unit clause
        if(clause.isTrue) {++statistics.removedClauses; return;}
        if(clause.isFalse) throw new UnsatClause(problemId,solverId,clause.inputClause);
        Clause conjunction = clause.simplify(trackReasoning,monitor,symboltable);
        if(conjunction != null) makeTrueLiteralTask(conjunction);
        if(clause.isTrue || clause.quantifier == Quantifier.AND) {++statistics.removedClauses; return;}
        if(clause.isFalse) throw new UnsatClause(problemId,solverId,clause.inputClause);
        statistics.simplifiedClauses += clause.version;
        addClauseToIndex(clause);
        clauses.addToBack(clause);}


    /** applies the true literal to all clauses containing the literal.
     *
     * @param literal       a true literal
     * @throws Unsatisfiable if a contradiction is discovered.
     */
    void applyTrueLiteral(int literal) throws Unsatisfiable {
        int predicate = Math.abs(literal);
        for(ArrayList[] clausesList : indexLists) { // loop over all three index arrays.
            if(clausesList != null) {
                ArrayList<Clause> clausesArray = (ArrayList<Clause>)clausesList[predicate];
                if(clausesArray != null) {
                    for(int i = clausesArray.size()-1; i >= 0; --i) {
                        Clause clause = clausesArray.get(i);
                        removeClauseFromIndex(clause);
                        Clause conjunction = clause.applyTrueLiteral(literal,trackReasoning,monitor,symboltable);
                        if (conjunction != null) {makeTrueLiteralTask(conjunction);}
                        if(clause.isTrue || clause.quantifier == Quantifier.AND) {clauses.remove(clause); continue;}
                        if(clause.isFalse) throw new UnsatClause(problemId,solverId, clause);
                        addClauseToIndex(clause);}}}}
        applyTrueLiteralToEquivalences(literal);}

    /** applies the true literal to all equivalences in the input clauses.
     * <br>
     * Equivalent literals must get the same truth value.
     * Derived truth values are added only to the model.
     * It is assumed that equivalent literals are no longer in the clauses
     * because equivalences are applied before true literals are processed.
     *
     * @param trueLiteral   a true literal
     * @throws Unsatisfiable if two equivalent literals get different truth values.
     */
    void applyTrueLiteralToEquivalences(int trueLiteral) throws Unsatisfiable {
        boolean modelChanged = true;
        while(modelChanged) {
            modelChanged = false;
            for (int[] inputClause : problemSupervisor.inputClauses.equivalences) {
                byte sign = 0;
                int trueLit = trueLiteral;
                for(int i = Quantifier.EQUIV.firstLiteralIndex; i < inputClause.length; ++i) {
                    int literal = inputClause[i];
                    if(literal ==  trueLiteral)  {sign =  1; break;}
                    if(literal == -trueLiteral ) {sign = -1; break;}
                    if(model.status(literal) ==  1) {sign = 1;  trueLit =  literal; break;}
                    if(model.status(literal) == -1) {sign = -1; trueLit = -literal; break;}
                }
                if(sign != 0) {
                    for(int i = Quantifier.EQUIV.firstLiteralIndex; i < inputClause.length; ++i) {
                        int literal = inputClause[i];
                        if(model.status(literal) ==  sign) continue;
                        if(model.status(literal) == -sign) throw new NMUnsatEquivalence(problemId,solverId,inputClause,trueLit,literal); // ändern
                        NMISTrueLiteralToEquivalence step = trackReasoning ?
                                new NMISTrueLiteralToEquivalence(trueLit,inputClause,sign*literal) : null;
                        model.add(myThread,sign*literal,step);
                        modelChanged = true;
                        if(monitoring) monitor.println(monitorId,"True Literal " +  Symboltable.toString(trueLit,symboltable) +
                                " applied to " + InputClauses.toString(0,inputClause,symboltable) + " => " +
                                Symboltable.toString(sign*literal,symboltable));
                    }}}}}


    /** replaces all occurrences of the equivalentLiteral by the representative literal.
     *
     * @param representative    a literal
     * @param equivalentLiteral the equivalent literal
     * @param step              the inference step that caused the equivalence.
     * @throws Unsatisfiable    if a contradiction is encountered.
     */
    void applyEquivalence(int representative, int equivalentLiteral, InferenceStep step) throws Unsatisfiable {
        int predicate = Math.abs(equivalentLiteral);
        for(ArrayList[] clausesList : indexLists) {
            if(clausesList != null) {
                ArrayList<Clause> clausesArray = (ArrayList<Clause>)clausesList[predicate];
                if(clausesArray != null) {
                    for(int i = clausesArray.size()-1; i >= 0; --i) {
                        Clause clause = clausesArray.get(i);
                        removeClauseFromIndex(clause);
                        Clause conjunction = clause.replaceEquivalentLiterals(representative,equivalentLiteral, step,trackReasoning,monitor,symboltable);
                        if (conjunction != null) {makeTrueLiteralTask(conjunction);}
                        if(clause.isTrue) {clauses.remove(clause); continue;}
                        if(clause.isFalse) throw new UnsatClause(problemId,solverId, clause);
                        addClauseToIndex(clause);}}}}}

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
        switch(clause.quantifier) { // since the arrays may become very large, they are only created on demand.
            case OR:
            case ATLEAST:
                if(positiveOccAtleast == null) {
                    positiveOccAtleast = new ArrayList[predicates+1];
                    indexLists[0] = positiveOccAtleast;
                    negativeOccAtleast = new ArrayList[predicates+1];
                    indexLists[1] = negativeOccAtleast;}
                positiveArrayList = positiveOccAtleast;
                negativeArrayList = negativeOccAtleast;
                break;
            case ATMOST:
                if(positiveOccAtmost == null) {
                    positiveOccAtmost = new ArrayList[predicates+1];
                    indexLists[2] = positiveOccAtmost;
                    negativeOccAtmost = new ArrayList[predicates+1];
                    indexLists[3] = negativeOccAtmost;}
                positiveArrayList = positiveOccAtmost;
                negativeArrayList = negativeOccAtmost;
                break;
            case EXACTLY:
            case INTERVAL:
                if(positiveOccInterval == null) {
                    positiveOccInterval = new ArrayList[predicates+1];
                    indexLists[4] = positiveOccInterval;
                    negativeOccInterval = new ArrayList[predicates+1];
                    indexLists[5] = negativeOccInterval;}
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
     * Empty lists are entirely removed.
     *
     * @param clause the clause to be removed from the list.
     */
     void removeClauseFromIndex(Clause clause) {
        ArrayList<Clause>[] positiveArrayList = null;
        ArrayList<Clause>[] negativeArrayList =
                switch (clause.quantifier) {
            case OR, ATLEAST -> {
                positiveArrayList = positiveOccAtleast;
                yield negativeOccAtleast;}
            case ATMOST -> {
                positiveArrayList = positiveOccAtmost;
                yield negativeOccAtmost;}
            case EXACTLY, INTERVAL -> {
                positiveArrayList = positiveOccInterval;
                yield negativeOccInterval;}
            default -> null;};

         for(int i = 0; i < clause.literals.size()-1; i += 2) {
            int literal = clause.literals.getInt(i);
            int predicate = Math.abs(literal);
            ArrayList<Clause> clausesArray = (literal > 0) ? positiveArrayList[predicate] : negativeArrayList[predicate];
            clausesArray.remove(clause);
            if (clausesArray.isEmpty()) {
                if (literal > 0) {positiveArrayList[predicate] = null;}
                else             {negativeArrayList[predicate] = null;}}}}




    /** searches pure literals and process them.
     * <br>
     *  A literal p is pure in clauses which are NOT interval- or exactly-clauses if it occurs either <br>
     *  - only positively or only negatively in OR- or ATLEAST-clauses or<br>
     *  - only positively or only negatively in ATMOST-clauses<br>
     *  The literal p is singleton pure if it occurs only once in an interval- or exactly-clause.<br>
     *  A pure literal in  OR- or ATLEAST-clauses can be made true if occurs only positively,<br>
     *  or made false if it occurs only negatively. <br>
     *  In atmost clauses it is the other way round.<br>
     *  A singleton pure literal in an interval- or exactly-clause can be removed. <br>
     *  Its truth value can only be determined after a model for the entire clause set has been found.
     *
     * @throws Unsatisfiable if a contradiction is discovered.
     * */
    void processPurity()  throws Unsatisfiable{
        boolean purityFound = true;
        while(purityFound) {
            purityFound = false;
            for(int predicate = 1; predicate <= predicates; ++predicate) {
                int literal = isPositivelyPure(predicate);
                if(literal != 0) {
                    if(monitoring) {monitor.println(monitorId, "Literal " + Symboltable.toString(literal,symboltable) +
                            " is positively pure");}
                    ++statistics.pureLiterals;
                    purityFound = true;
                    ArrayList<Clause> causeList = (literal > 0) ? positiveOccAtleast[predicate] : negativeOccAtleast[predicate];
                    addTrueLiteralTask(literal,null);
                    for(int i = causeList.size()-1; i >= 0; --i) {
                        Clause clause = causeList.get(i);
                        removeClauseFromIndex(clause);
                        Clause conjunct = clause.applyTrueLiteral(literal,trackReasoning,monitor,symboltable);
                        if(conjunct != null) makeTrueLiteralTask(conjunct);
                        if(clause.isTrue) {clauses.remove(clause); continue;}
                        if(clause.isFalse) throw new UnsatClause(problemId,solverId,clause);
                        addClauseToIndex(clause);}
                    continue;}

                literal = isNegativelyPure(predicate);
                if(literal != 0) {
                    if(monitoring) {monitor.println(monitorId, "Literal " + Symboltable.toString(literal,symboltable) +
                            " is negatively pure");}
                    ++statistics.pureLiterals;
                    purityFound = true;
                    ArrayList<Clause> causeList = (literal > 0) ? positiveOccAtmost[predicate] : negativeOccAtmost[predicate];
                    addTrueLiteralTask(-literal,null);
                    for(int i = causeList.size()-1; i >= 0; --i) {
                        Clause clause = causeList.get(i);
                        removeClauseFromIndex(clause);
                        Clause conjunct = clause.applyTrueLiteral(-literal,trackReasoning,monitor,symboltable);
                        if(conjunct != null) makeTrueLiteralTask(conjunct);
                        if(clause.isTrue) {clauses.remove(clause); continue;}
                        if(clause.isFalse) throw new UnsatClause(problemId,solverId,clause);
                        addClauseToIndex(clause);}
                    continue;}

                literal = isSingletonPure(predicate);
                if(literal != 0) {
                    if(monitoring) {monitor.println(monitorId, "Literal " + Symboltable.toString(literal,symboltable) +
                            " is singleton pure");}
                    ++statistics.singletonLiterals;
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

    /** extends the model by determining the truth-value of singleton pure literals in interval- and exactly-clauses.
     *
     * @throws Unsatisfiable should not happen.
     */
    public void extendModel() throws Unsatisfiable {
         for(int i = singletons.size()-2; i >= 0; i -=2) {
             int literal   = (Integer)singletons.get(i);
             Clause clause = (Clause)singletons.get(i+1);
             int multiplicity = 0;
             for(int j = 0; j < clause.literals.size()-1; j += 2) {
                 if(clause.literals.getInt(j) == literal) {multiplicity = clause.literals.getInt(j+1); break;}}
             int trueLiterals = clause.trueLiterals(model::isTrue);
             if(trueLiterals + multiplicity < clause.min) {
                 ErrorReporter.reportErrorAndStop("Normalizer.extendModel: not enough true literals in clause " +
                         clause.toString(symboltable,0) + "\nnumber of true literals: " + (trueLiterals + multiplicity) +
                         "\nModel: " + model.toString(symboltable));}
             int trueLiteral = (trueLiterals+multiplicity <= clause.max) ? literal : -literal;
             if(monitoring) monitor.println(monitorId,"Extending model with " + Symboltable.toString(trueLiteral,symboltable) +
                     " for clause " + clause.toString(symboltable,0));
             InferenceStep step = trackReasoning ? new NMTrueSingletonLiteral(clause,trueLiteral): null;
             model.add(myThread,trueLiteral,step);}}

    /** contains pairs singleton-literal,clause. To be used when a model has to be completed. */
    private final ArrayList<Object> singletons = new ArrayList<>();

    /** lists the singletons as string
     *
     * @param symboltable null or a symboltable.
     * @return the singletons as a string.*/
    public String singletonsToString(Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        st.append("Singleton Literals:\n");
        for(int i = 0; i < singletons.size(); i += 2) {
            st.append(Symboltable.toString((int) singletons.get(i), symboltable)).append(" in clause ");
            st.append(((Clause) singletons.get(i+1)).toString(symboltable,0)).append("\n");}
        return st.toString();}


    /** Checks if the given predicate is positively pure, i.e. it occurs only in atleast- or or-clauses, either positively or negatively
     *
     * @param predicate The predicate to check.
     * @return +predicate if it occurs only positively in atleast or or-clauses, -predicate the other way round, otherwise 0.
     */
    int isPositivelyPure(int predicate) {
        if((positiveOccAtmost != null && positiveOccAtmost[predicate] != null) ||
                (negativeOccAtmost != null && negativeOccAtmost[predicate] != null) ||
                (positiveOccInterval != null && positiveOccInterval[predicate] != null) ||
                (negativeOccInterval != null && negativeOccInterval[predicate] != null)) return 0;
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
        if((positiveOccAtleast != null && positiveOccAtleast[predicate] != null) ||
                (negativeOccAtleast != null && negativeOccAtleast[predicate] != null) ||
                (positiveOccInterval != null && positiveOccInterval[predicate] != null) ||
                (negativeOccInterval != null && negativeOccInterval[predicate] != null)) return 0;
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
        if((positiveOccAtleast != null && positiveOccAtleast[predicate] != null) ||
                (negativeOccAtleast != null && negativeOccAtleast[predicate] != null) ||
                (positiveOccAtmost  != null && positiveOccAtmost[predicate] != null) ||
                (negativeOccAtmost  != null && negativeOccAtmost[predicate] != null)) return 0;
        if((negativeOccInterval == null || negativeOccInterval[predicate] == null) &&
                positiveOccInterval != null && positiveOccInterval[predicate] != null && positiveOccInterval[predicate].size() == 1) return predicate;
        if((positiveOccInterval == null || positiveOccInterval[predicate] == null) &&
                negativeOccInterval != null && negativeOccInterval[predicate] != null && negativeOccInterval[predicate].size() == 1) return -predicate;
        return 0;}

    /** lists the entire clause list as string.
     *
     * @param symboltable null or a symboltable
     * @return the clauses as string, one per line.
     */

    public String toString(Symboltable symboltable) {
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

    /** collects all the clauses in the indices as a string (for testing purposes)
     *
     * @param symboltable null or a symboltable.
     * @return all the clauses separated according to the index lists and the predicates.
     */
    public String indexToString(Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        final String[] header = new String[]{
                "Positive or- and atleast-clauses","Negative or- and atleast-clauses",
                "Positive atmost-clauses","Negative atmost-clauses",
                "Positive interval-clauses","Negative interval-clauses"};
        for(int i = 0; i < 6; ++i) {
            ArrayList<Clause>[] clauseList = indexLists[i];
            if(clauseList != null) {
                st.append(header[i]).append(":\n");
                for(int predicate = 1; predicate <= predicates; ++predicate) {
                    if(clauseList[predicate] != null) {
                        String pred = Symboltable.toString((i%2 == 1 ? -1:+1)*predicate,symboltable);
                        st.append(pred).append(": ");
                        boolean first = true;
                        for(Clause clause : clauseList[predicate]) {
                            int size = first ? 10-pred.length()-2 : 10; first = false;
                            st.append(clause.toString(symboltable,size)).append("\n");}}}}}
        return st.toString();}

    /**
     * Converts the queue of tasks to a string representation.
     *
     * @param symboltable the symbol table used to resolve task symbols.
     * @return a string representation of the tasks in the queue.
     */
    public String queueToString(Symboltable symboltable) {
        StringBuilder sb = new StringBuilder();
        for (Task task : queue) {
            sb.append(task.toString(symboltable)).append("\n");
        }
        return sb.toString();
    }

}