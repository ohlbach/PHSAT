package Solvers.Simplifier;

import Datastructures.Clauses.InputClauses;
import Datastructures.Results.EmptyClauses;
import Datastructures.Results.Result;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Statistics.Statistic;
import Datastructures.Symboltable;
import Datastructures.Task;
import Datastructures.Theory.EquivalenceClasses.EquivalenceClasses;
import InferenceSteps.InfInputClause;
import InferenceSteps.InferenceStep;
import Management.Monitor.Monitor;
import Management.ProblemSupervisor;
import Solvers.Solver;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.concurrent.PriorityBlockingQueue;
import java.util.function.IntSupplier;

import static java.lang.Thread.interrupted;

public class Simplifier extends Solver {

    protected InputClauses inputClauses;
    private ProblemSupervisor problemSupervisor;

    public EquivalenceClasses equivalenceClasses;

    /** The id of the current problem to be solved */
    private String problemId;

    /** for distinguishing the monitoring areas */
    private String monitorId;

    protected SimplifierStatistics statistics;

    protected Literals literalIndexTwo;
    protected Literals literalIndexMore;

    protected Clauses clauses;

    private IntSupplier nextId;

    private int timestamp = 1;



    public Simplifier(ProblemSupervisor problemSupervisor) {
        this.problemSupervisor = problemSupervisor;
        this.inputClauses = problemSupervisor.inputClauses;
    }

    public Simplifier(InputClauses inputClauses) {
        this.inputClauses = inputClauses;}

    public Simplifier(int predicates, Monitor monitor, boolean trackReasoning, IntSupplier nextId) {
        this.predicates = predicates;
        this.monitor = monitor;
        monitoring = monitor != null;
        this.monitorId = "Simplifier";
        this.trackReasoning = trackReasoning;
        this.nextId = nextId;
        equivalenceClasses = new EquivalenceClasses(null,monitor);
        literalIndexTwo = new Literals(predicates);
        literalIndexMore = new Literals(predicates);
        clauses = new Clauses();
        model = equivalenceClasses.model;
        statistics = new SimplifierStatistics("Simplifier");
    }

    /** adds the literals which are already true in the model to the task queue.
     * Installs the observer in the model.
     */
    public void initialize() {
        for(int literal: model.model) {
            addTrueLiteralTask(literal,model.getInferenceStep(literal));}
        model.addObserver(this::addTrueLiteralTask);}

    private enum TaskType {
        /** a new true literal is obtained from the model */
        ProcessTrueLiteral,
        /** a new binary equivalence is found in the TwoLiteral module. */
        ProcessEquivalence,
        ProcessBinaryClause,
        ProcessLongerClause
    }

    /** gets the priority for the objects in the queue.
     *
     * @param task the task in the queue
     * @return the priority of the objects in the queue.
     */
    private int getPriority(Task<Simplifier.TaskType> task) {
        switch(task.taskType) {
            case ProcessTrueLiteral: return Math.abs((Integer)task.a);
            case ProcessEquivalence: return predicates + (Math.abs((Integer)task.a)) + 1; // this guarantees a deterministic sequence of the tasks
            case ProcessBinaryClause:       return predicates + 100;
            case ProcessLongerClause:       return predicates + 101;}
        return 0;}

    /** A queue of newly derived unit literals and binary equivalences.
     * The unit literals are automatically put at the beginning of the queue.
     */
    private final PriorityBlockingQueue<Task<Simplifier.TaskType>> queue =
            new PriorityBlockingQueue<>(10, Comparator.comparingInt(this::getPriority));

    /** reads the disjunctions, the atleast, atmost, exactly and interval clauses from inputClauses and transforms them to atleast-clauses.#
     * The clauses themselves are simplified as far as possible.<br>
     * New unit clauses are put into the model. <br>
     * Two-literal clauses generate a corresponding task.
     * 
     * @throws Result if a contradiction or the empty clause is derived.
     */
    public void inputClausesToAtleast() throws Result{
        for(int[] inputClause : inputClauses.disjunctions) {
            Clause clause = new Clause(inputClause);
            if(clause.removeComplementaryLiterals()) {++statistics.notInternalizedInputClauses; continue;}
            clause.removeDoubleLiterals();
            if(clause.size() == 1) {
                ++statistics.derivedUnitClauses;
                ++statistics.notInternalizedInputClauses;
                model.add(clause.literals.get(0).literal,clause.inferenceStep);
                continue;}
            insertClause(clause);
            if(clause.size() == 2) addBinaryClauseTask(clause);}

        for(int[] inputClause : inputClauses.atleasts) {
            insertNewClause(inputClause,new Clause(inputClause));}

        for(int[] atmostClause : inputClauses.atmosts) {
            int[] atleastClause = InputClauses.atmostToAtleast(atmostClause);
            Clause clause = new Clause(atleastClause);
            if(trackReasoning) clause.inferenceStep = new InfAtmostToAtleast(atmostClause,atleastClause);
            if(insertNewClause(atmostClause,clause)) {
                if(monitoring) {
                    monitor.println(monitorId,"Atmost-clause: " +
                            InputClauses.toString(0,atmostClause,symboltable) + " turned to atleast-clause " +
                            clause.toString(symboltable,0));}}}

        for(int[] exactlyClause : inputClauses.exactlys) {
            int[][] atleastClauses = InputClauses.exactlyToAtleast(exactlyClause,nextId);
            for(int i = 0; i < 2; ++i) {
                int[] atleastClause = atleastClauses[i];
                Clause clause = new Clause(atleastClause);
                if(trackReasoning) clause.inferenceStep = new InfExactlyToAtleast(exactlyClause,atleastClause);
                if(insertNewClause(exactlyClause,clause)){
                    if(monitoring) {
                        monitor.println(monitorId,"Exactly-clause: " +
                                InputClauses.toString(0,exactlyClause,symboltable) + " turned to atleast-clause " +
                                clause.toString(symboltable,0));}}}}

        for(int[] intervalClause : inputClauses.intervals) {
            int[][] atleastClauses = InputClauses.intervalToAtleast(intervalClause,nextId);
            for(int i = 0; i < 2; ++i) {
                int[] atleastClause = atleastClauses[i];
                Clause clause = new Clause(atleastClause);
                if(trackReasoning) clause.inferenceStep = new InfIntervalToAtleast(intervalClause,atleastClause);
                if(insertNewClause(intervalClause,clause)){
                    if(monitoring) {
                        monitor.println(monitorId,"Interval-clause: " +
                                InputClauses.toString(0,intervalClause,symboltable) + " turned to atleast-clause " +
                                clause.toString(symboltable,0));}}}}
        checkAllPurities();
        statistics.orAndAtleastCLauses = clauses.size;}

    /** simplifies and inserts an atleast-clause derived from input clauses.
     *
     * @param inputClause the original input-clause.
     * @param clause      the clause to be inserted.
     * @return            true if the clause survived the simplifications.
     * @throws Result     if a contradiction is encountered.
     */
    private boolean insertNewClause(int[] inputClause, Clause clause) throws Result {
        if(clause.isTrue())  {++statistics.notInternalizedInputClauses; return false;}
        if(clause.isFalse()) {throw new UnsatisfiableClause(inputClause);}
        int size = clause.size();
        if(clause.removeComplementaryLiterals())   {++statistics.notInternalizedInputClauses; return false;}
        if(clause.size() != size) {
            if(monitoring)
                monitor.println(monitorId,"Complementary literals removed in clause " +
                        InputClauses.toString(0,inputClause,symboltable) + " -> " + clause.toString(symboltable,0));}
        if(!clause.isDisjunction) {
            if(!simplifyClause(clause, false)) {++statistics.notInternalizedInputClauses; return false;}}
        if(clause.size() == 2 && isSubsumedByBinaryClauses(clause)) return false;
        insertClause(clause);
        ++statistics.orAndAtleastCLauses;
        if(clause.size() == 2) addBinaryClauseTask(clause);
        return true;}

    /** checks all literals for purity.
     * Pure literals are inserted into the model.
     *
     * @throws Unsatisfiable should not happen.
     */
    protected void checkAllPurities() throws Unsatisfiable{
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            if(checkPurity(predicate)) continue;
            checkPurity(-predicate);}}

    /** checks the literal for purity. <br>
     * A literal l is pure if -l does not occur anymore. l can then be made true. <br>
     * A pure literal is put into the model.<br>
     * The literal is not checked if it is already in the model.
     *
     * @param literal a literal.
     * @return true if the literal is pure.
     * @throws Unsatisfiable should not happen.
     */
    protected boolean checkPurity(int literal) throws Unsatisfiable {
        if(model.status(literal) != 0) return false;
        if(literalIndexTwo.isEmpty(-literal) && literalIndexMore.isEmpty(-literal)) {
            model.add(literal, trackReasoning ? new InfPureLiteral(literal) : null);
            if(monitoring) monitor.println(monitorId,"Pure Literal: " + Symboltable.toString(literal,symboltable));
            ++statistics.pureLiterals;
            return true;}
        return false;}

    /** checks all literals in the clause for purity.
     * Pure literals are put into the model.
     *
     * @param clause         a clause to be checked.
     * @throws Unsatisfiable should not happen.
     */
    protected void checkPurity(Clause clause) throws Unsatisfiable {
        for(Literal literalObject : clause.literals) checkPurity(literalObject.literal);}

    /** adds a true literal to the queue
     *
     * @param literal a true literal
     * @param inferenceStep which caused the truth
     */
    public void addTrueLiteralTask(int literal, InferenceStep inferenceStep) {
        if(monitoring) {
            monitor.print(monitorId,"In:   True literal " +
                    Symboltable.toString(literal,symboltable));}
        synchronized (this) {queue.add(new Task<>(TaskType.ProcessTrueLiteral, literal, inferenceStep));}}


    public void run(int n) {
        Task<Simplifier.TaskType> task;
        int counter = 0;
        while(!interrupted()) {
            try {
                if(monitoring) {monitor.print(monitorId,"Queue is waiting\n" + Task.queueToString(queue));}
                task = queue.take(); // waits if the queue is empty
                switch(task.taskType){
                    case ProcessTrueLiteral:  processTrueLiteral((Integer)task.a); break;
                    case ProcessEquivalence:  processEquivalence((Integer)task.a,(Integer)task.b,(InferenceStep) task.c); break;
                    case ProcessBinaryClause: processBinaryClause((Clause)task.a); break;
                    case ProcessLongerClause: processLongerClause((Clause)task.a); break;}}
            catch(InterruptedException ex) {return;}
            catch(Result result) {
                if(problemSupervisor != null)
                    problemSupervisor.finished("Simplifier", result,"");
                return;}
            if(n > 0 && ++counter == n) return;}}

    /** The method applies a true literal to the clause.<br>
        * For a disjunction this means that the clause is true and can therefore be deleted.<br>
        * For a quantified clause this means that the literal can be deleted and the quantifier
        * must be reduced by the literal's multiplicity.
        * <br>
        * The resulting clause must be checked for the following phenomena: <br>
        *  - if the resulting quantifier is &lt;= 0, the clause is true and can be deleted.<br>
        *  - if the resulting quantifier is 1, the clause became a disjunction. <br>
        *  - if the quantifier is still &gt; 1, new true literals might be derived.<br>
        *  Example: atleast 4 p,q^2,r^2 and p is true<br>
        *  The clause is then: atleast 3 q^2,r^2. <br>
        *  Both q and r must now be true.
        *  */
    protected void processTrueLiteral(int literal) throws Result {
        processTrueLiteralTwo(literal);
        processTrueLiteralMore(literal);
        if(clauses.isEmpty()) throw new EmptyClauses(model);}

    /** applies a true literal to all two-literal clauses containing this literal.<br>
     * Clauses containing this literal are removed.<br>
     * Clauses containing -literal yield a new true literal, which is put into the model.<br>
     * The clause is removed as well.
     *
     * @param literal a true literal.
     */
    protected void processTrueLiteralTwo(int literal) throws Result{
        Literal literalObject = literalIndexTwo.getFirstLiteralObject(literal);
        while(literalObject != null) {
            Clause clause = literalObject.clause;
            literalObject = literalObject.nextLiteral;
            if(clause.exists) removeClause(clause,true);} // literals may become pure.

        literalObject = literalIndexTwo.getFirstLiteralObject(-literal);
        while(literalObject != null) {
            Literal nextLiteral = literalObject.nextLiteral;
            Clause clause = literalObject.clause;
            if(clause.exists) {
                Literal otherLiteral = (clause.literals.get(0) == literalObject) ? clause.literals.get(1) : clause.literals.get(0);
                if(monitoring) monitor.println(monitorId,clause.toString(symboltable,0) + " and false(" +
                        Symboltable.toString(literal,symboltable) + ") -> " +
                        "true("+Symboltable.toString(otherLiteral.literal,symboltable)+")");
                model.add(otherLiteral.literal,
                        trackReasoning ?
                                new InfUnitResolutionTwo(clause,literal,model.getInferenceStep(literal),otherLiteral.literal) :
                                null);
                ++statistics.derivedUnitClauses;
                removeClause(clause,true);}
            literalObject = nextLiteral;}

        literalIndexTwo.removePredicate(literal);}

    /** applies a true literal to all longer clauses containing this literal.<br>
     * All literals with a truth value in the model are removed.<br>
     * Clauses which become true in this step are entirely removed.<br>
     * The empty clause causes an UnsatEmptyClause exception to be thrown.<br>
     * Derived unit clauses are put into the model <br>
     * Shortened clauses cause new tasks to be inserted into the task queue.<br>

     * @param literal a true (or false) literal.
     */
    protected void processTrueLiteralMore(int literal) throws Result{
        for(int sign = 1; sign >= -1; sign -= 2) {
            literal *= sign;
            Literal literalObject = literalIndexMore.getFirstLiteralObject(literal);
            while(literalObject != null) { // the literal is true
                 Clause clause = literalObject.clause;
                 if(clause == null) {literalObject = literalObject.nextLiteral; continue;}
                 for(int i = 0; i < clause.literals.size(); ++i){
                     Literal litObject = clause.literals.get(i);
                     int status = model.status(litObject.literal);
                     if(status == 0) continue; // literal must not be removed.
                     boolean isTrue = status == 1;
                     String clauseBefore = (trackReasoning || monitoring) ? clause.toString(symboltable,0) : null;
                     boolean isDisjunction = clause.isDisjunction;

                     if(clause.removeLiteral(litObject,isTrue)) checkPurity(litObject.literal);
                     else {clauses.removeClause(clause); checkPurity(clause); break;} // clause is true now.

                     if(monitoring) monitor.println(monitorId,clauseBefore + " and " +
                                (isTrue ? "true":"false")+ "(" +
                                Symboltable.toString(litObject.literal,symboltable) + ") -> " + clause.toString(symboltable,0));
                     if(trackReasoning) {
                         clause.inferenceStep = new InfUnitResolution(clauseBefore,clause.inferenceStep,isDisjunction,
                                 IntArrayList.wrap(new int[]{litObject.literal}),isTrue,
                                 clause.toString(symboltable,0),model);}
                     --i;}
                if(clause.exists) {
                    if(clause.limit > clause.expandedSize) {
                        throw new UnsatisfiableClause(clause);}
                    switch(clause.size()) {
                        case 0: throw new UnsatEmptyClause(clause);
                        case 1:
                            int trueLiteral =  clause.literals.get(0).literal;
                            model.add(trueLiteral,clause.inferenceStep);
                            ++statistics.derivedUnitClauses;
                            if(monitoring)
                                monitor.println(monitorId,"New true literal " +
                                        Symboltable.toString(trueLiteral,symboltable) + " derived from clause " + clause.id);
                            clauses.removeClause(clause);
                            break;
                        case 2:  moveToIndexTwo(clause);
                                 addBinaryClauseTask(clause);
                                 break;
                        default: if(simplifyClause(clause,true)) {
                                    addShortenedClauseTask(clause);}
                                else clauses.removeClause(clause);}}
                else clauses.removeClause(clause);
            literalObject = literalObject.nextLiteral;}}
        literalIndexMore.removePredicate(literal);}

    /** moves a longer clause which has become a binary clause to the literalIndexTwo.
     * To this end the literals are copied.
     * Therefor an iteration over the old literals can still proceed.
     *
     * @param clause a binary clause.
     */
    protected void moveToIndexTwo(Clause clause) {
        assert(clause.size() == 2);
        for(int i = 0; i < 2; ++i) {
            Literal oldLiteralObject = clause.literals.get(i);
            Literal newLiteralObject = new Literal(oldLiteralObject.literal,oldLiteralObject.multiplicity);
            newLiteralObject.clause = clause;
            literalIndexMore.removeLiteral(oldLiteralObject);
            literalIndexTwo.addLiteral(newLiteralObject);
            clause.literals.set(i,newLiteralObject);}}





    /** adds a just shortened clause to the task queue
     *
     * @param clause a just shortened clause
     */
    protected void addShortenedClauseTask(Clause clause) {
        if(clause.size() == 2) addBinaryClauseTask(clause);
        else                   addLongerClauseTask(clause);}

    protected void processBinaryClause(Clause clause) throws Result {
        if(!clause.exists) return;
        int literal1 = clause.literals.get(0).literal;
        int literal2 = clause.literals.get(1).literal;
        removeClausesSubsumedByBinaryClause(clause);
        binaryMergeResolutionAndEquivalence(clause,literal1,literal2,true);
        if(!clause.exists) return;
        binaryMergeResolutionAndEquivalence(clause,literal2,literal1,false);
        if(!clause.exists) return;
        mergeResolutionWithBinaryClause(clause,literal1,literal2);
        if(!clause.exists) return;
        mergeResolutionWithBinaryClause(clause,literal2,literal1);
        if(!clause.exists) return;
        binaryClauseResolutionCompletion(clause,literal1);
        if(!clause.exists) return;
        binaryClauseResolutionCompletion(clause,literal2);}


    /** removes all longer disjunctions which are subsumed by a binary subsumer.
     *
     * @param subsumer a binary clause.
     * @throws Unsatisfiable should not happen.
     */
    protected void removeClausesSubsumedByBinaryClause(Clause subsumer) throws Unsatisfiable {
        assert(subsumer.size() == 2);
        Literal subsumerLiteral = literalIndexMore.getFirstLiteralObject(subsumer.literals.get(0).literal);
        while(subsumerLiteral != null) {
            Clause subsumee = subsumerLiteral.clause;
            if(subsumee.exists && subsumee.isDisjunction && subsumee != subsumer) subsumee.timestamp = timestamp;
            subsumerLiteral = subsumerLiteral.nextLiteral;}

        Literal subsumeeLiteral = literalIndexMore.getFirstLiteralObject(subsumer.literals.get(1).literal);
        while(subsumeeLiteral != null) {
            Clause subsumee = subsumeeLiteral.clause;
            if(subsumee == null) {subsumeeLiteral = subsumeeLiteral.nextLiteral; continue;}
            if(subsumee.timestamp == timestamp) {
                removeClause(subsumee,true);
                ++statistics.subsumedClauses;}
            subsumeeLiteral = subsumeeLiteral.nextLiteral;}
        ++timestamp;}

    /** removes all clauses subsumed by the given clause.<br>
     *  A clause atleast n    p_1^k1 ... p_n^kn subsumes <br>
     *  a clause atleast n-.. p_1^(k1+..) ... p_n^(kn+..) phi <br>
     *  where - means smaller or equal and + means larger or equal.<br>
     *  This is a sufficient, but not necessary condition. <br>
     *  A case not detected is: atleast 4 p^2,q^2,r subsumes atleast 3 p^2,q^3,r^2.
     *
     * @param subsumer       a clause,
     * @throws Unsatisfiable should not happen.
     */
    protected void removeClausesSubsumedByLongerClause(Clause subsumer) throws Unsatisfiable {
        int subsumerSize = subsumer.literals.size();
        int sumsumerLimit = subsumer.limit;
        Literal subsumerLiteral = subsumer.literals.get(0);
        Literal subsumeeLiteral = literalIndexMore.getFirstLiteralObject(subsumerLiteral.literal);
        while(subsumeeLiteral != null) { // mark all candidates with a timestamp
            Clause subsumee = subsumeeLiteral.clause;
            if(subsumee != subsumer && subsumee.limit <= sumsumerLimit &&
                    subsumee.literals.size() >= subsumerSize &&
                    subsumeeLiteral.multiplicity >= subsumerLiteral.multiplicity) {
                subsumee.timestamp = timestamp;}
            subsumeeLiteral = subsumeeLiteral.nextLiteral;}

        for(int i = 1; i < subsumer.literals.size(); ++i) { // find candidates.
            subsumerLiteral = subsumer.literals.get(i);
            subsumeeLiteral = literalIndexMore.getFirstLiteralObject(subsumerLiteral.literal);
            while(subsumeeLiteral != null) {
                Clause subsumee = subsumeeLiteral.clause;
                if(subsumee == null) {subsumeeLiteral = subsumeeLiteral.nextLiteral; continue;}
                if(subsumee.exists && (subsumee.timestamp - timestamp) == i - 1&&
                        subsumeeLiteral.multiplicity >= subsumerLiteral.multiplicity) {
                    ++subsumee.timestamp;
                    if(subsumee.timestamp - timestamp == subsumerSize-1){
                        removeClause(subsumee,true);}}
                subsumeeLiteral = subsumeeLiteral.nextLiteral;}}
        timestamp += subsumerSize + 1;}

        /** performs merge resolution between binary clauses and equivalence recognition, if possible.<br>
         * Binary MergeResolution:  p,q and -p,q -&gt; true(q).<br>
         * Equivalence Recognition: p,q and -p,-q -&gt; p == q.<br>
         * A derived true literal is inserted into the model.<br>
         * A derived equivalence is sent to the equivalence classes.<br>
         * In both cases the two clauses are removed and the search stops immediately.
         *
         * @param clause1   the binary clause.
         * @param literal1  either the first or the second literal.
         * @param literal2  the other literal.
         * @param checkEquivalence if true then equivalence check is done.
         * @throws Result if inserting a derived unit clause into the model causes a contradiction.
         */
    protected void binaryMergeResolutionAndEquivalence(Clause clause1, int literal1, int literal2,
                                                       boolean checkEquivalence) throws Result {
        assert(clause1.size() == 2);
        Literal literalObject = literalIndexTwo.getFirstLiteralObject(-literal1);
        while(literalObject != null) { // all clauses with -literal1 are marked.
            Clause clause = literalObject.clause;
            if(clause.exists && clause != clause1) clause.timestamp = timestamp;
            literalObject = literalObject.nextLiteral;}

        literalObject = literalIndexTwo.getFirstLiteralObject(literal2);
        while(literalObject != null) {
            Clause clause2 = literalObject.clause;
            if(clause2 == null) {literalObject = literalObject.nextLiteral; continue;}
            if(clause2.timestamp == timestamp) { // a partner clause is found
                if(monitoring) monitor.println(monitorId,clause1.toString(symboltable,0) + " and " +
                        clause2.toString(symboltable,0) + " -> " + "true("+ Symboltable.toString(literal2,symboltable)+")");
                model.add(literal2,trackReasoning ? new InfMergeResolutionTwo(clause1,clause2,literal2) : null);
                removeClause(clause1,true);
                removeClause(clause2,true);
                ++timestamp;
                return;}
            literalObject = literalObject.nextLiteral;}

        if(checkEquivalence) {
            literalObject = literalIndexTwo.getFirstLiteralObject(-literal2);
            while(literalObject != null) {
                Clause clause2 = literalObject.clause;
                if(clause2 == null) {literalObject = literalObject.nextLiteral; continue;}
                if(clause2.timestamp == timestamp) { // a partner clause is found.
                    if(monitoring) monitor.println(monitorId,clause1.toString(symboltable,0) + " and " +
                            clause2.toString(symboltable,0) + " -> " +
                            Symboltable.toString(literal1,symboltable)+" == " + Symboltable.toString(literal2,symboltable));
                    equivalenceClasses.addEquivalenceTask(literal1,literal2,trackReasoning ? new InfEquivalence(clause1,clause2) : null);
                    removeClause(clause1,false);
                    removeClause(clause2,false);
                    ++timestamp;
                    return;}
                literalObject = literalObject.nextLiteral;}}
        ++timestamp;}

    /** performs resolution between the given clause and other binary clauses.<br>
     * Subsumed resolvents are ignored. <br>
     * If the resolvent's literals are identical it is inserted into the model.<br>
     * Other resolvents generate a new BinaryClauseTask.
     *
     * @param clause     a binary clause,
     * @param literal1   a literal of the clause.
     * @throws Unsatisfiable  if the model detects a contradiction.
     */
    protected void binaryClauseResolutionCompletion(Clause clause, int literal1) throws Unsatisfiable {
        assert(clause.size() == 2);
        Literal literalObject = literalIndexTwo.getFirstLiteralObject(-literal1);
        while(literalObject != null) {
            Clause clause1 = literalObject.clause;
            if(clause1 == null) {literalObject = literalObject.nextLiteral; continue;}
            Clause resolvent = resolveBetweenBinaryClauses(clause,clause1);
            if(!clause.exists) return; // unit clause derived
            if(resolvent != null) {
                insertClause(resolvent);
                addBinaryClauseTask(resolvent);}
            literalObject = literalObject.nextLiteral;}}


    /** generates a resolvent between two binary clauses.
     * p,q and -p,r -> q,r.<br>
     * p,q and -p,q -> q.<br>
     * A subsumed resolvent is not returned.<br>
     * If the resolvent merges to a unit clauses, both clauses are removed.
     *
     * @param clause1 a binary disjunction
     * @param clause2 a binary disjunction
     * @return the resolvent or null if either the resolvent is subsumed, or a true literal is derived.
     */
    protected Clause resolveBetweenBinaryClauses(Clause clause1, Clause clause2) throws Unsatisfiable {
        assert(clause1.size() == 2);
        assert(clause2.size() == 2);
        int literala1 = clause1.literals.get(0).literal;
        int literala2 = clause1.literals.get(1).literal;
        int literalb1 = clause2.literals.get(0).literal;
        int literalb2 = clause2.literals.get(1).literal;
        int literal1,literal2;
        if(literala1 == -literalb1) {literal1 = literala2; literal2 = literalb2;}
        else {
            if(literala1 == -literalb2) {literal1 = literala2; literal2 = literalb1;}
            else {
                if(literala2 == -literalb1) {literal1 = literala1; literal2 = literalb2;}
                else {literal1 = literala1; literal2 = literalb1;}}}

        if(literal1 == -literal2) return null; // tautology
        if(literal1 == literal2) {
            ++statistics.derivedUnitClauses;
            model.add(literal1,trackReasoning ? new InfMergeResolutionTwo(clause1,clause2,literal1) : null);
            if(monitoring) {
                monitor.println(monitorId,clause1.toString(symboltable,0) + " and " +
                        clause2.toString(symboltable,0) + " -> " + Symboltable.toString(literal1,symboltable));}
            removeClause(clause1,true);
            removeClause(clause2,true);
            ++statistics.binaryResolvents;
            return null;}
        Clause resolvent = new Clause(nextId.getAsInt(), literal1, literal2);
        if(isSubsumedByBinaryClauses(resolvent)) return null;
        ++statistics.binaryResolvents;
        if(trackReasoning) resolvent.inferenceStep = new InfBinaryResolution(clause1,clause2,resolvent,symboltable);
        return resolvent;}



    /** checks if the binary clause is subsumed by another binary clause.
     *
     * @param clause a binary clause
     * @return true if the clause is subsumed.
     */
    protected boolean isSubsumedByBinaryClauses(Clause clause) {
        return isSubsumedByBinaryClauses(clause.literals.get(0).literal, clause.literals.get(1).literal);}

    /** checks if the binary clause is subsumed by another binary clause.
     * Since binary clauses are always disjunctions, quantification plays no role.
     *
     * @param literal1 the first literal of a binary clause
     * @param literal2 the second literal of a binary clause.
     * @return true if the clause is subsumed.
     */
    protected boolean isSubsumedByBinaryClauses(int literal1, int literal2) {
        Literal literalObject = literalIndexTwo.getFirstLiteralObject(literal1);
        while(literalObject != null) {
            Clause clause = literalObject.clause;
            if(clause.exists) clause.timestamp = timestamp;
            literalObject = literalObject.nextLiteral;}

        literalObject = literalIndexTwo.getFirstLiteralObject(literal2);
        while(literalObject != null) {
            if(literalObject.clause.timestamp == timestamp) {++timestamp; return true;}
            literalObject = literalObject.nextLiteral;}
        ++timestamp;
        return false;}

    /** performs merge resolution between a binary clause and a longer clause.<br>
     *  p,q and atleast n -p,q^n,phi -> atleast n q^n,phi<br>
     *  The resolvent is simplified further.
     *
     * @param clause   a binary clause
     * @param literal1 a literal of the binary clause
     * @param literal2 the other literal of the binary clause
     * @throws Result  if a contradiction is encountered.
     */
    protected void mergeResolutionWithBinaryClause(Clause clause, int literal1, int literal2) throws Result {
        assert(clause.size() == 2);
        Literal literalObject1 = literalIndexMore.getFirstLiteralObject(-literal1);
        while(literalObject1 != null) {
            Clause clause1 = literalObject1.clause;
            if(clause1.exists && literalObject1.multiplicity == 1 )
                clause1.timestamp = timestamp;
            literalObject1 = literalObject1.nextLiteral;}

        Literal literalObject2 = literalIndexMore.getFirstLiteralObject(literal2);
        while(literalObject2 != null) {
            Clause clause2 = literalObject2.clause;
            if(clause2 == null) {literalObject2 = literalObject2.nextLiteral; continue;}
            if(clause2.timestamp == timestamp && clause2.limit == literalObject2.multiplicity) {
                String clause2String = (trackReasoning || monitoring) ? clause2.toString(symboltable,0) : null;
                ++statistics.mergedClauses;
                if(removeLiteral(clause2.findLiteral(-literal1),false)) {
                    if(trackReasoning) clause2.inferenceStep = new InfMergeResolutionMore(clause,clause2String,clause2,symboltable);
                    if(monitoring) monitor.println(monitorId,"Merge Resolution between " +
                            clause.toString(symboltable,0) + " and " + clause2String + " -> " + clause2.toString(symboltable,0));
                    if(simplifyClause(clause2,true)) addShortenedClauseTask(clause2);
                    else {removeClause(clause2,true);}}}
            literalObject2 = literalObject2.nextLiteral;}
        ++timestamp;
    }

    /** removes all subsumed clauses and performs merge resolution with the longer clauses.
     *
     * @param clause a longer clause.
     * @throws Result if an exception is dicovered.
     */
    protected void processLongerClause(Clause clause) throws Result {
        removeClausesSubsumedByLongerClause(clause);
        mergeResolutionWithLongerClauseDirect(clause);
        if(!clause.exists) return;
        mergeResolutionWithLongerClauseIndirect(clause);
    }

    /** performs merging resolution between longer clauses.<br>
     * atleast n p^n',q_1^k_1,...,q_l^k_l and atleast m -p^n,q_1^m,...,q_l^m, phi -> atleast m q_1^m,...,q_l^m, phi<br>
     * If phi is empty then one clause is removed entirely.<br>
     * The shortened clause is simplified further and a new task is generated.
     *
     * @param clause a clause to be tested as parent clause for a merging resolution step
     * @throws Result if the simplification causes an Unsatisfiable exception.
     */
    protected void mergeResolutionWithLongerClauseDirect(Clause clause) throws Result {
        int clauseSize = clause.literals.size();
        int thisLimit = clause.limit;
        for(Literal firstLiteral : clause.literals) {
            int firstNegLiteral = -firstLiteral.literal;
            Literal negLiteral = literalIndexMore.getFirstLiteralObject(firstNegLiteral);
            while(negLiteral != null) {
                Clause otherClause = negLiteral.clause;
                if(otherClause.exists && otherClause.literals.size() >= clauseSize &&
                        otherClause.limit >= thisLimit && negLiteral.multiplicity == thisLimit) {
                    otherClause.timestamp = timestamp;}
                negLiteral = negLiteral.nextLiteral;}

            int i = 0;
            for(Literal otherLiteral : clause.literals) {
                if(otherLiteral == firstLiteral) continue;
                ++i;
                Literal posLiteral = literalIndexMore.getFirstLiteralObject(otherLiteral.literal);
                while(posLiteral != null) {
                    Clause otherClause = posLiteral.clause;
                    if(otherClause == null) {posLiteral = posLiteral.nextLiteral; continue;}
                    if((otherClause.timestamp -timestamp) == i-1 && posLiteral.multiplicity == otherClause.limit) {
                        ++otherClause.timestamp;
                        if(otherClause.timestamp - timestamp == clauseSize-1) { // mergepartner found
                            if(otherClause.size() == clauseSize) {
                                statistics.mergedClauses += 2;
                                removeClause(otherClause,true);
                                String resolventBefore = (trackReasoning | monitoring) ? clause.toString(symboltable,0) : null;
                                clause.removeLiteral(firstLiteral,false);

                                if(trackReasoning) {
                                    clause.inferenceStep =
                                            new InfMergeResolutionMore(otherClause,resolventBefore,clause,symboltable);}
                                if(monitoring) {
                                    monitor.println(monitorId,
                                            otherClause.toString(symboltable,0) + " and " +
                                            resolventBefore + " -> " + clause.toString(symboltable,0));}

                                if(simplifyClause(clause,true)) {
                                    addToIndex(clause);
                                    if(clause.size() == 2) addBinaryClauseTask(clause);
                                    else addLongerClauseTask(clause);}
                                else clauses.removeClause(clause);
                                timestamp += clauseSize + 1;
                                return;}
                            else {
                                ++statistics.mergedClauses;
                                removeFromIndex(otherClause);
                                String resolventBefore = trackReasoning ? otherClause.toString(symboltable,0) : null;
                                otherClause.removeLiteral(otherClause.findLiteral(firstNegLiteral),false);

                                if(trackReasoning) {
                                    otherClause.inferenceStep =
                                            new InfMergeResolutionMore(clause,resolventBefore,otherClause,symboltable);}
                                if(monitoring) {
                                    monitor.println(monitorId, resolventBefore + " and " +
                                            otherClause.toString(symboltable,0) + " -> " + otherClause.toString(symboltable,0));}

                                if(simplifyClause(otherClause,true)) {
                                    addToIndex(otherClause);
                                    if(otherClause.size() == 2) addBinaryClauseTask(otherClause);
                                    else addLongerClauseTask(otherClause);}
                                else clauses.removeClause(otherClause);}}}
                posLiteral = posLiteral.nextLiteral;}}
            timestamp += clauseSize + 1;}}


    /** performs merging resolution between longer clauses, including a binary clause.<br>
     * atleast n p^n',q_1^k_1,...,q_l^k_l and -p,s and  atleast m -s^(n-n'+1),q_1^m,...,q_l^m, phi -> atleast m q_1^m,...,q_l^m, phi<br>
     * If phi is empty then one clause is removed entirely.<br>
     * The shortened clause is simplified further and a new task is generated.
     *
     * @param clause a clause to be tested as parent clause for a merging resolution step
     * @throws Result if the simplification causes an Unsatisfiable exception.
     */
    protected void mergeResolutionWithLongerClauseIndirect(Clause clause) throws Result {
        int clauseSize = clause.literals.size();
        int thisLimit = clause.limit;
        for(Literal firstLiteral : clause.literals) {
            int firstNegLiteral = -firstLiteral.literal;
            int thisMultiplicity = firstLiteral.multiplicity;
            Literal twoLiteral1 = literalIndexTwo.getFirstLiteralObject(firstNegLiteral);
            while(twoLiteral1 != null) {
                Clause twoClause = twoLiteral1.clause;
                Literal twoLiteral2 = twoLiteral1.clause.otherLiteral(twoLiteral1);
                Literal negLiteral = literalIndexMore.getFirstLiteralObject(-twoLiteral2.literal);
                while(negLiteral != null) {
                    Clause otherClause = negLiteral.clause;
                    if(otherClause.exists && otherClause.literals.size() >= clauseSize &&
                            otherClause.limit >= thisLimit && negLiteral.multiplicity == thisLimit-thisMultiplicity+1) {
                         otherClause.timestamp = timestamp;}
                    negLiteral = negLiteral.nextLiteral;}

                int i = 0;
                for(Literal otherLiteral : clause.literals) {
                    if(otherLiteral == firstLiteral) continue;
                    ++i;
                    Literal posLiteral = literalIndexMore.getFirstLiteralObject(otherLiteral.literal);
                    while(posLiteral != null) {
                        Literal nextLiteral = posLiteral.nextLiteral;
                        Clause otherClause = posLiteral.clause;
                        if(otherClause.exists && (otherClause.timestamp -timestamp) == i-1 && posLiteral.multiplicity == otherClause.limit) {
                            ++otherClause.timestamp;
                            if(otherClause.timestamp - timestamp == clauseSize-1) { // mergepartner found
                                if(otherClause.size() == clauseSize) {
                                    statistics.mergedClauses += 2;
                                    removeClause(otherClause,true);
                                    removeFromIndex(clause);
                                    String resolventBefore = (trackReasoning | monitoring) ? clause.toString(symboltable,0) : null;
                                    clause.removeLiteral(firstLiteral,false);

                                    if(trackReasoning) {
                                        clause.inferenceStep =
                                            new InfMergeResolutionIndirect(otherClause,twoClause,resolventBefore,clause,symboltable);}
                                    if(monitoring) {
                                        monitor.println(monitorId,
                                            otherClause.toString(symboltable,0) + " and " +
                                                    twoClause.toString(symboltable,0)+ " and " +
                                                    resolventBefore + " -> " + clause.toString(symboltable,0));}

                                    if(simplifyClause(clause,true)) {
                                        addToIndex(clause);
                                        if(clause.size() == 2) addBinaryClauseTask(clause);
                                        else addLongerClauseTask(clause);}
                                    else clauses.removeClause(clause);
                                    timestamp += clauseSize + 1;
                                    return;}
                                else {
                                    ++statistics.mergedClauses;
                                    removeFromIndex(otherClause);
                                    String resolventBefore = trackReasoning ? otherClause.toString(symboltable,0) : null;
                                    otherClause.removeLiteral(otherClause.findLiteral(-twoLiteral2.literal),false);

                                    if(trackReasoning) {
                                        otherClause.inferenceStep =
                                            new InfMergeResolutionIndirect(clause,twoClause,resolventBefore,otherClause,symboltable);}
                                    if(monitoring) {
                                        monitor.println(monitorId, clause.toString(symboltable,0) + " and " +
                                                twoClause.toString(symboltable,0) + " and " +
                                                 resolventBefore + " -> " + otherClause.toString(symboltable,0));}

                                    if(simplifyClause(otherClause,true)) {
                                        addToIndex(otherClause);
                                        if(otherClause.size() == 2) addBinaryClauseTask(otherClause);
                                        else addLongerClauseTask(otherClause);}
                                    else clauses.removeClause(otherClause);}}}
                        posLiteral = nextLiteral;}}
                timestamp += clauseSize + 1;
                twoLiteral1 = twoLiteral1.nextLiteral;}
            timestamp += clauseSize + 1;}}


    protected void addBinaryClauseTask(Clause clause) {
        synchronized (this) {queue.add(new Task<>(TaskType.ProcessBinaryClause, clause));}}

    protected void addLongerClauseTask(Clause clause) {
        synchronized (this) {queue.add(new Task<>(TaskType.ProcessLongerClause, clause));}}


    /** This method replaces in all clauses the given literal by the given representative.<br>
     * The new clauses are simplified as far as possible.<br>
     * Derived true literals are inserted into the model.
     *
     * @param representative  the representative of an equivalence class.
     * @param literal         the literal of the equivalence class.
     * @param equivalenceStep which caused the equivalence.
     * @throws Result         if a contradiction is encountered.
     */
    protected void processEquivalence(int representative, int literal, InferenceStep equivalenceStep) throws Result {
        for(int sign = +1; sign >= -1; sign -=2) {
            representative *= sign;
            literal *= sign;
            Literal literalObject = literalIndexTwo.getFirstLiteralObject(literal);
            while(literalObject != null) {
                Literal nextLiteral = literalObject.nextLiteral;
                Clause  clause = literalObject.clause;
                if(clause.exists) {
                    removeFromIndex(clause);
                    String clauseString = (trackReasoning | monitoring) ? clause.toString(symboltable,0) : null;
                    if(clause.replaceEquivalenceTwo(representative,literal)) {
                        InferenceStep step = trackReasoning ? new InfEquivalenceMerge(clause,representative, literal, equivalenceStep) : null;
                        model.add(representative, step);
                        model.add(literal, step);
                        if(monitoring) {
                            monitor.println(monitorId,"Equivalence Replacement: " +
                                    clauseString + " and " + Symboltable.toString(literal,symboltable) + " = " +
                                    Symboltable.toString(representative,symboltable) + " -> true(" +
                                    Symboltable.toString(representative,symboltable)+ ","+
                                    Symboltable.toString(literal,symboltable)+")");}
                        removeClause(clause,true);
                        return;} // true literals need no further replacements.
                    else {
                        if(trackReasoning) {clause.inferenceStep =
                                new InfEquivalenceReplacement(clauseString,clause,representative,literal,equivalenceStep, symboltable);}
                        addToIndex(clause);
                        addBinaryClauseTask(clause);}}
                literalObject = nextLiteral;}

            literalObject = literalIndexMore.getFirstLiteralObject(literal);  // we check the longer clauses.
            while(literalObject != null) {
                Literal nextLiteral = literalObject.nextLiteral;
                Clause  clause = literalObject.clause;
                if(clause.exists) {
                    removeFromIndex(clause);
                    String clauseString = (trackReasoning | monitoring) ? clause.toString(symboltable,0) : null;
                    boolean merged = clause.replaceEquivalenceMore(representative,literal);
                    if(trackReasoning) clause.inferenceStep =
                            new InfEquivalenceReplacement(clauseString,clause,representative,literal,equivalenceStep, symboltable);
                    if(merged) { // two literals are merged. The new claus might be simplified.
                        simplifyClause(clause,true);
                        if(clause.exists) {
                            addToIndex(clause);
                            if(clause.size() == 2) addBinaryClauseTask(clause);
                            else addShortenedClauseTask(clause);}
                        else clauses.removeClause(clause);}} // changed clauses without merged literals are not further processed.
                literalObject = nextLiteral;}
            }
    }



    /** inserts a clause into the internal lists.
     *
     * @param clause a clause.
     */
    protected void insertClause(Clause clause) {
        Literals literalIndex = (clause.size() == 2) ? literalIndexTwo : literalIndexMore;
        for(Literal literalObject : clause.literals) literalIndex.addLiteral(literalObject);
        clauses.addClause(clause);}


    /** removes the clause from the internal lists.<br>
     *
     * @param clause  a clause to be removed.
     * @param checkPurity if true then the clause's literals are checked for purity.
     * @throws Unsatisfiable should not happen.
     */
    protected void removeClause(Clause clause, boolean checkPurity) throws Unsatisfiable {
        clauses.removeClause(clause);
        clause.exists = false;
        Literals literalIndex = (clause.size() == 2) ? literalIndexTwo :literalIndexMore;
        for(Literal literalObject : clause.literals) {
            literalIndex.removeLiteral(literalObject);
            if(checkPurity) {checkPurity(literalObject.literal); checkPurity(-literalObject.literal);}}}

    /** removes all clauses containing the literal.
     *
     * @param literal a literal.
     * @return the number of removed clauses.
     * @throws Result should not happen.
     */
    protected int removeClauses(int literal) throws Result {
        int removedClauses = 0;
        Literal literalObject = literalIndexTwo.getFirstLiteralObject(literal);
        while(literalObject != null) {
            Literal nextLiteralObject = literalObject.nextLiteral;
            Clause clause = literalObject.clause;
            if(clause.exists) {removeClause(clause,false);++removedClauses;}
            literalObject = nextLiteralObject;}

        literalObject = literalIndexMore.getFirstLiteralObject(literal);
        while(literalObject != null) {
            Literal nextLiteralObject = literalObject.nextLiteral;
            Clause clause = literalObject.clause;
            if(clause.exists) {removeClause(clause,false);++removedClauses;}
            literalObject = nextLiteralObject;}
        return removedClauses;}

    /** removes the clause from the corresponding index.
     *
     * @param clause a clause
     */
    protected void removeFromIndex(Clause clause) {
        Literals literalIndex = (clause.size() == 2) ? literalIndexTwo : literalIndexMore;
        for(Literal literalObject : clause.literals) {
            literalIndex.removeLiteral(literalObject);}}

    /** removes the Literal from the corresponding index.
     *
     * @param literalObject a Literal
     */
    protected void removeFromIndex(Literal literalObject) {
        Clause clause = literalObject.clause;
        Literals literalIndex = (clause.size() == 2) ? literalIndexTwo : literalIndexMore;
        literalIndex.removeLiteral(literalObject);}


    /** adds a clause to the corresponding index.
     *
     * @param clause a clause.
     */
    protected void addToIndex(Clause clause) {
        Literals literalIndex = (clause.size() == 2) ? literalIndexTwo : literalIndexMore;
        for(Literal literalObject : clause.literals) {
            literalIndex.addLiteral(literalObject);}}

    private final IntArrayList removedLiterals = new IntArrayList(5);

    /** removes the literal from the clause and the literalIndexMore index.
     * If the literal becomes pure, it is inserted into the model.<br>
     * If the limit is reduced to 0, the entire clause is removed.<br>
     *
     * @param literalObject the literal object to be removed.
     * @return true if the clause still exists.
     * @throws Unsatisfiable if inserting a pure literal into the model causes a contradiction.
     */
    protected boolean removeLiteral(Literal literalObject, boolean reduceQuantifier) throws Result {
        Clause clause = literalObject.clause;
        removeFromIndex(literalObject);
        if(clause.removeLiteral(literalObject,reduceQuantifier)){
            if(clause.size() == 2) {
                for(Literal litObject : clause.literals) {
                    literalIndexMore.removeLiteral(litObject);
                    literalIndexTwo.addLiteral(litObject);}}
            checkPurity(-literalObject.literal); // pure literals are just added to the model.
            return true;}
        removeClause(clause,true);
        return false;}

    /** simplifies an atleast-clause with multiplicities > 1.<br>
     * 1. True literals are extracted.<br>
     * Example: atleast 5 p^2,q^2,r,s -&gt; true(p,q) and r,s (disjunction).<br>
     * 2. Clause is reduced to essential literals.<br>
     * Example: atleast 2 p^2,q^2,r -&gt; p,q (disjunction).<br>
     * 3. Numbers are divided by the greatest common divisor (GCD). <br>
     * Example: atleast 6 p^4,q^4,r^4 -> atleast 3 p^2,q^2,r^2.
     *
     * @param clause         the clause to be simplified.
     * @param checkPurity    if true then the removed literals are checked for purity.
     * @return               true if the clause still exists.
     * @throws Result        if the model finds a contradiction.
     */
    protected boolean simplifyClause(Clause clause, boolean checkPurity) throws Result {  // ändern
        if(clause.isDisjunction || !clause.hasMultiplicities) return true; // nothing to be simplified.
        String clauseBefore = (trackReasoning || monitoring) ? clause.toString(symboltable,0) : null;
        boolean reducedByGCD = false;
        removedLiterals.clear();
        try {
            ArrayList<Literal> trueLiterals = clause.reduceByTrueLiterals(removedLiterals);
            if (trueLiterals != null) {
                String literalNames = "";
                for (Literal literalObject : trueLiterals) {
                    int literal = literalObject.literal;
                    ++statistics.derivedUnitClauses;
                    if (monitoring) literalNames += Symboltable.toString(literal, symboltable) + ",";
                    model.add(literal, trackReasoning ? new InfTrueLiteral(clauseBefore, clause.id, literal, clause.inferenceStep) : null);
                }
                if (monitoring) monitor.println(monitorId, "True literals " + literalNames + " extracted from clause " +
                        clauseBefore + ". new clause: " + clause.toString(symboltable, 0));
                if(!clause.exists) return false;}

            if (clause.isDisjunction || !clause.hasMultiplicities) return true; // nothing to be simplified.
            if (clause.reduceToEssentialLiterals(removedLiterals)) {
                if (monitoring)
                    monitor.println(monitorId, "Clause  " + clauseBefore + " reduced to essential literals:  " +
                            clause.toString(symboltable, 0));
                if (clause.limit <= 0) return false;
                if (clause.size() == 1) {
                    int literal = clause.literals.get(0).literal;
                    model.add(literal, clause.inferenceStep);
                    return false;
                }
                if (clause.limit <= 1) return clause.limit == 1;
            }

            reducedByGCD = clause.divideByGCD();
            if (reducedByGCD && monitoring) {
                monitor.println(monitorId, "Clause " + clauseBefore + " divided by gcd  to " +
                        clause.toString(symboltable, 0));}
            if (clause.size() == 1) {
                model.add(clause.literals.get(0).literal, new InfInputClause(clause.id));
                return false;}}
        finally {if(checkPurity) {for(int literal : removedLiterals) checkPurity(literal);}}
        if(reducedByGCD) return simplifyClause(clause,checkPurity);
        return true;}

    /** removes all clauses from the internal datastructures.
     * This is mainly for testing purposes.
     */
    public void clear() {
        if(inputClauses != null) inputClauses.clear();
        timestamp = 1;
        literalIndexTwo.clear();
        literalIndexMore.clear();
        clauses.clear();
        model.clear();
        statistics.clear();}

    @Override
    public Result solveProblem(InputClauses inputClauses) {
        return null;
    }

    @Override
    public void prepare() {

    }

    @Override
    public Statistic getStatistics() {
        return null;
    }
}
