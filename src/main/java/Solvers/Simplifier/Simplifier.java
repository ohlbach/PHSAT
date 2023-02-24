package Solvers.Simplifier;

import Datastructures.Clauses.InputClauses;
import Datastructures.Results.EmptyClauses;
import Datastructures.Results.Result;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Statistics.Statistic;
import Datastructures.Symboltable;
import Datastructures.Task;
import Datastructures.Theory.EquivalenceClasses.EquivalenceClasses;
import InferenceSteps.InferenceStep;
import Management.Monitor.Monitor;
import Management.ProblemSupervisor;
import Solvers.Solver;

import java.util.Comparator;
import java.util.concurrent.PriorityBlockingQueue;
import java.util.function.IntSupplier;

import static java.lang.Thread.interrupted;

public class Simplifier extends Solver {

    private InputClauses inputClauses;
    private ProblemSupervisor problemSupervisor;

    /** controls the computation of the clause's origins */
    public boolean trackReasoning;

    public EquivalenceClasses equivalenceClasses;

    /** The id of the current problem to be solved */
    private String problemId;

    /** for logging the actions of this class */
    private Monitor monitor;

    /** indicates monitoring is on */
    private boolean monitoring;

    /** for distinguishing the monitoring areas */
    private String monitorId;

    private Literals literalIndexTwo;
    private Literals literalIndexMore;
    private Clauses clauses;

    private IntSupplier nextId;

    private int timestamp = 1;

    public Simplifier(ProblemSupervisor problemSupervisor) {
        this.problemSupervisor = problemSupervisor;
        this.inputClauses = problemSupervisor.inputClauses;
    }

    public Simplifier(InputClauses inputClauses) {
        this.inputClauses = inputClauses;}

    private enum TaskType {
        /** a new true literal is obtained from the model */
        TrueLiteral,
        /** a new binary equivalence is found in the TwoLiteral module. */
        Equivalence,
        BinaryClause,
        SimplifyTwoLiteralClauses,
        SimplifyLongerClauses
    }

    /** gets the priority for the objects in the queue.
     *
     * @param task the task in the queue
     * @return the priority of the objects in the queue.
     */
    private int getPriority(Task<Simplifier.TaskType> task) {
        switch(task.taskType) {
            case TrueLiteral: return Integer.MIN_VALUE + Math.abs((Integer)task.a);
            case Equivalence: return (Math.abs((Integer)task.a)); // this guarantees a deterministic sequence of the tasks
            case BinaryClause:          return 100;
            case SimplifyTwoLiteralClauses: return 101;
            case SimplifyLongerClauses:     return 102;}
        return 0;}

    /** A queue of newly derived unit literals and binary equivalences.
     * The unit literals are automatically put at the beginning of the queue.
     */
    private final PriorityBlockingQueue<Task<Simplifier.TaskType>> queue =
            new PriorityBlockingQueue<>(10, Comparator.comparingInt(this::getPriority));

    /** adds a true literal to the queue
     *
     * @param literal a true literal
     * @param inferenceStep which caused the truth
     */
    public void addTrueLiteral(int literal, InferenceStep inferenceStep) {
        if(monitoring) {
            monitor.print(monitorId,"In:   True literal " +
                    Symboltable.toString(literal,symboltable));}
        synchronized (this) {queue.add(new Task<>(TaskType.TrueLiteral, literal, inferenceStep));}}


    public void run() {
        Task<Simplifier.TaskType> task;
        while(!interrupted()) {
            try {
                if(monitoring) {monitor.print(monitorId,"Queue is waiting\n" + Task.queueToString(queue));}
                task = queue.take(); // waits if the queue is empty
                switch(task.taskType){
                    case TrueLiteral: applyTrueLiteral((Integer)task.a); break;
                    case Equivalence: applyEquivalence((Integer)task.a,(Integer)task.b,(InferenceStep) task.c); break;
                    case BinaryClause: processBinaryClause((Clause)task.a); break;
                    case SimplifyTwoLiteralClauses: break;
                    case SimplifyLongerClauses: break;}}
            catch(InterruptedException ex) {return;}
            catch(Result result) {
                problemSupervisor.finished("Simplifier", result,"");
                return;}}}
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
    protected void applyTrueLiteral(int literal) throws Result {
        Literal literalObject = literalIndexTwo.getFirstLiteralObject(literal);
        while(literalObject != null) {
            Clause clause = literalObject.clause;
            if(clause.exists) {
                removeClause(clause); // clauses may become empty and literal may become pure.
                if(clause.quantifier != literalObject.multiplicity)
                    unitResolutionWithBinaryClause(literalObject);}
            literalObject = literalObject.nextLiteral;}

        literalObject = literalIndexTwo.getFirstLiteralObject(-literal);
        while(literalObject != null) {
            Clause clause = literalObject.clause;
            if(clause.exists) {
                unitResolutionWithBinaryClause(literalObject);
                removeClause(literalObject.clause);}
            literalObject = literalObject.nextLiteral;}

        literalIndexTwo.removePredicate(literal);

        literalObject = literalIndexMore.getFirstLiteralObject(literal);
        while(literalObject != null) {
            Clause clause = literalObject.clause;
            if(clause.exists) {
                if(clause.quantifier == literalObject.multiplicity) removeClause(clause);
                else {
                    clause.removeLiteral(literalObject,true);
                    analyseShortendClause(clause);}}
            literalObject = literalObject.nextLiteral;}

        literalObject = literalIndexMore.getFirstLiteralObject(-literal);
        while(literalObject != null) {
            Clause clause = literalObject.clause;
            if(clause.exists) {
                clause.removeLiteral(literalObject, false);
                analyseShortendClause(clause);}
            literalObject = literalObject.nextLiteral;}

        literalIndexMore.removePredicate(literal);}

    protected void analyseShortendClause(Clause clause) {
        if(clause.size() == 2) {
            for(Literal literalObject : clause.literals) {
                literalIndexMore.removeLiteral(literalObject);
                literalIndexTwo.addLiteral(literalObject);}
            addBinaryClauseTask(clause);}
        else {

        }}

    protected void processBinaryClause(Clause clause) throws Result {
        if(!clause.exists) return;
        removeClausesSubsumedByBinaryClause(clause);
        binaryClauseResolutionCompletion(clause,clause.literals.get(0).literal,clause.literals.get(1).literal);
        binaryClauseResolutionCompletion(clause,clause.literals.get(1).literal,clause.literals.get(0).literal);}


    protected void binaryClauseResolutionCompletion(Clause clause, int literal1, int literal2) throws Result {
        Literal literalObject = literalIndexTwo.getFirstLiteralObject(-literal1);
        while(literalObject != null) {
            Clause clause1 = literalObject.clause;
            if(clause1.exists) clause1.timestamp = timestamp;
            literalObject = literalObject.nextLiteral;}
        literalObject = literalIndexTwo.getFirstLiteralObject(literal2);
        while(literalObject != null) {
            Clause clause1 = literalObject.clause;
            if(clause1.timestamp == timestamp) {
                Clause resolvent = resolveBetweenBinaryClauses(clause,clause1);
                if(resolvent != null) {
                    Clause partnerClause = isEquivalence(resolvent);
                    if(partnerClause != null) {
                        equivalenceClasses.addEquivalence(clause.literals.get(0).literal, -clause.literals.get(1).literal,null);
                        removeClause(partnerClause);
                        resolvent = null;}}
                if(resolvent != null) {
                    removeClausesSubsumedByBinaryClause(resolvent);
                    replacementResolutionWithBinaryClause(resolvent, clause.literals.get(0).literal, clause.literals.get(1).literal);
                    replacementResolutionWithBinaryClause(resolvent, clause.literals.get(1).literal, clause.literals.get(0).literal);
                }
            }
            literalObject = literalObject.nextLiteral;}
    }


    /** generates a resolvent between two binary clauses.
     * Since binary clauses are always disjunctions, quantification plays no role.
     *
     * @param clause1 a binary disjunction
     * @param clause2 a binary disjunction
     */
    protected Clause resolveBetweenBinaryClauses(Clause clause1, Clause clause2) {
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

        if(literal1 == -literal2) return null;
        if(literal1 == literal2) {addTrueLiteral(literal1,null); return  null;}
        Clause resolvent = new Clause(nextId.getAsInt(), literal1, literal2); // anpassen
        if(isSubsumedByBinaryClauses(resolvent)) return null;
        return resolvent;}


    /** removes all longer clauses which are subsumed by a binary clause.
     * Since binary clauses are disjunctions, the quantification of the other clauses plays no role.
     *
     * @param clause a binary clause
     * @throws Result if the clause set becomes empty.
     */
    protected void removeClausesSubsumedByBinaryClause(Clause clause) throws Result {
        Literal literalObject = literalIndexMore.getFirstLiteralObject(clause.literals.get(0).literal);
        while(literalObject != null) {
            Clause clause1 = literalObject.clause;
            if(clause1.exists && clause1 != clause) clause1.timestamp = timestamp;
            literalObject = literalObject.nextLiteral;}
        literalObject = literalIndexMore.getFirstLiteralObject(clause.literals.get(1).literal);
        while(literalObject != null) {
            Clause clause1 = literalObject.clause;
            literalObject = literalObject.nextLiteral;
            if(clause1.timestamp == timestamp) removeClause(clause1);}
        ++timestamp;}

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

    /** checks if the binary clause is part of an equivalence.<br>
     * Example: p,q and -p,-q means p == -q
     *
     * @param clause the clause to be checked
     * @return null or the partner clause for the equivalence.
     */
    protected Clause isEquivalence(Clause clause) {
        Literal literalObject = literalIndexTwo.getFirstLiteralObject(-clause.literals.get(0).literal);
        while(literalObject != null) {
            Clause clause1 = literalObject.clause;
            if(clause1.exists) clause1.timestamp = timestamp;
            literalObject = literalObject.nextLiteral;}
        literalObject = literalIndexTwo.getFirstLiteralObject(-clause.literals.get(1).literal);
        while(literalObject != null) {
            Clause clause1 = literalObject.clause;
            if(clause1.timestamp == timestamp) {++timestamp; return clause1;}
            literalObject = literalObject.nextLiteral;}
        ++timestamp;
        return null;}

    protected void replacementResolutionWithBinaryClause(Clause clause, int literal1, int literal2) throws Result {
        Literal literalObject1 = literalIndexMore.getFirstLiteralObject(-literal1);
        while(literalObject1 != null) {
            Clause clause1 = literalObject1.clause;
            if(clause1.exists) clause1.timestamp = timestamp;
            literalObject1 = literalObject1.nextLiteral;}
        Literal literalObject2 = literalIndexMore.getFirstLiteralObject(literal2);
        while(literalObject2 != null) {
            Clause clause1 = literalObject2.clause;
            if(clause1.timestamp == timestamp); {
                Clause resolvent = replacementResolventWithBinaryClause(clause1,clause,-literal1);
            }
            literalObject2 = literalObject2.nextLiteral;}
    }

    protected Clause replacementResolventWithBinaryClause(Clause longerClause, Clause binaryClause, int literal) throws Result {
        Literal literalObject = longerClause.findLiteral(literal);
        assert(literalObject != null);
        removeClause(longerClause);
        longerClause.removeLiteral(literalObject,false);
        if(longerClause.size() == 2) {
            Clause shortenedClause = reduceShortenedToBinaryClause(longerClause);
            if(shortenedClause != null) {insertClause(shortenedClause); addBinaryClauseTask(shortenedClause);}
        }
        return null;
    }

    protected Clause reduceShortenedToBinaryClause(Clause clause) throws Result {


    }

    protected void addBinaryClauseTask(Clause clause) {
        synchronized (this) {queue.add(new Task<>(TaskType.BinaryClause, clause));}}

    protected void addSimplifyClauseTask(Clause clause) {
        synchronized (this) {queue.add(new Task<>(TaskType.SimplifyLongerClauses, clause));}}


    protected void applyEquivalence(int representative, int literal, InferenceStep inferenceStep) {

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
     * If the clause list becomes empty a EmptyClauses exception is thrown.<br>
     * If removing a literal causes that the corresponding list of literal objects becomes empty
     * then the negated literal becomes pure and is inserted into the model
     *
     * @param clause  a clause to be removed.
     * @throws Result if the clause set becomes empty or a pure literal causes an contradiction.
     */
    protected void removeClause(Clause clause) throws Result {
        if(clauses.removeClause(clause) == 0) throw new EmptyClauses(model);
        clause.exists = false;
        Literals literalIndex1,literalIndex2;
        if(clause.size() == 2) {literalIndex1 = literalIndexTwo; literalIndex2 = literalIndexMore;}
        else {literalIndex1 = literalIndexMore; literalIndex2 = literalIndexTwo;}
        for(Literal literalObject : clause.literals) {
            int literal = literalObject.literal;
            if(literalIndex1.removeLiteral(literalObject) && literalIndex2.isEmpty(literal)) {
                model.add(-literal,trackReasoning ? new InfPureLiteral(literal) : null);}}}

    /** removes the literal from the literalIndexMore index.
     * If the literal becomes pure, its negation is inserted into the model.
     *
     * @param literalObject the literal object to be removed
     * @throws Unsatisfiable if inserting a pure literal into the model causes a contradiction.
     */
    protected void removeLiteral(Literal literalObject, boolean reduceQuantifier) throws Unsatisfiable {
        Clause clause = literalObject.clause;
        clause.removeLiteral(literalObject,reduceQuantifier);
        int literal = literalObject.literal;
        if(literalIndexMore.removeLiteral(literalObject)) {
            model.add(-literal,trackReasoning ? new InfPureLiteral(literal) : null);}}


    /** unit resolution with a binary disjunction or quantified clause.<br>
     * Example: atleast 2 p,q^2 and true(p) -&gt; atleast 1 q^2 -&gt; true(q).<br>
     * Example: atleast 2 p,q^2 and false(p) -&gt; atleast 2 q^2 -&gt; true(q).<br>
     *
     * @param literalObject a literal as part of a quantified binary clause.
     * @throws Unsatisfiable if inserting the second literal into the model causes a contradiction.
     */
    public void unitResolutionWithBinaryClause(Literal literalObject) throws Unsatisfiable {
        Clause clause = literalObject.clause;
        Literal literalObject1 = clause.literals.get(0);
        Literal literalObject2 = clause.literals.get(1);
        int literal = literalObject == literalObject1 ? literalObject2.literal : literalObject1.literal;
        InferenceStep step = null;
        if(trackReasoning) {
        }
        model.add(literal, step);}

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