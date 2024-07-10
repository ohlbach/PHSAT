package Solvers.Normalizer;

import Datastructures.Clause;
import Datastructures.ClauseList;
import Datastructures.Clauses.InputClauses;
import Datastructures.Clauses.Quantifier;
import Datastructures.Literal;
import Datastructures.Results.Result;
import Datastructures.Results.UnsatClause;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Datastructures.Theory.Model;
import InferenceSteps.InfInputClause;
import InferenceSteps.InferenceStep;
import Management.Monitor.Monitor;
import Utilities.BiConsumerWithUnsatisfiable;

import java.util.ArrayList;
import java.util.function.Consumer;

/** The normalizer turns the inputClauses into Clause and ClauseList datastructures, while removing obvious redundancies. <br>
 * In particular: <br>
 * - superfluous multiplicities of literals<br>
 * - complementary literals<br>
 * - immediately recognizable true or false clauses<br>
 * - multiplicities dividable by the greatest common divisor<br>
 * - extractable true or false literals (e.g. atmost 2 p^3,q,r: p must be false)
 * - tightening the boundaries of interval clauses.<br>
 * <br>
 * The quantifiers of the clauses are optimized as far as possible (e.g. atleast 1 p,... =&gt; or p,...)<br>
 * Pure and singleton predicates are identified and eliminated.<br>
 * True predicates are put into the model.<br>
 * The simplification operations can be accompanied by inference steps. The soundness of these steps can be verified.<br>
 * The result is a list of simplified clauses which can be submitted to the solvers.
 * <br>
 * The Normalizer instance can be reused for other problems by calling the initialize-method.
 */
public class Normalizer {

    // Problem-independent parameters
    // ******************************

    /** if true then inference steps are generated */
    private final boolean trackReasoning;

    /** if true then inference steps are immediately model-based verified */
    private final boolean verify;

    /** null or a monitor for recording infenrence steps  */
    private final Consumer<String> monitor;


    /** just 'Normalizer'*/
    private final static String solverId = "Normalizer";

    // Problem-specific parameters
    // ***************************

    /** the problem's identifier */
    private String problemId;

    /** the number of predicates in the problem */
    public int predicates;

    /** the clauses to be normalized */
    protected InputClauses inputClauses;

    /** the list of clauses together with the literal index */
    public ClauseList clauseList;

    /** the global model.*/
    public Model model;

    /** null or a symboltable */
    public Symboltable symboltable;

    /**the normalizer statistics */
    public StatisticsNormalizer statistics;

    /** the current thread*/
    private Thread myThread;

    /** the list of equivalence classes */
    protected ArrayList<Equivalence> equivalences = new ArrayList<>();

    /** to be called when true literals are derived */
    private final BiConsumerWithUnsatisfiable<Integer,InferenceStep> reportTrueLiteral =
            (literal,step) -> {++statistics.initialTrueLiterals;
                                clauseList.addTrueLiteralTask(literal,step);};

    /** creates a Normalizer which can be used for a sequence of QSAT-problems.
     *
     * @param trackReasoning if true then the inference steps are tracked.
     * @param verify if true then the inference steps are verified.
     * @param monitor for monitoring the inference steps.
     */
    public Normalizer(ClauseList clauseList, boolean trackReasoning, boolean verify, Monitor monitor) {
        this.trackReasoning = trackReasoning;
        this.verify = verify;
        this.monitor = monitor == null ? null : (string -> monitor.println(solverId,string));}

    /** Initializes the normalizer for a new problem.
     * <br>
     * The must be called for each problem separately
     *
     * @param inputClauses the original clauses
     * @param model The model to be initialized.
     */
    public void initialize(InputClauses inputClauses, Model model) {
        this.inputClauses = inputClauses;
        this.problemId    = inputClauses.problemId;
        this.model        = model;
        this.symboltable  = inputClauses.symboltable;
        this.predicates   = inputClauses.predicates;
        statistics        = new StatisticsNormalizer(null);
        clauseList.initialize(problemId,model,symboltable);
        equivalences.clear();
        myThread = Thread.currentThread();}

    /** turn the inputClauses into Clause datastructures, and simplifies them as far as possible.
     * <br>
     * After normalization the QSAT-solvers may work with clauseList.
     *
     * @return the simplified ClauseList
     * @throws Unsatisfiable or Satisfiable
     */
    public ClauseList normalizeClauses() throws Result {
        try {
            for (int[] inputClause : inputClauses.conjunctions) transformConjunction(inputClause);
            for (int[] inputClause : inputClauses.equivalences) transformEquivalence(inputClause);
            for (int[] inputClause : inputClauses.disjunctions) transformAndSimplify(inputClause);
            for (int[] inputClause : inputClauses.atleasts)     transformAndSimplify(inputClause);
            for (int[] inputClause : inputClauses.atmosts)      transformAndSimplify(inputClause);
            for (int[] inputClause : inputClauses.exactlys)     transformAndSimplify(inputClause);
            for (int[] inputClause : inputClauses.intervals)    transformAndSimplify(inputClause);
           applyEquivalencesToModel();
           clauseList.allClausesInserted();}
        catch (Result result) {
            result.solverId = solverId;
            result.problemId = problemId;
            statistics.survivedClauses = clauseList.clauses.size();
            throw result;}
        statistics.survivedClauses = clauseList.clauses.size();
        return clauseList;}


    /** Puts the literals in the AND-clause into the model.
     *
     * @param inputClause the input clause representing the conjunction of literals
     * @throws Unsatisfiable if the conjuncts are contradictory.
     */
    protected void transformConjunction(int[] inputClause) throws Unsatisfiable {
        Quantifier quantifier = Quantifier.getQuantifier(inputClause[1]);
        assert(quantifier == Quantifier.AND);
        int firstLiteralIndex = quantifier.firstLiteralIndex;
        for(int i = firstLiteralIndex; i < inputClause.length; ++i) {
            int literal = inputClause[i];
            switch(model.status(literal)) {
                case 1: continue;
                case -1: throw new UnsatClause(problemId,solverId,inputClause);}
            ++statistics.initialTrueLiterals;
            InferenceStep step = null;
            if(trackReasoning) {
                step = new InfInputClause(inputClause,literal);
                if(verify) step.verify(monitor,symboltable);}
            model.add(myThread,literal,step);}}

    /** Generates for an equivalence in the input clauses a corresponding Equivalence object.
     *
     * @param inputClause the input clause representing an equivalence
     * @throws Unsatisfiable if the equivalence contains a contradiction like p == -p
     */
    protected void transformEquivalence(int[] inputClause) throws Unsatisfiable {
        Quantifier quantifier = Quantifier.getQuantifier(inputClause[1]);
        assert(quantifier == Quantifier.EQUIV);
        int firstLiteralIndex = quantifier.firstLiteralIndex;
        Equivalence equivalence= new Equivalence(inputClause,inputClause[firstLiteralIndex]);
        for(int i = firstLiteralIndex+1; i < inputClause.length; ++i) {
            int literal = inputClause[i];
            equivalence.addLiteral(literal);}
        if(!equivalence.isEmpty()) {
            ++statistics.initialEquivalences;
            equivalences.add(equivalence);}}



    /** The method turns an inputClause into a Clause data structure and simplifies the clause.
     *
     * @param inputClause   an input clause
     * @throws Unsatisfiable if a contradiction is discovered.
     */
    protected void transformAndSimplify(int[] inputClause) throws Unsatisfiable {
        Clause clause = new Clause(inputClause,trackReasoning,(lit -> new Literal(lit,1)),symboltable);
        switch(clause.simplify(trackReasoning,null,reportTrueLiteral,monitor,symboltable)) {
            case -1: throw new UnsatClause(problemId,solverId, inputClause);
            case 1: ++statistics.removedClauses; return;}
        statistics.simplifiedClauses += clause.version;
        if(!applyEquivalences(clause)) clauseList.addClause(clause);}

    /** If the clause contains a literal which is in an equivalence class, it is replaced by the representative literal.
     * <br>
     * The clause is not yet in the ClauseList
     *
     * @param clause    a new clause
     * @return true if the clause has become a true clause.
     * @throws Unsatisfiable if a contradiction is encountered.
     */
    protected boolean applyEquivalences(Clause clause) throws Unsatisfiable {
        for(Equivalence equivalence : equivalences) {
            int representative = equivalence.representative;
            for(int i = 0; i < equivalence.literals.size(); ++i) {
               int equivalentLiteral = equivalence.literals.getInt(i);
               InferenceStep step = equivalence.inferenceSteps.get(i);
               if(clause.findPredicate(equivalentLiteral) != null) {
                    switch(clause.applyEquivalentLiteral(representative,equivalentLiteral, step,trackReasoning,
                    null, reportTrueLiteral,monitor,symboltable)) {
                        case 0: ++statistics.equivalenceReplacements; break;
                        case -1: throw new UnsatClause(problemId,solverId, clause);
                        case +1: ++statistics.removedClauses; return true;}}}}
        return false;}


    /**
     * Applies all equivalences to the model.
     * Equivalent literals must get the same truth value.
     *
     * @throws Unsatisfiable if a contradiction is encountered
     */
    void applyEquivalencesToModel() throws Unsatisfiable {
        for(Equivalence equivalence : equivalences) {
            int representative = equivalence.representative;
            int status1 = model.status(representative);
            if(status1 != 0) {
                for(int i = 0; i < equivalence.literals.size(); ++i) {
                    int equivalentLiteral = equivalence.literals.getInt(i);
                    InferenceStep step = trackReasoning ? equivalence.inferenceSteps.get(i) : null;
                    int status2 = model.status(equivalentLiteral);
                    if(status2 != 0) {
                        if(status1 != status2) throw new UnsatEquivalence(problemId,solverId,equivalence,
                                representative, equivalentLiteral,model.getInferenceStep(representative),
                                model.getInferenceStep(equivalentLiteral));}
                    else model.add(myThread, status1*equivalentLiteral,step);}
                continue;}

            // status(representative) = 0
            // if an equivalent literal has a truth value then all the others must get the same truth value.
            for(int i = 0; i < equivalence.literals.size(); ++i) {
                int literal = equivalence.literals.getInt(i);
                status1 = model.status(literal);
                if(status1 != 0) {
                    InferenceStep step = trackReasoning ? equivalence.inferenceSteps.get(i) : null;
                    model.add(myThread, status1*representative,step);
                    for(int j = 0; j < equivalence.literals.size(); ++j) {
                        int equivalentLiteral = equivalence.literals.getInt(j);
                        if(literal == equivalentLiteral) continue;
                        int status2 = model.status(equivalentLiteral);
                        if(status2 != 0) {
                            if(status1 != status2)
                                if(status1 == 1)
                                    throw new UnsatEquivalence(problemId,solverId,equivalence,
                                        representative, equivalentLiteral,
                                        model.getInferenceStep(representative),
                                        model.getInferenceStep(equivalentLiteral));
                                else throw new UnsatEquivalence(problemId,solverId,equivalence,
                                        equivalentLiteral,representative,
                                        model.getInferenceStep(equivalentLiteral),
                                        model.getInferenceStep(representative));}
                        else model.add(myThread, status1*equivalentLiteral,step);}
                    break;}}}}


}