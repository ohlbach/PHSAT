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
import Management.ProblemSupervisor;
import Utilities.BiConsumerWithUnsatisfiable;

import java.util.ArrayList;
import java.util.function.Consumer;

/** The normalizer takes the inputClauses provided by the supervisor and removes any immediately recognizable redundancies.<br>
 * In particular: <br>
 * - superfluous multiplicities of literals<br>
 * - complementary literals<br>
 * - immediately recognizable true or false clauses<br>
 * - multiplicities dividable by the greatest common divisor<br>
 * - extractable true or false literals (e.g. atmost 2 p^3,q,r: p must be false)<br>
 * <br>
 * The quantifiers of the clauses are optimized as far as possible (e.g. atleast 1 p,... =&gt; or p,...)<br>
 * Pure and singleton predicates are identified and eliminated.<br>
 * True predicates are put into the model.<br>
 * The simplification operations can be accompanied by inference steps. The soundness of these steps can be verified.<br>
 * The result is a list of simplified clauses which can be submitted to the solvers.
 */
public class Normalizer {

    /** the supervisor for the given problem.*/
    ProblemSupervisor problemSupervisor;

    /** the problem's identifier */
    String problemId;

    /** the clauses to be normalized */
    public InputClauses inputClauses;

    public ClauseList clauseList;

    /** just 'Normalizer'*/
    final static String solverId = "Normalizer";

    /** the global model.*/
    public Model model;

    /** the number of predicates in the problem */
    public int predicates;

    /** null or a monitor */
    public Consumer<String> monitor;

    /** true if there is a monitor */
    public boolean monitoring;
    /** the monitor's identifier */
    public String monitorId;

    /** null or a symboltable */
    public Symboltable symboltable;

    /** if true then inference steps are generated */
    public boolean trackReasoning;

    public boolean verify;

    /**the normalizer statistics */
    public StatisticsNormalizer statistics;

    /** the current thread*/
    public Thread myThread;

    protected ArrayList<Equivalence> equivalences;

    private BiConsumerWithUnsatisfiable<Integer,InferenceStep> reportTrueLiteral =
            (literal,step) -> model.add(myThread,literal,step);

    /**
     * Creates a new instance of the Normalizer with parameters which are common for a series of problems.
     * <br>
     * The constructor is usually called once for a sequence of problems.
     *
     * @param trackReasoning specifies whether to track reasoning steps
     * @param verify specifies whether to verify the inference steps
     * @param monitor a {@link Consumer} that will be used to monitor progress or log messages
     */
    public Normalizer(boolean trackReasoning, boolean verify, Consumer<String> monitor ) {
        this.trackReasoning = trackReasoning;
        this.monitor = monitor;
        this.verify = verify;
        clauseList = new ClauseList(trackReasoning,verify,monitor);
    }

    /**
     * Initializes the normalizer for a new problem.
     * <br>
     * The must be called for each problem separately
     *
     * @param model The model to be initialized.
     * @param problemId The ID of the problem.
     * @param symboltable The symbol table.
     */
    public void initialize(String problemId, Model model, Symboltable symboltable) {
        this.problemId = problemId;
        this.model = model;
        this.predicates = model.predicates;
        this.symboltable = symboltable;
        clauseList.initialize(problemId,model,symboltable);
        equivalences = null;
        myThread = Thread.currentThread();
        statistics = new StatisticsNormalizer(null);}



    /**Creates a Normalizer object with the given ProblemSupervisor.
     *
     * @param problemSupervisor the ProblemSupervisor object that controls the solution of the problem.
     */
    public Normalizer(ProblemSupervisor problemSupervisor) {
        this.problemSupervisor = problemSupervisor;
        problemId              = problemSupervisor.problemId;
        inputClauses           = problemSupervisor.inputClauses;
        model                  = problemSupervisor.model;
        statistics             = new StatisticsNormalizer(null);
        monitoring             = problemSupervisor.monitor != null;
        monitorId              = "Normalizer_"+ problemId;
        monitor                = monitoring ? (message -> problemSupervisor.monitor.println(monitorId,message)) : null;
        symboltable            = problemSupervisor.inputClauses.symboltable;
        trackReasoning         = problemSupervisor.globalParameters.trackReasoning;
        predicates             = problemSupervisor.inputClauses.predicates;
        myThread               = Thread.currentThread();
    }

    /**
     * Initializes a Normalizer object with the given parameters (for testing purposes).
     *
     * @param problemId       the problem ID
     * @param monitorId       the monitor ID
     * @param trackReasoning  a boolean indicating whether to track reasoning
     * @param symboltable     the symbol table
     * @param predicates      the number of predicates
     */
    public Normalizer(String problemId, String monitorId, boolean trackReasoning, Symboltable symboltable, int predicates ) {
        this.problemId = problemId;
        statistics = new StatisticsNormalizer(null);
        monitoring = monitorId != null;
        monitor = monitoring ? (System.out::println) : null;
        this.trackReasoning = trackReasoning;
        this.symboltable = symboltable;
        this.predicates = predicates;
        myThread = Thread.currentThread();
        model = new Model(predicates);
    }



    /** turn the inputClauses into Clause datastructures, and simplifies them as far as possible.
     *
     * @return null or Unsatisfiable
     */
    public Result normalizeClauses() {
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
        catch (Result result) {return result;}
        return null;}


    /**
     * Puts the literals in the AND-clause into the model.
     *
     * @param inputClause the input clause representing the conjunction of literals
     * @throws Unsatisfiable if the conjuncts are contradictory.
     */
    void transformConjunction(int[] inputClause) throws Unsatisfiable {
        Quantifier quantifier = Quantifier.getQuantifier(inputClause[1]);
        assert(quantifier == Quantifier.AND);
        int firstLiteralIndex = quantifier.firstLiteralIndex;
        for(int i = firstLiteralIndex; i < inputClause.length; ++i) {
            int literal = inputClause[i];
            InferenceStep step = null;
            if(trackReasoning) {
                step = new InfInputClause(inputClause,literal);
                if(verify) step.verify(monitor,symboltable);}
            model.add(myThread,literal,step);}}

    /** Generates for an equivalence in the input clauses a corresponding Equivalence object.
     * @param inputClause the input clause representing an equivalence
     * @throws Unsatisfiable if the equivalence contains a contradiction like p == -p
     */
    void transformEquivalence(int[] inputClause) throws Unsatisfiable {
        if(equivalences == null) equivalences = new ArrayList<>();
        Quantifier quantifier = Quantifier.getQuantifier(inputClause[1]);
        assert(quantifier == Quantifier.EQUIV);
        int firstLiteralIndex = quantifier.firstLiteralIndex;
        Equivalence equivalence= new Equivalence(inputClause,inputClause[firstLiteralIndex]);
        for(int i = firstLiteralIndex+1; i < inputClause.length; ++i) {
            int literal = inputClause[i];
            equivalence.addLiteral(literal);}
        if(!equivalence.literals.isEmpty()) equivalences.add(equivalence);}



    /** The method turns an inputClause into a Clause data structure and simplifies the clause.
     *
     * @param inputClause   an input clause
     * @throws Unsatisfiable if a contradiction is discovered.
     */
    void transformAndSimplify(int[] inputClause) throws Unsatisfiable {
        Clause clause = new Clause(inputClause,trackReasoning,(lit -> new Literal(lit,1)),symboltable);
        switch(clause.simplify(trackReasoning,null, clauseList::addTrueLiteralTask, monitor,symboltable)) {
            case -1: throw new UnsatClause(problemId,solverId, clause.inputClause);
            case 1: return;}
        statistics.simplifiedClauses += clause.version;
        if(!applyEquivalences(clause)) clauseList.addClause(clause);}

    /** If the clause contains a literal which is in an equivalence class, it is replaced by the representative literal.
     *
     * @param clause    a new clause
     * @return true if the clause has become a true clause.
     * @throws Unsatisfiable    if a contradiction is encountered.
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
                    case -1: clauseList.removeClause(clause);
                        throw new UnsatClause(problemId,solverId, clause);
                    case 1:
                        clauseList.removeClause(clause);
                        return true;}}}}
            return false;}


    /**
     * Applies all equivalences to the model.
     *
     * @throws Unsatisfiable if a contradiction is encountered
     */
    void applyEquivalencesToModel() throws Unsatisfiable {
        for(Equivalence equivalence : equivalences) {
            int representative = equivalence.representative;
            int status1 = model.status(representative);
            if(status1 != 0) {
                for(int i = 0; i < equivalence.literals.size(); ++i) {
                    int equivalentLiteral = equivalence.literals.get(i);
                    InferenceStep step = trackReasoning ? equivalence.inferenceSteps.get(i) : null;
                    int status2 = model.status(equivalentLiteral);
                    if(status1 != status2) throw new UnsatEquivalence(problemId,solverId,
                            representative, equivalentLiteral,step);
                    if(status1 == status2) continue;
                    model.add(myThread, status1*equivalentLiteral,step);}
                continue;}

            for(int i = 0; i < equivalence.literals.size(); ++i) {
                int literal = equivalence.literals.get(i);
                status1 = model.status(literal);
                if(status1 != 0) {
                    InferenceStep step = trackReasoning ? equivalence.inferenceSteps.get(i) : null;
                    model.add(myThread, status1*representative,step);
                    for(int j = 0; j < equivalence.literals.size(); ++j) {
                        int equivalentLiteral = equivalence.literals.get(j);
                        if(literal == equivalentLiteral) continue;
                        int status2 = model.status(equivalentLiteral);
                        if(status1 != status2) throw new UnsatEquivalence(problemId,solverId,
                                representative, equivalentLiteral,step);
                        if(status1 == status2) continue;
                        model.add(myThread, status1*equivalentLiteral,step);}
                    break;}}}}







}