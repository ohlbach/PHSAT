package Solvers.Walker;

import Datastructures.Clauses.Quantifier;
import Datastructures.Results.Aborted;
import Datastructures.Results.Result;
import Datastructures.Results.Satisfiable;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Statistics.Statistic;
import Datastructures.Symboltable;
import Datastructures.Theory.EquivalenceClasses.EquivalenceClasses;
import Datastructures.Theory.Model;
import InferenceSteps.InferenceStep;
import Management.ErrorReporter;
import Management.ProblemSupervisor;
import Solvers.Simplifier.UnsatClause;
import Solvers.Solver;
import Utilities.Utilities;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.*;

/** This is a "random walker" for finding a model for QuSat-clauses.
 * <br>
 * All clauses are put into interval-normalform.
 * An initial candidate model is created by comparing the positive and negative occurrences of literals.<br>
 * If the numbers are the same then a random number generator is used for constructing the initial model.<br>
 * The truth values are then heuristically flipped until there are no false clauses anymore.
 */
public class Walker extends Solver {

    // Control Parameters
    // ******************

    /** maximum number of allowed flips. */
    int maxFlips;

    /** for creating the random number generator */
    final int seed;

    /** the default value for the jump frequency. */
    private static final int jumpFrequencyDefault = 10;

    /** after jumpFrequency many flips, a random jump is inserted.*/
    int jumpFrequency;

    // Active Data
    // ***********

    /** collects all clauses. */
    ArrayList<Clause> clauses = new ArrayList<>();

    /** the list of false clauses. */
    Clauses falseClauseList = new Clauses();

    /** the number of false clauses. */
    int falseClauses = 0;

    /** an index for all literal occurrences in the clauses. */
    Literals literals;

    /** the current list of predicates with a positive score. */
    Predicates predicatesWithPositiveScore;

    /** maps all predicates to a truth value. */
    boolean[] localModel;

    /** a score of +x for predicate p means that by flipping(p) x more clauses become true. */
    float[] flipScores;

    /** a pointer to the equivalenceClasses solver. */
    EquivalenceClasses equivalenceClasses;

    /** collects the equivalence classes which are send by the observer */
    private final IntArrayList equivalentLiterals = new IntArrayList(5);

    /** becomes true when a new equivalence has been arrived. */
    boolean equivalenceInterrupt = false;

    /** becomes true when a new true literal has been arrived. */
    boolean trueLiteralInterrupt = false;

    /** collects statistical information. */
    Statistics statistics;

    /** random number generator for flip jumps. */
    Random random;

    /** a tiny flip score for globally true predicates. They should never be flipped again.*/
    private static final float trueLiteralScore = (float)(Integer.MIN_VALUE/2);

    /** flip scores of globally true literals should not be larger than this. */
    private static float trueLiteralScoreLimit = trueLiteralScore/(float)10.;


    /** the current thread. */
    Thread myThread;


    /** provides a help text about the parameters of the solver.
     * @return a help text.
     * */
    public static String help() {
        return "Solver Random Walker: modifies a candidate model until a real model is found.\n"+
                "parameters:\n" +
                "  seeds:  for the random number generator      (default: 0)\n" +
                "  flips:  for restricting the number of flips  (default: Max_Integer).\n" +
                "  jumps:  frequency of random jumps            (default: 10)\n";}

    /** contains the allowed keys in the specification. */
    private static final HashSet<String> keys = new HashSet<>();

    static { // these are the allowed keys in the specification.
        Collections.addAll(keys, "seeds", "flips", "jumps", "solver");}

    /** parses a HashMap with key-value pairs and creates corresponding Walkers.
     *
     * @param parameters  the parameters with the keys "seed", "flips", "jumps".
     * @param solvers     for adding the newly created Walkers.
     * @param errors      for error messages.
     * @param warnings    for warnings (not used here).
     */
    public static void makeSolvers(HashMap<String,String> parameters, ArrayList<Solver> solvers,
                                                StringBuilder errors, StringBuilder warnings){
        for(String key : parameters.keySet()) {
            if(!keys.contains(key)) {
                ErrorReporter.reportWarning("Walker: unknown key in parameters: " + key + "\n" +
                                "        allowed keys: seed, flips, jumps.\n");}}
        String seeds = parameters.get("seed");
        if(seeds == null) seeds = "0";
        String flips = parameters.get("flips");
        if(flips == null) flips = Integer.toString(Integer.MAX_VALUE);
        String jumps = parameters.get("jumps");
        if(jumps == null) jumps = Integer.toString(jumpFrequencyDefault);
        String place = "Walker: ";
        ArrayList seedA = Utilities.parseIntRange(place+"seed: ",seeds,errors);
        ArrayList flipA = Utilities.parseIntRange(place+"flips: ",flips,errors);
        ArrayList jumpA = Utilities.parseIntRange(place+"jumps: ",jumps,errors);
        if(errors.length() > 0) ErrorReporter.reportErrorAndStop("Check walker parameters!");
        ArrayList<ArrayList> pars = Utilities.crossProduct(seedA,flipA,jumpA);
        int solverNumber = 1;
        for(ArrayList<Object> p : pars ) {
            int seedV  = (int)p.get(0);
            int flipsV = (int)p.get(1);
            int jumpsV = (int)p.get(2);
            if(seedV < 0)   errors.append("Walker: seed < 0: ").append(seedV).append("\n");
            if(flipsV <= 0) errors.append("Walker: flips <= 0: ").append(flipsV).append("\n");
            if(jumpsV <= 0) errors.append("Walker: jumps <= 0: ").append(jumpsV).append("\n");
            HashMap<String,Object> solverParameters = new HashMap<>();
            solverParameters.put("seed",seedV);
            solverParameters.put("flips",flipsV);
            solverParameters.put("jumps",jumpsV);
            solverParameters.put("name",(pars.size() == 1 ? "Walker" : "Walker_"+solverNumber));
            solvers.add(new Walker(solverNumber++, solverParameters, seedV,flipsV,jumpsV));}}

    /** constructs a new Walker.
     *
     * @param solverNumber  for enumerating the walkers.
     * @param solverParameters which specified the walker (for documentation only).
     * @param seed          for starting the random number generator.
     * @param maxFlips      the maximum number of allowed flips.
     * @param jumpFrequency the frequency of random flips.
     */
    public Walker(int solverNumber, HashMap<String,Object> solverParameters, int seed, int maxFlips, int jumpFrequency) {
        super(solverNumber,solverParameters);
        this.seed = seed;
        this.maxFlips = maxFlips;
        this.jumpFrequency = jumpFrequency;
        monitorId = "Walker_"+solverNumber;}

    /** adds observers to the model and the equivalenceClasses.
     */
    @Override
    public void installCommunication(ProblemSupervisor problemSupervisor) {
        problemSupervisor.model.addObserver(this::addGloballyTrueLiteral);
        problemSupervisor.equivalenceClasses.addObserver((this::addEquivalence));}

    /** starts the search for a model.
     *
     * @param problemSupervisor the corresponding supervisor.
     * @return the result of the search.
     */
    @Override
    public Result solveProblem(ProblemSupervisor problemSupervisor) {
        initialize(problemSupervisor);
        long startTime     = System.nanoTime();
        myThread           = Thread.currentThread();
        localModel         = new boolean[predicates+1];
        random             = new Random(seed);
        statistics         = new Statistics(combinedId);
        clauses            = new ArrayList<>(inputClauses.nextId);
        literals           = new Literals(predicates);
        flipScores         = new float[predicates+1];
        predicatesWithPositiveScore = new Predicates(predicates);
        globalParameters.logstream.println(solverId + " for problem " + problemId + " started");

        try{
            readInputClauses();
            initializeModel();
            for(Clause clause : clauses) {
                initializeLocalTruthForClause(clause);
                initializeFlipScores(clause);}
            initializePredicatesWithPositiveScores();
            walk();}
        catch(Result result) {
            statistics.elapsedTime = System.nanoTime() - startTime;
            return result;}
        statistics.elapsedTime = System.nanoTime() - startTime;
        return null;
    }

    /** reads all input clauses except conjunctions and equivalences, and turns them into interval-normalform.
     * The original quantifiers, however, are kept.<br>
     * Complementary literals are removed.<br>
     * Derivable true or false literals are inserted into the global model.<br>
     *
     * @throws Result if a contradiction or the empty clause is derived.
     */
    void readInputClauses() throws Result{
        try{
            for(int[] inputClause    : inputClauses.disjunctions) insertClause(inputClause);
            for(int[] atleastClause  : inputClauses.atleasts)     insertClause(atleastClause);
            for(int[] atmostClause   : inputClauses.atmosts)      insertClause(atmostClause);
            for(int[] exactlyClause  : inputClauses.exactlys)     insertClause(exactlyClause);
            for(int[] intervalClause : inputClauses.intervals)    insertClause(intervalClause);
            if(clauses.isEmpty()) throw new Satisfiable(problemId,solverId, model);}
        catch(Result result) {
            result.solverId  = solverId;
            result.problemId = problemId;
            result.statistic = statistics;
            throw result;}
    }

    /** turns the inputClause into literals and clauses of the internal datastructures.
     * All clauses are stored with interval limits.<br>
     * Complementary literals are removed.<br>
     * Derivable true or false literals are inserted into the global model.<br>
     * Multiplicities are reduced.
     *
     * @param inputClause    an input clause.
     * @return null or the new clause.
     * @throws Unsatisfiable if the clause is false or the global model discovers a contradiction.
     */
    Clause insertClause(int[] inputClause) throws Unsatisfiable {
        Clause clause = new Clause(inputClause);
        if(clause.removeComplementaryLiterals(problemId,solverId,null)) return null;
        if(clause.quantifier == Quantifier.AND) {
            for(Literal literalObject : clause.literals) {
                model.add(literalObject.literal,clause.inferenceStep);}
            return null;}
        for(int i = 0; i < clause.literals.size(); ++i) {
            Literal literalObject = clause.literals.get(i);
            if(literalObject.multiplicity > clause.max) {
                model.add(-literalObject.literal,null);
                clause.expandedSize -= literalObject.multiplicity;
                clause.literals.remove(i--);
                clause.hasMultiplicities = clause.expandedSize > clause.literals.size();}}
        if(clause.isFalse()) throw new UnsatClause(problemId,solverId,inputClause);
        if(clause.isTrue()) return null;
        if(clause.hasMultiplicities) clause.reduceMultiplicities();
        for(Literal literalObject : clause.literals) {literals.addLiteral(literalObject);}
        clauses.add(clause);
        return clause;}



    /** initializes the local model.
     * a predicate is set to true if there are more positive literal occurrences than negative literal occurrences.<br>
     * If there are equally many positive and negative occurrences, the random number generator decides about the truth value.<br>
     * The global model is not yet taken into account.
     */
    void initializeModel() {
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            int posSize = literals.size(predicate);
            int negSize = literals.size(-predicate);
            if(posSize == negSize) localModel[predicate] = random.nextBoolean();
            else localModel[predicate] = (posSize >= negSize);}}


    /** computes the truth value of a clause in the local model.
     * The truth value and the number of true literals is stored in the clause.<br>
     * False clauses are collected in falseClauseList.
     *
     * @param clause a clause.
     * @return true if the clause is true in the local (and global) model.
     */
    boolean initializeLocalTruthForClause(Clause clause) {
        int trueLiterals = 0;
        for(Literal literalObject : clause.literals) {
            int literal = literalObject.literal;
            if(literal > 0)  {
                if(localModel[literal])   trueLiterals += literalObject.multiplicity;}
            else if(!localModel[-literal]) trueLiterals += literalObject.multiplicity;}

        clause.trueLiterals = trueLiterals;
        boolean isTrue = clause.min <= trueLiterals && trueLiterals <= clause.max;
        clause.isLocallyTrue = isTrue;
        if(!isTrue) {falseClauseList.addToBack(clause); ++falseClauses;}
        return isTrue;}


    /** computes the initial flip scores for the literals in the clause.
     *  A positive flip score for a literal indicates that flipping the truth value for the literal makes a false clause true,
     *  or brings a false clause closer to truth. <br>
     *  A negative flip score indicates that a true clause can become false. <br>
     *  Fractional flip scores indicate that more than one predicate need to be flipped in order to make a clause true or false.
     *
     * @param clause the clause to be investigated.
     */
    void initializeFlipScores(Clause clause) {
        int trueLiterals = clause.trueLiterals;
        int min = clause.min; int max = clause.max;
        if(clause.isLocallyTrue) {
            if(min == max) { // all flips make the clause false.
                for(Literal literalObject : clause.literals) {
                    literalObject.flipScorePart = -1;
                    flipScores[Math.abs(literalObject.literal)] -= 1;}
                return;}
            for(Literal literalObject : clause.literals) {
                int literal = literalObject.literal;
                int newTrueLiterals = isLocallyTrue(literal) ? trueLiterals - literalObject.multiplicity : trueLiterals + literalObject.multiplicity;
                if(newTrueLiterals < min || newTrueLiterals > max) {
                    literalObject.flipScorePart = -1;
                    flipScores[Math.abs(literalObject.literal)] -= 1;}}
            return;}

        if(trueLiterals < clause.min) { // false literals should become true and true literals should not become false.
            for(Literal literalObject : clause.literals) {
                int literal = literalObject.literal;
                float score = 0;
                if(isLocallyTrue(literal)) {
                    score = (float)-1. / (float)(min - (trueLiterals - literalObject.multiplicity));}
                else {
                    int newTrueLiterals = trueLiterals + literalObject.multiplicity;
                    if(newTrueLiterals <= max) // otherwise don't flip.
                        score = (float)1. / (float)(min - newTrueLiterals + 1);}
                literalObject.flipScorePart = score;
                flipScores[Math.abs(literalObject.literal)] += score;}
            return;}

        if(trueLiterals > clause.max) { // true literals should become false and false literals should not become true.
            for(Literal literalObject : clause.literals) {
                int literal = literalObject.literal;
                float score = 0;
                if(isLocallyTrue(literal)) {
                    int newTrueLiterals = trueLiterals - literalObject.multiplicity;
                    if(newTrueLiterals >= min) // otherwise don't flip.
                        score = (float)1. / (float)(newTrueLiterals - max + 1);}
                else {
                    score = (float)-1. / (float)((trueLiterals + literalObject.multiplicity) -max);}
            literalObject.flipScorePart = score;
            flipScores[Math.abs(literalObject.literal)] += score;}}}

    /** all predicates with positive score are collected in predicatesWithPositiveScore.
     * The predicates are not ordered according to the flip score.
     */
    void initializePredicatesWithPositiveScores() {
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            if(flipScores[predicate] > 0) predicatesWithPositiveScore.addToBack(predicate);}}


    /** controls the search for a model.
     * The truth values for predicates are flipped until there is no false clause anymore, or the search is interrupted.
     * The loop stops if either maxFlips is reached, or there is an external interrupt from another thread.
     * <br>
     * There may be two internal interrupts:<br>
     * - a new globally true literal is announced: <br>
     *   Its local truth value is adapted and the flip score is set to a large negative value. <br>
     * - a new equivalence is announced: <br>
     *   The literal is replaced by its representative and the two local truth values are synchronized.
     *

     * @throws Result the result of the search
     */
    void walk() throws Result {
        while(statistics.flips < maxFlips) {
            if(Thread.interrupted()) {
                if(trueLiteralInterrupt) integrateGloballyTrueLiterals();
                else {if(equivalenceInterrupt) integrateEquivalences();
                    else {
                        globalParameters.logstream.println("Walker " + combinedId + " interrupted after " + statistics.flips + " flips.\n");
                        break;}}}
            int predicate = selectFlipPredicate();
            flipPredicate(predicate);
            if(falseClauses == 0) {throw localToGlobalModel();}}
        throw new Aborted(problemId,solverId,"Walker aborted after " + statistics.flips + " flips");}


    /** selects a predicate to be flipped.
     * The priorities are: <br>
     * 1. a predicate with positive flipScore (any such predicate is good enough).<br>
     * 2. a predicate in a false clause.<br>
     *    - every jump-frequency time a randomly chosen false clause is selected.<br>
     *    - otherwise the first false clause in the list is chosen.
     *
     * @return a predicate to be flipped next.
     */
    int selectFlipPredicate() {
        Predicate predicateObject = predicatesWithPositiveScore.firstPredicate;
        if(predicateObject != null) return predicateObject.predicate;
        if(statistics.flips % jumpFrequency == 0) {
            int n = random.nextInt(falseClauseList.size);
            Clause clause = falseClauseList.getClause(n);
            return selectPredicateInFalseClause(clause);}
        return selectPredicateInFalseClause(falseClauseList.firstClause);}

    /** selects a predicate in a false clause with score %lt;= 0.
     * If there are not enough true literals then the first false clause is chosen to be flipped.<br>
     * If there are too many true literals then the first true clause is chosen to be flipped.<br>
     *
     * @param clause a false clause.
     * @return the predicate to be flipped.
     */
    int selectPredicateInFalseClause(Clause clause) {
        if(clause.trueLiterals < clause.min) { // not enough true literals. A false literal must be flipped.
            for(Literal literalObject : clause.literals) {
                int literal = literalObject.literal;
                if(flipScores[Math.abs(literal)] < trueLiteralScoreLimit) continue;
                if(!isLocallyTrue(literal)) return Math.abs(literal);}}
        // too many true literals. A true literal must be flipped.
        for(Literal literalObject : clause.literals) {
            int literal = literalObject.literal;
            if(flipScores[Math.abs(literal)] < trueLiteralScoreLimit) continue;
            if(isLocallyTrue(literal)) return Math.abs(literal);}
        assert(false);
        return 0;}





    /** flips the truth value of the predicate and updates the flipQueue and the falseClauses list
     *
     * @param predicate to be flipped
     */
    void flipPredicate(int predicate) {
        assert(predicate > 0);
        if(monitoring) monitor.println(monitorId,
                "Flipping predicate " + Symboltable.toString(predicate,symboltable) +
                        " with flip score " + flipScores[predicate] +
                        " for " + falseClauses + " false clauses.",
                "True Predicates: " + toString("model"),
                "False clauses:   \n"+ toString("falseClauses",symboltable));
        ++statistics.flips;
        localModel[predicate] = !localModel[predicate];
        updateFlipScores(predicate);}



    /** updates the flip scores of the literals in the clauses containing the flipped literal.
     * A literal's score is changed if, by flipping it, it makes a true clause false or vice versa.
     *
     * @param literal the literal with flipped truth value.
     */
     void updateFlipScores(int literal) {
        for(int sign = 1; sign >= -1; sign -= 2) {
            literal *= sign;
            Literal literalObject = literals.getFirstLiteralObject(literal);
            while(literalObject != null) {
                updateFlipScores(literalObject);
                literalObject = literalObject.nextLiteral;}}}

    /** updates the clause's flip score, the falseClauses list and the predicatesWithPositiveScore list.
     *  The previous score contribution is subtracted from flipScores and the new scores is added by calling initializeFlipScores.
     *
     * @param flippedLiteralObject the literal whose local truth value has already been flipped.
     */
     void updateFlipScores(Literal flippedLiteralObject) {
        Clause clause = flippedLiteralObject.clause;
        boolean wasTrue = clause.isLocallyTrue;
        int trueLiterals = 0;
        for(Literal literalObject : clause.literals) {
            int literal = literalObject.literal;
            flipScores[Math.abs(literal)] -= literalObject.flipScorePart;
            literalObject.flipScorePart = 0;
            if(isLocallyTrue(literal)) trueLiterals += literalObject.multiplicity;}
        clause.trueLiterals = trueLiterals;
        clause.isLocallyTrue = clause.min <= trueLiterals && trueLiterals <= clause.max;
        initializeFlipScores(clause);
        if(wasTrue) {
             if(!clause.isLocallyTrue) {falseClauseList.addToBack(clause);++falseClauses;}}
        else {if(clause.isLocallyTrue) {falseClauseList.remove(clause);--falseClauses;}}

        for(Literal literalObject : clause.literals) {
            updatePredicatesWithPositiveScore(Math.abs(literalObject.literal));}}


    /** updates the list of predicates with positive flip score.
     *
     * @param predicate a predicate.
     */
     void updatePredicatesWithPositiveScore(int predicate) {
        int pred = predicatesWithPositiveScore.predicates[predicate].predicate;
        if(flipScores[predicate] > 0) {
              if(pred == 0) predicatesWithPositiveScore.addToBack(predicate);}
        else {if(pred != 0) predicatesWithPositiveScore.remove(predicate);}}



    /** turns the local model into a new model and returns Satisfiable as result
     *
     * @return  Satisfiable with the transferred local model.
     */
    Satisfiable localToGlobalModel() {
        Model model = new Model(predicates);
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            model.addImmediately(localModel[predicate] ? predicate : -predicate);}
        return new Satisfiable(problemId,solverId,model);}



    protected boolean isLocallyTrue(int literal) {
        return (literal > 0) ? localModel[literal] : !localModel[-literal];}



    /** collects globally true literals which are inserted by other solvers  into the global model */
    private final IntArrayList globallyTrueLiterals = new IntArrayList();

    /** integrates globally true literals.
     * - their scores are minimized <br>
     * - If the literal's local model is different to the global model, the literal is flipped.
     */
    void integrateGloballyTrueLiterals() {
        for(int literal : getGloballyTrueLiterals()) {
            if(monitoring) monitor.println(monitorId,"Integrating true literal " + Symboltable.toString(literal,symboltable));
            ++statistics.importedTrueLiterals;
            int predicate = Math.abs(literal);
            flipScores[predicate] = trueLiteralScore;
            predicatesWithPositiveScore.remove(predicate);
            if((literal > 0 && localModel[literal] == model.isTrue(literal)) ||
                    (literal < 0 && localModel[-literal] == model.isFalse(-literal))) continue;
            flipPredicate(predicate);}}


    /** is called by the observer to add a globally true literal.
     *
     * @param literal a globally true literal.
     * @param step null or an inference step (is ignored).
     */
    public synchronized void addGloballyTrueLiteral(int literal, InferenceStep step) {
        globallyTrueLiterals.add(literal);
        trueLiteralInterrupt = true;
        myThread.interrupt();
    }

    /** copies the imported globally true literals.
     *  globallyTrueLiterals is cleared.
     *
     * @return a copy of the globally true literals.
     */
    synchronized IntArrayList getGloballyTrueLiterals() {
        if(globallyTrueLiterals.isEmpty()) return null;
        IntArrayList literals = globallyTrueLiterals.clone();
        globallyTrueLiterals.clear();
        trueLiteralInterrupt = false;
        return literals;}


    /** adds a new equivalence between literals (called by the observer),
     *
     * @param representative the representative of the equivalence.
     * @param literal the corresponding literal
     * @param step is ignored.
     */
    public synchronized void addEquivalence(int representative, int literal, InferenceStep step) {
        equivalentLiterals.add(representative); equivalentLiterals.add(literal);
        equivalenceInterrupt = true;
        myThread.interrupt();}

    /** copies the imported equivalences.
     *  equivalentLiterals is cleared.
     *
     * @return a copy of the imported equivalences.
     */
    private synchronized IntArrayList getEquivalences() {
        if(equivalentLiterals.isEmpty()) return null;
        IntArrayList literals = equivalentLiterals.clone();
        equivalentLiterals.clear();
        equivalenceInterrupt = false;
        return literals;}


    /** replaces for each equivalence of literals the literal by its representative.
     *
     * @throws Unsatisfiable if a contradiction is found.
     */
    void integrateEquivalences() throws Unsatisfiable{
        IntArrayList equivalences = getEquivalences();
        if(equivalences == null) return;
        for(int i = 0; i < equivalences.size(); i += 2) {
            int representative = equivalences.getInt(i);
            int literal = equivalences.getInt(i+1);
            if(monitoring) monitor.println(monitorId,"Integrating equivalence " +
                    Symboltable.toString(literal,symboltable) + " -> " + Symboltable.toString(representative,symboltable));
            ++statistics.importedEquivalentLiterals;
            if(localModel[literal] != localModel[representative]) flipPredicate(Math.abs(literal));
            for(int sign = 1; sign >= -1; sign -= 2) {
                replaceEquivalentLiterals(sign*representative,sign*literal);}}}

    /** replaces the literal by its representative in an equivalence class.
     *
     * @param representative the representative in an equivalence class.
     * @param literal        the literal
     * @throws Unsatisfiable if a contradiction is found.
     */
    void replaceEquivalentLiterals(int representative, int literal) throws Unsatisfiable {
        int predicateLiteral = Math.abs(literal);
        Literal literalObject = literals.getFirstLiteralObject(literal);
        while(literalObject != null) {
            Clause clause = literalObject.clause;
            if(clause == null) {literalObject = literalObject.nextLiteral; continue;}
            Literal representativeObject = clause.findLiteral(-representative);
            if(representativeObject != null) {  // example: atmost 3 -p^2,q,r,s  p == q -> atmost 3 -p^2,p,r,s -> atmost 2 -p,r,s
                if(clause.quantifier == Quantifier.OR) {
                    removeClause(clause); // tautology
                    literalObject = literalObject.nextLiteral;
                    continue;}
                else {
                    for(Literal litObject : clause.literals) {
                        int pred = Math.abs(litObject.literal);
                        flipScores[pred] -= litObject.flipScorePart;
                        updatePredicatesWithPositiveScore(pred);
                        litObject.flipScorePart = 0;}
                    literals.removeLiteral(literalObject);
                    Literal newLiteral = literalObject.clone(representative);
                    clause.replaceLiteral(literalObject,newLiteral);
                    literals.addLiteral(newLiteral);

                    if(clause.removeComplementaryLiterals(problemId,solverId,((Literal litObject) -> literals.removeLiteral(litObject))))
                        removeClause(clause);
                    else {
                        initializeFlipScores(clause);
                        int trueLiterals = 0;
                        for(Literal litObject : clause.literals) {
                            if(isLocallyTrue(litObject.literal)) trueLiterals += litObject.multiplicity;
                            updatePredicatesWithPositiveScore(Math.abs(litObject.literal));}
                        clause.trueLiterals = trueLiterals;
                        clause.isLocallyTrue = clause.min <= trueLiterals && trueLiterals <= clause.max;}

                    literalObject = literalObject.nextLiteral;
                    continue;}}
            representativeObject = clause.findLiteral(representative);
            if(representativeObject == null) {  // just replace literal by the representative.
                Literal newLiteral = literalObject.clone(representative);
                literals.removeLiteral(literalObject);
                literals.addLiteral(newLiteral);
                clause.replaceLiteral(literalObject,newLiteral);
                int predicateRepresentative = Math.abs(representative);
                flipScores[predicateRepresentative] += literalObject.flipScorePart;
                updatePredicatesWithPositiveScore(predicateRepresentative);}
            else {  // p,q,r and p = q -> p^2,r
                for(Literal litObject : clause.literals) {
                    flipScores[Math.abs(litObject.literal)] -= litObject.flipScorePart;
                    litObject.flipScorePart = 0;}
                representativeObject.multiplicity = representativeObject.multiplicity+literalObject.multiplicity;
                clause.literals.remove(literalObject);
                literals.removeLiteral(literalObject);
                clause.reduceMultiplicities();
                initializeFlipScores(clause);
                for(Literal litObject : clause.literals) {
                    updatePredicatesWithPositiveScore(Math.abs(litObject.literal));}}

            literalObject = literalObject.nextLiteral;}
        flipScores[predicateLiteral] = trueLiteralScore;
        predicatesWithPositiveScore.remove(predicateLiteral);
    }

    /** removes the clause from the clauses, the falseClauses list, the literals index and if necessary from predicatesWithPositiveScore.
     * The flip score is updated.<br>
     * This may happen after equivalence replacement generates a tautology.
     *
     * @param clause a clause to be removed.
     */
    protected void removeClause(Clause clause) {
        clauses.remove(clause);
        if(!clause.isLocallyTrue) {falseClauseList.remove(clause); --falseClauses;}
        for(Literal literalObject : clause.literals) {
            literals.removeLiteral(literalObject);
            int predicate = Math.abs(literalObject.literal);
            float score = flipScores[predicate];
            flipScores[predicate] -= literalObject.flipScorePart;
            if(score > 0 && flipScores[predicate] <= 0) predicatesWithPositiveScore.remove(predicate);}}

    /** returns the statistics
     *
     * @return the statistics
     */
    @Override
    public Statistic getStatistics() {
        return statistics;}

    /** lists the true predicates in the local model a comma separated string.
     *
     * @param symboltable null or a symboltable
     * @return the true predicates in the local model a comma separated string.
     */
    public String localModelToString(Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            if (localModel[predicate]) st.append(Symboltable.toString(predicate,symboltable)).append(",");}
        return st.toString();}

    /** collects the predicates with flipScore != 0 in a string.
     *
     * @param symboltable null or a symboltable.
     * @return the predicates with flipScore != 0 in a string.
     */
    public String flipScoresToString(Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            if(flipScores[predicate] != 0) st.append(Symboltable.toString(predicate,symboltable)).
                    append(":").append(flipScores[predicate]).append(",");}
        return st.toString();}

    /** turns different aspects into a string.
     *
     * @param version      clauses,falseClauses,literals,predicates,flipscores,model,statistic.
     * @return             different aspects as a string.
     */
    public String toString(String version) {
        return toString(version,null);}

    /** turns different aspects into a string.
     *
     * @param version      clauses,falseClauses,literals,predicates,flipscores,model,statistic.
     * @param symboltable  null or a symboltable.
     * @return             different aspects as a string.
     */
    public String toString(String version, Symboltable symboltable) {
        switch(version) {
            case "clauses":      return clausesToString(symboltable);
            case "falseClauses": return falseClauseList.toString(symboltable);
            case "literals":     return literals.toString(symboltable);
            case "predicates":   return predicatesWithPositiveScore.toString(symboltable);
            case "flipscores":   return flipScoresToString(symboltable);
            case "model":        return localModelToString(symboltable);
            case "statistic":    return statistics.toString();
        }
        return "Versions: clauses,falseClauses,literals,predicates,flipscores,model,statistic";
    }

    /** turns parameters into a string.
     *
     * @return the parameters as a string.
     */
    public String toString() {
        StringBuilder st = new StringBuilder();
        st.append("Random Walker ").append(solverId).append( " on Problem ").append(problemId).append("\n");
        st.append("Parameters:\n");
        st.append("  seed:  ").append(seed).append("\n");
        st.append("  flips: ").append(statistics.flips).append(" of ").append(maxFlips).append("\n");
        st.append("  jumps: ").append(jumpFrequency).append("\n");
        return st.toString();
    }

    /** collects the clauses as a string.
     *
     * @param symboltable null or a symboltable
     * @return the clauses as a string.
     */
    public String clausesToString(Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        for(Clause clause : clauses) {
            st.append(clause.toString(symboltable,0)).append("\n");}
        return st.toString();}
}