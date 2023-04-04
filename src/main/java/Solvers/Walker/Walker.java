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

public class Walker extends Solver {

    // Control Parameters
    // ******************

    /** maximum number of allowed flips. */
    public  int maxFlips;

    /** for creating the random number generator */
    private final int seed;

    /** the default value for the jump frequency. */
    private static final int jumpFrequencyDefault = 10;

    /** after jumpFrequency many flips, a random jump is inserted.*/
    public int jumpFrequency;

    // Active Data
    // ***********

    /** collects all clauses. */
    protected ArrayList<Clause> clauses = new ArrayList<>();

    /** the list of false clauses. */
    protected Clauses falseClauseList = new Clauses();

    /** the number of false clauses. */
    protected int falseClauses = 0;

    /** an index for all literal occurrences in the clauses. */
    protected Literals literals;

    /** the current list of predicates with a positive score. */
    protected Predicates predicatesWithPositiveScore;

    /** maps all predicates to a truth value. */
    protected boolean[] localModel;

    /** a score of +x for predicate p means that by flipping(p) x more clauses become true. */
    protected float[] flipScores;

    /** a pointer to the equivalenceClasses solver. */
    private EquivalenceClasses equivalenceClasses;

    /** collects the equivalence classes which are send by the observer */
    private final IntArrayList equivalentLiterals = new IntArrayList(5);

    /** becomes true when a new equivalence has been arrived. */
    boolean equivalenceInterrupt = false;

    /** becomes true when a new true literal has been arrived. */
    boolean trueLiteralInterrupt = false;

    /** collects statistical information. */
    public Statistics statistics;

    /** random number generator for flip jumps. */
    protected  Random random;

    /** a tiny flip score for globally true predicates. They should never be flipped again.*/
    protected static float trueLiteralScore = (float)(Integer.MIN_VALUE/2);

    /** the current thread. */
    Thread myThread;


    /** provides a help text about the parameters of the solver. */
    public static String help() {
        return "Random Walker: parameters:\n" +
                "seeds:  for the random number generator      (default: 0)\n" +
                "flips:  for restricting the number of flips  (default: Max_Integer).\n" +
                "jumps:  frequency of random jumps            (default: 10)\n";}

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
     * @param parameters    which specified the walker (for documentation only).
     * @param seed          for starting the random number generator.
     * @param maxFlips      the maximum number of allowed flips.
     * @param jumpFrequency the frequency of random flips.
     */
    public Walker(int solverNumber, HashMap<String,Object> parameters, int seed, int maxFlips, int jumpFrequency) {
        super(solverNumber,parameters);
        this.seed = seed;
        this.maxFlips = maxFlips;
        this.jumpFrequency = jumpFrequency;
        monitorId = "Walker_"+solverNumber;}

    /** adds observers to the model and the equivalenceClasses.
     */
    @Override
    public void initialize() {
        model.addObserver(this::addGloballyTrueLiteral);
        equivalenceClasses.addObserver((this::addEquivalence));}

    /** starts the search for a model.
     *
     * @param problemSupervisor the corresponding supervisor.
     * @return the result of the search.
     */
    @Override
    public Result solveProblem(ProblemSupervisor problemSupervisor) {
        this.problemSupervisor = problemSupervisor;
        long startTime     = System.nanoTime();
        myThread           = Thread.currentThread();
        solverId           = (String)solverParameters.get("name");
        problemId          = problemSupervisor.problemId;
        combinedId         = problemId+"@"+solverId + ":" + solverNumber;
        globalParameters   = problemSupervisor.globalParameters;
        inputClauses       = problemSupervisor.inputClauses;
        predicates         = inputClauses.predicates;
        symboltable        = inputClauses.symboltable;
        monitor            = problemSupervisor.monitor;
        monitoring         = monitor != null; //&& monitor.monitoring;
        model              = problemSupervisor.model;
        equivalenceClasses = problemSupervisor.equivalenceClasses;
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
    public void readInputClauses() throws Result{
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
     * @throws Unsatisfiable if the clause is false or the global model discovers a contradiction.
     */
    protected void insertClause(int[] inputClause) throws Unsatisfiable {
        Clause clause = new Clause(inputClause);
        if(clause.removeComplementaryLiterals()) return;
        if(clause.quantifier == Quantifier.AND) {
            for(Literal literalObject : clause.literals) {
                model.add(literalObject.literal,clause.inferenceStep);}
            return;}
        for(int i = 0; i < clause.literals.size(); ++i) {
            Literal literalObject = clause.literals.get(i);
            if(literalObject.multiplicity > clause.max) {
                model.add(-literalObject.literal,null);
                clause.expandedSize -= literalObject.multiplicity;
                clause.literals.remove(i--);
                clause.hasMultiplicities = clause.expandedSize > clause.literals.size();}}
        if(clause.isFalse()) throw new UnsatClause(problemId,solverId,inputClause);
        if(clause.isTrue()) return;
        if(clause.hasMultiplicities) clause.reduceMultiplicities();
        for(Literal literalObject : clause.literals) {literals.addLiteral(literalObject);}
        clauses.add(clause);}



    /** initializes the local model.
     * a predicate is set to true if there are more positive literal occurrences than negative literal occurrences.<br>
     * If there are equally many positive and negative occurrences, the random number generator decides about the truth value.<br>
     * The global model is not yet taken into account.
     */
    protected void initializeModel() {
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
    protected boolean initializeLocalTruthForClause(Clause clause) {
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
    protected void initializeFlipScores(Clause clause) {
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
    protected void initializePredicatesWithPositiveScores() {
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
    private void walk() throws Result {
        while(statistics.flips < maxFlips) {
            if(Thread.interrupted()) {
                if(trueLiteralInterrupt) integrateGloballyTrueLiterals();
                else {if(equivalenceInterrupt) integrateEquivalences();
                    else {
                        globalParameters.logstream.println("Walker " + combinedId + " interrupted after " + statistics.flips + " flips.\n");
                        break;}}}
            int predicate = selectFlipPredicate();
            if(monitoring) monitor.println(monitorId,"Flip " + predicate);
            flipPredicate(predicate);
            if(falseClauses == 0) {throw localToGlobalModel();}}
        throw new Aborted(null,solverId,"Walker aborted after " + statistics.flips + " flips");}


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
     * @return the predicate to be flipped.
     */
    int selectPredicateInFalseClause(Clause clause) {
        if(clause.trueLiterals < clause.min) { // not enough true literals. A false literal must be flipped.
            for(Literal literalObject : clause.literals) {if(!isLocallyTrue(literalObject.literal)) return literalObject.literal;}}
        // too many true literals. A true literal must be flipped.
        for(Literal literalObject : clause.literals) {if(isLocallyTrue(literalObject.literal)) return literalObject.literal;}
        assert(false);
        return 0;}



    void integrateEquivalences() throws Unsatisfiable{
        IntArrayList equivalences = getEquivalences();
        if(equivalences == null) return;
        for(int i = 0; i < equivalences.size(); i += 2) {
            int representative = equivalences.getInt(i);
            int literal = equivalences.getInt(i+1);
            if(localModel[literal] != localModel[representative]) flipPredicate(Math.abs(literal));
            for(int sign = 1; sign >= -1; sign -= 2) {
                replaceEquivalentLiterals(sign*representative,sign*literal);}}}

    protected void replaceEquivalentLiterals(int representative, int literal) throws Unsatisfiable {
        Literal literalObject = literals.getFirstLiteralObject(literal);
        while(literalObject != null) {
            Clause clause = literalObject.clause;
            while(clause == null) {literalObject = literalObject.nextLiteral; clause = literalObject.clause;}
            Literal representativeObject = clause.findLiteral(-representative);
            if(representativeObject != null) {
                if(clause.quantifier == Quantifier.OR) {
                    removeClause(clause);
                    literalObject = literalObject.nextLiteral;
                    continue;}
                else {
                    if(clause.removeComplementaryLiterals()){
                        literalObject.literal = representative;
                        removeClause(clause);
                        literalObject = literalObject.nextLiteral;
                        continue;}}}
            representativeObject = clause.findLiteral(representative);
            if(representativeObject == null) {
                literalObject.literal = representative;
                literals.replaceLiteral(literalObject,literal);}
            else {
                representativeObject.multiplicity = Math.max(clause.min, representativeObject.multiplicity+literalObject.multiplicity);
                clause.literals.remove(literalObject);
                literals.removeLiteral(literalObject);}
            literalObject = literalObject.nextLiteral;}}






    /** flips the truth value of the predicate and updates the flipQueue and the falseClauses list
     *
     * @param predicate to be flipped
     */
    void flipPredicate(int predicate) {
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
     * @throws Satisfiable with the transferred local model.
     */
    Satisfiable localToGlobalModel()  throws Satisfiable {
        Model model = new Model(predicates);
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            model.addImmediately(localModel[predicate] ? predicate : -predicate);}
        throw new Satisfiable(null,null,model);}



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

    synchronized IntArrayList getGloballyTrueLiterals() {
        if(globallyTrueLiterals.isEmpty()) return null;
        IntArrayList literals = globallyTrueLiterals.clone();
        globallyTrueLiterals.clear();
        trueLiteralInterrupt = false;
        return literals;}


    public synchronized void addEquivalence(int representative, int literal, InferenceStep step) {
        equivalentLiterals.add(representative); equivalentLiterals.add(literal);
        equivalenceInterrupt = true;
        myThread.interrupt();}

    private synchronized IntArrayList getEquivalences() {
        if(equivalentLiterals.isEmpty()) return null;
        IntArrayList literals = equivalentLiterals.clone();
        equivalentLiterals.clear();
        equivalenceInterrupt = false;
        return literals;}



    /** removes the clause from the clauses, the falseClauses list, the literals index and if necessary from predicatesWithPositiveScore.
     * The flip score is updated.<br>
     * This may happen after equivalence replacement generates a tautology.
     *
     * @param clause a clause to be removed.
     */
    protected void removeClause(Clause clause) {
        clauses.remove(clause);
        if(!clause.isLocallyTrue) {falseClauseList.remove(clause);}
        for(Literal literalObject : clause.literals) {
            literals.removeLiteral(literalObject);
            int predicate = Math.abs(literalObject.literal);
            float score = flipScores[predicate];
            flipScores[predicate] -= literalObject.flipScorePart;
            if(score > 0 && flipScores[predicate] <= 0) predicatesWithPositiveScore.remove(predicate);}}

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
     * @return the predicates with flipScore != 0 in a string.
     */
    public String flipScoresToString(Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            if(flipScores[predicate] != 0) st.append(Symboltable.toString(predicate,symboltable)).
                    append(":").append(flipScores[predicate]).append(",");}
        return st.toString();}

    public String toString(String version) {
        return toString(version,null);}

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
        return "Versions: clauses,literals,predicates,flipscores,model,statistic";
    }


    public String toString(Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        st.append("Random Walker ").append(solverId).append( " on Problem ").append(problemId).append("\n");
        st.append("Parameters:\n");
        st.append("  seed:           ").append(seed).append("\n");
        st.append("  flips:          ").append(statistics.flips).append(" of ").append(maxFlips).append("\n");
        st.append("Current model: ").append(localModelToString(symboltable)).append("\n");
      /*  st.append("False Clauses: " + falseClauses.size() + "\n");
        for(Clause Clause : clauses) {
            if(!Clause.isLocallyTrue) st.append(Clause.toString(idWidth+2,symboltable)).append("\n");}
        st.append(flipQueue.toString());*/
    return st.toString();
    }


    public String clausesToString(Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        for(Clause clause : clauses) {
            st.append(clause.toString(symboltable,0)).append("\n");}
        return st.toString();}
}