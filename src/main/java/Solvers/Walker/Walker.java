package Solvers.Walker;

import Datastructures.LinkedItemList;
import Datastructures.Results.Aborted;
import Datastructures.Results.Result;
import Datastructures.Results.Satisfiable;
import Datastructures.Statistics.Statistic;
import Datastructures.Symboltable;
import Datastructures.Theory.Model;
import InferenceSteps.InferenceStep;
import Management.ErrorReporter;
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
    LinkedItemList<Clause> falseClauseList = new LinkedItemList<Clause>("false clauses");


    /** an index for all literal occurrences in the clauses. */
    Literals literals;

    /** the current list of predicates with a positive score. */
    Predicates predicatesWithPositiveScore;

    /** maps all predicates to a truth value. */
    boolean[] localModel;

    /** a score of +x for predicate p means that by flipping(p) x more clauses become true. */
    int[] flipScores;

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
    private static final int trueLiteralScore = Integer.MIN_VALUE/2;



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


    /** starts the search for a model.
     *
     * @return the result of the search.
     */
    @Override
    public Result solveProblem() {
        long startTime     = System.nanoTime();
        myThread           = Thread.currentThread();
        localModel         = new boolean[predicates+1];
        random             = new Random(seed);
        statistics         = new Statistics(combinedId);
        clauses            = new ArrayList<>(problemSupervisor.inputClauses.nextId);
        literals           = new Literals(predicates);
        flipScores         = new int[predicates+1];
        predicatesWithPositiveScore = new Predicates(predicates);
        globalParameters.logstream.println(solverId + " for problem " + problemId + " started");

        try{
            readInputClauses();
            initializeModel();
            for(Clause clause : clauses) {
                initializeLocalTruthForClause(clause);
                initializeFlipScores(clause);}
            if(falseClauseList.size == 0) {throw localToGlobalModel();}
            initializePredicatesWithPositiveScores();
            walk();}
        catch(Result result) {
            statistics.elapsedTime = System.nanoTime() - startTime;
            System.out.println("RESULT " + result);
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
    void readInputClauses(){
        Solvers.Normalizer.Clause normalizedClause = problemSupervisor.normalizer.clauses.firstLinkedItem;
        while(normalizedClause != null) {
            insertClause(normalizedClause);
            normalizedClause = (Solvers.Normalizer.Clause)normalizedClause.nextItem;}}

    Clause insertClause(Solvers.Normalizer.Clause normalizedClause) {
        Clause clause = new Clause(normalizedClause);
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
        if(!isTrue) falseClauseList.addToBack(clause);
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
            for(Literal literalObject : clause.literals) { // flipping a literal may reduce the true literals below min, or increase them over max.
                int literal = literalObject.literal;
                int newTrueLiterals = isLocallyTrue(literal) ? trueLiterals - literalObject.multiplicity : trueLiterals + literalObject.multiplicity;
                if(newTrueLiterals < min || newTrueLiterals > max) {
                    literalObject.flipScorePart = -1;
                    flipScores[Math.abs(literalObject.literal)] -= 1;}}
            return;}

        // the clause is false and should become true
        if(trueLiterals < clause.min) { // false literals should become true.
            for(Literal literalObject : clause.literals) {
                int literal = literalObject.literal;
                if(!isLocallyTrue(literal)) {
                    literalObject.flipScorePart = +1;
                    flipScores[Math.abs(literalObject.literal)] += 1;}}
            return;}

        if(trueLiterals > clause.max) { // true literals should become false.
            for(Literal literalObject : clause.literals) {
                int literal = literalObject.literal;
                if(isLocallyTrue(literal)) {
                    literalObject.flipScorePart = +1;
                    flipScores[Math.abs(literalObject.literal)] += 1;}}}}

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
                else {
                    globalParameters.logstream.println("Walker " + combinedId + " interrupted after " + statistics.flips + " flips.\n");
                        break;}}
            int predicate = selectFlipPredicate();
            flipPredicate(predicate);
            if(falseClauseList.size == 0) {throw localToGlobalModel();}}
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
        if(statistics.flips > 0 &&  statistics.flips % jumpFrequency == 0) {
            int n = random.nextInt(falseClauseList.size);
            Clause clause = falseClauseList.getLinkedItem(n);
            return selectPredicateInFalseClause(clause);}
        return selectPredicateInFalseClause(falseClauseList.firstLinkedItem);}

    /** selects a predicate in a false clause.
     * If there are not enough true literals then a false literal is randomly chosen to be flipped.<br>
     * If there are too many true literals then a true literal is randomly chosen to be flipped.<br>
     *
     * @param clause a false clause.
     * @return the predicate to be flipped.
     */
    int selectPredicateInFalseClause(Clause clause) {
        if(clause.trueLiterals < clause.min) { // not enough true literals. A false literal must be flipped.
            int n = random.nextInt(clause.literals.size() - clause.trueLiterals);
            int counter = -1;
            for(Literal literalObject : clause.literals) {
                int literal = literalObject.literal;
                if(!isLocallyTrue(literal)) {if(++counter == n) return Math.abs(literal);}}
            assert(false);}
        // too many true literals. A true literal must be flipped.
        int n = random.nextInt(clause.trueLiterals);
        int counter = -1;
        for(Literal literalObject : clause.literals) {
            int literal = literalObject.literal;
            if(isLocallyTrue(literal)) {if(++counter == n) return Math.abs(literal);};}
        assert(false);
        return 0;}

    /** flips the truth value of the predicate and updates the flipQueue and the falseClauses list
     *
     * @param predicate to be flipped
     */
    void flipPredicate(int predicate) {
        assert(predicate > 0);
        if(monitoring) monitor.println(monitorId, statistics.flips + ". flip: predicate "+
                        Symboltable.toString(predicate,symboltable) +
                        " with flip score " + flipScores[predicate] +
                        " for " + falseClauseList.size + " false clauses.",
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
             if(!clause.isLocallyTrue) {falseClauseList.addToBack(clause);}}
        else {if(clause.isLocallyTrue) {falseClauseList.remove(clause);}}

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
    @SuppressWarnings("unused")
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
    @SuppressWarnings("unused")
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




    /** removes the clause from the clauses, the falseClauses list, the literals index and if necessary from predicatesWithPositiveScore.
     * The flip score is updated.<br>
     * This may happen after equivalence replacement generates a tautology.
     *
     * @param clause a clause to be removed.
     */
    protected void removeClause(Clause clause) {
        clauses.remove(clause);
        if(!clause.isLocallyTrue) falseClauseList.remove(clause);
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

    public String falseClausesString(Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        st.append("False Clauses\n");
        Clause clause = falseClauseList.firstLinkedItem;
        while(clause != null) {
            st.append(clause.toString(symboltable, 0)).append("\n");
            clause = (Clause) clause.nextItem;}
        return st.toString();}
}