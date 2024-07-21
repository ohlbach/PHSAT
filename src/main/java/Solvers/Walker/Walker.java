package Solvers.Walker;

import Datastructures.LinkedItemList;
import Datastructures.Results.Aborted;
import Datastructures.Results.Result;
import Datastructures.Results.Satisfiable;
import Datastructures.Statistics.Statistic;
import Datastructures.Symboltable;
import Datastructures.Theory.Model;
import InferenceSteps.InferenceStep;
import Management.Parameter;
import Management.Parameters;
import Management.ProblemSupervisor;
import Management.ValueType;
import Solvers.Solver;
import Utilities.Utilities;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.Random;

import static Utilities.Utilities.toArrayList;

/** This is a "random walker" for finding a model for QuSat-clauses.
 * <br>
 * All clauses are put into interval-normalform.
 * An initial candidate model is created by comparing the positive and negative occurrences of predicates.<br>
 * If the numbers are the same then a random number generator is used for constructing the initial model.<br>
 * The truth values are then heuristically flipped until there are no false clauses anymore.
 */
public class Walker extends Solver {

    // Control Parameters
    // ******************

    private static int maxFlipsDefault = 0;
    /** maximum number of allowed flips. */
    int maxFlips;

    private static int seedDefault = 0;

    /** for creating the random number generator */
    final int seed;

    /** the default value for the jump frequency. */
    private static int jumpFrequencyDefault = 10;

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

    /** becomes true when a new true literal has been arrived. */
    boolean trueLiteralInterrupt = false;

    /** collects statistical information. */
    WalkerStatistics statistics;

    /** random number generator for flip jumps. */
    Random random;

    /** a tiny flip score for globally true predicates. They should never be flipped again.*/
    private static final int trueLiteralScore = Integer.MIN_VALUE/2;

    public static void setDefaults(ArrayList<String> defaults, StringBuilder errors) {
        try{
            for(String line : defaults) {
                String[] parts = line.split("\\s*=\\s*");
                if(parts.length != 2) {continue;}
                String variable = parts[0];
                String value = parts[1];
                switch(variable.toLowerCase()) {
                    case "maxflips":      maxFlipsDefault = Integer.parseInt(value); break;
                    case "jumpfrequency": jumpFrequencyDefault = Integer.parseInt(value); break;
                    case "seed":          seedDefault = Integer.parseInt(value); break;
                }}}
        catch(NumberFormatException e) {
            errors.append("Walker: " + e.getMessage());}}

    public static Parameters makeParameter() {
        StringBuilder errors = new StringBuilder();
        Parameters parameters = new Parameters("Walker");
        Parameter selected = new Parameter("Select", Parameter.DisplayType.Button,
                new ValueType.Booleans(),
                "false",errors,
                "Select the Walker Solver");
        parameters.add(selected);
        Parameter maxFlips = new Parameter("MaxFlips", Parameter.DisplayType.String,
                new ValueType.Integers(1,maxFlipsDefault,true),
                Integer.toString(maxFlipsDefault),errors,
                "The maximum number of flips at which the search is stopped.");
       parameters.add(maxFlips);

        Parameter jumps = new Parameter("Jump Frequency", Parameter.DisplayType.String,
                new ValueType.Integers(2,Integer.MAX_VALUE,true),
                Integer.toString(jumpFrequencyDefault),errors,
                "Random flips are performed in this frequency");
        parameters.add(jumps);

        Parameter seed = new Parameter("Seed", Parameter.DisplayType.String,
                new ValueType.Integers(0,Integer.MAX_VALUE,true),
                Integer.toString(seedDefault),errors,
                "The seed for the random number generator");
        parameters.add(seed);
        parameters.setDescription("Random search for a model (is incomplete for unsatisfiable clauses)");
        if(!errors.isEmpty()) {
            System.err.println("Walker: Errors in makeGlobalParameters:\n"+errors.toString());
            System.exit(1);}
        return parameters;
    }

    /**
     * Generates and adds new problem generators based on the provided parameters.
     *
     * @param parameters The parameters containing the values necessary to create the problem generators.
     * @param walkers The list of walkers to add the newly created walkers to.
     */
    public static void makeSolvers(Parameters parameters,ArrayList<Solver> walkers) {
        int i = 0;
        IntArrayList maxFlips =    (IntArrayList)parameters.parameters.get(++i).value;
        IntArrayList jumps =      (IntArrayList)parameters.parameters.get(++i).value;
        IntArrayList seeds =      (IntArrayList)parameters.parameters.get(++i).value;
        int solverNumber = 0;
        for(ArrayList<Object> p : (ArrayList<ArrayList>)Utilities.crossProduct(toArrayList(maxFlips),toArrayList(jumps),toArrayList(seeds))) {
            int maxFlipsv = (int)p.get(0);
            int jumpsv    = (int)p.get(1);
            int seedsv    = (int)p.get(2);
            walkers.add(new Walker(++solverNumber,maxFlipsv,jumpsv,seedsv));}}


    /** constructs a new Walker.
     *
     * @param solverNumber  for enumerating the walkers.
     * @param seed          for starting the random number generator.
     * @param maxFlips      the maximum number of allowed flips.
     * @param jumps the frequency of random flips.
     */
    public Walker(int solverNumber, int maxFlips,int jumps,int seed) {
        super(solverNumber);
        this.seed = seed;
        this.maxFlips = maxFlips;
        this.jumpFrequency = jumps;
        monitorId = "Walker_"+solverNumber;}

    /** initializes the parameters which are common to all solvers.
     *
     * @param problemSupervisor the supervisor for the problem.
     */
    @Override
    public void initialize(ProblemSupervisor problemSupervisor) {
        super.initialize(problemSupervisor);
        model.addObserver(myThread,this::addGloballyTrueLiteral);}

    /** starts the search for a model.
     *
     * @return the result of the search.
     */
    @Override
    public Result solveProblem() {
        solverStartTime = System.nanoTime();
        localModel         = new boolean[predicates+1];
        random             = new Random(seed);
        statistics         = new WalkerStatistics(combinedId);
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
            statistics.elapsedTime = System.nanoTime() - solverStartTime;
            System.out.println("RESULT " + result);
            return result;}
        statistics.elapsedTime = System.nanoTime() - solverStartTime;
        return null;
    }

    /** reads all input clauses except conjunctions and equivalences, and turns them into interval-normalform.
     * The original quantifiers, however, are kept.<br>
     * Complementary predicates are removed.<br>
     * Derivable true or false predicates are inserted into the global model.<br>
     */
    void readInputClauses(){
        Clause normalizedClause = null; //problemSupervisor.normalizer.clauses.firstLinkedItem;
        while(normalizedClause != null) {
            insertClause(normalizedClause);
            normalizedClause = (Clause)normalizedClause.nextItem;}}

    /** Inserts a normalized clause into the internal datastructures.
     * <br>
     * The clauses should be free of redundancies.
     *
     * @param normalizedClause a normalized clause.
     */
    void insertClause(Datastructures.Clause normalizedClause) {
        Clause clause = new Clause(normalizedClause);
        //for(Literal literalObject : clause.literals) {literals.addLiteral(literalObject);}
        clauses.add(clause);}


    /** initializes the local model.
     * a predicate is set to true if there are more positive literal occurrences than negative literal occurrences.<br>
     * If there are equally many positive and negative occurrences, the random number generator decides about the truth value.<br>
     * The global model is taken into account.<br>
     * It should, however be irrelevant because the Normalizer has eliminated all predicates with truth values.
     */
    void initializeModel() {
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            byte status = model.status(predicate);
            if(status == 1)  {localModel[predicate] = true; continue;}
            if(status == -1) {localModel[predicate] = false; continue;}
            int posSize = literals.size(predicate);
            int negSize = literals.size(-predicate);
            if(posSize == negSize) localModel[predicate] = random.nextBoolean();
            else localModel[predicate] = (posSize >= negSize);}}


    /** computes the truth value of a clause in the local model.
     * The truth value and the number of true predicates is stored in the clause.<br>
     * False clauses are collected in falseClauseList.
     *
     * @param clause a clause.
     * @return true if the clause is true in the local (and global) model.
     */
    boolean initializeLocalTruthForClause(Clause clause) {
        int trueLiterals = 0;
        for(Datastructures.Literal literalObject : clause.literals) {
            int literal = literalObject.literal;
            if(literal > 0)  {
                if(localModel[literal])    trueLiterals += literalObject.multiplicity;}
            else if(!localModel[-literal]) trueLiterals += literalObject.multiplicity;}

        clause.trueLiterals = trueLiterals;
        boolean isTrue = clause.min <= trueLiterals && trueLiterals <= clause.max;
        clause.isLocallyTrue = isTrue;
        if(!isTrue) falseClauseList.addToBack(clause);
        return isTrue;}


    /** computes the initial flip scores for the predicates in the clause.
     *  A positive flip score for a literal indicates that flipping the truth value for the literal makes a false clause true.<br>
     *  A negative flip score indicates that a true clause can become false. <br>
     *
     * @param clause the clause to be investigated.
     */
    void initializeFlipScores(Clause clause) {
        int trueLiterals = clause.trueLiterals;
        int min = clause.min; int max = clause.max;
        if(clause.isLocallyTrue) {
            if(min == max) { // all flips make the clause false.
                for(Datastructures.Literal literalObject : clause.literals) {
                  //  literalObject.flipScorePart = -1;
                    flipScores[Math.abs(literalObject.literal)] -= 1;}
                return;}
            for(Datastructures.Literal literalObject : clause.literals) { // flipping a literal may reduce the true predicates below min, or increase them over max.
                int literal = literalObject.literal;
                int newTrueLiterals = isLocallyTrue(literal) ? trueLiterals - literalObject.multiplicity : trueLiterals + literalObject.multiplicity;
                if(newTrueLiterals < min || newTrueLiterals > max) {
                   // literalObject.flipScorePart = -1;
                    flipScores[Math.abs(literalObject.literal)] -= 1;}}
            return;}

        // the clause is false and should become true
        if(trueLiterals < clause.min) {
            for(Datastructures.Literal literalObject : clause.literals) {
                int literal = literalObject.literal;
                if(isLocallyTrue(literal) || trueLiterals + literalObject.multiplicity > clause.max) {  // true predicates should not become false.
                    //literalObject.flipScorePart = -1;
                    flipScores[Math.abs(literalObject.literal)] -= 1;}
                else {
                    //literalObject.flipScorePart = +1; // false predicates should become true.
                    flipScores[Math.abs(literalObject.literal)] += 1;}}
            return;}
        if(trueLiterals > clause.max) {
            for(Datastructures.Literal literalObject : clause.literals) {
                int literal = literalObject.literal;
                if(isLocallyTrue(literal) && !(trueLiterals - literalObject.multiplicity < clause.min)) {
                    //literalObject.flipScorePart = 1; // true predicates should become false.
                    flipScores[Math.abs(literalObject.literal)] += 1;}
                else {
                    //literalObject.flipScorePart = -1; // false predicates should not become true.
                    flipScores[Math.abs(literalObject.literal)] -= 1;}}}}

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
        throw new Aborted(problemId,solverId, solverStartTime,"Aborted after " + statistics.flips + " flips");}


    /** selects a predicate to be flipped.
     * <br>
     * The priorities are: <br>
     * 1. a predicate with positive flipScore (any such predicate is good enough).<br>
     * 2. a predicate in a false clause.<br>
     *    - every jump-frequency time a randomly chosen false clause is selected.<br>
     *    - otherwise the first false clause in the list is chosen.<br>
     * The predicate is removed from the list of predicates with positive score.
     *
     * @return a predicate to be flipped next.
     */
    int selectFlipPredicate() {
        Predicate predicateObject = predicatesWithPositiveScore.firstPredicate;
        if(predicateObject != null) {
            int predicate = predicateObject.predicate;
            predicatesWithPositiveScore.remove(predicate);
            return predicate;}
        if(statistics.flips > 0 &&  statistics.flips % jumpFrequency == 0) {
            int n = random.nextInt(falseClauseList.size);
            Clause clause = falseClauseList.getLinkedItem(n);
            return selectPredicateInFalseClause(clause);}
        return selectPredicateInFalseClause(falseClauseList.firstLinkedItem);}

    /** selects a predicate in a false clause.
     * If there are not enough true predicates then a false literal is randomly chosen to be flipped.<br>
     * If there are too many true predicates then a true literal is randomly chosen to be flipped.<br>
     *
     * @param clause a false clause.
     * @return the predicate to be flipped.
     */
    int selectPredicateInFalseClause(Clause clause) {
        if(clause.trueLiterals < clause.min) { // not enough true predicates. A false literal must be flipped.
            int n = random.nextInt(clause.literals.size() - clause.trueLiterals);
            int counter = -1;
            for(Datastructures.Literal literalObject : clause.literals) {
                int literal = literalObject.literal;
                if(!isLocallyTrue(literal)) {if(++counter == n) return Math.abs(literal);}}
            assert(false);}
        // too many true predicates. A true literal must be flipped.
        int n = random.nextInt(clause.trueLiterals);
        int counter = -1;
        for(Datastructures.Literal literalObject : clause.literals) {
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



    /** updates the flip scores of the predicates in the clauses containing the flipped literal.
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
                literalObject = (Literal)literalObject.nextItem;}}}

    /** updates the clause's flip score, the falseClauses list and the predicatesWithPositiveScore list.
     *  The previous score contribution is subtracted from flipScores and the new scores is added by calling initializeFlipScores.
     *
     * @param flippedLiteralObject the literal whose local truth value has already been flipped.
     */
     void updateFlipScores(Literal flippedLiteralObject) {
        Clause clause = (Clause) flippedLiteralObject.clause;
        boolean wasTrue = clause.isLocallyTrue;
        int trueLiterals = 0;
        for(Datastructures.Literal literalObject : clause.literals) {
            int literal = literalObject.literal;
           // flipScores[Math.abs(literal)] -= literalObject.flipScorePart;
           // literalObject.flipScorePart = 0;
            if(isLocallyTrue(literal)) trueLiterals += literalObject.multiplicity;}
        clause.trueLiterals = trueLiterals;
        clause.isLocallyTrue = clause.min <= trueLiterals && trueLiterals <= clause.max;
        initializeFlipScores(clause);
        if(wasTrue) {
             if(!clause.isLocallyTrue) {falseClauseList.addToBack(clause);}}
        else {if(clause.isLocallyTrue) {falseClauseList.remove(clause);}}

        for(Datastructures.Literal literalObject : clause.literals) {
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
        return new Satisfiable(problemId,solverId, model);}



    protected boolean isLocallyTrue(int literal) {
        return (literal > 0) ? localModel[literal] : !localModel[-literal];}



    /** collects globally true predicates which are inserted by other solvers  into the global model */
    private final IntArrayList globallyTrueLiterals = new IntArrayList();

    /** integrates globally true predicates.
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
                    (literal < 0 && localModel[-literal] == model.isFalse(literal))) continue;
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

    /** copies the imported globally true predicates.
     *  globallyTrueLiterals is cleared.
     *
     * @return a copy of the globally true predicates.
     */
    synchronized IntArrayList getGloballyTrueLiterals() {
        if(globallyTrueLiterals.isEmpty()) return null;
        IntArrayList literals = globallyTrueLiterals.clone();
        globallyTrueLiterals.clear();
        trueLiteralInterrupt = false;
        return literals;}



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
     * @param version      clauses,falseClauses,predicates,predicates,flipscores,model,statistic.
     * @return             different aspects as a string.
     */
    public String toString(String version) {
        return toString(version,null);}

    /** turns different aspects into a string.
     *
     * @param version      clauses,falseClauses,predicates,predicates,flipscores,model,statistic.
     * @param symboltable  null or a symboltable.
     * @return             different aspects as a string.
     */
    public String toString(String version, Symboltable symboltable) {
        return switch (version) {
            case "clauses"      -> clausesToString(symboltable);
            case "falseClauses" -> falseClauseList.toString(symboltable);
            case "literals"     -> literals.toString(symboltable);
            case "predicates"   -> predicatesWithPositiveScore.toString(symboltable);
            case "flipscores"   -> flipScoresToString(symboltable);
            case "model"        -> localModelToString(symboltable);
            case "statistic"    -> statistics == null ? "null" : statistics.toString();
            default -> "Versions: clauses,falseClauses,predicates,predicates,flipscores,model,statistic";
        };
    }

    /** turns parameters into a string.
     *
     * @return the parameters as a string.
     */
    public String toString() {
        return "Random Walker " + solverId + " on Problem " + problemId + "\n" +
                "Parameters:\n" +
                "  seed:  " + seed + "\n" +
                "  flips: " + ((statistics == null) ? "0" : statistics.flips) + " of " + maxFlips + "\n" +
                "  jumps: " + jumpFrequency + "\n";}

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