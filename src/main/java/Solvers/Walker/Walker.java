package Solvers.Walker;

import Datastructures.Clauses.Quantifier;
import Datastructures.Results.Aborted;
import Datastructures.Results.Result;
import Datastructures.Results.Satisfiable;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Statistics.Statistic;
import Datastructures.Symboltable;
import Datastructures.Theory.Model;
import Management.ErrorReporter;
import Management.ProblemSupervisor;
import Solvers.Simplifier.UnsatClause;
import Solvers.Solver;
import Utilities.Utilities;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.*;
import java.util.function.IntConsumer;

public class Walker extends Solver {

    protected static float trueLiteralScore = Integer.MIN_VALUE/2;
    private ArrayList<Solver> workerSolvers = new ArrayList<>();

    protected ArrayList<Clause> clauses = new ArrayList<>();         // collects all clauses

    protected Literals literals;
    protected boolean[] localModel;              // maps all predicates to a truth value
    protected float[] flipScores;

    protected Predicates predicatesWithPositiveScore;
    private static final int jumpFrequencyDefault = 10;
    public  int jumpFrequency;                   // after jumpFrequency many flips, a random jump is inserted
    private static final int maxFlipsDefault = Integer.MAX_VALUE;
    public  int maxFlips;                        // maximum number of allowed flips
    public Statistics statistics;                // collects statistical information
    private final int seed;                                  // for the random number generator
    protected  Random random;                       // random number generator for flip jumps
    private int idWidth = 0;

    private int flipHistoryLength = 5;
    private int[] flipHistory = new int[flipHistoryLength];
    private int flipHistoryIndex = 0;
    private int flipHistoryEnd = 0;

    protected int falseClauses = 0;

    private boolean monitoring = false;

    private String monitorId;

    protected Clauses falseClauseList = new Clauses();

    public static String help() {
        return "Random Walker: parameters:\n" +
                "seeds:  for the random number generator      (default: 0)\n" +
                "flips:  for restricting the number of flips  (default: Max_Integer).\n" +
                "jumps:  frequency of random jumps            (default: 10)\n";}

    private static final HashSet<String> keys = new HashSet<>(); // contains the allowed keys in the specification.
    static { // these are the allowed keys in the specification.
        Collections.addAll(keys, "seeds", "flips", "jumps", "solver");}

    /** parses a HashMap with key-value pairs<br>
     *
     * @param parameters  the parameters with the keys "seed", "flips", "jumps"
     * @return            a list of Walker solvers
     */
    public static void makeSolvers(HashMap<String,String> parameters, ArrayList<Solver> solvers,
                                                StringBuilder errors, StringBuilder warnings){
        for(String key : parameters.keySet()) {
            if(!keys.contains(key)) {
                ErrorReporter.reportWarning("Walker: unknown key in parameters: " + key + "\n" +
                                "        allowed keys: seed, flips, jumps.\n");}}
        ArrayList<HashMap<String,Object>> list = new ArrayList<>();
        String seeds = parameters.get("seed");
        if(seeds == null) seeds = "0";
        String flips = parameters.get("flips");
        if(flips == null) flips = Integer.toString(maxFlipsDefault);
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

    public Walker(int solverNumber, HashMap<String,Object> parameters, int seed, int maxFlips, int jumpFrequency) {
        super(solverNumber,parameters);
        this.seed = seed;
        this.maxFlips = maxFlips;
        this.jumpFrequency = jumpFrequency;
        monitorId = "Walker_"+solverNumber;}

    @Override
    public Result solveProblem(ProblemSupervisor problemSupervisor) {
        //   globalParameters.log(solverId + " for problem " + problemId + " started");
        long startTime = System.nanoTime();
        this.problemSupervisor = problemSupervisor;
        readModel();
        localModel     = new boolean[predicates+1];
        random         = new Random(seed);
        statistics     = new Statistics(combinedId);
        clauses   = new ArrayList<>(inputClauses.nextId);
        literals  = new Literals(predicates);
        flipScores = new float[predicates+1];
        try{
            initializeModelAndScores();
            readInputClauses();
            walk();}
        catch(Result result) {
            statistics.elapsedTime = System.nanoTime() - startTime;
            return result;}
        statistics.elapsedTime = System.nanoTime() - startTime;
        return null;
    }


    /** reads the disjunctions, the atleast, atmost, exactly and interval clauses from inputClauses and transforms them to atleast-clauses.
     * The clauses themselves are simplified as far as possible.<br>
     * New unit clauses are put into the model. <br>
     * Two-literal clauses generate a corresponding task.
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

    /** collects globally true literals which are inserted by other solvers  into the global model */
    private final IntArrayList globallyTrueLiterals = new IntArrayList();

    /** a temporary local copy of the globally true literals */
    private final IntArrayList globallyTrueLiteralsCopy = new IntArrayList();

    /** copies the globally true literals to a local copy, such that other threads could fill it anew
     *
     * @return a copy of the globally true literals
     */
    private synchronized IntArrayList copyGloballyTrueLiterals() {
        globallyTrueLiteralsCopy.clear();
        globallyTrueLiterals.forEach((IntConsumer) globallyTrueLiteralsCopy::add);
        globallyTrueLiterals.clear();
        return globallyTrueLiteralsCopy;}


    /** turns the inputClause into literals and clauses of the internal datastructures.
     * All clauses are stored with interval limits.<br>
     * Complementary literals are removed.<br>
     * Derivable true or false literals are inserted into the global model.<br>
     *
     * @param inputClause    an input clause.
     * @throws Unsatisfiable if the clause is false or the global model discovers a contradiction.
     */
    protected void insertClause(int[] inputClause) throws Unsatisfiable {
        Clause clause = new Clause(inputClause);
        if(clause.removeComplementaryLiterals(inputClause)) return;
        if(clause.quantifier == Quantifier.AND) {
            for(Literal literalObject : clause.literals) {
                model.add(literalObject.literal,clause.inferenceStep);}
            return;}
        for(int i = 0; i < clause.literals.size(); ++i) {
            Literal literalObject = clause.literals.get(i);
            if(literalObject.multiplicity > clause.max) {
                model.add(-literalObject.literal,null);
                clause.expandedSize -= literalObject.multiplicity;
                clause.literals.remove(i--);}}
        if(clause.isFalse()) throw new UnsatClause(problemId,solverId,inputClause);
        if(clause.isTrue()) return;

        for(Literal literalObject : clause.literals) {
            literals.addLiteral(literalObject);}
        clauses.add(clause);}


    /** Initializes the local model, the local truth values of the clauses, the falseClauses number and the initial flip scores.
     * A predicate is made true if it makes more clauses true than making the predicate false.
     * The number of true clauses when making a predicate true is estimated by its initial score.
     * If the same number of clauses are made true/false then the truth value of the predicate is
     * selected randomly.
     * Global truth value are transferred unchanged to the local model.
     */
    protected void initializeModelAndScores() {
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            int status = model.status(predicate);
            if(status != 0) {
                localModel[predicate] = (status == 1);
                flipScores[predicate] = Integer.MIN_VALUE/2;}
            else {
                int posSize = literals.size(predicate);
                int negSize = literals.size(-predicate);
                if(posSize == negSize) localModel[predicate] = random.nextBoolean();
                else localModel[predicate] = (posSize >= negSize);}}

        for(Clause clause: clauses) {
            if(!setLocalTruth(clause)) ++falseClauses;
            initializeFlipScores(clause);}
        if(monitoring) monitor.println(monitorId, "Initial Data:\n" + toString());}

    /** initializes the local model.
     * a predicate is set to true if there are more positive literal occurrences than negative literal occurrences.<br>
     * Globally true literals are made true in the local model.<br>
     * Their flipScore is set to trueLiteralScore, an extremly small value.
     */
    protected void initializeModel() {
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            int status = model.status(predicate);
            if(status != 0) {
                localModel[predicate] = (status == 1);
                flipScores[predicate] = trueLiteralScore;}
            else {
                int posSize = literals.size(predicate);
                int negSize = literals.size(-predicate);
                if(posSize == negSize) localModel[predicate] = random.nextBoolean();
                else localModel[predicate] = (posSize >= negSize);}}}



    /** controls the search for a model.
     *
     * @return the result of the search
     */
    private Result walk() {
        while(statistics.flips < maxFlips) {
            if(Thread.interrupted()) {
              //  globalParameters.log("Walker " + combinedId + " interrupted after " + statistics.flips + " flips.\n");
                break;}
            integrateGloballyTrueLiterals();
            int predicate = selectFlipPredicate();
            if(monitoring) monitor.println(monitorId,"Flip " + predicate);
            flipPredicate(predicate);
            if(falseClauses == 0) {return localToGlobalModel();}}
        return new Aborted(null,solverId,"Walker aborted after " + statistics.flips + " flips");}


    private final ArrayList<Clause> globallyTrueClauses = new ArrayList<>();

    /** integrates globally true literals:
     * - their scores are minimized <br>
     * - clauses which now become globally true are marked<br>
     * Clauses with globally false literals are not touched.
     */
    private void integrateGloballyTrueLiterals() {}

    /** selects a predicate to be flipped.
     * The priorities are: <br>
     * 1. predicate with top score > 0
     * 2. predicate in false clause with score = 0. <br>
     *    The predicate must occur in another clause togehter with another predicate with score = 0. <br>
     *    This predicate is flipped first<br>
     * 3. predicate in false clause with rop score<br>
     * 4. randomly chosen predicate.<br>
     * Predicates in the flip history list are not chosen.
     * @return a predicate to be flipped next.
     */
    private int selectFlipPredicate() {
        /*
        int predicate = flipQueue.topItem();
        float topScore = flipQueue.scores[predicate];
        if(topScore > 0) return predicate;
        if(topScore == 0) {
            predicate = select0InFalseClause();
            if(predicate != 0) return predicate;}
        predicate = selectTopInFalseClauses();
        if(predicate != 0) return predicate;
        return selectRandomPredicate();*/
    return 0;}


    /** selects a predicate in a false clause with score = 0.
     *  The predicate must occur in another clause together with another predicate with score = 0. <br>
     *  This predicate is flipped first<br>
     *
     * @return 0 or the predicate to be flipped.
     */
    private int select0InFalseClause() {
        /*
        for(Clause Clause : falseClauses) {
            for(int literal : Clause.literals) {
                int predicate = Math.abs(literal);
                if(isInFlipHistory(predicate)) continue;
                if(flipQueue.scores[predicate] == 0 && flipOtherPredicate(Clause,predicate))
                    return predicate;}}*/
        return 0;}

    /** flips a predicate in another clause containing the selected predicate
     *
     * @param Clause   a false clause
     * @param predicate a predicate in this clause with score = 0.
     * @return true if a predicate has been found and flipped.
     */
    private boolean flipOtherPredicate(Clause Clause, int predicate) {
        return false;}

    /** selects the predicate in the false clauses with top score, and which is not in the flip history
     *
     * @return 0 or the predicate in the false clauses with top score, and which is not in the flip history
     */
    private int selectTopInFalseClauses() {
        return 0;}

    /** selects a predicate randomly among the 50% predicates with best scores.
     *
     * @return a randomly selected predicate
     */
    private int selectRandomPredicate() {
        /*
        int predicate = flipQueue.queue[random.nextInt(predicates/2)+1];
        int counter = 0;
        while(isInFlipHistory(predicate) && ++counter < 10) {
            predicate = flipQueue.queue[random.nextInt(predicates/2)+1];}*/
        return 0;}

    /** flips the truth value of the predicate and updates the flipQueue and the falseClauses list
     *
     * @param predicate to be flipped
     */
    void flipPredicate(int predicate) {
        /*++statistics.flips;
        addToFlipHistory(predicate);
        for(Clause clause : getClauses(predicate)) updateFlipScores(clause,-1);
        for(Clause clause : getClauses(-predicate)) updateFlipScores(clause,-1);

        localModel[predicate] = !localModel[predicate];

        for(int sign = -1; sign <= 1; sign += 2) {
            for(Clause clause : getClauses(sign*predicate)) {
                if(!clause.isLocallyTrue) removeFalseClause(clause);
                clause.isLocallyTrue = setLocalTruth(clause);
                if(!clause.isLocallyTrue) {addFalseClause(clause);}
                updateFlipScores(clause,1);}*/
        }


    /** turns the local model into a new model and returns Satisfiable as result
     *
     * @return Satisfiable with the transferred local model.
     */
    private Satisfiable localToGlobalModel() {
        Model model = new Model(predicates);
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            model.addImmediately(localModel[predicate] ? predicate : -predicate);}
        return new Satisfiable(null,null,model);}






    /** computes the truth value of a clause in the local (and global) model.
     * The truth value and the number of true literals is stored in the clause.<br>
     * False clauses are collected in falseClauseList.
     *
     * @param clause a clause.
     * @return true if the clause is true in the local (and global) model.
     */
    protected boolean setLocalTruth(Clause clause) {
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

    protected boolean isLocallyTrue(int literal) {
        return (literal > 0) ? localModel[literal] : !localModel[-literal];}

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

        // clause is false. Either there are not enough or too many true literals.
        for(Literal literalObject : clause.literals) {
            int literal = literalObject.literal;
            int newTrueLiterals = isLocallyTrue(literal) ? trueLiterals - literalObject.multiplicity : trueLiterals + literalObject.multiplicity;
            if(min <= newTrueLiterals && newTrueLiterals <= max) {
                literalObject.flipScorePart = 1;
                flipScores[Math.abs(literalObject.literal)] += 1;
                continue;}
            if(newTrueLiterals < min) {
                float score = (float)1. / (float)(min - newTrueLiterals + 1);
                literalObject.flipScorePart = -score; // true -> false brings the clause more away from truth.
                flipScores[Math.abs(literalObject.literal)] -= score;
                continue;}
            if(newTrueLiterals > max) {
                float score = (float)1. / (float)(newTrueLiterals - max + 1);
                literalObject.flipScorePart = -score; // true -> false brings the clause more away from truth.
                flipScores[Math.abs(literalObject.literal)] -= score;}}}


    protected void initializePredicatesWithPositiveScores() {
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            if(flipScores[predicate] > 0) predicatesWithPositiveScore.addToBack(predicate);}}


    /** updates the flip scores of the literals in the clauses containing the flipped literal.
     * A literal's score is changed if, by flipping it, it makes a true clause false or vice versa.
     *
     * @param literal the flipped literal
     */
    protected void updateFlipScores(int literal) {
        for(int sign = 1; sign >= -1; sign -= 2) {
            literal *= sign;
            Literal literalObject = literals.getFirstLiteralObject(literal);
            while(literalObject != null) {
                updateFlipScores(literalObject);
                literalObject = literalObject.nextLiteral;}}}

    protected void updateFlipScores(Literal flippedLiteralObject) {
        Clause clause = flippedLiteralObject.clause;
        int min = clause.min; int max = clause.max;
        int oldTrueLiterals = clause.trueLiterals;
        int trueLiterals = isLocallyTrue(flippedLiteralObject.literal) ? oldTrueLiterals+flippedLiteralObject.multiplicity:
                oldTrueLiterals-flippedLiteralObject.multiplicity;
        clause.trueLiterals = trueLiterals;
        boolean wasTrue = clause.isLocallyTrue;
        boolean isNowTrue = min <= trueLiterals && trueLiterals <= max;
        clause.isLocallyTrue = isNowTrue;
        if(wasTrue) {if(!isNowTrue) {falseClauseList.addToBack(clause); ++falseClauses;}}
        else        {if(isNowTrue)  {falseClauseList.remove(clause);    --falseClauses;}}

        if(isNowTrue) {
            if(min == max) { // all flips make the clause false.
                for(Literal literalObject : clause.literals) {
                    float scoreChange = -1 - literalObject.flipScorePart;
                    literalObject.flipScorePart += scoreChange;
                    int predicate = Math.abs(literalObject.literal);
                    flipScores[predicate] += scoreChange;
                    updatePredicatesWithPositiveScore(predicate);}
                return;}
            for(Literal literalObject : clause.literals) {
                int literal = literalObject.literal;
                int newTrueLiterals = isLocallyTrue(literal) ? trueLiterals - literalObject.multiplicity : trueLiterals + literalObject.multiplicity;
                if(newTrueLiterals < min || newTrueLiterals > max) {
                    float scoreChange = -1 - literalObject.flipScorePart;
                    literalObject.flipScorePart += scoreChange;
                    int predicate = Math.abs(literal);
                    flipScores[predicate] += scoreChange;
                    updatePredicatesWithPositiveScore(predicate);}}
            return;}

        // clause is false. Either there are not enough or too many true literals.
        for(Literal literalObject : clause.literals) {
            int literal = literalObject.literal;
            int newTrueLiterals = isLocallyTrue(literal) ? trueLiterals - literalObject.multiplicity : trueLiterals + literalObject.multiplicity;
            float scoreChange = 0;
            if(min <= newTrueLiterals && newTrueLiterals <= max) {
                scoreChange = 1 -literalObject.flipScorePart;}
            else {
                if(newTrueLiterals < min) {
                    float score = (float)1. / (float)(min - newTrueLiterals + 1);
                    scoreChange = score -literalObject.flipScorePart;}
                else {
                    if(newTrueLiterals > max) {
                        float score = (float)1. / (float)(newTrueLiterals - max + 1);
                        scoreChange = score -literalObject.flipScorePart;}}}
            int predicate = Math.abs(literal);
            literalObject.flipScorePart += scoreChange;
            flipScores[predicate] += scoreChange;
            updatePredicatesWithPositiveScore(predicate);}}

    protected void updatePredicatesWithPositiveScore(int predicate) {
        if(flipScores[predicate] > 0) {
            if(predicatesWithPositiveScore.predicates[predicate].predicate == 0) predicatesWithPositiveScore.addToBack(predicate);}
        else {if(predicatesWithPositiveScore.predicates[predicate].predicate != 0) predicatesWithPositiveScore.remove(predicate);}
    }






    /** gets all clauses containing the literal
     *
     * @param literal a literal
     * @return a list of all clauses containing the literal
     */
    private ArrayList<Clause> getClauses(int literal) {return null;}


    /** adds the flipped predicate to the flip history
     *
     * @param predicate the flipped predicate
     */
    private void addToFlipHistory(int predicate) {
        if(flipHistoryEnd < flipHistoryLength) {flipHistory[flipHistoryEnd++] = predicate; return;}
        flipHistory[flipHistoryIndex] = predicate;
        flipHistoryIndex = (flipHistoryIndex +1) % flipHistoryLength;}


    /** checks if a predicate is in the flip history
     *
     * @param predicate a predicate
     * @return true if the prediate is in the flip history.
     */
    private boolean isInFlipHistory(int predicate) {
        for(int i = 0; i < flipHistoryEnd; ++i) {
            if(predicate == flipHistory[i]) return true;}
        return false;}

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
            case "clauses":    return clausesToString(symboltable);
            case "literals":   return literals.toString(symboltable);
            case "predicates": return predicatesWithPositiveScore.toString(symboltable);
            case "flipscores": return flipScoresToString(symboltable);
            case "model":      return localModelToString(symboltable);
            case "statistic":  return statistics.toString();
        }
        return "Versions: clauses,literals,predicates,flipscores,model,statistic";
    }


    public String toString(Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        st.append("Random Walker ").append(solverId).append( " on Problem ").append(problemId).append("\n");
        st.append("Parameters:\n");
        st.append("  seed:           ").append(seed).append("\n");
        st.append("  flips:          ").append(statistics.flips).append(" of ").append(maxFlips).append("\n");
        st.append("  history: ").append(Arrays.toString(flipHistory)).append("\n\n");
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