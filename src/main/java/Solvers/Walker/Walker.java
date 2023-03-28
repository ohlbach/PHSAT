package Solvers.Walker;

import Datastructures.Results.Aborted;
import Datastructures.Results.Result;
import Datastructures.Results.Satisfiable;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Statistics.Statistic;
import Datastructures.Symboltable;
import Datastructures.Theory.Model;
import Management.ErrorReporter;
import Management.ProblemSupervisor;
import Solvers.Simplifier.UnsatEmptyClause;
import Solvers.Solver;
import Utilities.IntegerQueue;
import Utilities.Utilities;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.*;
import java.util.function.IntConsumer;

public class Walker extends Solver {

    private ArrayList<Solver> workerSolvers = new ArrayList<>();

    private ArrayList<Clause> clauses;         // collects all clauses

    private Literals literals;
    protected boolean[] localModel;              // maps all predicates to a truth value
    private IntegerQueue flipQueue = null;
    private static final int jumpFrequencyDefault = 10;
    public  int jumpFrequency;                   // after jumpFrequency many flips, a random jump is inserted
    private static final int maxFlipsDefault = Integer.MAX_VALUE;
    public  int maxFlips;                        // maximum number of allowed flips
    public Statistics statistics;                // collects statistical information
    private final int seed;                                  // for the random number generator
    private  Random random;                       // random number generator for flip jumps
    private int idWidth = 0;

    private int flipHistoryLength = 5;
    private int[] flipHistory = new int[flipHistoryLength];
    private int flipHistoryIndex = 0;
    private int flipHistoryEnd = 0;

    protected int falseClauses = 0;

    private boolean monitoring = false;

    private String monitorId;

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
        flipQueue = new IntegerQueue(predicates+1);
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
            for(int[] inputClause    : inputClauses.atleasts)     insertClause(inputClause);
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
     * All clauses are stored with interval limits.
     * Complementary literals are removed.
     * Literals with multiplicities &gt; clause.max are declared false in the global model.
     *
     * @param inputClause    an input clause
     * @throws Unsatisfiable if the global model discovers a contradiction.
     */
    protected void insertClause(int[] inputClause) throws Unsatisfiable {
        Clause clause = new Clause(inputClause);
        if(!clause.removeComplementaryLiterals()) return;
        for(int i = 0; i < clause.literals.size(); ++i) {
            Literal literalObject = clause.literals.get(i);
            if(literalObject.multiplicity > clause.max) {
                model.add(-literalObject.literal,null);
                clause.expandedSize -= literalObject.multiplicity;
                clause.literals.remove(i--);}}
        if(clause.literals.isEmpty()) throw new UnsatEmptyClause(problemId,solverId,clause.id,clause.inferenceStep);
        for(Literal literalObject : clause.literals) {
            literals.addLiteral(literalObject);
            clauses.add(clause);}}


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
                flipQueue.setScore(predicate,Integer.MIN_VALUE/2);}
            else {
                int posSize = literals.size(predicate);
                int negSize = literals.size(-predicate);
                if(posSize == negSize) localModel[predicate] = random.nextBoolean();
                else localModel[predicate] = (posSize >= negSize);}}

        for(Clause clause: clauses) {
            if(!setLocalTruth(clause)) ++falseClauses;
            setInitialFlipScores(clause);}
        if(monitoring) monitor.println(monitorId, "Initial Data:\n" + toString());}




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
        int predicate = flipQueue.topItem();
        float topScore = flipQueue.scores[predicate];
        if(topScore > 0) return predicate;
        if(topScore == 0) {
            predicate = select0InFalseClause();
            if(predicate != 0) return predicate;}
        predicate = selectTopInFalseClauses();
        if(predicate != 0) return predicate;
        return selectRandomPredicate();}


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

    /** selects the predicate in the false clauses with top score, an which is not in the flip history
     *
     * @return 0 or the predicate in the false clauses with top score, an which is not in the flip history
     */
    private int selectTopInFalseClauses() {
        return 0;}

    /** selects a predicate randomly among the 50% predicates with best scores.
     *
     * @return a randomly selected predicate
     */
    private int selectRandomPredicate() {
        int predicate = flipQueue.queue[random.nextInt(predicates/2)+1];
        int counter = 0;
        while(isInFlipHistory(predicate) && ++counter < 10) {
            predicate = flipQueue.queue[random.nextInt(predicates/2)+1];}
        return predicate;}

    /** flips the truth value of the predicate and updates the flipQueue and the falseClauses list
     *
     * @param predicate to be flipped
     */
    void flipPredicate(int predicate) {
        ++statistics.flips;
        addToFlipHistory(predicate);
        for(Clause clause : getClauses(predicate)) updateFlipScores(clause,-1);
        for(Clause clause : getClauses(-predicate)) updateFlipScores(clause,-1);

        localModel[predicate] = !localModel[predicate];

        for(int sign = -1; sign <= 1; sign += 2) {
            for(Clause clause : getClauses(sign*predicate)) {
                if(!clause.isTrue) removeFalseClause(clause);
                clause.isTrue = setLocalTruth(clause);
                if(!clause.isTrue) {addFalseClause(clause);}
                updateFlipScores(clause,1);}}}


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
     * The truth value and the number of true literals is stored in the clause.
     *
     * @param clause a clause
     * @return true if the clause is true in the local (and global) model
     */
    protected boolean setLocalTruth(Clause clause) {
        int trueLiterals = 0;
        for(Literal literalObject : clause.literals) {
            int literal = literalObject.literal;
            if(literal > 0)  {
                 if(localModel[literal])  trueLiterals += literalObject.multiplicity;}
            else if(localModel[-literal]) trueLiterals += literalObject.multiplicity;}
        clause.trueLiterals = trueLiterals;
        boolean isTrue = clause.min <= trueLiterals && trueLiterals <= clause.max;
        clause.isTrue = isTrue;
        return isTrue;}

    protected void setInitialFlipScores(Clause clause) {
        float flipScore = 0;
        int trueLiterals = clause.trueLiterals;
        if(clause.isTrue) {
            for(Literal literalObject : clause.literals) {
                int literal = literalObject.literal;
                flipScore = setLocalTruth(literal) ?
                        ((trueLiterals == clause.min) ? -1 : 0) :
                        ((trueLiterals == clause.max) ? -1 : 0);
                literalObject.flipScorePart = flipScore;
                flipQueue.addScore(Math.abs(literal),flipScore);}}
        else {
            for(Literal literalObject : clause.literals) {
                int literal = literalObject.literal;
                if(trueLiterals < clause.min) {
                    flipScore = setLocalTruth(literal) ? -1 / (float)(clause.min - trueLiterals) :
                            1/ (float)(clause.min - trueLiterals);}
                if(trueLiterals > clause.max) {
                    flipScore = setLocalTruth(literal) ? -1 / (float)(trueLiterals - clause.max) :
                            1/ (float)(trueLiterals - clause.max);}
                literalObject.flipScorePart = flipScore;
                flipQueue.addScore(Math.abs(literal),flipScore);}}
    }



    /** updates the flip scores of the literals in the clauses containing the flipped literal.
     * A literal's score is changed if, by flipping it, it makes a true clause false or vice versa.
     *
     * @param Clause the clause to be updated
     * @param sign -1 (removing old score) +1 (adding new score)
     */
    protected void updateFlipScores(Clause Clause, int sign) {}


    /** counts the number of locally true literals in the clause, including multiplicities.
     * Example: p^2,q^3,r with true(p,q) yields 5.
     *
     * @param Clause a clause
     * @return the number of true literals in the clause, including multiplicities.
     */
    private short currentlyTrueLiterals(Clause Clause) {
        return 0;}

    /** returns true if the literal is true in the local model
     *
     * @param literal a literal
     * @return true if the literal is true in the local model
     */
    private boolean setLocalTruth(int literal) {
        return (literal > 0) ? localModel[literal] : !localModel[-literal];}

    /** adds the clause to the posOccurrences and negOccurrences
     *
     * @param Clause a new clause
     */
    private void addToIndex(Clause Clause) {}

    /** removes the claus from posOccurrences and negOccurrences
     *
     * @param Clause a clause to be removed.
     */
    private void removeFromIndex(Clause Clause) {}

    /** adds a false clause to the falseClauses list and stores its position in the clause's position slot. 
     * 
     * @param falseClause a false clause
     */
    private void addFalseClause(Clause falseClause) {}

    /** removes a false clause from the list. The last clause in the list takes its position.
     * The operation is constant in time.
     * 
     * @param falseClause a clause to be removed.
     */
    private void removeFalseClause(Clause falseClause) {}

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

    /** turns the flips scores to a string.
     *
     * @return the flip scores as a string.
     */
    public String flipScoresToString() {
        return flipQueue.toString();}

    public String toString() {
        return toString(null);}

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
}