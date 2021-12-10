package Solvers.Walker;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.Connective;
import Datastructures.Results.Aborted;
import Datastructures.Results.Result;
import Datastructures.Results.Satisfiable;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Statistics.Statistic;
import Datastructures.Symboltable;
import Datastructures.Theory.Model;
import Management.ProblemSupervisor;
import Solvers.Solver;
import Utilities.Utilities;
import Utilities.IntegerQueue;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import javafx.scene.web.WebView;

import java.math.MathContext;
import java.util.*;
import java.util.function.IntConsumer;

public class Walker extends Solver {

    private final ArrayList<WClause> wClauses;         // collects all clauses
    private final ArrayList<WClause>[] posOccurrences; // maps predicate to clauses containing them positively
    private final ArrayList<WClause>[] negOccurrences; // maps predicate to clauses containing them negatively
    protected final boolean[] localModel;              // maps all predicates to a truth value
    protected int falseClauses = 0;                    // counts the clauses which are false in the model
    private final IntegerQueue predicateQueue;         // sorts the predicates according to the flipScore.
                                                       // predicates whose flip makes more clauses true come to the front
    private static final int jumpFrequencyDefault = 10;
    public  int jumpFrequency;                   // after jumpFrequency many flips, a random jump is inserted
    private static final int maxFlipsDefault = Integer.MAX_VALUE;
    public  int maxFlips;                        // maximum number of allowed flips
    public WalkerStatistics statistics;                // collects statistical information
    private int seed;                                  // for the random number generator
    private Random random;                       // random number generator for flip jumps

    private ArrayList<WClause> falseList = new ArrayList<>();

    public static String help() {
        return "Random Walker: parameters:\n" +
                "seed:   for the random number generator      (default: 0)\n" +
                "flips:  for restricting the number of flips  (default: Max_Integer).\n" +
                "jumps:  frequency of random jumps            (default: 10)\n";}

    private static final HashSet<String> keys = new HashSet<>(); // contains the allowed keys in the specification.
    static { // these are the allowed keys in the specification.
        Collections.addAll(keys, "name", "seed", "flips", "jumps", "type", "solver");}

    /** parses a HashMap with key-value pairs<br>
     *
     * @param parameters  the parameters with the keys "seed", "flips", "jumps"
     * @param errors      for error messages
     * @param warnings    for warnings
     * @return            a list of HashMaps with these keys.
     */
    public static ArrayList<HashMap<String,Object>> parseParameters(HashMap<String,String> parameters, StringBuilder errors, StringBuilder warnings){
        for(String key : parameters.keySet()) {
            if(!keys.contains(key)) {
                warnings.append("Walker: unknown key in parameters: " + key + "\n" +
                                "        allowed keys: seed, flips, jumps.\n");}}
        ArrayList<HashMap<String,Object>> list = new ArrayList<>();
        String seeds = parameters.get("seed");
        if(seeds == null) seeds = "0";
        String flips = parameters.get("flips");
        if(flips == null) flips = Integer.toString(maxFlipsDefault);
        String jumps = parameters.get("jumps");
        if(jumps == null) jumps = Integer.toString(jumpFrequencyDefault);

        String place = "Walker: ";

        ArrayList seed = Utilities.parseIntRange(place+"seed: ",seeds,errors);
        ArrayList flip = Utilities.parseIntRange(place+"flips: ",flips,errors);
        ArrayList jump = Utilities.parseIntRange(place+"jumps: ",jumps,errors);
        ArrayList<ArrayList> pars = Utilities.crossProduct(seed,flip,jump);
        int counter = 0;
        for(ArrayList<Object> p : pars ) {
            HashMap<String,Object> map = new HashMap<>();
            map.put("seed",p.get(0));
            map.put("flips",p.get(1));
            map.put("jumps",p.get(2));
            map.put("name","W" + ++counter);
            list.add(map);}
        return list;}


    /** constructs a new Walker solver.
     *
     * @param solverNumber         for distinguishing different solvers of the same type, but different parameters
     * @param solverParameters     contains the parameters for controlling the solver
     * @param problemSupervisor    coordinates several solvers.
     */
    public Walker(Integer solverNumber, HashMap<String,Object> solverParameters, ProblemSupervisor problemSupervisor) {
        super(solverNumber,solverParameters, problemSupervisor);
        super.initialize();
        posOccurrences = new ArrayList[predicates+1];
        negOccurrences = new ArrayList[predicates+1];
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            posOccurrences[predicate] = new ArrayList<>();
            negOccurrences[predicate] = new ArrayList<>();}
        localModel     = new boolean[predicates+1];
        predicateQueue = new IntegerQueue(predicates);
        predicateQueue.setScore(0,Integer.MIN_VALUE);
        seed           = (int)solverParameters.get("seed");
        random         = new Random(seed);
        maxFlips       = (int)solverParameters.get("flips");
        jumpFrequency  = (int)solverParameters.get("jumps");
        statistics     = new WalkerStatistics(combinedId);
        wClauses       = new ArrayList<>();
        model.addObserver(Thread.currentThread(),(literal,step) -> {
            synchronized(this) {globallyTrueLiterals.add((int)literal);}});}


    /** collects globally true literals which are inserted by other solvers  into the global model */
    private final IntArrayList globallyTrueLiterals = new IntArrayList();

    /** a temporary local copy of the globally true literals */
    private final IntArrayList globallyTrueLiteralsCopy = new IntArrayList();

    /** copies the globally true literals to a local copy, such that other thread could fill it anew
     *
     * @return a copy of the globally true literals
     */
    private synchronized IntArrayList copyGloballyTrueLiterals() {
        globallyTrueLiteralsCopy.clear();
        globallyTrueLiterals.forEach((IntConsumer) globallyTrueLiteralsCopy::add);
        globallyTrueLiterals.clear();
        return globallyTrueLiteralsCopy;}

    /** transforms a clause to a WClause and adds it to the internal data
     *
     * @param clause a clause
     * @return the generated wClause
     */
    public WClause addClause(Clause clause) {
        WClause wClause = new WClause(clause);
        if(isGloballyTrue(wClause)) return null;
        wClauses.add(wClause);
        addToIndex(wClause);
        return wClause;}

    /** removes a wClause from the internal lists
     *
     * @param wClause a clause to be removed
     */
    private void removeClause(WClause wClause) {
        wClauses.remove(wClause);
        removeFromIndex(wClause);}

    /** starts the solver
     *
     * @return the result of the solver
     */
    @Override
    public Result solve() {
        globalParameters.log(solverId + " for problem " + problemId + " started");
        long time = System.nanoTime();
        initializeModel();
        Result result =  walk();
        statistics.elapsedTime = System.nanoTime() - time;
        System.out.println("Result " + result.toString());
        System.out.println("Time " + statistics.elapsedTime);
        System.out.println("Flips " + statistics.flips);
        //problemSupervisor.finished(this, result, "done");
        //globalParameters.log(solverId + " for problem " + problemId + " finished");
        return result;}


    /** controls the search for a model.
     *
     * @return the result of the search
     */
    private Result walk() {
        while(statistics.flips < maxFlips) {
            if(Thread.interrupted()) {
                globalParameters.log("Walker " + combinedId + " interrupted after " + statistics.flips + " flips.\n");
                break;}
            integrateGloballyTrueLiterals();
            int predicate = selectFlipPredicate();
            flipPredicate(predicate);
            if(falseList.size() == 0) {return localToGlobalModel();}}
        return new Aborted("Walker aborted after " + statistics.flips + " flips");}


    private final ArrayList<WClause> globallyTrueClauses = new ArrayList<>();

    /** integrates globally true literals:
     * - their scores are minimized <br>
     * - clauses which now become globally true are marked<br>
     * Clauses with globally false literals are not touched.
     */
    private void integrateGloballyTrueLiterals() {
        globallyTrueClauses.clear();
        for(int literal : copyGloballyTrueLiterals()) {
            ++statistics.importedTrueLiterals;
            predicateQueue.setScore(Math.abs(literal),Integer.MIN_VALUE);

            for(WClause wClause : getClauses(literal)) { // update global truth
                if(isGloballyTrue(wClause)) {
                    globallyTrueClauses.add(wClause);
                    if(!wClause.isLocallyTrue) --falseClauses;
                    wClause.isLocallyTrue = true;}}
            if(!isLocallyTrue(literal)) flipPredicate(Math.abs(literal));}
        for(WClause wClause : globallyTrueClauses) removeClause(wClause);}

    private WClause falseClause = null;

    /** flips the truth value of the predicate and updates the predicateQueue and the falseClauses counter
     *
     * @param predicate to be flipped
     */
    void flipPredicateOld(int predicate) {
        ++statistics.flips;

        localModel[predicate] = !localModel[predicate];

        for(WClause wClause : getClauses(predicate)) {
            if(!wClause.isLocallyTrue) removeFalseClause(wClause);
            wClause.isLocallyTrue = getLocalTruthValue(wClause);
            if(!wClause.isLocallyTrue) addFalseClause(wClause);}

        for(WClause wClause : getClauses(-predicate)) {
            if(!wClause.isLocallyTrue) removeFalseClause(wClause);
            wClause.isLocallyTrue = getLocalTruthValue(wClause);
            if(!wClause.isLocallyTrue) addFalseClause(wClause);}
        if(print) System.out.println(predicate + " " + statistics.flips + " " + falseList.size());
           }

    void flipPredicate(int predicate) {
        ++statistics.flips;

        for(WClause wClause : getClauses(predicate)) updateFlipScores(wClause,-1);
        for(WClause wClause : getClauses(-predicate)) updateFlipScores(wClause,-1);

        localModel[predicate] = !localModel[predicate];

        for(WClause wClause : getClauses(predicate)) {
            if(!wClause.isLocallyTrue) removeFalseClause(wClause);
            wClause.isLocallyTrue = getLocalTruthValue(wClause);
            if(!wClause.isLocallyTrue) {addFalseClause(wClause);}
            updateFlipScores(wClause,1);}

        for(WClause wClause : getClauses(-predicate)) {
            if(!wClause.isLocallyTrue) removeFalseClause(wClause);
            wClause.isLocallyTrue = getLocalTruthValue(wClause);
            if(!wClause.isLocallyTrue) {addFalseClause(wClause);}
            updateFlipScores(wClause,1);}}


    /** turns the local model into a new model and returns Satisfiable as result
     *
     * @return Satisfiable with the transferred local model.
     */
    private Satisfiable localToGlobalModel() {
        Model model = new Model(predicates,this.model.symboltable);
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            model.addImmediately(localModel[predicate] ? predicate : -predicate);}
        return new Satisfiable(model);}

    /** Initializes the local model, together with the local and global truth values of the clauses.
     * A predicate is made true if it makes more clauses true than making the predicate false.
     * The number of true clauses when making a predicate true is estimated by its initial score.
     * Global truth value are transferred unchanged to the local model.
     */
    protected void initializeModelOld() {
        float[] posScores = new float[predicates+1];
        float[] negScores = new float[predicates+1];
        setInitialScores(posScores,negScores);
        //System.out.println("P "+Arrays.toString(posScores) );
        //System.out.println("N "+Arrays.toString(negScores) );
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            int status = model.status(predicate);
            if(status != 0) {localModel[predicate] = (status == 1); continue;}
            float posScore = posScores[predicate];
            float negScore = negScores[predicate];
            if(posScore == negScore) localModel[predicate] = random.nextBoolean();
            else localModel[predicate] = (posScores[predicate] >= negScores[predicate]);
            //localModel[predicate] = (posScores[predicate] >= negScores[predicate]);
        }

        falseClauses = 0;
        for(WClause wClause: wClauses) {
            if(getLocalTruthValue(wClause)) wClause.isLocallyTrue = true;
            else addFalseClause(wClause);}
        System.out.println("Initial False Clauses: " + falseList.size());}

    protected void initializeModel() {
        float[] posScores = new float[predicates+1];
        float[] negScores = new float[predicates+1];
        setInitialScores(posScores,negScores);
        //System.out.println("P "+Arrays.toString(posScores) );
        //System.out.println("N "+Arrays.toString(negScores) );
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            int status = model.status(predicate);
            if(status != 0) {localModel[predicate] = (status == 1); continue;}
            float posScore = posScores[predicate];
            float negScore = negScores[predicate];
            if(posScore == negScore) localModel[predicate] = random.nextBoolean();
            else localModel[predicate] = (posScores[predicate] >= negScores[predicate]);
        }
        for(WClause wClause: wClauses) {
            if(getLocalTruthValue(wClause)) wClause.isLocallyTrue = true;
            else addFalseClause(wClause);
            updateFlipScores(wClause,1);}
        System.out.println("Initial False Clauses: " + falseList.size());}

    /** computes the initial predicate scores
     * The higher the score of a predicate the more likely it is to make clauses true.
     * The score contribution for a clause counts<br>
     * how many predicates must be made true/false in order to make the clause true.<br>
     * 1 predicate:  score += 1<br>
     * 2 predicates: score += 1/2<br>
     * etc. <br>
     * If the literal is positive, then the score is added to posScores, otherwise to negScores.
     * <br>
     * For clauses [0,max] literals  the key factor is literals.length - max. <br>
     * So many literals must be made false, in order to remain within the bounds [0,max]<br>
     * Example: [0,2] p,q,r,s,t<br>
     * Atleast 5-2 = 3 literals must be made false, otherwise more than 2 literals could become true.
     *
     * @param posScores for setting the scores to make a predicate true.
     * @param negScores for setting the scores to make a predicate false.
     */
    protected void setInitialScores(float[] posScores,float[] negScores) {
        for(WClause wClause: wClauses) {
            int min = wClause.min;
            float one = (float)1;
            if(min > 0) {
                float score = one/min;
                for(int literal : wClause.literals) {
                    if(literal > 0) posScores[literal] += score;
                    else negScores[-literal] += score;}}
            else {
                float score = one/(wClause.literals.length-wClause.max); // can't be 1/0
                for(int literal : wClause.literals) {
                    if(literal > 0) negScores[literal] += score;
                    else posScores[-literal] += score;}}}}

    /** checks if the clause is globally true in the current global model, and remains true.
     *  Counter example: [2,3] p,q,r,s.<br>
     *  If p,q,r are true, and s is undefined, then the clause is currently globally true.<br>
     *  If, however, s becomes true later on, the clause would become false.<br>
     *  In this case the method would return false.<br>
     *
     * @param wClause a claus
     * @return true if the clause is permanently globally true
     */
    protected boolean isGloballyTrue(WClause wClause) {
        if(model.isEmpty()) return false;
        int globallyTrueLiterals = 0;
        int globallyUndefinedLiterals = 0;
        for(int literal : wClause.literals) {
            switch(model.status(literal)) {
                case +1: ++globallyTrueLiterals; break;
                case 0:  ++globallyUndefinedLiterals;}}
        int min = wClause.min;
        int max = wClause.max;
        if(globallyTrueLiterals < min || globallyTrueLiterals > max) return false;
        if(globallyUndefinedLiterals >= wClause.literals.length - max) return false;
        return true;}


    /** computes the truth value of a clause in the local (and global) model
     *
     * @param wClause a clause
     * @return true if the clause is true in the local (and global) model
     */
    protected boolean getLocalTruthValue(WClause wClause) {
        if(wClause.connective == Connective.OR) {
            for(int literal : wClause.literals) {
                if(isLocallyTrue(literal)) {return true;} }
            return false;}

        int trueLiterals = currentlyTrueLiterals(wClause);
        return wClause.min <= trueLiterals && trueLiterals <= wClause.max;}

    /** updates the flip scores of the literals in the clauses containing the flipped literal.
     *
     * @param wClause the clause to be updated
     * @param sign -1 (removing old score) +1 (adding new score)
     */
    protected void updateFlipScores(WClause wClause, int sign) {
        int min = wClause.min;
        int max = wClause.max;
        int trueLiterals = 0;
        for(int literal : wClause.literals) {
            if(isLocallyTrue(literal)) ++trueLiterals;}
        if(wClause.isLocallyTrue) {
            if (min == max) { // every flip makes the clause false
                for (int literal : wClause.literals) predicateQueue.addScore(Math.abs(literal), -sign);
                return;}
            if(trueLiterals == min) { // flipping a true literal -> not enough true literals any more
                for (int literal : wClause.literals) { // flipping a false literal changes nothing
                    if (isLocallyTrue(literal)) predicateQueue.addScore(Math.abs(literal), -sign);}
                return;}
            if(trueLiterals == max) { // flipping a false literal -> too many true literals
                for (int literal : wClause.literals) { // flipping a true literal changes nothing
                    if (!isLocallyTrue(literal)) predicateQueue.addScore(Math.abs(literal), -sign);}
                return;}
            return;}

        // clause is locally false now
        if(trueLiterals < min) { // not enough true literals
            float d = min - trueLiterals;          // so many false literals must be made true
            for (int literal : wClause.literals) {  // flipping a true literal makes the clause even 'more false'
                float scoreDiff = (float)sign/d * (isLocallyTrue(literal) ? (float)-1 : (float)1);
                predicateQueue.addScore(Math.abs(literal), scoreDiff);}
            return;}
        if(trueLiterals > max) {  // too many true literals
            float d = trueLiterals - max;          // so many true literals must be made false
            for (int literal : wClause.literals) { // flipping a false literal makes the clause even 'more false'
                float scoreDiff = (float)sign/d * (isLocallyTrue(literal) ? 1 : -1);
                predicateQueue.addScore(Math.abs(literal), scoreDiff);}}
    }


    /** selects the next flip predicate.
     * Normally the next flip predicate is the top of the predicateQueue.
     * Only if the same flip predicate has been selected the last or second but last time,
     * it is the second flip predicate in the predicateQueue.
     *
     * If the flips has reached a multiple of the jumpFrequency, then a random predicate is chosen.
     * @return the next flip predicate.
     */
    int selectFlipPredicateRandom() {
        return (statistics.flips % jumpFrequency == 0) ?
            predicateQueue.getRandom(random,exponent,jumpDistance) :
            predicateQueue.topScore();}

    public int exponent = 3;
    public boolean blocked = true;
    public int threshold = 5;
    public boolean print = false;
    public int jumpDistance = 50;

    int constantFalseClausesCounter = -1;
    int lastFalseClauses = 0;
    int zeroScoreCounter = 0;

    int selectFlipPredicateOld2() {
        int[] literals = falseList.get(random.nextInt(falseList.size())).literals;
        return Math.abs(literals[random.nextInt(literals.length)]);}

    int selectFlipPredicate() {
        int predicate = blocked ? predicateQueue.getTopItem(threshold) : predicateQueue.topScore();
        float score = predicateQueue.scores[predicate];
        if(score <= 0.0 && falseList.size() < 3) {
            int[] literals = falseList.get(random.nextInt(falseList.size())).literals;
            predicate = Math.abs(literals[random.nextInt(literals.length)]);}
        if(print) System.out.println("S " + predicate + " " + statistics.flips + " " + falseList.size() + " " + predicateQueue.scores[predicate]);
        if(print && falseList.size() == 1) {
            for(WClause cl : falseList)
            System.out.println("FALSE " + cl.toString());}

        return predicate;}

    int selectFlipPredicateOld() {
        if(falseClauses == lastFalseClauses) ++constantFalseClausesCounter;
        else {constantFalseClausesCounter = 0; lastFalseClauses = falseClauses;}
        int predicate;
        if(constantFalseClausesCounter > 0 && constantFalseClausesCounter % jumpFrequency == 0) {
            System.out.println("jump");
            predicate = predicateQueue.getRandom(random,exponent,jumpDistance);}
        else {
            if(falseClauses == 1) {predicate = selectFromFalseClause();}
            else {predicate = blocked ? predicateQueue.getTopItem(threshold) : predicateQueue.topScore();}}
        if(print) System.out.println(predicate + " " + falseClauses + " " + predicateQueue.scores[predicate] +
                " " +(falseClause == null ? "": falseClause.toString()));
        return predicate;}


    private int selectFromFalseClause() {
        return Math.abs(falseClause.nextLiteral());}

    private int selectFromFalseClauseOld() {
        int[] literals = falseClause.literals;
        int literal = literals[0];
        float score = predicateQueue.scores[Math.abs(literal)];
        boolean changes = false;
        for(int i = 1; i < literals.length; ++i) {
            int literali = literals[i];
            float scorei = predicateQueue.scores[Math.abs(literali)];
            if(scorei > score) {score = scorei; changes = true; literal = literali; continue;}
            if(scorei < score) {changes = true;}}
        return Math.abs(changes ? literal : falseClause.nextLiteral());}



    /** counts the number of locally true literals in the clause
         *
         * @param wClause a clause
         * @return the number of true literals in the clause.
         */
    private int currentlyTrueLiterals(WClause wClause) {
        int trueLiterals = 0;
        for(int literal : wClause.literals) {if(isLocallyTrue(literal)) ++trueLiterals;}
        return trueLiterals;}

    /** returns true if the literal is true in the local model
     *
     * @param literal a literal
     * @return true if the literal is true in the local model
     */
    private boolean isLocallyTrue(int literal) {
        return (literal > 0) ? localModel[literal] : !localModel[-literal];}

    /** adds the clause to the posOccurrences and negOccurrences
     *
     * @param wClause a new clause
     */
    private void addToIndex(WClause wClause) {
        for(int literal : wClause.literals) {
            if(literal > 0) posOccurrences[literal].add(wClause);
            else            posOccurrences[-literal].add(wClause);}}

    /** removes the claus from posOccurrences and negOccurrences
     *
     * @param wClause a clause to be removed.
     */
    private void removeFromIndex(WClause wClause) {
        for(int literal : wClause.literals) {
            if(literal > 0) posOccurrences[literal].remove(wClause);
            else            posOccurrences[-literal].remove(wClause);}}

    private void addFalseClause(WClause falseClause) {
        int position = falseList.size();
        falseClause.position = position;
        falseList.add(falseClause);}

    private void removeFalseClause(WClause falseClause) {
        int position = falseClause.position;
        if(position < 0) return;
        int lastPosition = falseList.size()-1;
        if(position < lastPosition) {
            WClause lastClause = falseList.get(lastPosition);
            falseList.set(position,lastClause);
            lastClause.position = position;}
        falseClause.position = -1;
        falseList.remove(lastPosition);}

    /** gets all clauses containing the literal
     *
     * @param literal a literal
     * @return a list of all clauses containing the literal
     */
    private ArrayList<WClause> getClauses(int literal) {
        return (literal > 0) ? posOccurrences[literal] : negOccurrences[-literal];}

    @Override
    public Statistic getStatistics() {
        return statistics;}

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
        return predicateQueue.toString();}

    public String toString() {
        return toString(null);}

    public String toString(Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        st.append("Random Walker ").append(solverId).append( " on Problem ").append(problemId).append("\n");
        st.append("Parameters:\n");
        st.append("  seed:           ").append(seed).append("\n");
        st.append("  flips:          ").append(statistics.flips).append(" of ").append(maxFlips).append("\n");
        st.append("  jump frequency: ").append(jumpFrequency).append("\n\n");
        st.append("Current model: ").append(localModelToString(symboltable)).append("\n");
        st.append("False Clauses: " + falseClauses + "\n");
        for(WClause wClause : wClauses) {if(!wClause.isLocallyTrue) st.append(wClause.toString(3,symboltable)).append("\n");}
    return st.toString();
    }
}