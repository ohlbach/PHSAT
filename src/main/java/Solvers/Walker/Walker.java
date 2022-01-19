package Solvers.Walker;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.Connective;
import Datastructures.Results.Aborted;
import Datastructures.Results.Result;
import Datastructures.Results.Satisfiable;
import Datastructures.Statistics.Statistic;
import Datastructures.Symboltable;
import Datastructures.Theory.Model;
import Management.ProblemSupervisor;
import Solvers.Solver;
import Utilities.Utilities;
import Utilities.IntegerQueue;
import it.unimi.dsi.fastutil.ints.IntArrayList;

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
    private final int seed;                                  // for the random number generator
    private final Random random;                       // random number generator for flip jumps

    private final ArrayList<WClause> falseList = new ArrayList<>();

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
        predicateQueue.setScore(0,Short.MIN_VALUE);
        seed           = (int)solverParameters.get("seed");
        random         = new Random(seed);
        maxFlips       = (int)solverParameters.get("flips");
        jumpFrequency  = (int)solverParameters.get("jumps");
        statistics     = new WalkerStatistics(combinedId);
        wClauses       = new ArrayList<>();
        model.addObserver((literal,step) -> {
            synchronized(this) {globallyTrueLiterals.add((int)literal);}});}


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
        System.out.println("Result " + result);
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
            predicateQueue.setScore(Math.abs(literal),Short.MIN_VALUE);

            for(WClause wClause : getClauses(literal)) { // update global truth
                if(isGloballyTrue(wClause)) {
                    globallyTrueClauses.add(wClause);
                    if(!wClause.isLocallyTrue) --falseClauses;
                    wClause.isLocallyTrue = true;}}
            if(!isLocallyTrue(literal)) flipPredicate(Math.abs(literal));}
        for(WClause wClause : globallyTrueClauses) removeClause(wClause);}

    private int selectFlipPredicate() {
        return predicateQueue.topItem();}


    private WClause falseClause = null;

    /** flips the truth value of the predicate and updates the predicateQueue and the falseClauses counter
     *
     * @param predicate to be flipped
     */
    void flipPredicate(int predicate) {
        ++statistics.flips;
        for(WClause wClause : getClauses(predicate)) updateFlipScores(wClause,-1);
        for(WClause wClause : getClauses(-predicate)) updateFlipScores(wClause,-1);

        localModel[predicate] = !localModel[predicate];

        for(int sign = -1; sign <= 1; sign += 2) {
            for(WClause wClause : getClauses(sign*predicate)) {
                if(!wClause.isLocallyTrue) removeFalseClause(wClause);
                wClause.isLocallyTrue = getLocalTruthValue(wClause);
                if(!wClause.isLocallyTrue) {addFalseClause(wClause);}
                updateFlipScores(wClause,1);}}}


    /** turns the local model into a new model and returns Satisfiable as result
     *
     * @return Satisfiable with the transferred local model.
     */
    private Satisfiable localToGlobalModel() {
        Model model = new Model(predicates,this.model.symboltable);
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            model.addImmediately(localModel[predicate] ? predicate : -predicate);}
        return new Satisfiable(model);}

    /** Initializes the local model, the local and global truth values of the clauses, the falseClauses list, and the initial flip scores.
     * A predicate is made true if it makes more clauses true than making the predicate false.
     * The number of true clauses when making a predicate true is estimated by its initial score.
     * If the same number of clauses are made true/false the the truth value of the predicate is
     * selected randomly.
     * Global truth value are transferred unchanged to the local model.
     */
    protected void initializeModel() {
        int[] posScores = new int[predicates+1];
        int[] negScores = new int[predicates+1];
        initialScores(posScores,negScores);
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            int status = model.status(predicate);
            if(status != 0) {localModel[predicate] = (status == 1); continue;}
            int posScore = posScores[predicate];
            int negScore = negScores[predicate];
            if(posScore == negScore) localModel[predicate] = random.nextBoolean();
            else localModel[predicate] = (posScores[predicate] >= negScores[predicate]);}
        
        for(WClause wClause: wClauses) {
            if(getLocalTruthValue(wClause)) wClause.isLocallyTrue = true;
            else {addFalseClause(wClause); ++falseClauses;}
            updateFlipScores(wClause,1);}
        System.out.println("Initial False Clauses: " + falseList.size());}

    /** computes the initial predicate scores.
     * A contribution +1 of a literal l in a clause C means: making l true makes the entire clause C true.
     * This applies to clauses with minLimit = 1 and to clauses with multiplicity[l] >= minLimit.
     *
     * @param posScores for setting the scores to make a predicate true.
     * @param negScores for setting the scores to make a predicate false.
     */
    protected void initialScores(int[] posScores, int[] negScores) {
        for(WClause wClause: wClauses) {
            short minLimit = wClause.minLimit;
            if(minLimit == 0) continue;  // clause is already true
            short maxLimit = wClause.maxLimit;
            for(int position = 0; position < wClause.literals.length; ++position) {
                int literal = wClause.literals[position];
                short multiplicity = wClause.multiplicity(position);
                if(minLimit <= multiplicity && multiplicity <= maxLimit) {
                    if(literal > 0) ++posScores[literal];
                    else            ++negScores[-literal];}}}}



    /** checks if the clause is globally true in the current global model, and remains true.
     *  Counter example: [2,3] p,q,r,s.<br>
     *  If p,q,r are true, and s is undefined, then the clause is currently globally true.<br>
     *  If, however, s becomes true later on, the clause would become false.<br>
     *  In this case the method would return false.<br>
     *
     * @param wClause a clause
     * @return true if the clause is permanently globally true
     */
    protected boolean isGloballyTrue(WClause wClause) {
        if(model.isEmpty()) return false;
        if(wClause.connective == Connective.OR) {
            for(int literal : wClause.literals) {if(model.isTrue(literal)) return true;}
            return false;}
        int globallyTrueLiterals = 0;
        int globallyUndefinedLiterals = 0;
        for(int literal : wClause.literals) {
            switch(model.status(literal)) {
                case +1: ++globallyTrueLiterals; break;
                case 0:  ++globallyUndefinedLiterals;}}
        int min = wClause.minLimit;
        int max = wClause.maxLimit;
        if(globallyTrueLiterals < min || globallyTrueLiterals > max) return false;
        return globallyTrueLiterals + globallyUndefinedLiterals <= max - min + 1;}


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
        return wClause.minLimit <= trueLiterals && trueLiterals <= wClause.maxLimit;}

    /** updates the flip scores of the literals in the clauses containing the flipped literal.
     * A literal's score is changed if, by flipping it, it makes a true clause false or vice versa.
     *
     * @param wClause the clause to be updated
     * @param sign -1 (removing old score) +1 (adding new score)
     */
    protected void updateFlipScores(WClause wClause, int sign) {
        if(wClause.connective == Connective.OR) { // separate treatment is more efficient
            if(wClause.isLocallyTrue) {
                int trueLiteral = 0;
                for(int literal : wClause.literals) {
                    if(isLocallyTrue(literal)) {
                        if(trueLiteral != 0) return; // if there are two true literals, nothing changes by flipping
                        else trueLiteral = literal;}}
                if(trueLiteral != 0) {// flipping the single true literal makes the clause false.
                    predicateQueue.addScore(Math.abs(trueLiteral), -sign);
                    return;}}
            // all literals are false
            for(int literal : wClause.literals) predicateQueue.addScore(Math.abs(literal), sign);
            return;}

        short min = wClause.minLimit;
        short max = wClause.maxLimit;
        if(wClause.isLocallyTrue) {
            if (min == max) { // every flip makes the clause false
                for (int literal : wClause.literals) predicateQueue.addScore(Math.abs(literal), -sign);
                return;}
            // a flip can bring the number of true literals outside the range [min,max]
            short trueLiterals = currentlyTrueLiterals(wClause);
            for(int position = 0; position < wClause.literals.length; ++position) {
                int literal = wClause.literals[position];
                short newTrueLiterals = (short)(trueLiterals +
                        (isLocallyTrue(literal) ? -wClause.multiplicity(position) : wClause.multiplicity(position)));
                if((newTrueLiterals < min) || (newTrueLiterals > max)) // no longer true
                    predicateQueue.addScore(Math.abs(literal), -sign);}
            return;}
        // the clause is false now
        // a flip can bring the number of true literals into the range [min,max]
        short trueLiterals = currentlyTrueLiterals(wClause);
        for(int position = 0; position < wClause.literals.length; ++position) {
            int literal = wClause.literals[position];
            short newTrueLiterals = (short)(trueLiterals +
                    (isLocallyTrue(literal) ? -wClause.multiplicity(position) : wClause.multiplicity(position)));
            if((min <= newTrueLiterals) && (newTrueLiterals <= max)) // now true
                predicateQueue.addScore(Math.abs(literal), sign);}}


    /** counts the number of locally true literals in the clause, including multiplicities.
     * Example: p^2,q^3,r with true(p,q) yields 5.
     *
     * @param wClause a clause
     * @return the number of true literals in the clause, including multiplicities.
     */
    private short currentlyTrueLiterals(WClause wClause) {
        short trueLiterals = 0;
        for(int position = 0; position < wClause.literals.length; ++position) {
            int literal = wClause.literals[position];
            if(isLocallyTrue(literal)) trueLiterals += wClause.multiplicity(position);}
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

    /** adds a false clause to the falseClauses list and stores its position in the clause's position slot. 
     * 
     * @param falseClause a false clause
     */
    private void addFalseClause(WClause falseClause) {
        falseClause.position = falseList.size();
        falseList.add(falseClause);}

    /** removes a false clause from the list. The last clause in the list takes its position.
     * The operation is constant in time.
     * 
     * @param falseClause a clause to be removed.
     */
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