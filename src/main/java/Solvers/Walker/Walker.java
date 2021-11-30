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

import java.util.*;

public class Walker extends Solver {

    private final ArrayList<WClause> wClauses;         // collects all clauses
    private final ArrayList<WClause>[] posOccurrences; // maps predicate to clauses containing them positively
    private final ArrayList<WClause>[] negOccurrences; // maps predicate to clauses containing them negatively
    protected final boolean[] localModel;              // maps all predicates to a truth value
    protected int falseClauses = 0;                    // counts the clauses which are false in the model
    private final IntegerQueue predicateQueue;         // sorts the predicates according to the flipScore.
                                                       // predicates whose flip makes more clauses true come to the front
    private static final int jumpFrequencyDefault = 10;
    private final int jumpFrequency;                   // after jumpFrequency many flips, a random jump is inserted
    private static final int maxFlipsDefault = Integer.MAX_VALUE;
    private final int maxFlips;                        // maximum number of allowed flips
    public WalkerStatistics statistics;                // collects statistical information
    private int seed;                                  // for the random number genwrator
    private final Random random;                       // random number generator for flip jumps

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
        seed           = (int)solverParameters.get("seed");
        random         = new Random(seed);
        maxFlips       = (int)solverParameters.get("flips");
        jumpFrequency  = (int)solverParameters.get("jumps");
        statistics     = new WalkerStatistics(combinedId);
        wClauses       = new ArrayList<>();}

    /** collects the quantifiers, in order not to have fractional score values */
    private final IntArrayList quantifiers = new IntArrayList();

    /** transforms a clause to a WClause and adds it to the internal data
     *
     * @param clause a clause
     * @return the generated wClause
     */
    public WClause addClause(Clause clause) {
        WClause wClause = new WClause(clause);
        wClauses.add(wClause);
        addWClauseToIndex(wClause);
        if(!quantifiers.contains(wClause.quantifier)) quantifiers.add(wClause.quantifier);
        return wClause;}

    /** collects globally true literals which are inserted by other solvers  into the global model */
    private final IntArrayList globallyTrueLiterals = new IntArrayList();

    /** starts the solver
     *
     * @return the result of the solver
     */
    @Override
    public Result solve() {
        globalParameters.log(solverId + " for problem " + problemId + " started");
        long time = System.nanoTime();
        initializeModel();
        model.addObserver(Thread.currentThread(),(literal,step) -> {
                synchronized(this) {globallyTrueLiterals.add((int)literal);}});
        Result result =  walk();
        statistics.elapsedTime = System.nanoTime() - time;
        problemSupervisor.finished(this, result, "done");
        globalParameters.log(solverId + " for problem " + problemId + " finished");
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
            try {if(!globallyTrueLiterals.isEmpty()) integrateGloballyTrueLiterals();}
            catch(Unsatisfiable unsatisfiable) {return unsatisfiable;}
            int predicate = selectFlipPredicate();
            flipPredicate(predicate);
            if(falseClauses == 0) {return localToGlobalModel();}}
        return new Aborted("Walker aborted after " + statistics.flips + " flips");}

    /** a temporary local copy of the globally true literals */
    private final IntArrayList globallyTrueLiteralsCopy = new IntArrayList();

    /** integrates globally true literals:
     * - their scores are minimized <br>
     * - clauses which now become globally true are marked<br>
     * Clauses with globally false literals are not touched.
     *
     * @throws Unsatisfiable if the clause can never be made true (should actually never happen here)
     */
    private void integrateGloballyTrueLiterals() throws Unsatisfiable {
        globallyTrueLiteralsCopy.clear();
        synchronized (this) { // copy to local array in order not to hinder other threads for too long
            for(int literal : globallyTrueLiterals) globallyTrueLiteralsCopy.add(literal);
            globallyTrueLiterals.clear();} // can now be filled again

        for(int literal : globallyTrueLiteralsCopy) {
            ++statistics.importedTrueLiterals;
            predicateQueue.setScore(Math.abs(literal),Integer.MIN_VALUE / 2);

            for(WClause wClause : getClauses(literal)) { // update global truth
                if(getGlobalTruthValue(wClause)) {
                    if(!wClause.isLocallyTrue) --falseClauses;
                    wClause.isGloballyTrue = true;
                    wClause.isLocallyTrue = true;}}
            if(!isLocallyTrue(literal)) flipPredicate(Math.abs(literal));}}


    /** flips the truth value of the predicate and updates the predicateQueue and the falseClauses counter
     *
     * @param predicate to be flipped
     */
    void flipPredicate(int predicate) {
        ++statistics.flips;
        localModel[predicate] = !localModel[predicate];

        for(WClause wClause : getClauses(predicate)) {
            if(!wClause.isLocallyTrue) --falseClauses;
            wClause.isLocallyTrue = getLocalTruthValue(wClause);
            if(!wClause.isLocallyTrue) ++falseClauses;}

        for(WClause wClause : getClauses(-predicate)) {
            if(!wClause.isLocallyTrue) --falseClauses;
            wClause.isLocallyTrue = getLocalTruthValue(wClause);
            if(!wClause.isLocallyTrue) ++falseClauses;}

        updateFlipScores(predicate);}

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
    protected void initializeModel() {
        int[] posScores = new int[predicates+1];
        int[] negScores = new int[predicates+1];
        setInitialScores(posScores,negScores);
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            int status = model.status(predicate);
            if(status != 0) {localModel[predicate] = (status == 1); continue;}
            localModel[predicate] = (posScores[predicate] >= negScores[predicate]);}

        for(WClause wClause: wClauses) {
            if(!setInitialTruthValue(wClause)) ++falseClauses;
            setInitialFlipScores(wClause);}
    }

    /** computes the initial predicate scores
     * The higher the score of a predicate the more likely it is to make clauses true.
     * If there are no numeric clause types then
     * posScore[5] = 10 means that it is very likely to make 10 clauses true, if the predicate is made true.<br>
     * For numeric clause types truth does not depend on one literal. <br>
     * Example: atleast 2 p,q,r <br>
     *  In this case two predicates must be made true. <br>
     *  Therefore the score of p, q, and r is incremented by 0.5.<br>
     * Example: atmost 2 p,q,r <br>
     * In this case making a predicate true decrements the chance to make the clause true. <br>
     * Therefore the score of p, q, and r is decremented by 0.5.
     * <br>
     * Example: exactly 2 p,q,r <br>
     * In this case making a predicate true or false has the same chance to make the clause true or false.
     * The score is therefore not changed.
     * <br>
     * In order to keep the scores an integer, they are multiplied by the product of all quantifiers.
     *
     * @param posScores for setting the scores to make a predicate true.
     * @param negScores for setting the scores to make a predicate false.
     */
    protected void setInitialScores(int[] posScores,int[] negScores) {
        int productQuantifier = Utilities.product(quantifiers);
        for(WClause wClause: wClauses) {
            Connective connective = wClause.connective;
            if(connective == Connective.EXACTLY) continue;

            int score = productQuantifier;
            switch(wClause.connective) {
                case ATLEAST: score /=  wClause.quantifier; break;
                case ATMOST:  score /= -wClause.quantifier; break;}

            for(int literal : wClause.literals) {
                if(literal > 0) posScores[literal] += score;
                else            negScores[-literal] += score;}}}


    /** sets the initial global and local truth values of a given clause.
     * A clause which is once true in the global model must remain true if the global model is extended.
     * This is critical for atmost-clauses.
     *
     * @param wClause a clause
     * @return true if the clause is true (globally or locally)
     */
    protected boolean setInitialTruthValue(WClause wClause) {
        Connective connective = wClause.connective;

        if(connective == Connective.OR) {
            int[] literals = wClause.literals;
            for(int literal : literals) {
                if(model.isTrue(literal)) {
                    wClause.isGloballyTrue = true;
                    wClause.isLocallyTrue = true;
                    return true;}}
            for(int literal : literals) {
                if(isLocallyTrue(literal)) {wClause.isLocallyTrue = true; return true;}}
            return false;}

        int globallyTrueLiterals = 0;
        int locallyTrueLiterals = 0;
        int globallyUndefinedLiterals = 0;
        for(int literal : wClause.literals) {
            switch(model.status(literal)) {
                case +1: ++globallyTrueLiterals; break;
                case 0:  ++globallyUndefinedLiterals;}
            if(isLocallyTrue(literal)) ++locallyTrueLiterals;}

        int quantifier = wClause.quantifier;
        switch (connective) {
            case ATLEAST:
                if(globallyTrueLiterals >= quantifier) {wClause.isGloballyTrue = true; wClause.isLocallyTrue = true; return true;}
                if(locallyTrueLiterals >= quantifier)  {wClause.isLocallyTrue = true;  return true;}
                break;
            case ATMOST:
                if(globallyTrueLiterals <= quantifier &&
                        (globallyTrueLiterals != 0 || globallyUndefinedLiterals != wClause.literals.length)){
                    wClause.isGloballyTrue = true; wClause.isLocallyTrue = true; return true;}
                if(locallyTrueLiterals <= quantifier)  {wClause.isLocallyTrue = true;  return true;}
                break;
            case EXACTLY:
                if(globallyTrueLiterals == quantifier) {wClause.isGloballyTrue = true; wClause.isLocallyTrue = true; return true;}
                if(locallyTrueLiterals == quantifier)  {wClause.isLocallyTrue = true;  return true;}}
        return false;}

    /** computes the truth value of a clause in the local (and global) model
     *
     * @param wClause a clause
     * @return true if the clause is true in the local (and global) model
     */
    protected boolean getLocalTruthValue(WClause wClause) {
        if(wClause.isGloballyTrue) return true;
        Connective connective = wClause.connective;

        if(connective == Connective.OR) {
            for(int literal : wClause.literals) {
                if(isLocallyTrue(literal)) {return true;} }
            return false;}

        int trueLiterals = currentlyTrueLiterals(wClause);
        int quantifier = wClause.quantifier;

        switch (connective) {
            case ATLEAST: return trueLiterals >= quantifier;
            case ATMOST:  return trueLiterals <= quantifier;
            case EXACTLY: return trueLiterals == quantifier;}
        return false;}

    /** computes the truth value of a clause in the global model
     *
     * @param wClause a clause
     * @return true if the clause is true in the global model
     * @throws Unsatisfiable if the clause can never be made true (should actually never happen here)
     */
    protected boolean getGlobalTruthValue(WClause wClause) throws Unsatisfiable {
        int trueLiterals = 0;
        int falseLiterals = 0;
        int undefinedLiterals = 0;

        for(int literal : wClause.literals) {
            switch(model.status(literal)) {
                case +1: ++trueLiterals;      break;
                case  0: ++undefinedLiterals; break;
                case -1: ++falseLiterals;     break;}}

        Connective connective = wClause.connective;

        int quantifier = wClause.quantifier;
        int length = wClause.literals.length;

        /*
        switch (connective) {
            case OR:
            case ATLEAST:
                if(falseLiterals >= length - quantifier + 1) // not enough potentially true literals
                    throw new Unsatisfiable(wClause,model,symboltable);
                return trueLiterals >= quantifier;
            case ATMOST:
                if(trueLiterals == 0 && undefinedLiterals == length) return false;
                if(trueLiterals > quantifier) // can never be made true
                    throw new Unsatisfiable(wClause,model,symboltable);
                return trueLiterals <= quantifier;
            case EXACTLY:
                if(trueLiterals > quantifier) // can never be made true
                    throw new Unsatisfiable(wClause,model,symboltable);
                if(falseLiterals >= length - quantifier + 1) // not enough potentially true literals
                    throw new Unsatisfiable(wClause,model,symboltable);
                return trueLiterals == quantifier;}
                */

        return false;}

    /** computes the initial flip scores for the literals of the given clause
     * The flip score for a predicate p counts the net number of clauses which become true when flipping the
     * truth-value of p, i.e. additional true clauses - new false clauses.
     *
     * @param wClause a clause
     */
    protected void setInitialFlipScores(WClause wClause) {
        if(wClause.isGloballyTrue) return;
        boolean hasDoubles = wClause.hasDoubles;
        int trueLiterals = currentlyTrueLiterals(wClause);
        switch(wClause.connective) {
            case OR:
            case ATLEAST:
                if(hasDoubles) addScoreAtleastWithDoubles(wClause,trueLiterals);
                else addScoreAtleast(wClause,trueLiterals);
                break;
            case ATMOST:
                if(hasDoubles) addScoreAtmostWithDoubles(wClause,trueLiterals);
                else addScoreAtmost(wClause,trueLiterals);
                break;
            case EXACTLY:
                if(hasDoubles) addScoreExactlyWithDoubles(wClause,trueLiterals);
                else addScoreExactly(wClause,trueLiterals);
                break;}}

    /** updates the flip scores of the literals in the clauses containing the flipped literal.
     *
     * @param flippedPredicate the flipped predicate
     */
    private void updateFlipScores(int flippedPredicate) {
        for(int sign = -1; sign <= +1; sign += 2) {
            int flippedLiteral = sign * flippedPredicate;
            for(WClause wClause : getClauses(flippedLiteral)) {
                if(wClause.isGloballyTrue) continue;
                boolean hasDoubles = wClause.hasDoubles;
                switch(wClause.connective) {
                    case OR:
                    case ATLEAST:
                        if(hasDoubles) updateFlipScoresATLEASTwithDoubles(flippedLiteral,wClause);
                        else updateFlipScoresATLEAST(flippedLiteral,wClause);
                        break;
                    case ATMOST:
                        if(hasDoubles) updateFlipScoresATMOSTwithDoubles(flippedLiteral,wClause);
                        else updateFlipScoresATMOST(flippedLiteral,wClause);
                        break;
                    case EXACTLY:
                        if(hasDoubles) updateFlipScoresEXACTLYwithDoubles(flippedLiteral,wClause);
                        else updateFlipScoresEXACTLY(flippedLiteral,wClause);
                    break;}}}}



    /** updates the flip score for an Atleast-clause which contains the flipped literal.
     *
     * @param flippedLiteral the flipped literal
     * @param wClause an Atleast-clause containing the flipped literal
     */
    private void updateFlipScoresATLEAST(int flippedLiteral, WClause wClause) {
        int quantifier = wClause.quantifier;
        int newTrueLiterals = currentlyTrueLiterals(wClause);
        int oldTrueLiterals = isLocallyTrue(flippedLiteral) ? newTrueLiterals -1 : newTrueLiterals +1;
        // undo old flip scores
        if(oldTrueLiterals == quantifier) {
            for(int literal : wClause.literals) {
                if(literal != flippedLiteral && isLocallyTrue(literal))   // flipping it made the clause false.
                    predicateQueue.addScore(Math.abs(literal),+1);}} // score: -1. Must be undone.
        else {
            if(oldTrueLiterals == quantifier-1) {
                for(int literal : wClause.literals) {
                    if(literal != flippedLiteral && !isLocallyTrue(literal))   // flipping it made the clause true.
                        predicateQueue.addScore(Math.abs(literal),-1);}} // score: +1. Must be undone.
            }
        addScoreAtleast(wClause,newTrueLiterals);}


    protected void addScoreAtleast(WClause wClause,int trueLiterals) {
        int quantifier = wClause.quantifier;
        if(trueLiterals == quantifier) {        // just enough true literals
            for(int literal : wClause.literals) {
                if(isLocallyTrue(literal))         // flipping a true literal makes the clause false.
                    predicateQueue.addScore(Math.abs(literal),-1);} // score: -1.
            return;}
        if(trueLiterals == quantifier-1) {       // not enough true literals
            for(int literal : wClause.literals) {
                if(!isLocallyTrue(literal))         // flipping a false literal it makes the clause true.
                    predicateQueue.addScore(Math.abs(literal),+1);}} // score: +1.
        } // all other cases do not change anything.

    /** updates the flip score for an Atleast-clause with double literals which contains the flipped literal.
     *
     * @param flippedLiteral the flipped literal
     * @param wClause an Atleast-clause containing the flipped literal
     */
    private void updateFlipScoresATLEASTwithDoubles(int flippedLiteral, WClause wClause) {
        int quantifier = wClause.quantifier;
        int newTrueLiterals = currentlyTrueLiterals(wClause);
        int oldTrueLiterals = isLocallyTrue(flippedLiteral) ?
                newTrueLiterals - wClause.multiplicity(flippedLiteral) :
                newTrueLiterals + wClause.multiplicity(flippedLiteral);
        int[] literals = wClause.literals;
        int length = literals.length;

        // undo old flip scores
        if(oldTrueLiterals == quantifier) { // the clause was true
            for(int i = 0; i < length; ++i) {
                int literal = literals[i]; // flipping a true literal made the clause false.
                if(literal != flippedLiteral && isLocallyTrue(literal) && !contains(literals,literal,i))
                    predicateQueue.addScore(Math.abs(literal),+1);}} // score: -1. Must be undone.
        else {
            if(oldTrueLiterals < quantifier) { // the clause was false
                // flipping a false literal with enough multiplicity made the clause true.
                for (int literal : literals) {
                    if (literal != flippedLiteral && !isLocallyTrue(literal) &&
                            (oldTrueLiterals + wClause.multiplicity(literal) > quantifier))
                        predicateQueue.addScore(Math.abs(literal), -1);
                }
            } // score: +1. Must be undone.
        }
        // new flip scores
        addScoreAtleastWithDoubles(wClause,newTrueLiterals);}


    private void addScoreAtleastWithDoubles(WClause wClause,int trueLiterals) {
        int quantifier = wClause.quantifier;
        int[] literals = wClause.literals;
        int length = literals.length;

        if(trueLiterals == quantifier) {        // clause ist true, just enough true literals
            for(int i = 0; i < length; ++i) {
                int literal = literals[i];
                if(isLocallyTrue(literal) && !contains(literals,literal,i))         // flipping a true literal makes the clause false.
                    predicateQueue.addScore(Math.abs(literal),-1);} // score: -1.
            return;}
        if(trueLiterals < quantifier) {       // clause is false, not enough true literals
            for(int i = 0; i < length; ++i) {
                int literal = literals[i]; // flipping a false literal with enough multiplicity made the clause true.
                if(!isLocallyTrue(literal) && (trueLiterals + wClause.multiplicity(literal) > quantifier) &&
                        !contains(literals,literal,i))
                    predicateQueue.addScore(Math.abs(literal),+1);}} // score: +1.
    } // all other cases do not change anything.

    /** updates the flip score for an Atmost-clause which contains the flipped literal.
     *
     * @param flippedLiteral the flipped literal
     * @param wClause an Atmost-clause containing the flipped literal
     */
    private void updateFlipScoresATMOST(int flippedLiteral, WClause wClause) {
        int quantifier = wClause.quantifier;
        int newTrueLiterals = currentlyTrueLiterals(wClause);
        int oldTrueLiterals = isLocallyTrue(flippedLiteral) ? newTrueLiterals -1 : newTrueLiterals +1;
        // undo old flip scores
        if(oldTrueLiterals == quantifier) {
            for(int literal : wClause.literals) {
                if(literal != flippedLiteral && !isLocallyTrue(literal))   // flipping it made the clause false.
                    predicateQueue.addScore(Math.abs(literal),+1);}} // score: -1. Must be undone.
        else {
            if(oldTrueLiterals == quantifier+1) {
                for(int literal : wClause.literals) {
                    if(literal != flippedLiteral && isLocallyTrue(literal))   // flipping it made the clause true.
                        predicateQueue.addScore(Math.abs(literal),-1);}} // score: +1. Must be undone.
        }
        // new flip scores
        addScoreAtmost(wClause,newTrueLiterals);}

    private void addScoreAtmost(WClause wClause,int trueLiterals) {
        int quantifier = wClause.quantifier;

        if(trueLiterals == quantifier) {                // just enough true literals
            for(int literal : wClause.literals) {
                if(!isLocallyTrue(literal))                // flipping a false literal it makes the clause false.
                    predicateQueue.addScore(Math.abs(literal),-1);} // score: -1.
            return;}

        if(trueLiterals == quantifier+1) {             // too many true literals
            for(int literal : wClause.literals) {
                if(isLocallyTrue(literal))                // flipping a true literal it makes the clause true.
                    predicateQueue.addScore(Math.abs(literal),+1);}} // score: +1.
        } // all other cases do not change anything.

    /** updates the flip score for an Atmost-clause with double literals which contains the flipped literal.
     *
     * @param flippedLiteral the flipped literal
     * @param wClause an Atmost-clause containing the flipped literal
     */
    private void updateFlipScoresATMOSTwithDoubles(int flippedLiteral, WClause wClause) {
        int quantifier = wClause.quantifier;
        int newTrueLiterals = currentlyTrueLiterals(wClause);
        int oldTrueLiterals = isLocallyTrue(flippedLiteral) ?
                newTrueLiterals - wClause.multiplicity(flippedLiteral) :
                newTrueLiterals + wClause.multiplicity(flippedLiteral);
        int[] literals = wClause.literals;
        int length = literals.length;

        // undo old flip scores
        if(oldTrueLiterals == quantifier) {      // the clause was true
            for(int i = 0; i < length; ++i) {
                int literal = literals[i];       // flipping a false literal made the clause false.
                if(literal != flippedLiteral && !isLocallyTrue(literal) && !contains(literals,literal,i))
                    predicateQueue.addScore(Math.abs(literal),+1);}} // score: -1. Must be undone.
        else {
            if(oldTrueLiterals > quantifier) {     // the clause was false
                for(int i = 0; i < length; ++i) {
                    int literal = literals[i];    // flipping a true literal with enough multiplicity made the clause true.
                    if(literal != flippedLiteral && isLocallyTrue(literal) &&
                            (oldTrueLiterals - wClause.multiplicity(literal) <= quantifier) &&
                            !contains(literals,literal,i))
                        predicateQueue.addScore(Math.abs(literal),-1);}} // score: +1. Must be undone.
        }
        // new flip scores
        addScoreAtmostWithDoubles(wClause,newTrueLiterals);}

    private void addScoreAtmostWithDoubles(WClause wClause,int trueLiterals) {
        int quantifier = wClause.quantifier;
        int[] literals = wClause.literals;

        if(trueLiterals == quantifier) {     // the clause is true, just enough true literals
            for (int literal : literals) {
                if (!isLocallyTrue(literal))                // flipping a false literal makes the clause false.
                    predicateQueue.addScore(Math.abs(literal), -1);
            } // score: -1.
            return;}

        if(trueLiterals > quantifier) {     // the clause is false, too many true literals
            for (int literal : literals) {
                if (isLocallyTrue(literal) && // flipping a true literal with enough multiplicity makes the clause true.
                        (trueLiterals - wClause.multiplicity(literal) <= quantifier))
                    predicateQueue.addScore(Math.abs(literal), +1);
            }
        } // score: +1.
    } // all other cases do not change anything.


    /** updates the flip score for an Exactly-clause which contains the flipped literal.
     *
     * @param flippedLiteral the flipped literal
     * @param wClause an Exactly-clause containing the flipped literal
     */
    private void updateFlipScoresEXACTLY(int flippedLiteral, WClause wClause) {
        int quantifier = wClause.quantifier;
        int newTrueLiterals = currentlyTrueLiterals(wClause);
        int oldTrueLiterals = isLocallyTrue(flippedLiteral) ? newTrueLiterals -1 : newTrueLiterals +1;
        // undo old flip scores
        if(oldTrueLiterals == quantifier) {
            for(int literal : wClause.literals) {
                if(literal != flippedLiteral)   // flipping it made the clause false.
                    predicateQueue.addScore(Math.abs(literal),+1);}} // score: -1. Must be undone.
        else {
            if(oldTrueLiterals == quantifier+1) {
                for(int literal : wClause.literals) {
                    if(literal != flippedLiteral && isLocallyTrue(literal))   // flipping it made the clause true.
                        predicateQueue.addScore(Math.abs(literal),-1);}} // score: +1. Must be undone.else {
            else {if(oldTrueLiterals == quantifier-1) {
                    for(int literal : wClause.literals) {
                        if(literal != flippedLiteral && !isLocallyTrue(literal))   // flipping it made the clause true.
                            predicateQueue.addScore(Math.abs(literal),-1);}}} // score: +1. Must be undone.
        }
        // new flip scores
        addScoreExactly(wClause,newTrueLiterals);}

    private void addScoreExactly(WClause wClause,int trueLiterals) {
        int quantifier = wClause.quantifier;

        if(trueLiterals == quantifier) {             // the right number of true literals
            for(int literal : wClause.literals) {       // flipping any literal makes the clause false.
                predicateQueue.addScore(Math.abs(literal),-1);} // score: -1.
            return;}

        if(trueLiterals == quantifier+1) {      // too many true literals
            for(int literal : wClause.literals) {
                if(isLocallyTrue(literal))         // flipping a true literal makes the clause true.
                    predicateQueue.addScore(Math.abs(literal),+1);}
            return;}

        if(trueLiterals == quantifier-1) {      // not enough true literals
            for(int literal : wClause.literals) {
                if(!isLocallyTrue(literal))        // flipping a false literal makes the clause true.
                    predicateQueue.addScore(Math.abs(literal),+1);}} // score: +1.
        } // all other cases do not change anything.

    /** updates the flip score for an Exactly-clause with doubles which contains the flipped literal.
     *
     * @param flippedLiteral the flipped literal
     * @param wClause an Exactly-clause containing the flipped literal
     */
    private void updateFlipScoresEXACTLYwithDoubles(int flippedLiteral, WClause wClause) {
        int quantifier = wClause.quantifier;
        int newTrueLiterals = currentlyTrueLiterals(wClause);
        int oldTrueLiterals = isLocallyTrue(flippedLiteral) ? newTrueLiterals -1 : newTrueLiterals +1;
        int[] literals = wClause.literals;
        int length = literals.length;
        // undo old flip scores
        if(oldTrueLiterals == quantifier) {
            for(int i = 0; i < length; ++i) {
                int literal = literals[i];
                if(literal != flippedLiteral && !contains(literals,literal,i))   // flipping it made the clause false.
                    predicateQueue.addScore(Math.abs(literal),+1);}} // score: -1. Must be undone.
        else {
            if(oldTrueLiterals == quantifier+1) {
                for(int i = 0; i < length; ++i) {
                    int literal = literals[i];
                    if(literal != flippedLiteral && isLocallyTrue(literal) && !contains(literals,literal,i))   // flipping it made the clause true.
                        predicateQueue.addScore(Math.abs(literal),-1);}} // score: +1. Must be undone.else {
            else {if(oldTrueLiterals == quantifier-1) {
                for(int i = 0; i < length; ++i) {
                    int literal = literals[i];
                    if(literal != flippedLiteral && !isLocallyTrue(literal) && !contains(literals,literal,i))   // flipping it made the clause true.
                        predicateQueue.addScore(Math.abs(literal),-1);}}} // score: +1. Must be undone.
        }
        // new flip scores
        addScoreExactlyWithDoubles(wClause,newTrueLiterals);}

    private void addScoreExactlyWithDoubles(WClause wClause,int trueLiterals) {
        int quantifier = wClause.quantifier;
        int[] literals = wClause.literals;
        int length = literals.length;

        if(trueLiterals == quantifier) {             // the right number of true literals
            for(int i = 0; i < length; ++i) {
                int literal = literals[i];       // flipping any literal makes the clause false.
                if(!contains(literals,literal,i)) predicateQueue.addScore(Math.abs(literal),-1);} // score: -1.
            return;}

        if(trueLiterals == quantifier+1) {      // too many true literals
            for(int i = 0; i < length; ++i) {
                int literal = literals[i];
                if(isLocallyTrue(literal) && !contains(literals,literal,i))    // flipping a true literal makes the clause true.
                    predicateQueue.addScore(Math.abs(literal),+1);}
            return;}

        if(trueLiterals == quantifier-1) {      // not enough true literals
            for(int i = 0; i < length; ++i) {
                int literal = literals[i];
                if(!isLocallyTrue(literal) && !contains(literals,literal,i))  // flipping a false literal makes the clause true.
                    predicateQueue.addScore(Math.abs(literal),+1);}}    // score: +1.
    } // all other cases do not change anything.


    int oldPredicate,oldoldPredicate;

    /** selects the next flip predicate.
     * Normally the next flip predicate is the top of the predicateQueue.
     * Only if the same flip predicate has been selected the last or second but last time,
     * it is the second flip predicate in the predicateQueue.
     *
     * If the flips has reached a multiple of the jumpFrequency, then a random predicate is chosen.
     * @return the next flip predicate.
     */
    int selectFlipPredicate() {
        int predicate = (statistics.flips % jumpFrequency == 0) ?
            predicateQueue.getRandom(random,3) :
            predicateQueue.topScore();
        if(predicate == oldPredicate || predicate == oldoldPredicate) {
            predicate = predicateQueue.nthTopScore(1);}
        oldoldPredicate = oldPredicate;
        oldPredicate = predicate;
        return predicate;}

    /** checks if the given literal is in literals[0] ... literals[i-1]
     *
     * @param literals an array of literals
     * @param literal a literal
     * @param index an index into the array
     * @return true if the given literal is in literals[0] ... literals[i-1]
     */
    private boolean contains(int[] literals, int literal, int index) {
        for(int i = 0; i < index; ++i) {if(literals[i] == literal); return  true;}
        return false;}

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
    private void addWClauseToIndex(WClause wClause) {
        for(int literal : wClause.literals) {
            if(literal > 0) posOccurrences[literal].add(wClause);
            else            posOccurrences[-literal].add(wClause);}}

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
        st.append("Globally true clauses:\n");
        for(WClause wClause : wClauses) {if(wClause.isGloballyTrue) st.append(wClause.toString(3,symboltable)).append("\n");}
        st.append("False Clauses:\n");
        for(WClause wClause : wClauses) {if(!wClause.isLocallyTrue) st.append(wClause.toString(3,symboltable)).append("\n");}
    return st.toString();
    }
}