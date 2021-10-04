package Solvers.Walker;

import Datastructures.Clauses.BasicClauseList;
import Datastructures.Clauses.Clause;
import Datastructures.Literals.CLiteral;
import Datastructures.Results.Aborted;
import Datastructures.Results.Result;
import Datastructures.Results.Satisfiable;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Theory.DisjointnessClasses;
import Datastructures.Theory.EquivalenceClasses;
import Management.ProblemSupervisor;
import Solvers.Solver;
import Utilities.Utilities;
import Utilities.IntegerQueue;
import Utilities.BucketSortedIndex;
import Utilities.BucketSortedList;
import it.unimi.dsi.fastutil.ints.Int2IntOpenHashMap;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.*;
import java.util.function.Consumer;

/** This is the abstract superclass of Random Walker classes.
 * It contains the data and methods common to all subclasses.
 * The idea of Random Walk is as follows:
 * A candidate model is heuristically generated.
 * As long as there are false clauses in this model, a predicate is selected and its truth value is flipped.
 * If the clause set has a model it might eventually be found.
 * If it is unsatisfiable, the process does not terminate.
 * Therefore the loop is aborted after a maximum number of flips.
 * <br>
 * Important data structures are: <br>
 * - rWModel, the local model <br>
 * - flipScores[1..predicates]:  flipScores[5] = 3 means: flipping predicate 5 causes 3 more clauses to become true. <br>
 * - predicateQueue: keeps the predicates sorted according to their flipScores. <br>
 *   predicates whose flip causes most clauses to become true came to the front of the queue.
 *
 * Created by ohlbach on 16.05.2019.
 */
public abstract class Walker extends Solver {

    private static HashSet<String> keys = new HashSet<>(); // contains the allowed keys in the specification.
    static { // these are the allowed keys in the specification.
        for(String key : new String[]{"name", "seed", "flips", "jumps", "type", "solver"}) {
            keys.add(key);}}

    /** parses a HashMap with key-value pairs<br>
     *
     * @param parameters  the parameters with the keys "seed", "flips", "jumpFrequency"
     * @param errors      for error messages
     * @param warnings    for warnings
     * @return            a list of HashMaps with these keys.
     */
    public static ArrayList<HashMap<String,Object>> parseParameters(HashMap<String,String> parameters, StringBuffer errors, StringBuffer warnings){
        for(String key : parameters.keySet()) {
            if(!keys.contains(key)) {warnings.append("Walker: unknown key in parameters: " + key + "\n");}}
        ArrayList<HashMap<String,Object>> list = new ArrayList<>();
        String seeds = parameters.get("seed");
        String flips = parameters.get("flips");
        if(flips == null) {flips = Integer.toString(Integer.MAX_VALUE);}
        String jumps = parameters.get("jumps");
        if(jumps == null) {jumps = Integer.toString(10);}
        String implications = parameters.get("ID_Implications");
        if(implications == null) {implications = "false";}
        String place = "Walker: ";

        ArrayList seed = null;
        if(seeds != null) {seed = Utilities.parseIntRange(place+"seed: ",seeds,errors);}
        else              {seed = new ArrayList(); seed.add(null);}
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

    public static String help() {
        return "Random Walker: parameters:\n" +
                "seed:   for the random number generator      (default: random)\n" +
                "flips:  for restricting the number of flips  (default: Max_Integer).\n" +
                "jumps:  frequency of random jumps            (default: 10)\n";}


    /** the maximum allowed number of flips */
    protected int maxFlips;

    /** the number of flips between two random jumpFrequency */
    protected int jumpFrequency = 10;

    /** a random number generator */
    protected Random random;

    /** a the local model. It is modified until all clauses become true */
    protected int[] localModel;

    /** sorts the predicates according to the flipScore. predicates whose flip makes more clauses true come to the front.*/
    protected IntegerQueue predicateQueue;


    /** collects statistical information */
    public WalkerStatistics statistics;

    /** EquivalenceClasses manage equivalent literals.
     *  In each equivalence class the literals are mapped to their representatives,
     *  which is always the predicate with the smallest number.
     */
    private EquivalenceClasses equivalenceClasses;

    private DisjointnessClasses disjointnessClasses;

    private ArrayList<Clause> clauses ;

    private BucketSortedIndex<WLiteral> literalIndex;

    private int[] id = {0};

    private int maxClauseLength = 3;

    private Object seed = null;

    private int timestamp = 1;

    private int[][] posTruths = null;

    private int[][] negTruths = null;


    /** constructs a new Walker solver.
     *
     * @param solverNumber         for distinguishing different solvers of the same type, but different parameters
     * @param solverParameters     contains the parameters for controlling the solver
     * @param problemSupervisor    coordinates several solvers.
     */
    public Walker(Integer solverNumber, HashMap<String,Object> solverParameters, ProblemSupervisor problemSupervisor) {
        super(solverNumber,solverParameters, problemSupervisor);}

    /** initializes walker specific data structures*/
    private void initializeData() {
        seed           = solverParameters.get("seed");
        random         = (seed != null) ? new Random((Integer)seed) : new Random();
        maxFlips       = (Integer)solverParameters.get("flips");
        jumpFrequency  = (Integer)solverParameters.get("jumps");
        localModel     = new int[predicates];
        statistics     = new WalkerStatistics(combinedId);
        predicateQueue = new IntegerQueue(predicates);
        clauses        = new ArrayList<Clause>();
        literalIndex   = new BucketSortedIndex<WLiteral>(predicates+1,
                (cLiteral->cLiteral.literal),
                (cLiteral->cLiteral.clause.size()));
    }




    /** This function is called when a new disjunction is to be inserted.
     *  It generates a simplifyBackwards task.
     */
    private Consumer<Clause> insertHandler = (
            clause -> {if(clause.size() == 1) {model.addImmediately(clause.getLiteral(0));}
                       else                   {insertClause(clause);}});


    /** This method turns the simplified basic clauses into the internal data structures.
     *
     * @param basicClauseList a BasicClauseList with equivalences, conjunctions, disjunctions and disjointnesses
     * @throws InterruptedException
     */
    private Result initializeClauses(BasicClauseList basicClauseList) throws InterruptedException {
      /*  if(basicClauseList.equivalences != null) {
            equivalenceClasses = new EquivalenceClasses(trackReasoning, symboltable,null);
            for(int[] equivalences : basicClauseList.equivalences) {
                equivalenceClasses.addSimplifiedEquivalenceClass(equivalences);}}
        if(basicClauseList.conjunctions != null) {
            for(int[] conjunction : basicClauseList.conjunctions) {
                for(int i = 2; i < conjunction.length; ++i) {
                    int literal = conjunction[i];
                    model.add(literal);
                    makeLocallyTrue(literal);}}}
        if(basicClauseList.disjunctions != null) {
            for(int[] disjunction : basicClauseList.disjunctions) {
                Clause clause = new Clause(++id[0],disjunction.length-2);
                for(int i = 2; i < disjunction.length; ++i) {
                    WLiteral wliteral = new WLiteral(disjunction[i]);
                    clause.add(wliteral);}
                insertClause(clause);}}
        if(basicClauseList.disjoints != null) {
            for(int[] disjoints : basicClauseList.disjoints) {
            disjointnessClasses.addSimplifiedDisjointnessClass(disjoints);}}
        if(Thread.interrupted()) {throw new InterruptedException();}
        if(equivalenceClasses != null || disjointnessClasses != null) {setTruths();}
        initializeModel();
        initializeScores();
        if(statistics.falseClauses == 0) {return completeModel();} */
        return null;}

    /** This method computes the initial consequences of true literals, derivable from equivalences and disjointnesses.
     *  For example, if p = q = r and p != s,t then <br>
     *  posTruth[p] = q,r,-s,-t (if p is disjoint to s and t, and p is true then s and t must be false) and <br>
     *  negTruths[p] = -q,-r (if p is false then this has no consequences for s and t)<br>
     *  These lists must be updated when equivalences and disjointnesses are imported from other solvers.
     *
     */
    void setTruths() {
        posTruths = new int[predicates+1][];
        negTruths = new int[predicates+1][];
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            dummyLiterals1.clear();
         //   if(equivalenceClasses != null)  {equivalenceClasses.truths(predicate,dummyLiterals1);}
            //if(disjointnessClasses != null) {disjointnessClasses.truths(predicate,dummyLiterals1);}
            if(!dummyLiterals1.isEmpty()) {
                int[] truths = new int[dummyLiterals1.size()];
                for(int i = 0; i < dummyLiterals1.size(); ++i) {truths[i] = dummyLiterals1.getInt(i);}
                posTruths[predicate] = truths;}
            else {posTruths[predicate] = null;}
            dummyLiterals1.clear();
         //   if(equivalenceClasses != null)  {equivalenceClasses.truths(-predicate,dummyLiterals1);}
            //if(disjointnessClasses != null) {disjointnessClasses.truths(-predicate,dummyLiterals1);}
            if(!dummyLiterals1.isEmpty()) {
                int[] truths = new int[dummyLiterals1.size()];
                for(int i = 0; i < dummyLiterals1.size(); ++i) {truths[i] = dummyLiterals1.getInt(i);}
                negTruths[predicate] = truths;}
            else {negTruths[predicate] = null;}}}

    /** searches for a satisfying model.
     * The search stops by either: model found, maximum number of flips reached, or thread interrupted.
     *
     * @return the Result, either Satisfiable, Aborted or Erraneous.
     */
    public Result solve() {
        super.initialize();
        globalParameters.log(solverId + " for problem " + problemId + " started");
        long time = System.currentTimeMillis();
        initializeData();
        Result result;
        try{result = null; //initializeClauses(simplifiedBasicClauseList);
            if(result == null) {result = walk();}}
        catch(InterruptedException ex) {
            globalParameters.log("Walker " + combinedId + " interrupted after " + statistics.flips + " flips.\n");
            result = new Aborted("Walker aborted after " + statistics.flips + " flips");}
        statistics.elapsedTime = System.currentTimeMillis() - time;
        System.out.println("RESULT " + result.toString());
        problemSupervisor.finished(this, result, "done");
        return result;}

    private Result walk() throws InterruptedException {
        while(statistics.flips < maxFlips) {
            if(Thread.interrupted()) {throw new InterruptedException();}
            int predicate = selectFlipPredicate();
            flipPredicate(predicate);
            if(statistics.falseClauses == 0) {return completeModel();}}
        return new Aborted("Walker aborted after " + statistics.flips + " flips");}

    /** initializes a model by making a predicate p true if p occurs more often than -p.*/

    void initializeModel() {
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            if(localModel[predicate] == 0) {
                makeInitiallyTrue(numberOfClausesIfTrue(predicate) >= numberOfClausesIfTrue(-predicate) ?
                        predicate : -predicate);}}}

    private IntArrayList dummyLiterals1 = new IntArrayList();
    private IntArrayList dummyLiterals2 = new IntArrayList();

    /** makes the literal and all its consequences true in the local model
     *
     * @param literal to be made true.
     */
    void makeInitiallyTrue(int literal) {
        makeLocallyTrue(literal);
        if(posTruths != null) {
            int[] truths = (literal > 0) ? posTruths[literal] : negTruths[-literal];
            if(truths != null) {for(int lit : truths) {makeLocallyTrue(lit);;}}}}

    /** makes this literal true in the local model
     *
     * @param literal a literal to be made true
     */
    void makeLocallyTrue(int literal) {
        localModel[Math.abs(literal)] = (literal > 0) ? 1: -1;}

    /** checks if the literal is true in the local model
     *
     * @param literal the literal to be checked
     * @return true if the literal is true in the local model, otherwise false.
     */
    boolean isLocallyTrue(int literal) {
        int status = localModel[Math.abs(literal)];
        return (literal > 0) ? status == 1 : status == -1;}

    /** checks if the literal is false in the local model
     *
     * @param literal the literal to be checked
     * @return true if the literal is false in the local model, otherwise false.
     */
    boolean isLocallyFalse(int literal) {
        int status = localModel[Math.abs(literal)];
        return (literal > 0) ? status == -1 : status == +1;}

    /** counts the number of clauses which become true it the literal is made true.
     *
     * @param literal a literal
     * @return the number of clauses which become true it the literal is made true.
     */
    int numberOfClausesIfTrue(int literal) {
        int counter = 0;

        BucketSortedList<WLiteral>.BucketIterator iterator = literalIndex.popIterator(literal);
        while(iterator.hasNext()) {++counter; iterator.next().clause.timestamp = timestamp;}
        literalIndex.pushIterator(literal,iterator);

        if(posTruths != null) {
            int[] truths = (literal > 0) ? posTruths[literal] : negTruths[-literal];
            if(truths != null) {
                for(int lit : truths) { // literals which must be made true if the literal is made true.
                    iterator = literalIndex.popIterator(lit);
                    while(iterator.hasNext()) {
                        Clause clause = iterator.next().clause;
                        if(clause.timestamp != timestamp) {++counter; clause.timestamp = timestamp;}}
                    literalIndex.pushIterator(lit,iterator);}}}
        ++timestamp;
        return counter;}


    /** tests if the clause is false in the local model
     *
     * @param clause the clause to be tested
     * @return true if the clause is false in the local model.
     */
    boolean isFalse(Clause clause) {
        for(CLiteral cLiteral : clause.cliterals) {
            if(isLocallyTrue(cLiteral.literal)) {return false;}}
        return true;}

    private final static int[] signs = {+1,-1};

    /** This method analyses the clauses after the initial model has been defined.
     * False clauses are marked, and the counter is incremented.<br>
     * Each WLiteral gets a score -1,0,1.  <br>
     *     -1 means flipping the truth value of this literal makes the clause false,<br>
     *     +1 means  flipping the truth value of this literal makes the clause true.<br>
     * These scores are accumulated in the priorityQueue. <br>
     * Finally the priorityQueue is sorted such that predicates with larger scores come first. <br>
     * (Flipping their truth value makes more clauses true).
     */
    void initializeScores() {
        for(Clause clause : clauses) {
            int trueLiterals = 0;
            for(CLiteral cliteral : clause) {
                if(isLocallyTrue(cliteral.literal)) {++trueLiterals;}}

            if(trueLiterals == 0) {
                markAsFalse(clause);
                for(CLiteral cliteral : clause) {
                    ((WLiteral)cliteral).score = 1; // flipping any literal makes the clause true.
                    predicateQueue.addScore(cliteral.literal,1);}
                continue;}

            // some literal must be locally true
            if(posTruths == null) {   // simple case: no equivalences or disjointnesses
                for(CLiteral cliteral : clause) {
                    int literal = cliteral.literal;
                    if(isLocallyTrue(literal) && trueLiterals == 1) {
                        ((WLiteral)cliteral).score = -1; // flipping this literal makes the clause false.
                        predicateQueue.addScore(literal,-1);}}} // flipping the others does'nt change anything
            else {                                   // now we treat the case that flipping the truth value of
                for(CLiteral cliteral : clause) {    // one literal in the clause causes flipping of another literal in the clause
                    int literal = cliteral.literal;
                    if(isLocallyTrue(literal)) {
                        int predicate = Math.abs(literal);
                        findFlippingPredicates(predicate,dummyLiterals1);
                        if(dummyLiterals1.isEmpty() && trueLiterals == 1) {
                            ((WLiteral)cliteral).score = -1; // no other literals in the clause are flipped.
                            predicateQueue.addScore(literal,-1);
                            continue;}
                        flipOnly(predicate,dummyLiterals1);
                        if(isFalse(clause)) {
                            ((WLiteral)cliteral).score = -1; // flipping this literal makes the clause false.
                             predicateQueue.addScore(literal,-1);}
                        flipOnly(predicate,dummyLiterals1);}}}}
        predicateQueue.sort();}

    /** collects the literals which are affected by flipping the local truth value of the predicate.
     * If the predicate is to be made false, then all positive consequences of the false predicate
     * which are not true in the local model are collected in dummyLiterals.<br>
     * If the predicate is to be made true, then all positive consequences of the true predicate
     * which are not true in the local model are collected in dummyLiterals.
     *
     * @param predicate a predicate to be flipped.
     */
    void findFlippingPredicates(int predicate, IntArrayList dummyLiterals) {
        if(posTruths == null) {return;}
        dummyLiterals.clear();
        int[] truths = localModel[predicate] == 1 ? negTruths[predicate] : posTruths[predicate];
        if(truths != null) {
            for(int literal : truths) {if(!isLocallyTrue(literal)) {dummyLiterals.add(Math.abs(literal));}}}}


    /** flips the local truth value of the predicate and the predicates affected by the flip.
     *
     * @param predicate a predicate whose truth value is to be flip.
     */
    void flipOnly(int predicate, IntArrayList dummyLiterals) {
        localModel[predicate] *= -1;
        if(posTruths != null) {
            for(int literal : dummyLiterals) {localModel[Math.abs(literal)] *= -1;}}}


    private Int2IntOpenHashMap flippedScores  = new Int2IntOpenHashMap();
    private Int2IntOpenHashMap affectedScores = new Int2IntOpenHashMap();

    /** flips the truth value of the predicate and updates the predicateQueue and the falseClauses counter
     *
     * @param predicate to be flipped
     */
     void flipPredicate(int predicate) {
         ++statistics.flips;
         flippedScores.clear();
         affectedScores.clear();
         findFlippingPredicates(predicate,dummyLiterals1);
         flippedScores.put(predicate,0);
         if(posTruths != null) {for(int pred : dummyLiterals1) {flippedScores.put(pred,0);}}
         flipOnly(predicate,dummyLiterals1);

         for(int sign : signs) {
            int pred = sign*predicate;
            BucketSortedList<WLiteral>.BucketIterator iterator = literalIndex.popIterator(pred);
            while(iterator.hasNext()) {
                Clause clause = iterator.next().clause;
                updateClauseScore(clause, predicate);
                clause.timestamp = timestamp;}
            literalIndex.pushIterator(pred,iterator);
            if(posTruths != null) {
                for(int predic : dummyLiterals1) {
                    int lit = sign*predic;
                    iterator = literalIndex.popIterator(lit);
                    while(iterator.hasNext()) {
                        Clause clause = iterator.next().clause;
                        if(clause.timestamp == timestamp) {continue;}
                        clause.timestamp = timestamp;
                        updateClauseScore(clause, predicate);}
                    literalIndex.pushIterator(lit,iterator);}}}
          ++timestamp;

         predicateQueue.changeScore(predicate,flippedScores.get(predicate));
         if(posTruths != null) {
             for(int pred : dummyLiterals1) {predicateQueue.changeScore(pred,flippedScores.get(pred));}}
         for(int literal : affectedScores.keySet()) {
             predicateQueue.changeScore(literal,predicateQueue.getScore(literal) + flippedScores.get(literal));}
         }


    /** updates the flipScore for a given clause.
     *
     * @param clause  the clause
     * @return the change in score for the given predicate
     */
    void updateClauseScore(Clause clause, int predicate) {
        int trueLiterals = 0;
        for(CLiteral cliteral : clause) {
            if(isLocallyTrue(cliteral.literal)) {++trueLiterals;}}

        if(trueLiterals == 0) { // flipping any of the literals makes the clause true.
            markAsFalse(clause);
            for(CLiteral cliteral : clause) {
                int pred = Math.abs(cliteral.literal);
                if(pred == predicate || dummyLiterals1.contains(pred)) {
                    flippedScores.addTo(pred,1);}
                else {
                    int oldScore = ((WLiteral)cliteral).score;
                    affectedScores.addTo(pred,1-oldScore);}
                ((WLiteral)cliteral).score = 1;}
            return;}
        markAsTrue(clause);

        // some literal must be locally true
        if(posTruths == null) {   // simple case: no equivalences or disjointnesses
            for(CLiteral cliteral : clause) {
                int literal = cliteral.literal;
                int change = (isLocallyTrue(literal) && trueLiterals == 1) ? -1 : 0; // flipping a single true literal makes the clause false
                int pred = Math.abs(literal);                                        // all other cases cause no change
                if(pred == predicate) {flippedScores.addTo(pred,change);}
                else                  {affectedScores.addTo(pred,change-((WLiteral)cliteral).score);}
                ((WLiteral)cliteral).score = change;}}
        else {                                   // now we treat the case that flipping the truth value of
            for(CLiteral cliteral : clause) {    // one literal in the clause causes flipping of another literal in the clause
                int literal = cliteral.literal;
                int change = 0;
                int pred = Math.abs(literal);
                if(isLocallyTrue(literal)) { // flipping may make the clause false
                    findFlippingPredicates(pred,dummyLiterals2);
                    if(dummyLiterals2.isEmpty() && trueLiterals == 1) {change = -1;} // the clause becomes false
                    else {                     // other literals my be flipped to true
                        flipOnly(pred,dummyLiterals2);        // this must be tested
                        if(isFalse(clause)) {change = -1;}
                        flipOnly(pred,dummyLiterals2);}}
                // all other cases cause no change
                if(pred == predicate || dummyLiterals1.contains(pred)) {flippedScores.addTo(pred,change);}
                else  {affectedScores.addTo(pred,change-((WLiteral)cliteral).score);}}}
    }


    private int oldPredicate = 0;
    private int oldoldPredicate = 0;

    /** selects the next flip predicate.
     * Normally the next flip predicate is the top of the predicateQueue.
     * Only if the same flip predicate has been selected the last or second but last time,
     * it is the second flip predicate in the predicateQueue.
     *
     * If the flips has reached a multiple of the jumpFrequency, then a random predicate is chosen.
     * @return the next flip predicate.
     */
    int selectFlipPredicate() {
        int predicate = 0;
        if(statistics.flips % jumpFrequency == 0) {
            predicate =  predicateQueue.getRandom(random,3);}
            if(predicateQueue.getScore(predicate) <= 0) {predicate = predicateQueue.nthTopScore(2);}
        else {
            predicate = predicateQueue.topScore();
            if(predicate == oldPredicate || predicate == oldoldPredicate) {
                predicate = predicateQueue.nthTopScore(1);}}
        oldoldPredicate = oldPredicate;
        oldPredicate = predicate;
        return predicate;}

    public Result importTrueLiteral(int literal) {
        if(isLocallyTrue(-literal)) {
            transferLocalModel();
            //return new Unsatisfiable(null,null);
        } //model,literal,symboltable);}
        ++statistics.importedUnitClauses;
        model.addImmediately(literal);
        int predicate = Math.abs(literal);
        if(isLocallyTrue(-literal)) {flipPredicate(predicate);}
        if(statistics.falseClauses == 0) {return completeModel();}
        predicateQueue.setScore(predicate,Integer.MIN_VALUE);
        return null;}

    void transferLocalModel() {
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            model.setStatus(predicate,localModel[predicate]);}}

    /** completes a model after resolution has finished.
     * Strategy INPUT or SOS: all remaining clauses should be true <br>
     * Strategy POSITIVE: all remaining clauses are negative of mixed. <br>
     * Choose an unassigned negative literal. <br>
     *
     * Strategy NEGATIVE: all remaining clauses are positive of mixed. <br>
     * Choose an unassigned positive literal. <br>
     *
     * @return Satisfiable or Erraneous (if something went wrong).
     */
    Result completeModel() {
        System.out.println("Completing Model\n"+toString());
        equivalenceClasses.completeModel();
        Result result = checkModel(model);
        if(result != null) {return result;}
        return new Satisfiable(model);}




    /** inserts the clause into the local data structures.
     *
     * @param clause  the clause to be inserted.
     */
    void insertClause(Clause clause) {
        if(clause.size() > 1) {
            ++statistics.clauses;
            maxClauseLength = Math.max(maxClauseLength,clause.size());
            clause.setPosition(clauses.size());
            clauses.add(clause);
            for(CLiteral cliteral : clause) {literalIndex.add((WLiteral)cliteral);}}}

    /** The method uses the position attribute to mark a clause as false or true.
     *  position = -1:  true <br>
     *  position = +1:  false
     *
     * @param clause the clause to be marked
     */
    void markAsFalse(Clause clause) {
        int position = clause.getPosition();
        if(position > 0) {return;}
        clause.setPosition(+1);
        ++statistics.falseClauses;}

    boolean isMarkedFalse(Clause clause) {
        return clause.getPosition() > 0;}

    /** The method uses the position attribute to mark a clause as false or true.
     *  position = -1:  true <br>
     *  position = +1:  false
     *
     * @param clause the clause to be marked
     */
    void markAsTrue(Clause clause) {
        int position = clause.getPosition();
        if(position < 0) {return;}
        clause.setPosition(-1);
        --statistics.falseClauses;}

    boolean isMarkedTrue(Clause clause) {
        return clause.getPosition() < 0;}



    public String toString() {
        StringBuilder st = new StringBuilder();
        st.append("Random Walker ").append(getClass().getName()).append(" ").append(id).append( " on Problem ").append(problemId).append("\n");
        st.append("Parameters:\n");
        st.append("  seed:           ").append((seed == null) ? "random" : " "+seed.toString()).append("\n");
        st.append("  flips:          ").append(Integer.toString(statistics.flips)).append(" of ").append(Integer.toString(maxFlips)).append("\n");
        st.append("  jump frequency: ").append(Integer.toString(jumpFrequency)).append("\n\n");
        st.append("Current model: ").append(localModel.toString()).append("\n");
        st.append("False Clauses:\n");
        for(Clause clause : clauses) {if(isMarkedFalse(clause)) st.append(clause.toString()).append("\n");}
        st.append("\nPredicate Queue:\n").append(predicateQueue.toString()).append("\n");
        if(equivalenceClasses != null) {
            st.append("Equivalence Classes:\n").append(equivalenceClasses.toString()).append("\n");}
        if(disjointnessClasses != null) {
            st.append("Disjointness Classes:\n").append(disjointnessClasses.toString());}
        return st.toString();
    }

}
