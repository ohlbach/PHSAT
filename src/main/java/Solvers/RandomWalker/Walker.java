package Solvers.RandomWalker;

import Coordinator.CentralProcessor;
import Datastructures.Clauses.Clause;
import Datastructures.Literals.CLiteral;
import Datastructures.Literals.LiteralIndex;
import Datastructures.Results.Erraneous;
import Datastructures.Results.Result;
import Datastructures.Theory.ImplicationNode;
import Datastructures.Theory.Model;
import Management.GlobalParameters;
import Solvers.Solver;
import Utilities.Utilities;

import java.util.*;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

/**
 * Created by Ohlbach on 01.09.2018.
 */
public class Walker extends Solver {


    private static HashSet<String> keys = new HashSet<>(); // contains the allowed keys in the specification.
    static { // these are the allowed keys in the specification.
        for(String key : new String[]{"name", "seed", "flips", "jumps"}) {
            keys.add(key);}}

    /** parses a HashMap with key-value pairs<br>
     *
     * @param parameters  the parameters with the keys "seed", "flips", "jumps"
     * @param errors      for error messages
     * @param warnings    for warnings
     * @return            a list of HashMaps with these keys.
     */
    public static ArrayList<HashMap<String,Object>> parseParameters(HashMap<String,String> parameters, StringBuffer errors, StringBuffer warnings){
        for(String key : parameters.keySet()) {
            if(!keys.contains(key)) {warnings.append("RandomWalker: unknown key in parameters: " + key + "\n");}}
        ArrayList<HashMap<String,Object>> list = new ArrayList<>();
        String seeds = parameters.get("seed");
        if(seeds == null) {seeds = "0";}
        String flips = parameters.get("flips");
        if(flips == null) {flips = Integer.toString(Integer.MAX_VALUE);}
        String jumps = parameters.get("jumps");
        if(jumps == null) {jumps = Integer.toString(10);}
        String implications = parameters.get("ID_Implications");
        if(implications == null) {implications = "false";}
        String place = "Random Walker: ";
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

    public static String help() {
        return "Random Walker: parameters:\n" +
                "seed:   for the random number generator           (default: 0)\n" +
                "flips:  for restricting the number of flips       (default: Max_Integer).\n" +
                "jumps:  frequency of random jumps                 (default: 10)\n";}


    
    private RWModel rwModel;
    private int timestamp = 0;

    /** constructs a new solver of type RandomWalker.
     * The constructor is called serially. Therefore there are no race conditions.
     * globalParameters and centralProcessor are shared between different threads.
     * The walker is passive in the sense that it does not send data to the CentralDataHolder.
     *
     * @param applicationParameters     contains the parameters for controlling the solver
     * @param centralProcessor       contains the result of parsing and initializing the problem data.
     */
    public Walker(HashMap<String,Object> applicationParameters, CentralProcessor centralProcessor) {
        super(applicationParameters,centralProcessor);
        initializeData();
        addObservers();
    }
    
    private void initializeData() {
        rwModel = new RWModel(centralProcessor.model);
        clauses = centralProcessor.clauses.clone(); // now centralProcessor may change its clauses
        statistics = new WalkerStatistics(this);
        initializeModel();
        initializeQueue();
    }

    private Random random;
    private LiteralIndex index ;
    private int[] flipScore;
    private PriorityQueue<Integer> predicateQueue;
    private ArrayList<Clause> falseClauses;
    int flipCounter  = 0;
    int randomFrequency = 0;

    public Result solve() {
        globalParameters.log("Random Walker " + id + " starting at problem " + problemId);
        long start = System.currentTimeMillis();
        int maxFlips     = (Integer)applicationParameters.get("flips");
        randomFrequency  = (Integer)applicationParameters.get("jumps");
        flipScore        = new int[predicates];
        index            = clauses.literalIndex;
        int seed         = (Integer)applicationParameters.get("seed");
        random           = new Random(seed);
        predicateQueue = new PriorityQueue<Integer>(predicates,(
                (l1,l2) -> {
                    int f1 = combinedScore(l1);
                    int f2 = combinedScore(l2);
                    if(f1 > f2) {return -1;}
                    return(f1 < f2) ? 1 : 0;}));
        Thread thread = Thread.currentThread();
        try{
            while (++flipCounter <= maxFlips && !thread.isInterrupted() && !falseClauses.isEmpty()) {
                integrateNewFacts();
                flip(selectFlipPredicate());
                updateScores();}
            if(falseClauses.isEmpty()) {
                Result result = Result.makeResult(transferModel(rwModel),basicClauseList);
                reportFinished(result,flipCounter,thread);
                return result;}
            else {reportAbortion(maxFlips);}}
        finally{statistics.elapsedTime = System.currentTimeMillis()-start;}
        return null;}

    private ArrayList<Integer> trueLiterals = new ArrayList<>();
    private synchronized void addTrueLiteral(int literal) {trueLiterals.add(literal);}
    private Consumer<Integer> trueLiteralObserver = literal-> addTrueLiteral(literal);

    private ArrayList<Integer> implications = new ArrayList<>();
    private synchronized void addImplication(int from, int to) {implications.add(from); implications.add(to);}
    private BiConsumer<ImplicationNode,ImplicationNode> implicationObserver = (from, to) -> addImplication(from.literal,to.literal);


    protected void addObservers() {
        centralProcessor.model.addTrueLiteralObserver(trueLiteralObserver);
        centralProcessor.implicationDAG.addImplicationObserver(implicationObserver);
        implicationDAG.addTrueLiteralObserver(trueLiteralObserver);
    }

    protected void removeObservers() {
        centralProcessor.model.removeTrueLiteralObserver(trueLiteralObserver);
        centralProcessor.implicationDAG.removeImplicationObserver(implicationObserver);}

    private synchronized void integrateNewFacts() {
        for(int i = 0; i < implications.size(); i += 2) {
            int from = implications.get(i);
            int to   = implications.get(i+1);
            if(rwModel.isTrue(from) && rwModel.isFalse(to)) {flip(to);}
            implicationDAG.addClause(-from,to);} // this may generate new true literals
        implications.clear();

        for(int literal : trueLiterals) {
            int predicate = Math.abs(literal);
            if(rwModel.isFalse(literal)) {flip(predicate);}
            predicateQueue.remove(predicate);}
        trueLiterals.clear();}


    /** generates a candidate rwModel for the clauses.
     * A predicate becomes true if it occurs in more clauses than its negation.
     */
    private void initializeModel() {
        implicationDAG.applyToRoots(literal -> {
            literal = getOccurrences(literal) > getOccurrences(-literal) ? literal : -literal;
            implicationDAG.apply(literal,true,(lit-> {
                rwModel.status[ Math.abs(lit)] = (byte)(lit > 0 ? 1 : -1);}));});
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            if(rwModel.status[predicate] == 0) {
                rwModel.status[predicate] = (byte)(getOccurrences(predicate) > getOccurrences(-predicate) ? 1 : -1);}}}

    /** adds (change = 1) or removes (change = -1) a clause.
     * Updates flipScore and falseClauses
     *
     * @param clause a clause
     * @param change +1 for adding the clause, -1 for removing the clause.
     */
    void updateClause(Clause clause, int change) {
        CLiteral trueLiteral = null;
        for(CLiteral cLiteral : clause.cliterals) {
            if(rwModel.isTrue(cLiteral.literal)) {
                if(trueLiteral != null) {return;}  // at least two true literals. Flips don't change the status
                else{trueLiteral = cLiteral;}}}
        if(trueLiteral == null) {
            if(change > 0) {falseClauses.add(clause);} else {falseClauses.remove(clause);}
            clause.applyToLiteral(literal -> changeScore(Math.abs(literal), change));} // flipping a literal increases the number of true literals
        else {changeScore(Math.abs(trueLiteral.literal),-change); }} // flipping this literal destroys a true literal.


    private int[] counter = new int[]{0};

    /** counts the clauses containing the literal and its implied literals
     *
     * @param literal a literal
     * @return the number of clauses containing the literal and its implied literals.
     */
    int getOccurrences(int literal) {
        ++timestamp;
        counter[0] = 0;
        implicationDAG.apply(literal,true,(lit-> {
            for(CLiteral cLiteral : clauses.getLiterals(lit)){
                Clause clause = cLiteral.clause;
                if(clause.timestamp != timestamp) {
                    clause.timestamp = timestamp;
                    ++counter[0];}}}));
        return counter[0];}

                /** initializes the falseClauses array with all clauses which are false in the current rwModel.
                 */
    private void initializeQueue() {
        falseClauses = new ArrayList<>();
        for(Clause clause : clauses.getClauses(0)) {
            updateClause(clause,1);}
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            if(centralProcessor.model.status(predicate) == 0) {predicateQueue.add(predicate);}}}

    private int[] score = new int[]{0};
    int combinedScore(int predicate) {
        score[0] = 0;
        int literal = rwModel.isTrue(predicate) ? -predicate : predicate;
        implicationDAG.apply(literal,true,
                (lit -> {if(rwModel.isFalse(lit)) {score[0] += flipScore[Math.abs(lit)];}}));
        return score[0];}


    /** flips the truth value of the predicate and updates the predicateQueue
     *
     * @param predicate to be flipped
     */
    void flip(int predicate) {
        ++((WalkerStatistics)statistics).RW_flips;
        implicationDAG.apply(-rwModel.status[predicate]*predicate,true,(literal -> {
            if(rwModel.isFalse(literal)) {
                for(CLiteral cLiteral : index.getLiterals(literal)) {
                    Clause clause = cLiteral.clause;
                    if(isFalse(clause)) {
                        falseClauses.remove(clause); // flipping the other literals does not change the status for the clause
                        clause.applyToLiteral(lit->changeScore(lit,-1));}
                    else {CLiteral otherTrueLiteral = findOtherTrueLiteral(cLiteral);
                        if(otherTrueLiteral != null) {changeScore(otherTrueLiteral.literal,1);}}}};
                rwModel.flip(literal);}));}

    private int oldPredicate = 0;
    private int oldoldPredicate = 0;
    private int randomCounter = 0;

    int selectFlipPredicate() {
        int predicate = 0;
        if(++randomCounter == randomFrequency) {
            randomCounter = 0;
            Clause clause = falseClauses.get(random.nextInt(falseClauses.size()));
            return Math.abs(clause.cliterals.get(random.nextInt(clause.cliterals.size())).literal);}

        predicate = predicateQueue.peek();
        if(predicate == oldPredicate || predicate == oldoldPredicate) {
            predicateQueue.poll();
            int predicate1 = predicateQueue.peek();
            predicateQueue.add(predicate);
            predicate = predicate1;}
        oldoldPredicate = oldPredicate;
        oldPredicate = predicate;
        return predicate;}


    boolean isFalse(Clause clause) {
        for(CLiteral cLit : clause.cliterals) {
            if(rwModel.isTrue(cLit.literal)) {return false;}}
        return true;}


    /** searches for the only other true literal besides the given one
     *
     * @param cLiteral a CLiteral
     * @return a true literal where all others except the given one are false, null if no such literal exists.
     */
     CLiteral findOtherTrueLiteral(CLiteral cLiteral) {
        CLiteral trueLiteral = null;
        for(CLiteral cLit : cLiteral.clause.cliterals) {
            if(cLit != cLiteral && rwModel.isTrue(cLit.literal)) {
                if(trueLiteral != null) {return null;}
                trueLiteral = cLit;}}
        return trueLiteral;}

    private TreeMap<Integer,Integer> affected = new TreeMap();
    /** changes the flipScore of the given literal and updates the predicateQueue
     *
     * @param literal    an literal
     * @param difference the change to the score
     */
    void changeScore(int literal, int difference) {
        int predicate = Math.abs(literal);
        Integer score = affected.get(predicate);
        if(score == null) {score = difference;}
        else {score += difference;}
        affected.put(predicate,score);}

    /** updates the flip scores and the predicate queue.
     * Since the order in the queue may depend on several predicates,
     * one must first remove all affected predicates from the queue, then update the scores and then add the
     * predicate to the queue.
     */
     void updateScores() {
        for(Map.Entry<Integer,Integer> entry : affected.entrySet()) {
            Integer predicate = entry.getKey();
            Integer value = entry.getValue();
            if(value != 0) {implicationDAG.apply(predicate,false,(p->predicateQueue.remove(Math.abs(p))));}}
        for(Map.Entry<Integer,Integer> entry : affected.entrySet()) {
            Integer predicate = entry.getKey();
            Integer value = entry.getValue();
            if(value != 0) {flipScore[predicate] += value;}}
        for(Map.Entry<Integer,Integer> entry : affected.entrySet()) {
            Integer predicate = entry.getKey();
            Integer value = entry.getValue();
            if(value != 0) {implicationDAG.apply(predicate,false,(p->predicateQueue.add(Math.abs(p))));}}
        affected.clear();}

    /** transfers an rwModel to a Model.
     *  This is called on success, just to get a model in standard form.
     *
     * @param rwModel  the rwModel
     * @return the new Model
     */
    private Model transferModel(RWModel rwModel) {
        model = new Model(predicates);
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            model.add(predicate*rwModel.status[predicate]);}
        return model;}

    /** reports that the resolution has been aborted
     *
     * @param limit the maximum number of flips
     */
    private void reportAbortion(int limit) {
        globalParameters.log("Random Walker " + id + " for problem " + problemId +" stopped after " + limit + " flips");
        supervisor.statistics.incAborted();
        supervisor.aborted(id);}

    /** reports that the resolution has been finished
     *
     * @param result     the result of the random walker
     * @param flips      the number of flips.
     * @param thread     which executed the solver
     */
    private void reportFinished(Result result, int flips, Thread thread) {
        if(thread.isInterrupted()) {
            globalParameters.log("Random Walker " + id + " for problem " + problemId +" interrupted after " + flips + " flips.\n");}
        else {
            globalParameters.log("Random Walker " + id + " for problem " + problemId +" finished after " + flips + " flips.\n" +
                    "Result: " + result.toString());
            if(result instanceof Erraneous) {supervisor.statistics.incErraneous();}}}
}







