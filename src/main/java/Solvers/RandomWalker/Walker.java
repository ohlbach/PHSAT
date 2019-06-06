package Solvers.RandomWalker;

import Coordinator.CentralProcessor;
import Datastructures.Clauses.BasicClauseList;
import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseList;
import Datastructures.Literals.CLiteral;
import Datastructures.Results.Aborted;
import Datastructures.Results.Erraneous;
import Datastructures.Results.Result;
import Datastructures.Theory.Model;
import Solvers.Solver;
import Utilities.Utilities;

import java.lang.reflect.Array;
import java.util.*;

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
        String isolated = parameters.get("isolated");
        if(isolated == null) {isolated = "true";}
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
                "seed:   for the random number generator      (default: 0)\n" +
                "flips:  for restricting the number of flips  (default: Max_Integer).\n" +
                "jumps:  frequency of random jumps            (default: 10)\n";}


    /** the number of predicates in the problem */
    protected int predicates;
    /** the maximum allowed number of flips */
    protected int maxFlips;
    /** the number of flips between two random jumpFrequency */
    protected int jumpFrequency = 10;
    /** a random number generator */
    protected Random random;
    /** a the local model. It is modified until all clauses become true */
    protected RWModel rwModel;
    /** stores for each predicate the net number of clauses which become true after flipping this predicate */
    protected int[] flipScores;
    /** a list of clauses which are false under the current version of rwModel. */
    protected ArrayList<Clause> falseClauses = new ArrayList<>();
    /** sorts the predicates according to the flipScore. predicates whose flip makes more clauses true come to the front.*/
    protected PriorityQueue<Integer> predicateQueue;

    /** constructs a Walker and initializes some parameters.
     *
     * @param applicationParameters maps keywords to values
     * @param centralProcessor  controls the processing
     */
    public Walker(HashMap<String,Object> applicationParameters, CentralProcessor centralProcessor) {
        super(applicationParameters,centralProcessor);
        this.centralProcessor = centralProcessor;
        implicationDAG = centralProcessor.implicationDAG;
        initializeWalker(centralProcessor.predicates,applicationParameters,centralProcessor.model,centralProcessor.clauses.clone());}

    public Walker(int predicates, HashMap<String,Object> applicationParameters, Model model, ClauseList clauses) {
        super();
        this.applicationParameters = applicationParameters;
        implicationDAG = centralProcessor.implicationDAG;
        initializeWalker(predicates, applicationParameters, model, clauses);}


    private void initializeWalker(int predicates, HashMap<String,Object> applicationParameters, Model model, ClauseList clauses) {
        this.predicates = predicates;
        maxFlips      = (Integer)applicationParameters.get("flips");
        jumpFrequency = (Integer)applicationParameters.get("jumps");
        random        = new Random((Integer)applicationParameters.get("seed"));
        rwModel       = new RWModel(model);
        this.clauses  = clauses;
        statistics    = new WalkerStatistics(this);
        flipScores    = new int[predicates+1];
        predicateQueue = new PriorityQueue<Integer>(predicates,(
                (l1,l2) -> {
                    int f1 = flipScores[l1];
                    int f2 = flipScores[l2];
                    if(f1 > f2) {return -1;}
                    return(f1 < f2) ? 1 : 0;}));
    }

    /** initializes the flipScores, the false Clauses and the affected predicates
     * A flipScore for a predicate, say 5, is a number n if flipping its truth value makes n clauses more true than false
     */
    public void initializeScores() {
        for(Clause clause : clauses.getClauses(0)) {
            updateClauseScore(clause,1);}}

    /** adds the given score to the predicate's score and updates the predicateQueue
     *
     * @param predicate
     * @param score
     */
    public void changeScore(int predicate, int score) {
        predicateQueue.remove(predicate);
        flipScores[predicate] += score;
        predicateQueue.add(predicate);}

    int flipCounter = 0;

    /** searches for a satisfying model.
     * The search stops by either: model found, maximum number of flips reached, or thread interrupted.
     *
     * @return the Result, either Satisfiable, Aborted or Erraneous.
     */
    public Result solve() {
        if(centralProcessor != null) {
            centralProcessor.globalParameters.log(getClass().getName() + " " + id + " starting at problem " + problemId);}
        long start = System.currentTimeMillis();
        Result result = null;
        if(debug) {System.out.println(clauses.toString());}
        try{
            Thread thread = Thread.currentThread();
            initializeModel();
            initializeScores();
            if(falseClauses.isEmpty()) {
                result = Result.makeResult(transferModel(),basicClauseList);
                reportFinished(result,0,thread);
                return result;}
            while (++flipCounter <= maxFlips && !thread.isInterrupted() && !falseClauses.isEmpty()) {
                integrateNewFacts();
                int predicate = selectFlipPredicate();
                if(debug) {System.out.println("Flipping " + predicate);}
                flip(predicate);
                if(falseClauses.isEmpty()) {
                    result = Result.makeResult(transferModel(),basicClauseList);
                    reportFinished(result,flipCounter,thread);
                    return result;}
                else {
                    if(debug) {
                        System.out.printf("Current Model: ");
                        System.out.println(rwModel.toString());
                        System.out.println("False Clauses:");
                        for(Clause clause : falseClauses) {System.out.println(clause.toString());}}}
            }
            if(flipCounter >= maxFlips) {
                reportAbortion();
                result = new Aborted("Maximum number of flips: " + maxFlips + " reached.");}}
        finally{
            statistics.elapsedTime = System.currentTimeMillis()-start;
            if(centralProcessor != null) {
                centralProcessor.globalParameters.log(getClass().getName() + " " + id + " finished problem " + problemId);}}
        return result;}




    /** initializes a model by making a predicate p true if p occurs more often than -p.
     * A global model is transferred unchanged to the new model.
     */
    public abstract void initializeModel();

    /** integrates new facts coming from the coordinator.
     */
    public void integrateNewFacts() {}

    /** flips the truth value of the predicate and updates the predicateQueue and the falseClauses list
     *
     * @param predicate to be flipped
     */
    abstract public void flip(int predicate);


    /** flips the truth value of the predicate and updates the predicateQueue and the falseClauses list
     *
     * @param predicate to be flipped
     */
    public void flipPredicate(int predicate) {
        for(CLiteral cliteral : clauses.literalIndex.getLiterals(predicate)) {
            updateClauseScore(cliteral.clause,-1);}
        for(CLiteral cliteral : clauses.literalIndex.getLiterals(-predicate)) {
            updateClauseScore(cliteral.clause,-1);}
        rwModel.flip(predicate);
        for(CLiteral cliteral : clauses.literalIndex.getLiterals(predicate)) {
            updateClauseScore(cliteral.clause,1);}
        for(CLiteral cliteral : clauses.literalIndex.getLiterals(-predicate)) {
            updateClauseScore(cliteral.clause,1);}}


    /** updates the flipScore for a given clause.
     *
     * @param clause  the clause
     * @param change  +1 if the clause is to be added, -1 if it is to be removed.
     */
    public abstract void updateClauseScore(Clause clause, int change);

    /** transfers an rwModel to a Model.
     *  This is called on success, just to get a model in standard form.
     *
     * @return the new Model
     */
    public Model transferModel() {
        int predicates = rwModel.predicates();
        Model model = new Model(predicates);
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            model.add(predicate*rwModel.status[predicate]);}
        return model;}


    private int oldPredicate = 0;
    private int oldoldPredicate = 0;

    /** selects the next flip predicate.
     * Normally the next flip predicate is the top of the predicateQueue.
     * Only if the same flip predicate has been selected the last or second but last time,
     * it is the second flip predicate in the predicateQueue.
     *
     * If the flipCounter has reached a multiple of the jumpFrequency, then a random predicate is chosen
     * from one of the false clauses.
     *
     * @return the next flip predicate.
     */
    int selectFlipPredicate() {
        int predicate = 0;
        if(flipCounter % jumpFrequency == 0) {
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


    /** reports that the resolution has been finished
     *
     * @param result     the result of the random walker
     * @param flips      the number of flips.
     * @param thread     which executed the solver
     */
    public void reportFinished(Result result, int flips, Thread thread) {
        if(thread.isInterrupted()) {
            if(globalParameters != null) {
                globalParameters.log(getClass().getName() + " " + id + " for problem " + problemId +" interrupted after " + flips + " flips.\n");}}
        else {
            if(globalParameters != null) {
                globalParameters.log(getClass().getName() + " " + id + " for problem " + problemId +" finished after " + flips + " flips.\n" +
                        "Result: " + result.toString());}
            if(result instanceof Erraneous && supervisor != null) {supervisor.statistics.incErraneous();}}}

    /** reports that the search has been aborted
     */
    private void reportAbortion() {
        if(globalParameters != null) {
            globalParameters.log(getClass().getName()+ " " + id + " for problem " + problemId +" stopped after " + maxFlips + " flips");}
        if(supervisor != null) {
            supervisor.statistics.incAborted();
            supervisor.aborted(id);}}


    public String toString() {
        StringBuilder st = new StringBuilder();
        st.append("Solver ").append(getClass().getName()).append(" ").append(id).append( " on Problem ").append(problemId).append("\n");
        st.append("Parameters:\n");
        st.append("  seed:           ").append(applicationParameters.get("seed")).append("\n");
        st.append("  flips:          ").append(Integer.toString(flipCounter)).append(" of ").append(Integer.toString(maxFlips)).append("\n");
        st.append("  jump frequency: ").append(Integer.toString(jumpFrequency)).append("\n\n");
        st.append("Current model: ").append(rwModel.toString()).append("\n");
        st.append("False Clauses:\n");
        for(Clause clause : falseClauses) {st.append(clause.toString());}
        st.append("\nScores:\n");
        for(int pred = 1; pred < predicates; ++pred) {
            if(flipScores[pred] != 0) {
            st.append(Integer.toString(pred)).append(":").append(Integer.toString(flipScores[pred])).append("; ");}}
        st.append("\n");
        st.append("Queue:\n");
        st.append(predicateQueue.toString()).append("\n");
        return st.toString();

    }


}
