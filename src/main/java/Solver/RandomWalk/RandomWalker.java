package Solver.RandomWalk;

import Coordinator.CentralDataHolder;
import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseList;
import Datastructures.Literals.CLiteral;
import Datastructures.Literals.LiteralIndex;
import Datastructures.Theory.ImplicationGraph;
import Datastructures.Theory.Model;
import Utilities.Utilities;

import java.util.*;
import java.util.function.BiConsumer;

/**
 * Created by ohlbach on 01.09.2018.
 */
public class RandomWalker {


    private static HashSet<String> keys = new HashSet<>(); // contains the allowed keys in the specification.
    static { // these are the allowed keys in the specification.
        for(String key : new String[]{"class", "seed", "flips"}) {
            keys.add(key);}}

    /** parses a HashMap with key-value pairs:<br/>
     * file: a comma separated list of pathnames<br/>
     * directory: a comma separated list of directories (all .cnf files in this directory are adressed) <br/>
     * regExpr: a regular expression: All files in the directories matching the expression are addressed
     *
     * @param parameters  the parameters with the keys "file", "directory", "regExpr"
     * @param errors      for error messages
     * @param warnings    for warnings
     * @return            a list of HashMaps with key "file" and value the corresponding File object.
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
        String implications = parameters.get("implications");
        if(implications == null) {implications = "false";}
        String place = "Random Walker: ";
        ArrayList seed = Utilities.parseIntRange(place+"seed: ",seeds,errors);
        ArrayList flip = Utilities.parseIntRange(place+"flips: ",flips,errors);
        ArrayList jump = Utilities.parseIntRange(place+"jumps: ",jumps,errors);
        ArrayList<ArrayList> pars = Utilities.crossProduct(seed,flip,jump);
        for(ArrayList<Object> p : pars ) {
            HashMap<String,Object> map = new HashMap<>();
            map.put("seed",pars.get(0));
            map.put("flips",pars.get(1));
            map.put("jumps",pars.get(2));
            list.add(map);}
        return list;}

    public static String help() {
        return "Random Walker: parameters:\n" +
                "seed:   for the random number generator           (default: 0)\n" +
                "flips:  for restricting the number of flips       (default: Max_Integer).\n" +
                "jumps:  frequency of random jumps                 (default: 10)\n";}



    class RWModel {
        short[] status;

        RWModel(Model model) {
            this.status = model.cloneStatus();}

        boolean isTrue(int literal) {
            short status = this.status[Math.abs(literal)];
            return literal > 0 ? status == 1: status == -1;}

        boolean isFalse(int literal) {return !isTrue(literal);}

        void flip(int literal) {
            int predicate = Math.abs(literal);
            status[predicate] = (short)(-1*status[predicate]);
        }
    }

    private String id;
    private HashMap<String,Object> solverControl;
    private HashMap<String,Object> globalParameters;
    private CentralDataHolder centralData;
    private ClauseList clauseList;
    private Model globalModel;
    private RWModel rwModel;
    private ArrayList<Integer> newTrueLiterals = new ArrayList<>();

    /** constructs a new solver of type RandomWalker.
     * The constructor is called serially. Therefore there are no race conditions.
     * globalParameters and centralData are shared between different threads.
     * The walker is passive in the sense that it does not send data to the CentralDataHolder.
     *
     * @param walker            counts the constructed walker
     * @param solverControl     contains the parameters for controlling the solger
     * @param globalParameters  contains the global control parameters
     * @param centralData       contains the result of parsing and initializing the problem data.
     */
    public RandomWalker(Integer walker,  HashMap<String,Object> solverControl, HashMap<String,Object> globalParameters,
                        CentralDataHolder centralData) {
        id = "Walker_"+walker;
        this.solverControl  = solverControl;
        this.globalParameters = globalParameters;
        globalModel = centralData.model;
        rwModel = new RWModel(globalModel);
        globalModel.addNewTruthObserver(literal -> newTrueLiterals.add(literal));
        clauseList = centralData.disjunctions.disjunctions.clone(); // now centralDataHolder may change its clauses
    }

    public String info;
    private int predicates;
    private ImplicationGraph implicationGraph;
    private BiConsumer<String,String> logger;
    private Random random;
    private LiteralIndex index ;
    private int[] flipConsequences;
    private PriorityQueue<Integer> literalQueue;
    private ArrayList<Clause> falseClauses;
    int flipCounter  = 0;

    public void solve(StringBuffer errors, StringBuffer warnings) {
        logger = (BiConsumer<String,String>)globalParameters.get("logger");
        logger.accept(id,"starting");
        random = new Random((Integer)solverControl.get("seed"));
        int maxFlips     = (Integer)solverControl.get("flips");
        randomFrequency  = (Integer)solverControl.get("jumps");
        predicates       = centralData.predicates;
        globalModel      = centralData.model;
        implicationGraph = centralData.implicationGraph;
        index            = clauseList.literalIndex;
        int seed         = (Integer)solverControl.get("seed");
        random           = new Random(seed);
        info = id + "(seed:"+seed+",flips:"+maxFlips + ")";
        literalQueue     = new PriorityQueue<Integer>(predicates,(
                (l1,l2) -> {
                    int f1 = flipConsequences[l1];
                    int f2 = flipConsequences[l2];
                    if(f1 > f2) {return -1;}
                    return(f1 < f2) ? 1 : 0;}));
        integrateNewTrueLiterals();
        initializeModel();
        initializeFlipConsequences();
        initializeFalseClauses();
        Thread thread = Thread.currentThread();
        while (++flipCounter <= maxFlips && !thread.isInterrupted() && !falseClauses.isEmpty()) {
            integrateNewTrueLiterals();
            flip(selectFlipPredicate());}
            }

    /** generates a candidate rwModel for the clauses.
     * A predicate becomes true if it occurs in more clauses than its negation.
     */
    private void initializeModel() {
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            if(rwModel.status[predicate] == 0) {
                rwModel.status[predicate] = (short)((clauseList.getOccurrences(predicate) > clauseList.getOccurrences(-predicate)) ? 1 : -1);}}}

    /** initializes the falseClauses array with all clauses which are false in the current rwModel.
     */
    private void initializeFalseClauses() {
        falseClauses = new ArrayList<>();
        for(Clause clause : clauseList.clauses) {
            boolean isFalse = true;
            for(CLiteral cLiteral : clause.cliterals) {
                if(rwModel.isTrue(cLiteral.literal)) {isFalse = false; break;}}
            if(isFalse) {falseClauses.add(clause);}}}


    /** initializes flipConsequences and literalQueue.
     *  literalQueue contains the predicates ordered by the number
     *  of disjunctions made true when flipping the predicate.
     *  The head of the queue is the predicate which makes most disjunctions true by flipping it.
     */
    private void initializeFlipConsequences() {
        globalModel.readLock();
        try{
            for(int predicate = 1; predicate <= predicates; ++predicate) {
                if(!globalModel.contains(predicate)) {
                    flipConsequences[predicate] = flipMakesTrue(predicate);}}
            for(int predicate = 1; predicate <= predicates; ++predicate) {
                if(!globalModel.contains(predicate)) {literalQueue.add(predicate);}}}
        finally{globalModel.readUnLock();}}


    /** computes how many disjunctions more become true after flipping the predicate.
     * Example: p is true. </br>
     * All disjunctions with -p where all literals are false become true</br>
     * All disjunctions with p where all other literals are false become false</br>
     * The result is then the difference between these numbers.
     *
     * @param predicate the predicate to be checked
     * @return how many disjunctions more become true after flipping the predicate.
     */
    private int flipMakesTrue(int predicate) {
        int becomesTrue = 0;
        for(CLiteral cliteral : index.getLiterals(predicate* rwModel.status[predicate])) {
            boolean remainstrue = false;
            for(CLiteral othercliteral : cliteral.clause.cliterals) {
                if(cliteral != othercliteral && rwModel.isTrue(othercliteral.literal)) {remainstrue  = true; break;}}
            if(!remainstrue) {--becomesTrue;}      // clause becomes false after flip.
            }
        for(CLiteral cliteral : index.getLiterals(-predicate* rwModel.status[predicate])) {
            boolean remainstrue = false;
            for(CLiteral othercliteral : cliteral.clause.cliterals) {
                if(cliteral != othercliteral && rwModel.isTrue(othercliteral.literal)) {remainstrue  = true; break;}}
            if(!remainstrue) {++becomesTrue;}      // clause becomes true after flip.
        }
        return becomesTrue;}


    private HashSet<Integer> affected = new HashSet<>();

    /** flips the truth value of the predicate and updates the literalQueue
     *
     * @param predicate
     */
    private void flip(int predicate) {
        affected.clear();
        int trueLiteral = 0;
        for(CLiteral cliteral : index.getLiterals(predicate* rwModel.status[predicate])) {
            trueLiteral = findTrueLiteral(cliteral);}
        if(trueLiteral < 0) { // all are false
            for(CLiteral cliteral : index.getLiterals(predicate* rwModel.status[predicate])) {
                int pred = Math.abs(cliteral.literal);
                ++flipConsequences[pred];
                affected.add(pred);}}
        else {
            if(trueLiteral != 0) {
                int pred = Math.abs(trueLiteral);
                --flipConsequences[pred];
                affected.add(pred);}}

        for(CLiteral cliteral : index.getLiterals(-predicate* rwModel.status[predicate])) {
            trueLiteral = findTrueLiteral(cliteral);
            if(trueLiteral < 0) {
                --flipConsequences[predicate];
                affected.add(predicate);}
            else {
            if(trueLiteral > 0) {
                int pred = Math.abs(trueLiteral);
                ++flipConsequences[pred];
                affected.add(pred);}}}

        for(Integer pred : affected) {
            literalQueue.remove(pred);
            literalQueue.add(pred);}
        rwModel.flip(predicate);}

    /** checks if all literals in the clause except the literals itself are false or at most one is true
     *
     * @param cliteral the literal to be checked
     * @return -1 if all other literals are false, 0 if there is more than one true literal, otherwise the true literal.
     */
    private int findTrueLiteral(CLiteral cliteral) {
        boolean allFalse = true;
        int trueLiteral = 0;
        for(CLiteral otherliteral : cliteral.clause.cliterals) {
            if(otherliteral != cliteral && rwModel.isTrue(otherliteral.literal)) {
                allFalse = false;
                if(trueLiteral != 0) {trueLiteral = 0;}
                else {trueLiteral = otherliteral.literal;}}}
        return allFalse ? -1 : trueLiteral;}


    private int oldPredicate = 0;
    private int oldoldPredicate = 0;
    private int randomFrequency;
    private int randomCounter = 0;

    private int selectFlipPredicate() {
        int predicate = 0;
        if(++randomCounter == randomFrequency) {
            randomCounter = 0;
            Clause clause = falseClauses.get(random.nextInt(falseClauses.size()));
            return Math.abs(clause.cliterals.get(random.nextInt(clause.cliterals.size())).literal);}

        predicate = literalQueue.poll();
        int predicate1 = 0;
        if(predicate == oldoldPredicate || predicate == oldoldPredicate) {predicate1 = literalQueue.poll();}
        literalQueue.add(predicate);
        if(predicate1 != 0) {literalQueue.add(predicate1); predicate = predicate1;}
        oldoldPredicate = oldPredicate;
        oldPredicate = predicate;
        return predicate;}

    /** integrates the consequences of new true literals found out by centralDataHolder.
     */
    private void integrateNewTrueLiterals() {
        Integer[] trueLiterals = null;
        synchronized (globalModel) {
            if(newTrueLiterals.isEmpty()) {return;} // newTrueLiterals may be changed by another thread
            trueLiterals = new Integer[newTrueLiterals.size()];
            newTrueLiterals.toArray(trueLiterals);}
        for(Integer literal : trueLiterals) {makeGloballyTrue(literal);}}

    private void makeGloballyTrue(int literal) {

    }



}
