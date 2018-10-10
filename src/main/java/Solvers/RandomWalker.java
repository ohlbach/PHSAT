package Solvers;

import Coordinator.CentralProcessor;
import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseList;
import Datastructures.Literals.CLiteral;
import Datastructures.Literals.LiteralIndex;
import Datastructures.Results.Result;
import Datastructures.Theory.ImplicationDAG;
import Datastructures.Theory.Model;
import Utilities.Utilities;

import java.util.*;
import java.util.function.BiConsumer;

/**
 * Created by ohlbach on 01.09.2018.
 */
public class RandomWalker extends Solver {


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
    CentralProcessor centralProcessor;
    private ClauseList clauseList;
    private Model globalModel;
    private RWModel rwModel;
    private ArrayList<Integer> newTrueLiterals = new ArrayList<>();
    private ArrayList<int[]> newImplications = new ArrayList<>();
    private ArrayList<int[]> newEquivalences = new ArrayList<>();
    private int timestamp = 0;

    /** constructs a new solver of type RandomWalker.
     * The constructor is called serially. Therefore there are no race conditions.
     * globalParameters and centralProcessor are shared between different threads.
     * The walker is passive in the sense that it does not send data to the CentralDataHolder.
     *
     * @param walker            counts the constructed walker
     * @param solverControl     contains the parameters for controlling the solger
     * @param globalParameters  contains the global control parameters
     * @param centralProcessor       contains the result of parsing and initializing the problem data.
     */
    public RandomWalker(Integer walker,  HashMap<String,Object> solverControl, HashMap<String,Object> globalParameters,
                        CentralProcessor centralProcessor) {
        id = "Walker_"+walker;
        this.solverControl  = solverControl;
        this.globalParameters = globalParameters;
        this.centralProcessor = centralProcessor;
        globalModel = centralProcessor.model;
        rwModel = new RWModel(globalModel);
        clauseList = centralProcessor.clauses.clone(); // now centralDataHolder may change its clauses
        globalModel.addNewTruthObserver(literal         -> newTrueLiterals.add(literal));
        implicationDAG.addImplicationObserver((from,to) -> newImplications.add(new int[]{from,to}));
        implicationDAG.addEquivalenceObserver(eqv       -> newEquivalences.add(eqv));
    }

    public String info;
    private int predicates;
    private ImplicationDAG implicationDAG;
    private BiConsumer<String,String> logger;
    private Random random;
    private LiteralIndex index ;
    private int[] flipScore;
    private PriorityQueue<Integer> predicateQueue;
    private ArrayList<Clause> falseClauses;
    int flipCounter  = 0;

    public Result solve(StringBuffer errors, StringBuffer warnings) {
        logger = (BiConsumer<String,String>)globalParameters.get("logger");
        logger.accept(id,"starting");
        random = new Random((Integer)solverControl.get("seed"));
        int maxFlips     = (Integer)solverControl.get("flips");
        randomFrequency  = (Integer)solverControl.get("jumps");
        predicates       = centralProcessor.predicates;
        flipScore        = new int[predicates];
        globalModel      = centralProcessor.model;
        implicationDAG   = centralProcessor.implicationDAG; // clonen
        index            = clauseList.literalIndex;
        int seed         = (Integer)solverControl.get("seed");
        random           = new Random(seed);
        info = id + "(seed:"+seed+",flips:"+maxFlips + ")";
        predicateQueue = new PriorityQueue<Integer>(predicates,(
                (l1,l2) -> {
                    int f1 = combinedScore(l1);
                    int f2 = combinedScore(l2);
                    if(f1 > f2) {return -1;}
                    return(f1 < f2) ? 1 : 0;}));
        integrateNewFacts();
        initializeModel();
        initializeQueue();
        Thread thread = Thread.currentThread();
        while (++flipCounter <= maxFlips && !thread.isInterrupted() && !falseClauses.isEmpty()) {
            integrateNewFacts();
            flip(selectFlipPredicate());}
        return null;
            }

    private void addObservers() {
        clauseList.addClauseRemovalObserver(clause -> updateClause(clause,-1));
        clauseList.addLiteralRemovalObserver(cLiteral -> updateLiteralRemoval(cLiteral));
        clauseList.addLiteralReplacementObserver((cLiteral,before) -> updateClause(cLiteral.clause,before ? -1 : 1));
    }


    /** generates a candidate rwModel for the clauses.
     * A predicate becomes true if it occurs in more clauses than its negation.
     */
    private void initializeModel() {
        implicationDAG.applyToRoots(literal -> {
            literal = getOccurrences(literal) > getOccurrences(-literal) ? literal : -literal;
            implicationDAG.apply(literal,true,(lit-> {
                rwModel.status[ Math.abs(lit)] = (short)(lit > 0 ? 1 : -1);}));});
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            if(rwModel.status[predicate] == 0) {
                rwModel.status[predicate] = (short)(getOccurrences(predicate) > getOccurrences(-predicate) ? 1 : -1);}}}

    /** adds (change = 1) or removes (change = -1) a clause.
     * Updates flipScore and falseClauses
     *
     * @param clause a clause
     * @param change +1 for adding the clause, -1 for removing the clause.
     */
    private void updateClause(Clause clause, int change) {
        CLiteral trueLiteral = null;
        for(CLiteral cLiteral : clause.cliterals) {
            if(rwModel.isTrue(cLiteral.literal)) {
                if(trueLiteral != null) {return;}  // at least two true literals. Flips don't change the status
                else{trueLiteral = cLiteral;}}}
        if(trueLiteral == null) {
            if(change > 0) {falseClauses.add(clause);} else {falseClauses.remove(clause);}
            clause.applyToLiteral(literal -> changeScore(Math.abs(literal), change));} // flipping a literal increases the number of true literals
        else {changeScore(Math.abs(trueLiteral.literal),-change); }} // flipping this literal destroys a true literal.

    private void updateLiteralRemoval(CLiteral cLiteral) {
        Clause clause = cLiteral.clause;
        CLiteral trueLiteral = null;
        for(CLiteral cLit : clause.cliterals) {
            if(rwModel.isTrue(cLit.literal)) {
                if(trueLiteral == null) {trueLiteral = cLit; continue;}
                return;}} // two true literals in the remaining clause; nothing changes.

        boolean wasTrue = rwModel.isTrue(cLiteral.literal);
        if(trueLiteral == null) {
            if(wasTrue) { // the only true literal was removed.
                clause.applyToLiteral(literal -> changeScore(Math.abs(literal),1));
                falseClauses.add(clause);}
            return;} // all literals are false. Nothing changes
        if(wasTrue) { // trueLiteral is now the only true literal.
            changeScore(trueLiteral.literal,-1);}
    }

    private int[] counter = new int[]{0};

    /** counts the clauses containing the literal and its implied literals
     *
     * @param literal a literal
     * @return the number of clauses containing the literal and its implied literals.
     */
    private int getOccurrences(int literal) {
        ++timestamp;
        counter[0] = 0;
        implicationDAG.apply(literal,true,(lit-> {
            for(CLiteral cLiteral : clauseList.getLiterals(lit)){
                Clause clause = cLiteral.clause;
                if(clause.timestamp != timestamp) {
                    clause.timestamp = timestamp;
                    ++counter[0];}}}));
        return counter[0];}

                /** initializes the falseClauses array with all clauses which are false in the current rwModel.
                 */
    private void initializeQueue() {
        falseClauses = new ArrayList<>();
        for(Clause clause : clauseList.clauses) {
            updateClause(clause,1);}
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            if(globalModel.status(predicate) == 0) {predicateQueue.add(predicate);}}}

    private int[] score = new int[]{0};
    private int combinedScore(int predicate) {
        score[0] = 0;
        int literal = rwModel.isTrue(predicate) ? -predicate : predicate;
        implicationDAG.apply(literal,true,
                (lit -> {if(rwModel.isFalse(lit)) {score[0] += flipScore[Math.abs(lit)];}}));
        return score[0];}


    /** flips the truth value of the predicate and updates the predicateQueue
     *
     * @param predicate
     */
    private void flip(int predicate) {
        implicationDAG.apply(-rwModel.status[predicate]*predicate,true,(p -> {
            if(rwModel.isFalse(p)) {
                for(CLiteral cLiteral : index.getLiterals(p)) {
                    Clause clause = cLiteral.clause;
                    if(isFalse(clause)) {
                        falseClauses.remove(clause);
                        clause.applyToLiteral(literal->changeScore(literal,-1));}
                    else {CLiteral otherTrueLiteral = findOtherTrueLiteral(cLiteral);
                        if(otherTrueLiteral != null) {changeScore(otherTrueLiteral.literal,1);}}}};
                rwModel.flip(p);}));}



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

        predicate = predicateQueue.poll();
        int predicate1 = 0;
        if(predicate == oldoldPredicate || predicate == oldoldPredicate) {predicate1 = predicateQueue.poll();}
        predicateQueue.add(predicate);
        if(predicate1 != 0) {
            predicateQueue.add(predicate1); predicate = predicate1;}
        oldoldPredicate = oldPredicate;
        oldPredicate = predicate;
        return predicate;}

    /** integrates the consequences of new true literals found out by centralDataHolder.
     */
    private void integrateNewFacts() {
        Integer[] trueLiterals = null;
        globalModel.readLock();
        try {
            if(!newTrueLiterals.isEmpty()) { // newTrueLiterals may be changed by another thread
                trueLiterals = new Integer[newTrueLiterals.size()];
                newTrueLiterals.toArray(trueLiterals);
                newTrueLiterals.clear();}}
        finally{globalModel.readUnLock();}
        if(trueLiterals != null) {for(Integer literal : trueLiterals) {clauseList.makeTrue(literal);}}

        int[][] implications = null;
        int[][] equivalences = null;
        implicationDAG.readLock();
        try {
            if(!newImplications.isEmpty()) {
                implications = new int[newImplications.size()][];
                newImplications.toArray(implications);
                newImplications.clear();}
            if(!newEquivalences.isEmpty()) {
                equivalences = new int[newEquivalences.size()][];
                newEquivalences.toArray(equivalences);
                newEquivalences.clear();}}
        finally{implicationDAG.readUnLock();}
        if(equivalences != null) {for(int[] eqv : equivalences) {integrateEquivalence(eqv);}}
        for(int[] implication : implications) {integrateImplication(implication);}}




    private void integrateEquivalence(int[] equivalence) {
        int representative = equivalence[0];
        for (int i = 1; i < equivalence.length; ++i) {clauseList.replaceByRepresentative(representative,equivalence[i]);}
    }

    private ArrayList<Object> toBeRemoved = new ArrayList<>();
    private void integrateImplication(int[] implication) {
        int timestamp = ++clauseList.timestamp;
        int from = implication[0];
        int to   = implication[1];
        toBeRemoved.clear();
        for(CLiteral cLiteral : clauseList.literalIndex.getLiterals(-from)) {
            cLiteral.clause.timestamp = timestamp;}
        for(CLiteral cLiteral : clauseList.literalIndex.getLiterals(to)) {
            if(cLiteral.clause.timestamp == timestamp) {toBeRemoved.add(cLiteral.clause);}
            else{cLiteral.clause.timestamp = timestamp;}}
        for(Object object : toBeRemoved) {clauseList.removeClause((Clause)object);}
        toBeRemoved.clear();
        for(CLiteral cLiteral : clauseList.literalIndex.getLiterals(-to)) {
            if(cLiteral.clause.timestamp == timestamp) {toBeRemoved.add(cLiteral);}}
        for(Object object : toBeRemoved) {clauseList.removeLiteral((CLiteral) object);}
        toBeRemoved.clear();
        for(CLiteral cLiteral : clauseList.literalIndex.getLiterals(from)) {
            if(cLiteral.clause.timestamp == timestamp) {toBeRemoved.add(cLiteral);}}
        for(Object object : toBeRemoved) {clauseList.removeLiteral((CLiteral) object);}
    }


    private boolean isFalse(Clause clause) {
        for(CLiteral cLit : clause.cliterals) {
            if(rwModel.isTrue(cLit.literal)) {return false;}}
        return true;}


    /** searches for the only other true literal besides the given one
     *
     * @param cLiteral a CLiteral
     * @return a true literal where all others except the given one are false, null if no such literal exists.
     */
    private CLiteral findOtherTrueLiteral(CLiteral cLiteral) {
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
    private void changeScore(int literal, int difference) {
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
    private void updateScores() {
        for(Map.Entry<Integer,Integer> entry : affected.entrySet()) {
            Integer predicate = entry.getKey();
            Integer value = entry.getValue();
            if(value != 0) {
                implicationDAG.apply(predicate,false,(p->predicateQueue.remove(Math.abs(p))));}}
        for(Map.Entry<Integer,Integer> entry : affected.entrySet()) {
            Integer predicate = entry.getKey();
            Integer value = entry.getValue();
            if(value != 0) {flipScore[predicate] = value;}}
        for(Map.Entry<Integer,Integer> entry : affected.entrySet()) {
            Integer predicate = entry.getKey();
            Integer value = entry.getValue();
            if(value != 0) {
                implicationDAG.apply(predicate,false,(p->predicateQueue.add(Math.abs(p))));}}
        affected.clear();}

}







