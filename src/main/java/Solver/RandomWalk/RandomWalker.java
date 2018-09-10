package Solver.RandomWalk;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseList;
import Datastructures.Literals.CLiteral;
import Datastructures.Literals.LiteralIndex;
import Datastructures.Model;
import Utilities.Utilities;

import java.util.*;

/**
 * Created by ohlbach on 01.09.2018.
 */
public class RandomWalker {
    private int walker;
    private ClauseList clauseList;
    private int maxFlips;
    private int flipCounter = 0;
    private int seed;
    public String info;
    private LiteralIndex index ;
    private Model globalModel;
    private Model localModel;
    private int predicates;
    private int[] flipConsequences;
    private PriorityQueue<Integer> literalQueue;
    private Random random;
    private ArrayList<Clause> falseClauses;
    private int externallyTerminated = 0;
    private ArrayList<Integer> globalUnits = new ArrayList<>();

    private static HashSet<String> keys = new HashSet<>(); // contains the allowed keys in the specification.
    static { // these are the allowed keys in the specification.
        for(String key : new String[]{"type", "seed", "flips"}) {
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
        String place = "Random Walker: ";
        ArrayList seed = Utilities.parseIntRange(place+"seed: ",seeds,errors);
        ArrayList flip = Utilities.parseIntRange(place+"flips: ",flips,errors);
        ArrayList<ArrayList> pars = Utilities.crossProduct(seed,flip);
        for(ArrayList<Object> p : pars ) {
            HashMap<String,Object> map = new HashMap<>();
            map.put("seed",pars.get(0));
            map.put("flips",pars.get(1));
            list.add(map);}
        return list;}

    public static String help() {
        return "Random Walker: parameters:\n" +
                "seed: for the random number generator\n" +
                "flips: for restricting the number of flips.\n";}

    public RandomWalker(Integer walker) {
        this.walker = walker;}

    public RandomWalker(int walker, ClauseList clauseList, int maxFlips, int seed) {
        this.walker = walker;
        this.clauseList = clauseList;
        index = clauseList.literalIndex;
        globalModel = clauseList.model;
        predicates = globalModel.predicates;
        flipConsequences = new int[predicates+1];
        literalQueue = new PriorityQueue<Integer>(predicates,(
                (l1,l2) -> {
                    int f1 = flipConsequences[l1];
                    int f2 = flipConsequences[l2];
                    if(f1 > f2) {return -1;}
                    return(f1 < f2) ? 1 : 0;}));
        this.seed = seed;
        random = new Random(seed);
        this.maxFlips = maxFlips;
        info = "Random Walker " + walker + " with seed " + seed;
        globalModel.addFinalObserver(literal -> {externallyTerminated = literal;});
        globalModel.addPushObserver(literal -> addGlobalUnit(literal));
    }



    public void solve(HashMap<String,Object> solverControl, HashMap<String,Object> problemControl, Model globalModel,
                      StringBuffer errors, StringBuffer warnings) {
        seed = (Integer)solverControl.get("seed");
        maxFlips = (Integer)solverControl.get("flips");
        localModel = globalModel.copy();
        initializeModel();
        initializeFlipConsequences();
        falseClauses = clauseList.falseClauses(localModel);
        while (++flipCounter <= maxFlips && externallyTerminated == 0 && !falseClauses.isEmpty()) {
            copyGlobalUnits();
            integrateGlobalUnits();
            flip(selectFlipPredicate());}}



    /** generates a candidate localModel for the clauses.
     * A predicate becomes true if it occurs in more clauses than its negation.
     */
    private void initializeModel() {
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            if(!globalModel.contains(predicate)) {
                localModel.push((index.getLiterals(predicate).size() > index.getLiterals(-predicate).size()) ? predicate : -predicate);}}}

    /** initializes flipConsequences and literalQueue.
     *  literalQueue contains the predicates ordered by the number
     *  of clauses made true when flipping the predicate.
     *  The head of the queue is the predicate which makes most clauses true by flipping it.
     */
    private void initializeFlipConsequences() {
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            if(!globalModel.contains(predicate)) {
                flipConsequences[predicate] = flipMakesTrue(predicate);}}
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            if(!globalModel.contains(predicate)) {literalQueue.add(predicate);}}}


    /** computes how many clauses more become true after flipping the predicate.
     * Example: p is true. </br>
     * All clauses with -p where all literals are false become true</br>
     * All clauses with p where all other literals are false become false</br>
     * The result is then the difference between these numbers.
     *
     * @param predicate the predicate to be checked
     * @return how many clauses more become true after flipping the predicate.
     */
    private int flipMakesTrue(int predicate) {
        int becomesTrue = 0;
        for(CLiteral cliteral : index.getLiterals(predicate* localModel.status(predicate))) {
            boolean remainstrue = false;
            for(CLiteral othercliteral : cliteral.getClause().cliterals) {
                if(cliteral != othercliteral && localModel.isTrue(othercliteral.literal)) {remainstrue  = true; break;}}
            if(!remainstrue) {--becomesTrue;}      // clause becomes false after flip.
            }
        for(CLiteral cliteral : index.getLiterals(-predicate* localModel.status(predicate))) {
            boolean remainstrue = false;
            for(CLiteral othercliteral : cliteral.getClause().cliterals) {
                if(cliteral != othercliteral && localModel.isTrue(othercliteral.literal)) {remainstrue  = true; break;}}
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
        for(CLiteral cliteral : index.getLiterals(predicate* localModel.status(predicate))) {
            trueLiteral = findTrueLiteral(cliteral);}
        if(trueLiteral < 0) { // all are false
            for(CLiteral cliteral : index.getLiterals(predicate* localModel.status(predicate))) {
                int pred = Math.abs(cliteral.literal);
                ++flipConsequences[pred];
                affected.add(pred);}}
        else {
            if(trueLiteral != 0) {
                int pred = Math.abs(trueLiteral);
                --flipConsequences[pred];
                affected.add(pred);}}

        for(CLiteral cliteral : index.getLiterals(-predicate* localModel.status(predicate))) {
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
        localModel.flip(predicate);}

    /** checks if all literals in the clause except the literals itself are false or at most one is true
     *
     * @param cliteral the literal to be checked
     * @return -1 if all other literals are false, 0 if there is more than one true literal, otherwise the true literal.
     */
    private int findTrueLiteral(CLiteral cliteral) {
        boolean allFalse = true;
        int trueLiteral = 0;
        for(CLiteral otherliteral : cliteral.getClause().cliterals) {
            if(otherliteral != cliteral && localModel.isTrue(otherliteral.literal)) {
                allFalse = false;
                if(trueLiteral != 0) {trueLiteral = 0;}
                else {trueLiteral = otherliteral.literal;}}}
        return allFalse ? -1 : trueLiteral;}


    private int oldPredicate = 0;
    private int oldoldPredicate = 0;
    private int randomFrequency = 10;
    private int randomCounter = 0;

    private int selectFlipPredicate() {
        int predicate = 0;
        if(++randomCounter == randomFrequency) {
            randomCounter = 0;
            Clause clause = falseClauses.get(random.nextInt(falseClauses.size()));
            return Math.abs(clause.cliterals[random.nextInt(clause.cliterals.length)].literal);}

        predicate = literalQueue.poll();
        int predicate1 = 0;
        if(predicate == oldoldPredicate || predicate == oldoldPredicate) {predicate1 = literalQueue.poll();}
        literalQueue.add(predicate);
        if(predicate1 != 0) {literalQueue.add(predicate1); predicate = predicate1;}
        oldoldPredicate = oldPredicate;
        oldPredicate = predicate;
        return predicate;}

    private synchronized void addGlobalUnit(int literal) {
        globalUnits.add(literal);}

    private Integer[] localGlobalUnits = null;

    private synchronized void copyGlobalUnits() {
        localGlobalUnits = new Integer[globalUnits.size()];
        globalUnits.toArray(localGlobalUnits);
        globalUnits.clear();}

    private void integrateGlobalUnits() {}


}
