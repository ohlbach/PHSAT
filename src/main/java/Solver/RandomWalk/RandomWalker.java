package Solver.RandomWalk;

import Datastructures.Clauses.BasicClauseList;
import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseList;
import Datastructures.Literals.CLiteral;
import Datastructures.Literals.LiteralIndex;
import Datastructures.LocalModel;
import Datastructures.Theory.ImplicationGraph;
import Datastructures.TrueLiterals;
import Utilities.Utilities;

import java.util.*;
import java.util.function.BiConsumer;

/**
 * Created by ohlbach on 01.09.2018.
 */
public class RandomWalker {
    private int walker;
    private String id;
    HashMap<String,Object> solverControl;
    HashMap<String,Object> problemControl;
    BiConsumer<String,String> logger;
    private int seed;
    private int maxFlips;
    private int flipCounter = 0;
    private boolean withImplications;
    private BasicClauseList basicClauses;
    private ClauseList clauseList;
    private ImplicationGraph implicationGraph = null;
    TrueLiterals trueLiterals;
    private Thread thread = null;
    boolean stopped = false;
    public String info;
    private LiteralIndex index ;
    private LocalModel globalModel;
    private LocalModel localModel;
    private int predicates;
    private int[] flipConsequences;
    private PriorityQueue<Integer> literalQueue;
    private Random random;
    private ArrayList<Clause> falseClauses;

    private static HashSet<String> keys = new HashSet<>(); // contains the allowed keys in the specification.
    static { // these are the allowed keys in the specification.
        for(String key : new String[]{"class", "seed", "flips", "implications"}) {
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
        ArrayList implication = Utilities.parseBoolean(place+"implications",implications,errors);
        ArrayList<ArrayList> pars = Utilities.crossProduct(seed,flip,jump,implication);
        for(ArrayList<Object> p : pars ) {
            HashMap<String,Object> map = new HashMap<>();
            map.put("seed",pars.get(0));
            map.put("flips",pars.get(1));
            map.put("jumps",pars.get(2));
            map.put("implications",pars.get(3));
            list.add(map);}
        return list;}

    public static String help() {
        return "Random Walker: parameters:\n" +
                "seed:         for the random number generator           (default: 0)\n" +
                "flips:        for restricting the number of flips       (default: Max_Integer).\n" +
                "jumps:        frequency of random jumps                 (default: 10)\n" +
                "implications: if true then an implication graph is used (default: false).\n";}


    public RandomWalker(Integer walker, HashMap<String,Object> solverControl, HashMap<String,Object> problemControl,
                        LocalModel globalModel, BiConsumer<String,String> logger) {
        this.walker = walker;
        id = "Walker_"+walker;
        this.solverControl  = solverControl;
        this.problemControl = problemControl;
        this.logger = logger;
        this.globalModel = globalModel;
        trueLiterals = new TrueLiterals(globalModel);
        globalModel.addUnsatisfiableObserver((reason,item) -> {stopped = true; if(thread!=null) {thread.interrupt();}});
        globalModel.addSatisfiableObserver((reason,item)   -> {stopped = true; if(thread!=null) {thread.interrupt();}});
        globalModel.addPushObserver(literal ->
            {if(!stopped && Thread.currentThread() != thread) {trueLiterals.addExternalLiteral(literal);}});
    }


    public void solve(HashMap<String,Object> solverControl, HashMap<String,Object> problemControl, LocalModel globalModel) {
        if(stopped) {return;}
        logger.accept(id,"starting");
        thread = Thread.currentThread();
        prepareData();
        BasicClausesAnalyser bca = null;//new BasicClausesAnalyser((literal-> new CLiteral(literal)),
                //((Integer number,ArrayList<CLiteral> literals) -> new Clause(number,null,literals)),
                //clauseList,trueLiterals,globalModel);
        //if(bca.analyse(basicClauses,withImplications)) {return;}
        implicationGraph = null; // clauseList.implicationGraph;
        localModel = globalModel.copy();
        initializeModel();
        initializeFlipConsequences();
        falseClauses = null; //clauseList.falseClauses(localModel);
        while (++flipCounter <= maxFlips && !thread.isInterrupted() && !falseClauses.isEmpty()) {
            integrateGlobalUnits();
            flip(selectFlipPredicate());}}

    private void prepareData() {
        seed             = (Integer)solverControl.get("seed");
        maxFlips         = (Integer)solverControl.get("flips");
        randomFrequency  = (Integer)solverControl.get("jumps");
        withImplications = (Boolean)solverControl.get("implications");
        basicClauses     = (BasicClauseList)problemControl.get("disjunctions");
        predicates       = basicClauses.predicates;
        clauseList       = null; //new ClauseList(predicates,basicClauses.symboltable);
        info = "RandomWalker_" + walker + "(seed:"+seed+",flips:"+maxFlips+ (withImplications ? ",implications" : "") + ")";
        random           = new Random(seed);
        literalQueue     = new PriorityQueue<Integer>(predicates,(
                (l1,l2) -> {
                    int f1 = flipConsequences[l1];
                    int f2 = flipConsequences[l2];
                    if(f1 > f2) {return -1;}
                    return(f1 < f2) ? 1 : 0;}));}

    /** generates a candidate localModel for the disjunctions.
     * A predicate becomes true if it occurs in more disjunctions than its negation.
     */
    private void initializeModel() {
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            if(!globalModel.contains(predicate)) {
                localModel.push((clauseList.getOccurrences(predicate) > clauseList.getOccurrences(-predicate)) ? predicate : -predicate);}}}



    /** initializes flipConsequences and literalQueue.
     *  literalQueue contains the predicates ordered by the number
     *  of disjunctions made true when flipping the predicate.
     *  The head of the queue is the predicate which makes most disjunctions true by flipping it.
     */
    private void initializeFlipConsequences() {
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            if(!globalModel.contains(predicate)) {
                flipConsequences[predicate] = flipMakesTrue(predicate);}}
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            if(!globalModel.contains(predicate)) {literalQueue.add(predicate);}}}


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
        for(CLiteral cliteral : index.getLiterals(predicate* localModel.status(predicate))) {
            boolean remainstrue = false;
            for(CLiteral othercliteral : cliteral.clause.cliterals) {
                if(cliteral != othercliteral && localModel.isTrue(othercliteral.literal)) {remainstrue  = true; break;}}
            if(!remainstrue) {--becomesTrue;}      // clause becomes false after flip.
            }
        for(CLiteral cliteral : index.getLiterals(-predicate* localModel.status(predicate))) {
            boolean remainstrue = false;
            for(CLiteral othercliteral : cliteral.clause.cliterals) {
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
        for(CLiteral otherliteral : cliteral.clause.cliterals) {
            if(otherliteral != cliteral && localModel.isTrue(otherliteral.literal)) {
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


    private void integrateGlobalUnits() {}


}
