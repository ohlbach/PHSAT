package Solver.RandomWalk;

import Coordinator.CentralDataHolder;
import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseList;
import Datastructures.Literals.CLiteral;
import Datastructures.Literals.LiteralIndex;
import Datastructures.Theory.ImplicationDAG;
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
    CentralDataHolder centralData;
    private ClauseList clauseList;
    private Model globalModel;
    private RWModel rwModel;
    private ArrayList<Integer> newTrueLiterals = new ArrayList<>();
    private ArrayList<int[]> newImplications = new ArrayList<>();
    private ArrayList<int[]> newEquivalences = new ArrayList<>();
    private int timestamp = 0;

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
        this.centralData = centralData;
        globalModel = centralData.model;
        rwModel = new RWModel(globalModel);
        clauseList = centralData.disjunctions.disjunctions.clone(); // now centralDataHolder may change its clauses
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

    public void solve(StringBuffer errors, StringBuffer warnings) {
        logger = (BiConsumer<String,String>)globalParameters.get("logger");
        logger.accept(id,"starting");
        random = new Random((Integer)solverControl.get("seed"));
        int maxFlips     = (Integer)solverControl.get("flips");
        randomFrequency  = (Integer)solverControl.get("jumps");
        predicates       = centralData.predicates;
        flipScore        = new int[predicates];
        globalModel      = centralData.model;
        implicationDAG = centralData.implicationDAG; // clonen
        index            = clauseList.literalIndex;
        int seed         = (Integer)solverControl.get("seed");
        random           = new Random(seed);
        info = id + "(seed:"+seed+",flips:"+maxFlips + ")";
        predicateQueue = new PriorityQueue<Integer>(predicates,(
                (l1,l2) -> {
                    int f1 = flipScore[l1];
                    int f2 = flipScore[l2];
                    if(f1 > f2) {return -1;}
                    return(f1 < f2) ? 1 : 0;}));
        integrateNewFacts();
        initializeModel();
        initializeFlipConsequences();
        initializeFalseClauses();
        Thread thread = Thread.currentThread();
        while (++flipCounter <= maxFlips && !thread.isInterrupted() && !falseClauses.isEmpty()) {
            integrateNewFacts();
            flip(selectFlipPredicate());}
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
    private void initializeFalseClauses() {
        falseClauses = new ArrayList<>();
        for(Clause clause : clauseList.clauses) {
            boolean isFalse = true;
            for(CLiteral cLiteral : clause.cliterals) {
                if(rwModel.isTrue(cLiteral.literal)) {isFalse = false; break;}}
            if(isFalse) {falseClauses.add(clause);}}}


    /** initializes flipScore and predicateQueue.
     *  predicateQueue contains the predicates ordered by the number
     *  of disjunctions made true when flipping the predicate.
     *  The head of the queue is the predicate which makes most disjunctions true by flipping it.
     */
    private void initializeFlipConsequences() {
        globalModel.readLock();
        try{
            for(int predicate = 1; predicate <= predicates; ++predicate) {
                if(!globalModel.contains(predicate)) {
                    flipScore[predicate] = flipScore(predicate);}}
            for(int predicate = 1; predicate <= predicates; ++predicate) {
                if(!globalModel.contains(predicate)) {
                    predicateQueue.add(predicate);}}}
        finally{globalModel.readUnLock();}}




    private HashSet<Integer> affected = new HashSet<>();

    /** flips the truth value of the predicate and updates the predicateQueue
     *
     * @param predicate
     */
    private void flip(int predicate) {
        if(rwModel.isTrue(predicate)) {
            for(CLiteral cLiteral : index.getLiterals(predicate)) {
                Clause clause = cLiteral.clause;
                if(isFalseBut(cLiteral)) { // all other literals are false
                    falseClauses.add(clause);
                    for(CLiteral cLiteral1 : clause.cliterals) {
                        if(cLiteral1 != cLiteral1) {changeScore(cLiteral1.literal,1);}}}
                else {CLiteral otherTrueLiteral = findOtherTrueLiteral(cLiteral);
                    if(otherTrueLiteral != null) {changeScore(otherTrueLiteral.literal,-1);}}}
            implicationDAG.apply(-predicate,true,(p -> {
                if(rwModel.isFalse(p)) {
                    for(CLiteral cLiteral : index.getLiterals(p)) {
                        if(isFalse(cLiteral.clause)) {
                            falseClauses.remove(cLiteral.clause);
                            for(CLiteral cLit : cLiteral.clause.cliterals) {changeScore(cLit.literal,-1);}}
                        else {CLiteral otherTrueLiteral = findOtherTrueLiteral(cLiteral);
                            if(otherTrueLiteral != null) {changeScore(otherTrueLiteral.literal,1);}}}}}));
            }
            else {
            implicationDAG.apply(predicate,true,(p->{
                if(rwModel.isFalse(p)) {
                    for(CLiteral cLiteral : index.getLiterals(p)) {
                        if(isFalse(cLiteral.clause)) {
                            falseClauses.remove(cLiteral.clause);
                            for(CLiteral cLiteral1 : cLiteral.clause.cliterals) {changeScore(cLiteral.literal,-1);}}
                        else {CLiteral otherTrueLiteral = findOtherTrueLiteral(cLiteral);
                            if(otherTrueLiteral!= null) {changeScore(otherTrueLiteral.literal,1);}}}}}));
            for(CLiteral cLiteral : index.getLiterals(-predicate)) {
                if(isFalseBut(cLiteral)) {
                    falseClauses.add(cLiteral.clause);
                    for(CLiteral cLiteral1 : cLiteral.clause.cliterals) {
                        if(cLiteral != cLiteral) {changeScore(cLiteral1.literal,+1);}}}
                    else {
                    CLiteral otherTrueLiteral = findOtherTrueLiteral(cLiteral);
                    if(otherTrueLiteral != null) {changeScore(otherTrueLiteral.literal,-1);}}}}

        rwModel.flip(predicate);}



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
        if(trueLiterals != null) {for(Integer literal : trueLiterals) {integrateTrueLiteral(literal);}}

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


    private void integrateTrueLiteral(int literal) {
        clauseList.makeTrue(literal);
// weiter
    }

    private void integrateEquivalence(int[] equivalence) {
// weiter
    }

    private void integrateImplication(int[] implication) {
// weiter
    }


    private boolean isFalse(Clause clause) {
        for(CLiteral cLit : clause.cliterals) {
            if(rwModel.isTrue(cLit.literal)) {return false;}}
        return true;}

    /** checks if all literals but the given one are false
     *
     * @param cLiteral a CLiteral
     * @return true if all literals except the given one are false.
     */
    private boolean isFalseBut(CLiteral cLiteral) {
        for(CLiteral cLit : cLiteral.clause.cliterals) {
            if(cLit != cLiteral && rwModel.isTrue(cLit.literal)) {return false;}}
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

    /** changes the flipScore of the given literal and updates the predicateQueue
     *
     * @param literal    an literal
     * @param difference the change to the score
     */
    private void changeScore(int literal, int difference) {
        int predicate = Math.abs(literal);
        predicateQueue.remove(predicate);
        flipScore[predicate] += difference;
        predicateQueue.add(predicate);}


    int flipScore(int literal) {
        return becomeTrue(literal) - becomeFalse(literal);}

    /** counts the number of clauses which are false and become true when the literal is flipped.
     *
     * @param literal a literal
     * @return the number of clauses which are false and become true when the literal is flipped.
     */
    private int becomeTrue(int literal) {
        ++timestamp;
        counter[0] = 0;
        if(rwModel.isTrue(literal)) {
            implicationDAG.apply(-literal,true,(lit->{  // all literals implied by -literal become true
                for(CLiteral cLiteral : index.getLiterals(lit)) {
                    Clause clause = cLiteral.clause;
                    if(clause.timestamp != timestamp) {
                        clause.timestamp = timestamp;
                        if(isFalse(clause)) {++counter[0];}}}}));}
        else {
            implicationDAG.apply(literal,true,(lit->{  // all literals implied by literal become true
                for(CLiteral cLiteral : index.getLiterals(lit)) {
                    Clause clause = cLiteral.clause;
                    if(clause.timestamp != timestamp) {
                        clause.timestamp = timestamp;
                        if(isFalse(clause)) {++counter[0];}}}}));}
        return counter[0];}

    /** counts the number of clauses which are false and become true when the literal is flipped.
     *
     * @param literal a literal
     * @return the number of clauses which are false and become true when the literal is flipped.
     */
    private void updateBecomesTrue(int literal) {
        ++timestamp;
        counter[0] = 0;
        if(rwModel.isTrue(literal)) {
            implicationDAG.apply(-literal,true,(lit->{  // all literals implied by -literal become true
                for(CLiteral cLiteral : index.getLiterals(lit)) {
                    Clause clause = cLiteral.clause;
                    if(clause.timestamp != timestamp) {
                        clause.timestamp = timestamp;
                        if(isFalse(clause)) {falseClauses.remove(clause);}}}}));}
        else {
            implicationDAG.apply(literal,true,(lit->{  // all literals implied by literal become true
                for(CLiteral cLiteral : index.getLiterals(lit)) {
                    Clause clause = cLiteral.clause;
                    if(clause.timestamp != timestamp) {
                        clause.timestamp = timestamp;
                        if(isFalse(clause)) {falseClauses.remove(clause);}}}}));}
        }

    /** counts the number of clauses which are true and become false when the literal is flipped.
     *
     * @param literal a literal
     * @return the number of clauses which are true and become false when the literal is flipped.
     */
    private int becomeFalse(int literal) { // the clauses must have exactly one true literal, which becomes false
        ++timestamp;
        counter[0] = 0;
        if(rwModel.isTrue(literal)) { // and becomes false
            for(CLiteral cLiteral : index.getLiterals(literal)) {if(isFalseBut(cLiteral)) {++counter[0];}}
            implicationDAG.apply(-literal,true,(lit->{   // literals implied by -literal may have any truth value.
                if(lit != literal && rwModel.isFalse(lit)){    // the false ones become true. Their negation is true and becomes false.
                    for(CLiteral cLiteral : index.getLiterals(-lit)) {
                        Clause clause = cLiteral.clause;
                        if(clause.timestamp != timestamp) {
                            clause.timestamp = timestamp;
                            if(isFalseBut(cLiteral)) {++counter[0];}}}}}));}
        else { // literal is false and becomes true. Its negation is true and becomes false
            for(CLiteral cLiteral : index.getLiterals(-literal)) {if(isFalseBut(cLiteral)) {++counter[0];}}
            implicationDAG.apply(literal,true,(lit->{  // literals implied by literal may have any truth value.
                if(lit != literal && rwModel.isFalse(lit)){ // the false ones become true. Their negation is true and becomes false.
                    for(CLiteral cLiteral : index.getLiterals(-lit)) {
                        Clause clause = cLiteral.clause;
                        if(clause.timestamp != timestamp) {
                            clause.timestamp = timestamp;
                            if(isFalseBut(cLiteral)) {++counter[0];}}}}}));}
        return counter[0];}

    private void updateBecomesFalse(int literal) { // the clauses must have exactly one true literal, which becomes false
        ++timestamp;
        counter[0] = 0;
        if(rwModel.isTrue(literal)) { // and becomes false
            for(CLiteral cLiteral : index.getLiterals(literal)) {if(isFalseBut(cLiteral)) {++counter[0];}}
            implicationDAG.apply(-literal,true,(lit->{   // literals implied by -literal may have any truth value.
                if(lit != literal && rwModel.isFalse(lit)){    // the false ones become true. Their negation is true and becomes false.
                    for(CLiteral cLiteral : index.getLiterals(-lit)) {
                        Clause clause = cLiteral.clause;
                        if(clause.timestamp != timestamp) {
                            clause.timestamp = timestamp;
                            if(isFalseBut(cLiteral)) {falseClauses.add(clause);}}}}}));}
        else { // literal is false and becomes true. Its negation is true and becomes false
            for(CLiteral cLiteral : index.getLiterals(-literal)) {if(isFalseBut(cLiteral)) {++counter[0];}}
            implicationDAG.apply(literal,true,(lit->{  // literals implied by literal may have any truth value.
                if(lit != literal && rwModel.isFalse(lit)){ // the false ones become true. Their negation is true and becomes false.
                    for(CLiteral cLiteral : index.getLiterals(-lit)) {
                        Clause clause = cLiteral.clause;
                        if(clause.timestamp != timestamp) {
                            clause.timestamp = timestamp;
                            if(isFalseBut(cLiteral)) {falseClauses.add(clause);}}}}}));}}


    }





