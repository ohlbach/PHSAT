package Solvers.RecursiveSearch;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.InputClauses;
import Datastructures.Results.Result;
import Datastructures.Statistics.Statistic;
import Datastructures.Theory.Model;
import Management.ProblemSupervisor;
import Solvers.Solver;
import Utilities.Utilities;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import it.unimi.dsi.fastutil.ints.IntArrays;

import java.util.*;
import java.util.function.IntConsumer;

public class RecursiveSearcher  extends Solver {
    private final ArrayList<RSClause> rsClauses = new ArrayList<>();  // collects all clauses
    private final ArrayList<RSLiteral>[] posOccurrences; // maps predicate to clauses containing them positively
    private final ArrayList<RSLiteral>[] negOccurrences; // maps predicate to clauses containing them negatively
    private Model localModel = null;
    public RSStatistics statistics;
    private boolean[] blockedPredicates;
    private int[] literalsForSelection;
    private final int seed;                             // for the random number generator
    private final Random random;                       // random number generator for flip jumps
    /** collects globally true literals which are inserted by other solvers  into the global model */
    private final IntArrayList globallyTrueLiterals = new IntArrayList();
    /** a temporary local copy of the globally true literals */
    private final IntArrayList globallyTrueLiteralsCopy = new IntArrayList();
    private int timestamp = 1;

    public static String help() {
        return "Recursive Searcher: parameters:\n" +
                "seed:   for the random number generator      (default: 0)\n";}

    private static final HashSet<String> keys = new HashSet<>(); // contains the allowed keys in the specification.
    static { // these are the allowed keys in the specification.
        Collections.addAll(keys, "name", "seed", "type", "solver");}

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
                warnings.append("Recursive Searcher: unknown key in parameters: " + key + "\n" +
                        "        allowed keys: seed, flips, jumps.\n");}}
        ArrayList<HashMap<String,Object>> list = new ArrayList<>();
        String seeds = parameters.get("seed");
        if(seeds == null) seeds = "0";

        String place = "RS: ";

        ArrayList seed = Utilities.parseIntRange(place+"seed: ",seeds,errors);
        ArrayList<ArrayList> pars = Utilities.crossProduct(seed);
        int counter = 0;
        for(ArrayList<Object> p : pars ) {
            HashMap<String,Object> map = new HashMap<>();
            map.put("seed",p.get(0));
            map.put("name","RS" + ++counter);
            list.add(map);}
        return list;}

    /** constructs a new Walker solver.
     *
     * @param solverNumber         for distinguishing different solvers of the same type, but different parameters
     * @param solverParameters     contains the parameters for controlling the solver
     * @param problemSupervisor    coordinates several solvers.
     */
    public RecursiveSearcher(Integer solverNumber, HashMap<String,Object> solverParameters, ProblemSupervisor problemSupervisor) {
        super(solverNumber,solverParameters, problemSupervisor);
        super.initialize();
        posOccurrences = new ArrayList[predicates+1];
        negOccurrences = new ArrayList[predicates+1];
        blockedPredicates = new boolean[predicates+1];
        blockedPredicates[0] = true;
        statistics = new RSStatistics(combinedId);
        seed = (int) solverParameters.get("seed");
        random = new Random(seed);
        literalsForSelection = new int[2*predicates+1];
        int counter = 0;
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            literalsForSelection[++counter] = predicate;
            literalsForSelection[++counter] = -predicate;}
    }

    @Override
    public Result solveProblem(InputClauses inputClauses) {
        return null;
    }

    private ArrayList<RSClause> dummyClauses = new ArrayList<>(2);

    public void addClause(Clause clause) {
        RSClause.newRSClauses(clause,dummyClauses);
        for(RSClause rsClause : dummyClauses) addRSClause(rsClause);}


    @Override
    public void prepare() {
        localModel = model.clone();
        model.addObserver((literal,step) -> {
            synchronized(this) {globallyTrueLiterals.add((int)literal);}});
    }




    public Result solve() {
        integrateGloballyTrueLiterals();
        int literal = findStartLiteral();
        RSNode rsNode = RSNode.popRSNodeReserve(literal,null);
        while(rsNode != null) {
            rsNode = blockLiteral(literal,rsNode);
            if(rsNode == null) rsNode = RSNode.popRSNodeReserve(literal,null);
            literal = findStartLiteral();
            statistics.deepestSearchDepth = Math.max(statistics.deepestSearchDepth,rsNode.searchDepth);
        }
        return null;
    }

    protected RSNode blockLiteral(int literal, RSNode rsNode) {
        for(RSLiteral rsLiteral : (literal > 0 ? posOccurrences[literal] : negOccurrences[-literal])) {
            if(!rsLiteral.clause.isBlocked()) rsLiteral.declareTrue(rsNode);}
        for(RSLiteral rsLiteral : (literal > 0 ? negOccurrences[literal] : posOccurrences[-literal])) {
            if(!rsLiteral.clause.isBlocked()){
                RSNode backtrackedNode = rsLiteral.declareFalse(rsNode);
                if(backtrackedNode != null) return backtrackedNode;}}
        RSNode backtrackedNode = rsNode.propagateUnits(this);
        if(backtrackedNode != null) return backtrackedNode;
        return null;}


    /** The method blocks all clauses which are subsumed by the given clause.
     *  A subsumer C: atleast n phi subsumes a <br>
     *  sumsumee   D: atleast m psi iff <br>
     *  1. phi subseteq psi <br>
     *  2. n &ge; m <br>
     *  3. The sum of the multiplicity differences k - l for p^k in phi and p^l in psi &le; n-m, <br>
     *     for those multiplicities where l &lt; k
     *
     * @param rsClause a subsumer
     * @param rsNode  the current search node.
     */
    private void backwardSubsumption(RSClause rsClause, RSNode rsNode) {
        RSLiteral rsLiteral = rsClause.rsLiterals[0];
        int literal = rsLiteral.literal;
        int minLimit = rsClause.minLimit;
        short multiplicity = rsLiteral.multiplicity;
        for (RSLiteral otherRSLiteral : (literal > 0 ? posOccurrences[literal] : negOccurrences[-literal])) {
            RSClause otherClause = otherRSLiteral.clause;
            if(otherClause.isBlocked() || otherClause.minLimit > minLimit) continue;
            short otherMultiplicity = otherRSLiteral.multiplicity;
            if(otherMultiplicity > multiplicity) otherClause.timestamp = timestamp + 1;
            else {otherClause.timestamp = timestamp + 1 + multiplicity - otherMultiplicity;}}

        int length  = rsClause.rsLiterals.length;
        boolean last = false;
        int size = 0;
        for(int i = 1; i < length; ++i) {
            rsLiteral = rsClause.rsLiterals[i];
            if(rsLiteral.blockingNodeIds != 0) {last = true; size = i+1;}
            else {
                last = i == length-1; size = length;
                literal = rsLiteral.literal;
                for (RSLiteral otherRSLiteral : (literal > 0 ? posOccurrences[literal] : negOccurrences[-literal])) {
                    RSClause otherClause = otherRSLiteral.clause;
                    if(otherClause.timestamp < timestamp) continue;
                    short otherMultiplicity = otherRSLiteral.multiplicity;
                    if(otherMultiplicity > multiplicity) otherClause.timestamp += 1;
                    else {otherClause.timestamp += 1 + multiplicity - otherMultiplicity;}
                    if(last && otherClause.timestamp - timestamp == size)
                        declareSubsumption(rsClause, otherClause,rsNode);}}}
        timestamp += rsClause.rsLiterals.length + 1;}

    private void declareSubsumption(RSClause subsumer, RSClause subsumee, RSNode rsNode) {
        subsumee.block(rsNode);
        ++statistics.subsumptions;
    }


    private void integrateGloballyTrueLiterals() {
        copyGloballyTrueLiterals();
        if(globallyTrueLiteralsCopy.isEmpty()) return;
        for(int literal : globallyTrueLiteralsCopy) {
            integrateGloballyTrueLiteral(literal);
            blockedPredicates[Math.abs(literal)] = true;}}

    private void integrateGloballyTrueLiteral(int literal) {

    }

    /** copies the globally true literals to a local copy, such that other threads could fill it anew
     *
     * @return a copy of the globally true literals
     */
    private synchronized IntArrayList copyGloballyTrueLiterals() {
        globallyTrueLiteralsCopy.clear();
        globallyTrueLiterals.forEach((IntConsumer) globallyTrueLiteralsCopy::add);
        globallyTrueLiterals.clear();
        return globallyTrueLiteralsCopy;}


    /** returns a randomly selected unblocked literal of those literals which occur most
     *
     * @return a literal which occurs most as start literal for the search
     */
    private int findStartLiteral() {
        IntArrays.quickSort(literalsForSelection,((i, j)-> { // can be optimized by storing unblockedLiterals
            return Integer.compare(unblockedLiterals(j),unblockedLiterals(i));}));
        int counter = unblockedLiterals(literalsForSelection[0]);
        for(int i = 0; i < literalsForSelection.length; ++i) {
            if(unblockedLiterals(literalsForSelection[i]) != counter) break;}
        return literalsForSelection[random.nextInt(counter)];}

    @Override
    public Statistic getStatistics() {return statistics;}

    /** adds a new RSClause to the clause list and the literal index
     *
     * @param rsClause a new rsClause.
     */
    private void addRSClause(RSClause rsClause) {
        rsClauses.add(rsClause);
        for(RSLiteral rsLiteral :  rsClause.rsLiterals) {
            int literal = rsLiteral.literal;
            if(literal > 0 ) {
                ArrayList<RSLiteral> posLiterals = posOccurrences[predicates];
                if(posLiterals == null) {
                    posLiterals = new ArrayList<>();
                    posOccurrences[predicates] = posLiterals;}
                posOccurrences[literal].add(rsLiteral);}
            else {
                ArrayList<RSLiteral> negLiterals = negOccurrences[predicates];
                if(negLiterals == null) {
                    negLiterals = new ArrayList<>();
                    negOccurrences[predicates] = negLiterals;}
                negOccurrences[-literal].add(rsLiteral);}}}

    /** returns the current number of unblocked literals
     *
     * @param literal
     * @return the current number of unblocked literals
     */
    private int unblockedLiterals(int literal) {
        if(blockedPredicates[Math.abs(literal)]) return 0;
        ArrayList<RSLiteral> rsLiterals = (literal > 0) ? posOccurrences[literal] : negOccurrences[-literal];
        int counter = 0;
        for(RSLiteral rsLiteral :rsLiterals) {
            if(rsLiteral.blockingNodeIds == 0 && !rsLiteral.clause.isBlocked()) ++counter;}
        return counter;}

}
