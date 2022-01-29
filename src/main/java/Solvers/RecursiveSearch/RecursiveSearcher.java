package Solvers.RecursiveSearch;

import Datastructures.Clauses.Clause;
import Datastructures.Results.Result;
import Datastructures.Statistics.Statistic;
import Datastructures.Theory.Model;
import Management.ProblemSupervisor;
import Solvers.Solver;
import Utilities.Utilities;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.*;
import java.util.function.IntConsumer;

public class RecursiveSearcher  extends Solver {
    private final ArrayList<RSClause> rsClauses = new ArrayList<>();  // collects all clauses
    private final ArrayList<RSLiteral>[] posOccurrences; // maps predicate to clauses containing them positively
    private final ArrayList<RSLiteral>[] negOccurrences; // maps predicate to clauses containing them negatively
    private Model localModel = null;
    public RSStatistics statistics;
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
        statistics = new RSStatistics(combinedId);
        seed = (int) solverParameters.get("seed");
        random = new Random(seed);
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




    @Override
    public Result solve() {
        integrateGloballyTrueLiterals();
        int literal = findStartLiteral();
        RSNode rsNode = RSNode.popRSNodeReserve();
        rsNode.initialize(literal,null);
        return null;
    }

    private RSNode blockLiteral(int literal, RSNode rsNode) {
        for(RSLiteral rsLiteral : (literal > 0 ? posOccurrences[literal] : negOccurrences[-literal])) {
            rsLiteral.clause.blockClause(rsLiteral,rsNode);}
        for(RSLiteral rsLiteral : (literal > 0 ? negOccurrences[literal] : posOccurrences[-literal])) {
            RSClause emptyCLause = rsLiteral.blockLiteral(rsNode);
            if(emptyCLause != null) {return backtrack(emptyCLause,rsNode);}}
        return null;}

    /** blocks all literals which are stored as temporary true literals in the rsNode
     *
     * @param rsNode the current search node
     * @return null or, in case a clause became contradictious: the supernode after backtracking.
     */
    private RSNode unitSnowball(RSNode rsNode) {
        int literal = 0;
        while((literal = rsNode.nextTrueLiteral()) != 0) {
            RSNode supernode = blockLiteral(literal,rsNode);
            if(supernode != null) return supernode;}
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
            if(rsLiteral.rsNode != null) {last = true; size = i+1;}
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


    /** unblocks all rsLiterals stored in the rsNode.
     *
     * @param rsNode an rsNode which became superfluous
     */
    private void unblockLiterals(RSNode rsNode) {
        for(RSLiteral rsLiteral : rsNode.rsLiterals) rsLiteral.rsNode = null;}

    private RSNode backtrack(RSClause emptyClause, RSNode rsNode) {
        RSNode supernode = null;
        for(int i = 1; i < emptyClause.rsLiterals.length; ++i) {
            supernode = emptyClause.rsLiterals[i].rsNode;
            if(supernode != null) break;}
        if(supernode == null) { // example: atleast 3 p,q,r^2 and false(r)
            unblockLiterals(rsNode);
            supernode = rsNode.superNode;
            RSNode.pushRSNodeReserve(rsNode);
            return supernode;}
        while(rsNode != supernode) {
            unblockLiterals(rsNode);
            RSNode.pushRSNodeReserve(rsNode);
            rsNode = rsNode.superNode;}
        return supernode;}

    private void integrateGloballyTrueLiterals() {
        copyGloballyTrueLiterals();
        if(globallyTrueLiteralsCopy.isEmpty()) return;
        for(int literal : globallyTrueLiteralsCopy) {
            integrateGloballyTrueLiteral(literal);}}

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


    /** returns a randomly selected literal of those literals which occur most
     *
     * @return a literal which occurs most as start literal for the search
     */
    private int findStartLiteral() {
        IntArrayList literals = new IntArrayList();
        int occurrences = 0;
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            ArrayList<RSLiteral> posLiterals = posOccurrences[predicates];
            if(posLiterals != null) {
                int occ = posLiterals.size();
                if(occ > occurrences) {occurrences = occ; literals.clear(); literals.add(predicate);}
                else {if(occ == occurrences) literals.add(predicate);}}
            ArrayList<RSLiteral> negLiterals = negOccurrences[predicates];
            if(negLiterals != null) {
                int occ = negLiterals.size();
                if(occ > occurrences) {occurrences = occ; literals.clear(); literals.add(-predicate);}
                else {if(occ == occurrences) literals.add(-predicate);}}}
        return literals.getInt(random.nextInt(literals.size()));}

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
}
