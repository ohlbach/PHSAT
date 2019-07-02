package Solvers.Resolution;

import Algorithms.Algorithms;
import Coordinator.CentralProcessor;
import Coordinator.Tasks.Task;
import Coordinator.Tasks.TaskQueue;
import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseList;
import Datastructures.Clauses.ClauseStructure;
import Datastructures.Literals.CLiteral;
import Datastructures.Literals.LiteralIndexSorted;
import Datastructures.Results.Erraneous;
import Datastructures.Results.Result;
import Datastructures.Results.Satisfiable;
import Datastructures.Results.Unsatisfiable;
import Solvers.Solver;
import Utilities.Utilities;
import Utilities.BucketSortedList;
import Utilities.SingleList;

import java.util.*;

/**
 * Created by ohlbach on 18.10.2018.
 */
public class Resolution extends Solver {

    private static HashSet<String> keys = new HashSet<>(); // contains the allowed keys in the specification.
    static { // these are the allowed keys in the specification.
        for(String key : new String[]{"name", "seed", "strategy", "percentage", "limit", "type", "solver"}) {
            keys.add(key);}}

    /** parses a HashMap with key-value pairs:<br>
     *
     * @param parameters  the parameters with the keys "seed", "strategy", "percentage", "limit"
     * @param errors      for error messages
     * @param warnings    for warnings
     * @return            a list of HashMaps with keys "seed" and "sos", "limit".
     */
    public static ArrayList<HashMap<String,Object>> parseParameters(HashMap<String,String> parameters, StringBuffer errors, StringBuffer warnings){
        for(String key : parameters.keySet()) {
            if(!keys.contains(key)) {warnings.append("Resolution: unknown key in parameters: " + key + "\n");}}
        ArrayList<HashMap<String,Object>> list = new ArrayList<>();
        String seeds = parameters.get("seed");
        if(seeds == null) {seeds = "0";}
        String strategiess = parameters.get("strategy");
        if(strategiess == null) {strategiess = "INPUT";}
        String percentages = parameters.get("percentage");
        if(percentages == null) {percentages = "50";}
        String limits = parameters.get("limit");
        if(limits == null) {limits = Integer.toString(Integer.MAX_VALUE);}
        String place           = "Resolution: ";
        ArrayList seed         = Utilities.parseIntRange(place+"seed: ",seeds,errors);
        ArrayList percentage   = Utilities.parseIntRange(place+"percentage: ",percentages,errors);
        ArrayList limit = Utilities.parseIntRange(place+"limit: ",limits,errors);
        ArrayList strategies   = Strategy.parseStrategies(strategiess,place, warnings,errors);
        ArrayList<ArrayList> pars = Utilities.crossProduct(seed,strategies,percentage,limit);
        int counter = 0;
        for(ArrayList<Object> p : pars ) {
            Integer perc = (Integer)p.get(2);
            if(perc < 0 || perc > 100) {errors.append("Resolution: sos must be a percentage between 0 and 100, not"+perc);}
            Integer limitpar = (Integer)p.get(3);
            if(limitpar < 0) {errors.append("Resolution: limit must be positive: " + limitpar);}
            HashMap<String,Object> map = new HashMap<>();
            map.put("seed",     p.get(0));
            map.put("strategy", p.get(1));
            map.put("percentage",perc);
            map.put("limit",    limitpar);
            map.put("name","R" + ++counter);
            list.add(map);}
        return list;}

    public static String help() {
        return "Resolution parameters:\n" +  Strategy.help() +
                "  seed:       for the random number generator              (default: 0)\n" +
                "  percentage: percentage of clauses in the set of support. (default 50)\n" +
                "  limit:      maximal number of resolvents = limit*clauses (default unlimited) ";}


    private int predicates;
    private Random random;
    private Strategy strategy;
    private BucketSortedList<Clause> primaryClauses;
    private SingleList<Clause> secondaryClauses;
    public LiteralIndexSorted literalIndex;
    TaskQueue taskQueue;

    /** constructs a new solver of type RandomWalker.
     * The constructor is called serially. Therefore there are no race conditions.
     * globalParameters and centralProcessor are shared between different threads.
     * The walker is passive in the sense that it does not send data to the CentralDataHolder.
     *
     * @param solverControl     contains the parameters for controlling the solver
     * @param centralProcessor  contains the result of parsing and initializing the problem data.
     */
    public Resolution(HashMap<String,Object> solverControl,CentralProcessor centralProcessor) {
        super(solverControl, centralProcessor);
        predicates = centralProcessor.predicates;
        literalIndex = new LiteralIndexSorted(predicates);
        initializeData();
        addObservers();
        addMonitors("Resolution");
        statistics = new ResolutionStatistics(this);
        statistics.addStatisticsObservers();
    }
    private boolean initializing = true;



    int[][] originalClauses;
    String[] originalIds;
    int clauseCounter = 0;
    int percentage = 0;
    int resolutionLimit = 0;
    int resolvents = 0;

    protected void initializeData() {
        strategy = (Strategy)applicationParameters.get("strategy");
        random = new Random((Integer)applicationParameters.get("seed"));
        model = centralProcessor.model.clone();
        implicationDAG = centralProcessor.implicationDAG.clone();
        ClauseList centralClauses = centralProcessor.clauses;
        int size = centralClauses.size();
        originalClauses = new int[size][];
        originalIds = new String[size];
        int clauseCounter = 0;
        for(Clause clause  : centralClauses.clauses) {
            originalClauses[clauseCounter] = new int[clause.size()];
            for(int i = 0; i < clause.size(); ++i) {originalClauses[clauseCounter][i] = clause.getLiteral(i);}
            originalIds[clauseCounter++] = clause.id;}
        percentage = (Integer)applicationParameters.get("percentage");
        int limit = (int)applicationParameters.get("limit");
        resolutionLimit = (limit == Integer.MAX_VALUE) ? limit : limit * size;
        clauseCounter = 0;}


    public Result solve() {
        globalParameters.log("Resolution " + id + " for problem " + problemId + " started");
        long time = System.currentTimeMillis();
        try{
            taskQueue = new TaskQueue(this,globalParameters.debug);
            taskQueue.start();
        }
        finally{
            statistics.elapsedTime = System.currentTimeMillis() - time;
            removeObservers();
            removeMonitors();
            supervisor.finished(result, id, problemId, message);}}

    public Task nextTask() {
        Task task = null;
        if(initializing) {
            task = initializeNextClause();
            if(task != null) {return task;}}
        if(resolvents >= resolutionLimit) {return null;}
        return new ResolutionTask(this);}

    private Task initializeNextClause() {
        ArrayList<CLiteral<Clause>> literals = new ArrayList<>();
        while(true) {
            if(clauseCounter == originalClauses.length) {initializing = false; return null;}
            int[] lits =  originalClauses[clauseCounter++];
            boolean isTrue = false;
            literals.clear();
            for(int i = 0; i < lits.length; ++i) {
                int literal = lits[i];
                if(model.isTrue(literal)) {isTrue = true; break;}
                if(model.isFalse(literal)) {continue;}
                literals.add(new CLiteral<>(literal));}
            if(isTrue) {continue;}
            if(literals.isEmpty()) {
                return new Task.Unsatisfiability(new Unsatisfiable("All literals in clause " +
                        originalIds[clauseCounter] + " are false."),this);}
            ArrayList<CLiteral<Clause>> clits =  simplifyLiterals(literals);
            if(clits == null) {continue;}
            switch(clits.size()) {
                case 1: return new Task.TrueLiteral(clits.get(0).literal,this);
                case 2: return new Task.BinaryClause(clits.get(0).literal,clits.get(1).literal,this);
                default:
                    Clause rClause = new Clause(originalIds[clauseCounter],clits);
                    for(int i = 0; i < clits.size(); ++i) {
                        CLiteral<Clause> clit = clits.get(i);
                        clit.setClause(rClause,i);
                        literalIndex.addLiteral(clit);}
                    switch(strategy) {
                        case INPUT: primaryClauses.add(rClause); break;
                        case NEGATIVE:
                            if(rClause.structure == ClauseStructure.NEGATIVE) {primaryClauses.add(rClause);}
                            else                                              {secondaryClauses.addItem(rClause);}
                            break;
                        case POSITIVE:
                            if(rClause.structure == ClauseStructure.POSITIVE) {primaryClauses.add(rClause);}
                            else                                              {secondaryClauses.addItem(rClause);}
                            break;
                        case SOS:
                            if(random.nextInt(101) <= percentage) {primaryClauses.add(rClause);}
                            else                                     {secondaryClauses.addItem(rClause);}
                            break;}
                    return new Task.ShortenedClause(rClause,this);}}}

    private ArrayList<CLiteral<Clause>> simplifyLiterals(ArrayList<CLiteral<Clause>> literals) {
        if(literals.size() == 1) {return literals;}
        literals = Algorithms.subsumedAndResolved(literals,literalIndex,implicationDAG);
        if(literals == null) {return null;}
        if(literals.size() == 1) {return literals;}
        if(Algorithms.subsumedByID(literals,implicationDAG)) {return null;}
        Algorithms.replacementResolutionWithID(literals,implicationDAG);
        return literals;}



    int retry = 3;
    int timestamp = 0;

    void selectParentLiterals(CLiteral[] parentLiterals) {
        int trial = 0;
        while(++trial <= retry) {
            parentLiterals[1] = null;
            Clause parentClause1 = primaryClauses.getRandom(random);
            for(CLiteral parentLiteral1 : parentClause1) {
                if(parentLiterals[1] != null) {break;}
                ++timestamp;
                parentLiterals[0] = parentLiteral1;
                implicationDAG.apply(parentLiteral1.literal,true,
                        (literal->{for(CLiteral<Clause> clit : (AbstractCollection<CLiteral<Clause>>)literalIndex.getLiterals(-literal)) {
                            clit.timestamp = timestamp;
                            clit.clause.timestamp = timestamp;}})); // all potential resolution partners are stamped.

                for(CLiteral<Clause> cLiteral : parentClause1) {  // now we search for merges.
                    if(parentLiterals[1] != null) {break;}
                    if(parentLiteral1 != cLiteral) {
                        parentLiterals[1] = (CLiteral)
                            implicationDAG.find(cLiteral.literal,true,
                                (literal->{
                                    for(CLiteral<Clause> clit : (AbstractCollection<CLiteral<Clause>>)literalIndex.getLiterals(literal)) {
                                        if(clit.clause.timestamp == timestamp) {
                                            for(CLiteral parentLiteral2 : clit.clause) {
                                                if(parentLiteral2.timestamp == timestamp) {
                                                    return parentLiteral2;}}}}
                                    return null;}));}}}}
            if(parentLiterals[1] == null) { // no merge possibility found
                parentLiterals[1] = (CLiteral)
                    implicationDAG.find(parentLiterals[0].literal,true,
                        (literal->{
                            for(CLiteral clit : (AbstractCollection<CLiteral<Clause>>)literalIndex.getLiterals(-literal)) {return clit;}
                            return null;}));}}  // null should not happen.

    CLiteral<Clause>[] parentLiterals = new CLiteral[2];



    public Result resolve() {
        ArrayList<CLiteral<Clause>> literals;
        while (true) {
            selectParentLiterals(parentLiterals);
            if (parentLiterals[0] == null) {return new Satisfiable(model);}
            literals = resolvent(parentLiterals[0], parentLiterals[1]);
            if (literals == null) {continue;}  // tautology
            reportResolvent(literals);
            literals = simplifyLiterals(literals);
            if(literals == null) {continue;}
            switch(literals.size()) {
                case 1: taskQueue.addTask(new Task.TrueLiteral(literals.get(0).literal,this)); return null;
                case 2: taskQueue.addTask(new Task.BinaryClause(literals.get(0).literal,literals.get(1).literal,this));
                        return null;
                default:
                    ++resolvents;
                    Clause rClause = new Clause(parentLiterals[0].clause.id + "r"+parentLiterals[1].clause.id,literals);
                    for(int i = 0; i < literals.size(); ++i) {
                        CLiteral<Clause> clit = literals.get(i);
                        clit.setClause(rClause,i);
                        literalIndex.addLiteral(clit);}
                    switch(strategy) {
                        case INPUT: secondaryClauses.addItem(rClause); break;
                        case NEGATIVE:
                            if(rClause.structure == ClauseStructure.NEGATIVE) {primaryClauses.add(rClause);}
                            else                                              {secondaryClauses.addItem(rClause);}
                            break;
                        case POSITIVE:
                            if(rClause.structure == ClauseStructure.POSITIVE) {primaryClauses.add(rClause);}
                            else                                              {secondaryClauses.addItem(rClause);}
                            break;
                        case SOS:
                            primaryClauses.add(rClause);
                            break;}
                    taskQueue.addTask(new Task.ShortenedClause(rClause,this));}
                    return null;}}




    /** resolves the two clauses at the given literals.
     * All simplifications which are possible by the implicationDAG are performed
     *
     * @param cliteral1      a literal
     * @param cliteral2      a literal
     * @return  the resolvent, or null if it would be a tautology
     */
    public static ArrayList<CLiteral<Clause>> resolvent(CLiteral<Clause> cliteral1, CLiteral<Clause> cliteral2) {
        ArrayList<CLiteral<Clause>> resolvent = new ArrayList<>();
        for(CLiteral<Clause> clit1 : cliteral1.clause) {if(clit1 != cliteral1) {resolvent.add(clit1.clone());}}
        for(CLiteral<Clause> clit2 : cliteral2.clause) {
            if(clit2 == cliteral2) {continue;}
            boolean ignore = false;
            int literal2 = clit2.literal;
            for(CLiteral clit1 : cliteral1.clause) {
                if(clit1.literal == -literal2) {return null;}; // tautology
                if(clit1.literal == literal2) {ignore = true; break;}}
            if(!ignore) {resolvent.add(clit2.clone());}}
        return resolvent;}


    /** reports the resolvent via the monitor and updates the statistics
     *
     * @param resolvent the new resolvent
     */
    private void reportResolvent(ArrayList<CLiteral<Clause>> resolvent) {
        if(monitoring) {monitor.print(id,"Resolution: " + resolvent.toString());}
        ((ResolutionStatistics)statistics).resolvents++;}



    /** reports that the resolution has been finished
     *
     * @param result     the result of the resolution
     * @param resolvents the number of resolvents.
     * @param thread     which executed the solver
     */
    private void reportFinished(Result result, int resolvents, Thread thread) {
        if(thread.isInterrupted()) {
            globalParameters.log("Resolution " + id + " for problem " + problemId +" interrupted after " + resolvents + " resolvents.\n");}
        else {
            globalParameters.log("Resolution " + id + " for problem " + problemId +" finished after " + resolvents + " resolvents.\n" +
                    "Result: " + result.toString());
            if(result instanceof Erraneous) {supervisor.statistics.incErraneous();}}}


}
