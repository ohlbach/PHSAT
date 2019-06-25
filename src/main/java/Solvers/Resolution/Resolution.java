package Solvers.Resolution;

import Algorithms.Algorithms;
import Coordinator.CentralProcessor;
import Coordinator.Task;
import Coordinator.TaskQueueThread;
import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseList;
import Datastructures.Clauses.ClauseStructure;
import Datastructures.Literals.CLiteral;
import Datastructures.Literals.LiteralIndex;
import Datastructures.Literals.LiteralIndexSorted;
import Datastructures.Results.Erraneous;
import Datastructures.Results.Result;
import Management.GlobalParameters;
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

    private Random random;
    private Strategy strategy;
    private ClauseLists clauseLists;
    private int predicates;
    private BucketSortedList<Clause> primaryClauses;
    private SingleList<Clause> secondaryClauses;
    public LiteralIndexSorted literalIndex;
    TaskQueueThread taskQueue;

    /** constructs a new solver of type RandomWalker.
     * The constructor is called serially. Therefore there are no race conditions.
     * globalParameters and centralProcessor are shared between different threads.
     * The walker is passive in the sense that it does not send data to the CentralDataHolder.
     *
     * @param solverControl     contains the parameters for controlling the solver
     * @param centralProcessor  contains the result of parsing and initializing the problem data.
     */
    public Resolution(HashMap<String,Object> solverControl,
                        CentralProcessor centralProcessor) {
        super(solverControl, centralProcessor);
        predicates = centralProcessor.predicates;
        initializeData();
        addObservers();
        addMonitors("Resolution");
        statistics = new ResolutionStatistics(this);
        statistics.addStatisticsObservers();
    }

    public Task nextTask() {return new ResolutionTask(this);}

    protected void initializeData() {
        strategy = (Strategy)applicationParameters.get("strategy");
        random = new Random((Integer)applicationParameters.get("seed"));
        model = centralProcessor.model.clone();
        implicationDAG = centralProcessor.implicationDAG.clone();
        clauseLists = new ClauseLists(predicates);
        ClauseList centralClauses = centralProcessor.clauses;
        int size = centralClauses.size();
        for(Clause clause  : centralClauses.getClauses()) {
            Clause rClause = new Clause(clause, 0, true);
            int percentage = (Integer)applicationParameters.get("percentage");
            switch(strategy) {
                case INPUT: clauseLists.addClause1(rClause); break;
                case NEGATIVE:
                    if(rClause.structure == ClauseStructure.NEGATIVE) {clauseLists.addClause1(rClause);}
                    else                                              {clauseLists.addClause2(rClause);}
                    break;
                case POSITIVE:
                    if(rClause.structure == ClauseStructure.POSITIVE) {clauseLists.addClause1(rClause);}
                    else                                              {clauseLists.addClause2(rClause);}
                    break;
                case SOS:
                    if(random.nextInt(101) <= percentage) {clauseLists.addClause1(rClause);}
                    else                                     {clauseLists.addClause2(rClause);}
                    break;}}}


    public Result solve() {
        globalParameters.log("Resolution " + id + " for problem " + problemId + " started");
        long time = System.currentTimeMillis();
        int size = clauseLists.size();
        int limit = (Integer)applicationParameters.get("limit");
        limit = (limit == Integer.MAX_VALUE) ? limit : limit * size;
        CLiteral parentLiteral1 = null;
        CLiteral parentLiteral2 = null;
        int resolventCounter = -1;
        Result result = null;
        Thread thread = Thread.currentThread();
        CLiteral[] parentLiterals = new CLiteral[2];
        String message = null;
        try{
            while(!clauseLists.isEmpty1() && !thread.isInterrupted()) {
                selectParentLiterals(parentLiterals);
                ArrayList<CLiteral> resolventLiterals = Algorithms.resolve(parentLiterals[0],parentLiterals[1],implicationDAG);
                if(resolventLiterals != null) {
                    String id = parentLiteral1.clause.id+"+"+parentLiteral2.clause.id;
                    Clause resolvent =  new Clause(id,resolventLiterals,0,false);
                    reportResolvent(resolvent);
                    switch(strategy) {
                        case INPUT: clauseLists.addClause2(resolvent); break;
                        case POSITIVE:
                            if(resolvent.structure == ClauseStructure.POSITIVE) {clauseLists.addClause1(resolvent);}
                            else {clauseLists.addClause2(resolvent);}
                            break;
                        case NEGATIVE:
                            if(resolvent.structure == ClauseStructure.NEGATIVE) {clauseLists.addClause1(resolvent);}
                            else {clauseLists.addClause2(resolvent);}
                            break;
                        case SOS:
                            clauseLists.addClause1(resolvent); break;}
                    taskQueue.add(makeShortenedClauseTask(resolvent,this));
                    result = processTasks();
                    if(result != null) {return result;}}
                if(++resolventCounter == limit) {
                    message = "Maximum number of resolvents " + limit + " reached.";
                    return null;}}
            result = Result.makeResult(model,basicClauseList);
            return result;}
        finally{
            statistics.elapsedTime = System.currentTimeMillis() - time;
            removeObservers();
            removeMonitors();
            supervisor.finished(result, id, problemId, message);}}

    int retry = 3;
    int timestamp = 0;

    void selectParentLiterals(CLiteral[] parentLiterals) {
        int trial = 0;
        LiteralIndex literalIndex = clauseLists.literalIndex;
        while(++trial <= retry) {
            parentLiterals[1] = null;
            Clause parentClause1 = clauseLists.getRandom(random);
            for(CLiteral parentLiteral1 : parentClause1) {
                if(parentLiterals[1] != null) {break;}
                ++timestamp;
                parentLiterals[0] = parentLiteral1;
                implicationDAG.apply(parentLiteral1.literal,true,
                        (literal->{for(CLiteral clit : literalIndex.getLiterals(-literal)) {
                            clit.timestamp = timestamp;
                            clit.clause.timestamp = timestamp;}})); // all potential resolution partners are stamped.

                for(CLiteral cLiteral : parentClause1.cliterals) {  // now we search for merges.
                    if(parentLiterals[1] != null) {break;}
                    if(parentLiteral1 != cLiteral) {
                        parentLiterals[1] = (CLiteral)
                            implicationDAG.find(cLiteral.literal,true,
                                (literal->{
                                    for(CLiteral clit : literalIndex.getLiterals(literal)) {
                                        if(clit.clause.timestamp == timestamp) {
                                            for(CLiteral parentLiteral2 : clit.clause.cliterals) {
                                                if(parentLiteral2.timestamp == timestamp) {
                                                    return parentLiteral2;}}}}
                                    return null;}));}}}}
            if(parentLiterals[1] == null) { // no merge possibility found
                parentLiterals[1] = (CLiteral)
                    implicationDAG.find(parentLiterals[0].literal,true,
                        (literal->{
                            for(CLiteral clit : literalIndex.getLiterals(-literal)) {return clit;}
                            return null;}));}}  // null should not happen.

    CLiteral<Clause>[] parentLiterals = new CLiteral[2];

    public Result execute() {
        ArrayList<CLiteral<Clause>> literals;
        while (true) {
            selectParentLiterals(parentLiterals);
            if (parentLiterals[0] == null) {
                return null;
            } // satisfiable
            literals = Algorithms.resolve(parentLiterals[0], parentLiterals[1], resolver.implicationDAG);
            if (literals == null) {
                continue;
            }  // tautology or subsumed by implication dag
            if (literals.size() == 2) {
                resolver.taskQueue.addTask(new Task.BinaryClause(literals.get(0).literal, literals.get(1).literal, processor));
            }
            if (Algorithms.subsumed(literals, resolver.literalIndex, resolver.implicationDAG) != null) {
                break;
            }
        }
    }




    /** reports the resolvent via the monitor and updates the statistics
     *
     * @param resolvent the new resolvent
     */
    private void reportResolvent(Clause resolvent) {
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
