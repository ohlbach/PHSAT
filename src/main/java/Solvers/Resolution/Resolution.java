package Solvers.Resolution;

import Algorithms.Algorithms;
import Coordinator.CentralProcessor;
import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseList;
import Datastructures.Literals.CLiteral;
import Datastructures.Results.Erraneous;
import Datastructures.Results.Result;
import Management.GlobalParameters;
import Solvers.Solver;
import Utilities.Utilities;

import java.util.*;

/**
 * Created by ohlbach on 18.10.2018.
 */
public class Resolution extends Solver {



    private static HashSet<String> keys = new HashSet<>(); // contains the allowed keys in the specification.
    static { // these are the allowed keys in the specification.
        for(String key : new String[]{"name", "seed", "sos", "limit"}) {
            keys.add(key);}}

    /** parses a HashMap with key-value pairs:<br>
     *
     * @param parameters  the parameters with the keys "seed", "sos", "limit"
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
        String soss = parameters.get("sos");
        if(soss == null) {soss = "50";}
        String limits = parameters.get("limit");
        if(limits == null) {limits = Integer.toString(Integer.MAX_VALUE);}
        String place = "Resolution: ";
        ArrayList seed = Utilities.parseIntRange(place+"seed: ",seeds,errors);
        ArrayList sos = Utilities.parseIntRange(place+"sos: ",soss,errors);
        ArrayList limit = Utilities.parseIntRange(place+"limit: ",limits,errors);
        ArrayList<ArrayList> pars = Utilities.crossProduct(seed,sos,limit);
        int counter = 0;
        for(ArrayList<Object> p : pars ) {
            Integer sospar = (Integer)p.get(1);
            if(sospar < 0 || sospar > 100) {errors.append("Resolution: sos must be a percentage between 0 and 100, not"+sospar);}
            Integer limitpar = (Integer)p.get(2);
            if(limitpar < 0) {errors.append("Resolution: limit must be positive: " + limitpar);}
            HashMap<String,Object> map = new HashMap<>();
            map.put("seed",p.get(0));
            map.put("sos",sospar);
            map.put("limit",limitpar);
            map.put("name","R" + ++counter);
            list.add(map);}
        return list;}

    public static String help() {
        return "Resolution (Set of Support): parameters:\n" +
                "seed:   for the random number generator              (default: 0)\n" +
                "sos:    percentage of clauses in the set of support. (default 50)\n" +
                "limit:  maximal number of resolvents = limit*clauses (default unlimited) ";}

    private Random random;
    private boolean strategySOS = true;

    /** constructs a new solver of type RandomWalker.
     * The constructor is called serially. Therefore there are no race conditions.
     * globalParameters and centralProcessor are shared between different threads.
     * The walker is passive in the sense that it does not send data to the CentralDataHolder.
     *
     * @param solverControl     contains the parameters for controlling the solver
     * @param globalParameters  contains the global control parameters
     * @param centralProcessor  contains the result of parsing and initializing the problem data.
     */
    public Resolution(HashMap<String,Object> solverControl, GlobalParameters globalParameters,
                        CentralProcessor centralProcessor) {
        super(solverControl, globalParameters, centralProcessor);
        initializeData();
        addObservers();
        addMonitors("Resolution");
        statistics = new ResolutionStatistics(this);
        statistics.addStatisticsObservers();
    }

    protected void initializeData() {
        int seed = (Integer)applicationParameters.get("seed");
        random = new Random(seed);
        model = centralProcessor.model.clone();
        implicationDAG = centralProcessor.implicationDAG.clone();
        strategySOS = seed < 100;
        if(strategySOS) copyClausesSOS(); else copyClausesInput();}

    private final int normal = 0;
    private final int sos = 1;

    private void copyClausesSOS() {
        ClauseList centralClauses = centralProcessor.clauses;
        int size = centralClauses.size();
        clauses = new ClauseList(predicates,Clause.sizeComparator, Clause.priorityComparator); // sos
        int sosSize =(Integer) applicationParameters.get("sos");
        for(Clause clause  : centralClauses.getClauses(0)) {
            Clause rClause = new Clause(clause,random.nextInt(size)*clause.size(),true);
            clauses.addClause(rClause, random.nextInt(100) <= sosSize ? sos : normal);}}


    private void copyClausesInput() {
        ClauseList centralClauses = centralProcessor.clauses;
        int size = centralClauses.size();
        clauses = new ClauseList(predicates,Clause.priorityComparator, Clause.sizeComparator);
        for(Clause clause  : centralClauses.getClauses(0)) {
            Clause rClause = new Clause(clause,random.nextInt(size)*clause.size(),true);
            clauses.addClause(rClause, normal);}}


    public Result solve() {
        globalParameters.log("Resolution " + id + " for problem " + problemId + " started");
        long time = System.currentTimeMillis();
        int size = centralProcessor.clauses.size();
        int limit = (Integer)applicationParameters.get("limit");
        limit = (limit == Integer.MAX_VALUE) ? limit : limit * size;
        CLiteral parentLiteral1 = null;
        CLiteral parentLiteral2 = null;
        int resolventCounter = -1;
        Result result = null;
        int clauseGroup = strategySOS ? sos : normal;
        Thread thread = Thread.currentThread();
        try{
            while(!clauses.isEmpty(clauseGroup) && !thread.isInterrupted() ) {
                Clause parentClause1 = selectParentClause1(clauseGroup);
                parentLiteral1 = selectParentLiteral1(parentClause1);
                parentLiteral2 = selectParentLiteral2(parentLiteral1);
                ArrayList<CLiteral> resolventLiterals = Algorithms.resolve(parentLiteral1,parentLiteral2,implicationDAG);
                if(resolventLiterals != null) {
                    String id = parentLiteral1.clause.id+"+"+parentLiteral2.clause.id;
                    Clause resolvent =  new Clause(id,resolventLiterals,random.nextInt(size)*resolventLiterals.size(),false);
                    reportResolvent(resolvent);
                    clauses.addClause(resolvent,sos);
                    taskQueue.add(makeShortenedClauseTask(resolvent,this));
                    result = processTasks();
                    if(result != null) {return result;}}
                if(++resolventCounter == limit) {
                    reportAbortion(limit);
                    return null;}}
            result = Result.makeResult(model,basicClauseList);
            return result;}
        finally{
            statistics.elapsedTime = System.currentTimeMillis() - time;
            removeObservers();
            removeMonitors();
            reportFinished(result,resolventCounter, thread);}}

    /** selects the first parent clause for the resolution.
     * The clause is again inserted in the priority chain, but with different priority,
     * such that next time another clause will be chosen.
     *
     * @param group a clause group
     * @return the first parent clause
     */
    private Clause selectParentClause1(int group) {
        Clause clause = clauses.getClauses(group).poll();
        clause.priority = random.nextInt(clauses.size()*clause.size());
        clauses.addClause(clause,group);
        return clause;}

    /** chooses a resolution literal in the first parent clause.
     *  It chooses the literal which is most likely to get pure.
     *
     * @param parentClause1 the first parent clause
     * @return a literal in this clause
     */
    private CLiteral selectParentLiteral1(Clause parentClause1) {
        CLiteral parentLiteral1 = null;
        int purity = Integer.MAX_VALUE;
        for(CLiteral clit : parentClause1.cliterals) {   // choose the literal which is most likely to get pure
            int size = clauses.literalIndex.size(clit.literal);
            if(size < purity) {parentLiteral1 = clit; purity = size;}}
        return parentLiteral1;}

    /** chooses the second parent literal for the resolution.
     *  It selects the literal in the smallest clause
     *
     * @param parentLiteral1 the first parent literal
     * @return the second parent literal
     */
    private CLiteral selectParentLiteral2(CLiteral parentLiteral1) {
        CLiteral[] parentLiteral2 = new CLiteral[1];
        int[] minSize = new int[]{Integer.MAX_VALUE};
        clauses.streamContradicting(parentLiteral1.literal,implicationDAG).anyMatch(clit->{ // find the smallest clause
            int size = clit.clause.size();
            if(size == 3) {parentLiteral2[0] = clit; return true;}
            if(size < minSize[0]) {minSize[0] = size; parentLiteral2[0] = clit;}
            return false;});
        return parentLiteral2[0];}


    /** reports the resolvent via the monitor and updates the statistics
     *
     * @param resolvent the new resolvent
     */
    private void reportResolvent(Clause resolvent) {
        if(monitoring) {monitor.print(id,"Resolution: " + resolvent.toString());}
        ((ResolutionStatistics)statistics).resolvents++;}

    /** reports that the resolution has been aborted
     *
     * @param limit the maximum number of resolvents
     */
    private void reportAbortion(int limit) {
        globalParameters.log("Resolution " + id + " for problem " + problemId +" stopped after " + limit + " resolvents");
        supervisor.statistics.incAborted();
        globalParameters.supervisor.aborted(id);}

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
