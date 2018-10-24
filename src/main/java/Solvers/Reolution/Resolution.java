package Solvers.Reolution;

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
     * @param parameters  the parameters with the keys "seed", "sos"
     * @param errors      for error messages
     * @param warnings    for warnings
     * @return            a list of HashMaps with keys "seed" and "sos".
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
        random = new Random((Integer)applicationParameters.get("seed"));
        model = centralProcessor.model.clone();
        implicationDAG = centralProcessor.implicationDAG.clone();
        copyClauses();}

    private final int normal = 0;
    private final int sos = 1;

    private void copyClauses() {
        ClauseList centralClauses = centralProcessor.clauses;
        int size = centralClauses.size();
        clauses = new ClauseList(predicates,Clause.sizeComparator, Clause.priorityComparator); // sos
        int sosSize =(Integer) applicationParameters.get("sos");
        for(Clause clause  : centralClauses.getClauses(0)) {
            Clause rClause = new Clause(clause,random.nextInt(size)*clause.size(),true);
            clauses.addClause(rClause, random.nextInt(100) <= sosSize ? sos : normal);}}

    public Result solve() {
        globalParameters.log("Resolution " + id + " for problem " + problemId + " started");
        long time = System.currentTimeMillis();
        int size = centralProcessor.clauses.size();
        int limit = (Integer)applicationParameters.get("limit");
        limit = (limit == Integer.MAX_VALUE) ? limit : limit * size;
        CLiteral[] parentLiterals = new CLiteral[2];
        int resolventCounter = -1;
        Result result = null;
        try{
            while(!clauses.isEmpty()) {
                selectParentLiterals(parentLiterals);
                ArrayList<CLiteral> resolventLiterals = Algorithms.resolve(parentLiterals[0],parentLiterals[1],implicationDAG);
                if(resolventLiterals != null) {
                    String id = parentLiterals[0].clause.id+"+"+parentLiterals[1].clause.id;
                    Clause resolvent =  new Clause(id,resolventLiterals,random.nextInt(size)*resolventLiterals.size(),false);
                    reportResolvent(resolvent);
                    clauses.addClause(resolvent,sos);
                    taskQueue.add(makeShortenedClauseTask(resolvent));
                    result = processTasks();
                    if(result != null) {return result;}}
                if(++resolventCounter == limit) {
                    reportAbortion(limit);
                    return null;}}
            result = Result.makeResult(this.model,basicClauseList);
            return result;}
        finally{
            statistics.elapsedTime = System.currentTimeMillis() - time;
            removeObservers();
            removeMonitors();
            reportFinished(result,resolventCounter);}}

    private void selectParentLiterals(CLiteral[] parentLiterals) {
        Clause parentClause1 = clauses.getClauses(sos).poll();
        CLiteral parentLiteral1 = null;
        int purity = Integer.MAX_VALUE;
        for(CLiteral clit : parentClause1.cliterals) {   // choose the literal which is most likely to get pure
            int size = clauses.literalIndex.size(clit.literal);
            if(size < purity) {parentLiteral1 = clit; purity = size;}}
        parentLiterals[0] = parentLiteral1;
        int[] minSize = new int[]{Integer.MAX_VALUE};
        clauses.streamContradicting(parentLiteral1.literal,implicationDAG).anyMatch(clit->{ // find the smallest clause
            int size = clit.clause.size();
            if(size == 3) {parentLiterals[1] = clit; return true;}
            if(size < minSize[0]) {minSize[0] = size; parentLiterals[1] = clit;}
            return false;});
    }

    private void reportResolvent(Clause resolvent) {
        if(monitoring) {monitor.print(id,"Resolution: " + resolvent.toString());}
        ((ResolutionStatistics)statistics).resolvents++;}

    private void reportAbortion(int limit) {
        globalParameters.log("Resolution " + id + " for problem " + problemId +" stopped after " + limit + " resolvents");
        supervisor.statistics.incAborted();}

    private void reportFinished(Result result, int resolvents) {
        globalParameters.log("Resolution " + id + " for problem " + problemId +" finished after " + resolvents + " resolvents.\n" +
                "Result: " + result.toString());
        if(result instanceof Erraneous) {supervisor.statistics.incErraneous();}}


    public static void main(String[] args) {
        Random rnd = new Random();
        PriorityQueue<String> q  = new PriorityQueue<>(Comparator.comparingInt(s->s.length()));
        q.add("ab"); q.add("def");
        //q.remove("abcde");
        q.add("eh");
        System.out.println(q);
        q.poll();
        System.out.println(q);
    }

}
