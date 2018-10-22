package Solvers.Reolution;

import Algorithms.Algorithms;
import Coordinator.CentralProcessor;
import Coordinator.Task;
import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseList;
import Datastructures.Literals.CLiteral;
import Datastructures.Results.Result;
import Datastructures.Results.Satisfiable;
import Management.GlobalParameters;
import Solvers.Solver;
import Utilities.Utilities;
import sun.util.resources.cldr.de.CalendarData_de_LU;

import java.util.*;
import java.util.function.Consumer;

/**
 * Created by ohlbach on 18.10.2018.
 */
public class Resolution extends Solver {



    private static HashSet<String> keys = new HashSet<>(); // contains the allowed keys in the specification.
    static { // these are the allowed keys in the specification.
        for(String key : new String[]{"class", "seed", "sos"}) {
            keys.add(key);}}

    /** parses a HashMap with key-value pairs:<br/>
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
        String place = "Resolution: ";
        ArrayList seed = Utilities.parseIntRange(place+"seed: ",seeds,errors);
        ArrayList sos = Utilities.parseIntRange(place+"sos: ",soss,errors);
        ArrayList<ArrayList> pars = Utilities.crossProduct(seed,sos);
        int counter = 0;
        for(ArrayList<Object> p : pars ) {
            HashMap<String,Object> map = new HashMap<>();
            map.put("seed",pars.get(0));
            map.put("sos",pars.get(1));
            map.put("name","walker_" + ++counter);
            list.add(map);}
        return list;}

    public static String help() {
        return "Resolution (Set of Support): parameters:\n" +
                "seed:   for the random number generator              (default: 0)\n" +
                "sos:    percentage of clauses in the set of support. (default 50)";}

    int resolver = 0;
    int seed = (Integer)solverControl.get("seed");
    private ClauseList clauses;
    private Random random = new Random();
    protected PriorityQueue<Task> taskQueue = new PriorityQueue<Task>(Comparator.comparingInt(task->task.priority));


    protected void addTask(Task task) {taskQueue.add(task);}


    /** constructs a new solver of type RandomWalker.
     * The constructor is called serially. Therefore there are no race conditions.
     * globalParameters and centralProcessor are shared between different threads.
     * The walker is passive in the sense that it does not send data to the CentralDataHolder.
     *
     * @param resolver          counts the constructed solver
     * @param solverControl     contains the parameters for controlling the solger
     * @param globalParameters  contains the global control parameters
     * @param centralProcessor       contains the result of parsing and initializing the problem data.
     */
    public Resolution(Integer resolver,  HashMap<String,Object> solverControl, GlobalParameters globalParameters,
                        CentralProcessor centralProcessor) {
        super("Resolution_" + resolver, solverControl, globalParameters, centralProcessor);
        this.resolver = resolver;
        copyClauses();
        addObservers();

    }

    private Consumer<Integer> oneLiteralObserver = (literal-> addTask(new Task.OneLiteral(literal,this)));
    private void addObservers() {
        globalModel.addNewTruthObserver(oneLiteralObserver);
        //clauses.addLiteralRemovalObserver(cLiteral -> addTask(makeShortenedClauseTask(cLiteral.clause)));

    }


    private void copyClauses() {
        ClauseList centralClauses = centralProcessor.clauses;
        int size = centralClauses.size();
        clauses = new ClauseList(centralClauses.size(),predicates,(Comparator.comparingInt(clause->((RClause)clause).priority)));
        for(Clause clause  : centralClauses.clauses) {
            clauses.addClause(new RClause(clause,random.nextInt(size)*clause.size()));}
    }

    public Result solve() {
        CLiteral[] parentLiterals = new CLiteral[2];
        while(!clauses.isEmpty()) {
            selectParentLiterals(parentLiterals);
            Clause resolvent = Algorithms.resolve(parentLiterals[0],parentLiterals[1],implicationDAG);
            if(resolvent != null) {
                Algorithms.subsumeAndResolve(resolvent,clauses,implicationDAG);}
                Result result = processTasks();
                if(result != null) {return result;}}
        return Result.makeResult(model,basicClauseList);}

    private void selectParentLiterals(CLiteral[] parentLiterals) {
        Clause parentClause1 = clauses.clauses.poll();
        CLiteral parentLiteral1 = parentClause1.cliterals.get(random.nextInt(parentClause1.size()));

    }

}
