package Solvers;

import Coordinator.CentralProcessor;
import Datastructures.Clauses.ClauseList;
import Datastructures.Results.Result;
import Management.GlobalParameters;
import Utilities.Utilities;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;

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
    private ClauseList clauseList;

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
        super("Resolution_"+resolver,solverControl,globalParameters,centralProcessor);
        this.resolver = resolver;
        clauseList = centralProcessor.clauses.clone(); // now centralDataHolder may change its clauses

     /*   rwModel = new RandomWalker.RWModel(globalModel);
        clauseList = centralProcessor.clauses.clone(); // now centralDataHolder may change its clauses
        globalModel.addNewTruthObserver(literal         -> newTrueLiterals.add(literal));
        implicationDAG.addImplicationObserver((from,to) -> newImplications.add(new int[]{from,to}));
        implicationDAG.addEquivalenceObserver(eqv       -> newEquivalences.add(eqv)); */
    }

    public Result solve() {
        return null;
    }

}
