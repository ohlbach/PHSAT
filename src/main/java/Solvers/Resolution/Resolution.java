package Solvers.Resolution;

import Coordinator.Tasks.Task;
import Coordinator.Tasks.TaskQueue;
import Datastructures.Clauses.Clause;
import Datastructures.Literals.CLiteral;
import Datastructures.Literals.LitAlgorithms;
import Datastructures.Literals.LiteralIndexSorted;
import Datastructures.Results.*;
import Datastructures.Statistics.Statistic;
import Datastructures.Theory.EquivalenceClasses;
import Datastructures.Theory.Transformers;
import Management.ProblemSupervisor;
import Solvers.Solver;
import Utilities.Utilities;
import Utilities.BucketSortedList;
import Utilities.BucketSortedIndex;

import java.util.*;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

/**
 * Created by ohlbach on 18.10.2018.
 */
public class Resolution extends Solver {

    private static HashSet<String> keys = new HashSet<>(); // contains the allowed keys in the specification.
    static { // these are the allowed keys in the specification.
        for(String key : new String[]{"name", "seed", "strategy", "percentageOfSOSClauses", "limit", "type", "solver"}) {
            keys.add(key);}}

    /** parses a HashMap with key-value pairs:<br>
     *
     * @param parameters  the parameters with the keys "seed", "strategy", "percentageOfSOSClauses", "limit"
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
        String percentages = parameters.get("percentageOfSOSClauses");
        if(percentages == null) {percentages = "50";}
        String limits = parameters.get("limit");
        if(limits == null) {limits = Integer.toString(Integer.MAX_VALUE);}
        String place           = "Resolution: ";
        ArrayList seed         = Utilities.parseIntRange(place+"seed: ",seeds,errors);
        ArrayList percentage   = Utilities.parseIntRange(place+"percentageOfSOSClauses: ",percentages,errors);
        ArrayList limit = Utilities.parseIntRange(place+"limit: ",limits,errors);
        ArrayList strategies   = ResolutionStrategy.parseStrategies(strategiess,place, warnings,errors);
        ArrayList<ArrayList> pars = Utilities.crossProduct(seed,strategies,percentage,limit);
        int counter = 0;
        for(ArrayList<Object> p : pars ) {
            Integer perc = (Integer)p.get(2);
            if(perc < 0 || perc > 100) {errors.append("Resolution: sos must be a percentageOfSOSClauses between 0 and 100, not"+perc);}
            Integer limitpar = (Integer)p.get(3);
            if(limitpar < 0) {errors.append("Resolution: limit must be positive: " + limitpar);}
            HashMap<String,Object> map = new HashMap<>();
            map.put("seed",     p.get(0));
            map.put("strategy", p.get(1));
            map.put("percentageOfSOSClauses",perc);
            map.put("limit",    limitpar);
            map.put("name","R" + ++counter);
            list.add(map);}
        return list;}

    public static String help() {
        return "Resolution parameters:\n" +  ResolutionStrategy.help() +
                "  seed:       for the random number generator              (default: 0)\n" +
                "  percentageOfSOSClauses: percentageOfSOSClauses of clauses in the set of support. (default 50)\n" +
                "  limit:      maximal number of resolvents = limit*clauses (default unlimited) ";}


    private Random random;
    private ResolutionStrategy strategy;
    private TaskQueue taskQueue;

    private final int newClausePriority = 5;
    private final int trueLiteralPriority = 1;

    public ResolutionStatistics statistics;

    /** constructs a new Resolution solver.
     *
     * @param solverNumber         for distinguishing different solvers of the same type, but different parameters
     * @param solverParameters     contains the parameters for controlling the solver
     * @param problemSupervisor    coordinates several solvers.
     */
    public Resolution(Integer solverNumber, HashMap<String,Object> solverParameters, ProblemSupervisor problemSupervisor) {
        super(solverNumber,solverParameters, problemSupervisor);}

    /** This method controls the entire resolution sequence
     * 1. All local data are initialized <br>
     * 2. All basic clauses are transformed to clauses and distributed to the primary and secondary clause lists. <br>
     *    Equivalences are turned to equivalence classes <br>
     *    All literals are mapped to the representatives in their classes.<br>
     * 3. The initial causes are put into the task queue for simplifications.<br>
     * 4. Resolution ans simplification is started until <br>
     *     - a contradiction is formed or <br>
     *     - the primaryClauses became empty or<br>
     *     - the resolution limit is exceeded or <br>
     *     - the thread is interrupted.<br>
     *
     * @return the result of the resolution sequence.
     */
    public Result solve() {
        globalParameters.log(solverId + " for problem " + problemId + " started");
        long time = System.currentTimeMillis();
        taskQueue = new TaskQueue(combinedId,monitor);
        initializeData();
        Result result = null;
        try{result = initializeClauses();
            if(result == null) {result = resolve();}}
        catch(InterruptedException ex) {
            globalParameters.log("Resolution " + combinedId + " interrupted after " + resolvents + " resolvents.\n");
            result = new Aborted("Resolution aborted after " + resolvents + " resolvents");}
        statistics.elapsedTime = System.currentTimeMillis() - time;
        return result;}

    private BucketSortedList<Clause> primaryClauses;
    private BucketSortedList<Clause> secondaryClauses;
    public BucketSortedIndex<CLiteral<Clause>> literalIndex;

    private int percentageOfSOSClauses = 0;
    private int resolutionLimit = 0;

    protected void initializeData() {
        strategy = (ResolutionStrategy)solverParameters.get("strategy");
        statistics = new ResolutionStatistics(combinedId);
        random = new Random((Integer)solverParameters.get("seed"));
        percentageOfSOSClauses = (Integer)solverParameters.get("percentage");
        primaryClauses   = new BucketSortedList<Clause>(clause->clause.size());
        secondaryClauses = new BucketSortedList<Clause>(clause->clause.size());
        literalIndex = new BucketSortedIndex<CLiteral<Clause>>(predicates,
                (cLiteral->cLiteral.literal),
                (cLiteral->cLiteral.clause.size()));}

    private EquivalenceClasses equivalenceClasses = null;
    private BiConsumer<int[],Integer> contradictionHandler;
    private Consumer<Clause> insertHandler = (
            clause -> {insertClause(clause,isPrimary(clause,true));
                taskQueue.add(new Task(basicClauseList.maxClauseLength-clause.size()+2, // longer clauses should be
                        (()-> {simplifyBackward(clause); return null;}),    // checked first for subsumption and replacement resolution
                        (()-> "Simplify initial clause " + clause.toString())));});

    public Result initializeClauses() throws InterruptedException {
        if(basicClauseList.equivalences != null) {
            equivalenceClasses = Transformers.prepareEquivalences(basicClauseList,contradictionHandler);
            if(equivalenceClasses == null) {return null;}}

        Transformers.prepareConjunctions(basicClauseList,equivalenceClasses,
                (literal-> addTrueLiteralTask(literal)));
        if(Thread.interrupted()) {throw new InterruptedException();}
        Transformers.prepareDisjunctions(basicClauseList,equivalenceClasses,insertHandler);
        Transformers.prepareXors     (basicClauseList,equivalenceClasses,insertHandler);
        Transformers.prepareDisjoints(basicClauseList,equivalenceClasses,insertHandler);
        int limit = (int)solverParameters.get("limit");
        resolutionLimit = (limit == Integer.MAX_VALUE) ? limit : limit * clauseCounter;
        initializing = false;
        if(Thread.interrupted()) {throw new InterruptedException();}
        return null;}




    int resolvents = 0;

    public Result resolve()  throws InterruptedException {
        Result result = taskQueue.run();
        if(result != null){return result;}
        CLiteral[] parentLiterals = new CLiteral[2];
        while(resolvents <= resolutionLimit) {
            if(Thread.interrupted()) {throw new InterruptedException();}
            selectParentLiterals(parentLiterals);
            Clause resolvent = LitAlgorithms.resolve(parentLiterals[0],parentLiterals[1]);
            ++statistics.resolvents;
            if(resolvent == null) {continue;}
            if(monitoring) {
                monitor.print(combinedId,"Resolution between " + parentLiterals[0].clause.toString() +
                "@"+parentLiterals[0].literal + " and " + parentLiterals[1].clause.toString() +
                        "@"+parentLiterals[1].literal + " yields\n"+resolvent.toString());}
            ++resolvents;
            simplifyBackward(resolvent);
            if(resolvent.removed) {continue;}
            simplifyForward(resolvent);
            insertClause(resolvent,isPrimary(resolvent,false));
            result = taskQueue.run();
            if(result != null){return result;}}
        return new Aborted("Maximum Resolution Limit " + resolutionLimit + " exceeded");}

    void selectParentLiterals(CLiteral[] parentLiterals) {
        Clause parent1 = primaryClauses.getRandom(random);
        int size1 = parent1.size();
        for(CLiteral literal1 : parent1) {
            parentLiterals[0] = literal1;
            boolean first = false;
            Iterator<CLiteral<Clause>> iterator = literalIndex.iterator(-literal1.literal);
            while(iterator.hasNext()) {
                CLiteral<Clause> literal2 = iterator.next();
                if(first) {parentLiterals[1] = literal2; first = false;}
                if(literal2.clause.size() < size1) {parentLiterals[1] = literal2; return;}
                for(CLiteral<Clause> lit2 : literal2.clause) {
                    if(parent1.contains(lit2.literal) == 1) {parentLiterals[1] = literal2; return;}}}}
    }

    private boolean isPrimary(Clause clause, boolean input) {
        switch(strategy) {
            case INPUT:    return input;
            case POSITIVE: return clause.isPositive();
            case NEGATIVE: return clause.isNegative();
            case SOS:      return input ? (random.nextInt(101) <= percentageOfSOSClauses) : true;}
        return true;}


    private boolean initializing = true;

    private int timestamp = 0;

    /** checks if the clause is subsumed or some of its literals can be resolved away by replacement resolution.
     * If the clause is subsumed, it is removed from the clause lists and the literal index <br>
     *  - if the clause is in the primaryClauses and the subsumer is in the secondary clauses, <br>
     *    the subsumer is moved to the primaryClauses
     *    <br>
     *  - Example for replacement resolution:<br>
     *      p,q,r<br>
     *      -p,r <br>
     *    p in the first clause can be removed. <br>
     *    In more complex examples, several literals can be removed at once.<br>
     *    If the resulting clause is a unit clause, it generates a newTrueLiteral task.
     *    <br>
     *    Removing clauses may produce pure literals, whose negation can be made true and therefore
     *    generates a newTrueLiteral task.<br>
     *  - Shortened clauses may trigger forward subsumptions and forward replacement resolutions.
     *    Therefore thy generate a corresponding task.
     *
     * @param clause
     */
    private void simplifyBackward(Clause clause) {
        if(clause.removed) {return;}
        Clause subsumer = LitAlgorithms.isSubsumed(clause,literalIndex,++timestamp);
        if(subsumer != null) {
            ++statistics.backwardSubsumptions;
            if(primaryClauses.contains(clause) && !primaryClauses.contains(subsumer)) {replaceClause(clause,subsumer);}
            else {removeClause(clause,0);}
            if(monitoring) {
                monitor.print(combinedId,"Clause " + clause.toString() + " is subsumed by " + subsumer.toString());}
            if(subsumer.size() < clause.size()) {checkPurity(clause);}
            return;}

        Object[] replacements = LitAlgorithms.replacementResolutionBackwards(clause,literalIndex,++timestamp);
        while(replacements != null) { // several literals may become resolved away
             CLiteral<Clause> cLiteral = (CLiteral<Clause>)replacements[0];
            ++statistics.backwardReplacementResolutions;
            if(monitoring) {
                monitor.print(combinedId,
                        "Literal " + cLiteral.literal + " in clause " + clause.toString() + " resolved away by clause "
                        + ((Clause)replacements[1]).toString());}
            literalIndex.remove(cLiteral);
            if(removeLiteral(cLiteral)) {
                replacements = LitAlgorithms.replacementResolutionBackwards(clause,literalIndex,++timestamp);}}

        if(!clause.removed) {
            taskQueue.add(new Task(clause.size()+2,
                    (()->{simplifyForward(clause);return null;}),
                    (()->"Forward Simplifiation for shortened clause " + clause.toString())));}}



    private ArrayList<Clause> clauseList = new ArrayList<>();
    private ArrayList<CLiteral<Clause>> literalList = new ArrayList<>();

    public void simplifyForward(Clause clause) {
        if(clause.removed) {return;}
        clauseList.clear();
        LitAlgorithms.subsumes(clause,literalIndex,++timestamp,clauseList);
        for(Clause subsumedClause : clauseList) {
            ++statistics.forwardSubsumptions;
            if(monitoring) {monitor.print(combinedId,"Resolvent subsumes " + subsumedClause.toString());}
            removeClause(subsumedClause,0);}

        literalList.clear();
        LitAlgorithms.replacementResolutionForward(clause,literalIndex,++timestamp,literalList);
        for(CLiteral<Clause> cLiteral : literalList) {
            ++statistics.forwardReplacementResolutions;
            if(monitoring) {
                monitor.print(combinedId,"Literal " + cLiteral.literal + " in clause " + cLiteral.clause.toString() +
                        " resolved away by resolvent");}
            removeLiteral(cLiteral);}}


    /** turns the literal into a trueLiteralTask.
     * If it is a unit resolvent then it is forwarded to the problem supervisor.
     *
     * @param literal a unit literal.
     */
    public void addTrueLiteralTask(int literal) {
        taskQueue.add(new Task(trueLiteralPriority,
                (()->processTrueLiteral(literal)),
                (()->"New true literal derived: " + literal)));
        ++statistics.unitClauses;
        if(!initializing) problemSupervisor.forwardTrueLiteral(this,literal);}

    private Result processTrueLiteral(int literal) {
        switch(model.status(literal)) {
            case -1: return new Unsatisfiable(model,literal);
            case +1: return null;}
        model.add(literal);
        Iterator<CLiteral<Clause>> iterator = literalIndex.iterator(literal);
        while(iterator.hasNext()) {
            Clause clause = iterator.next().clause;
            removeClause(clause,literal);
            if(primaryClauses.isEmpty()) {return completeModel();}
            checkPurity(clause);}
        iterator = literalIndex.iterator(-literal);
        while(iterator.hasNext()) {
            CLiteral<Clause> cLiteral = iterator.next();
            Clause clause = cLiteral.clause;
            removeLiteral(cLiteral);
            if(primaryClauses.isEmpty()) {return completeModel();}}
        literalIndex.clearBoth(Math.abs(literal));
        return null;}

    /** completes a model after resolution has finished.
     * Strategy INPUT or SOS: all remaining clauses should be true <br>
     * Strategy POSITIVE: all remaining clauses are negative of mixed. <br>
     * Choose an unassigned negative literal. <br>
     *
     * Strategy NEGATIVE: all remaining clauses are positive of mixed. <br>
     * Choose an unassigned positive literal. <br>
     *
     * @return Satisfiable or Erraneous (if something went wrong).
     */
    private Result completeModel() {
        if(model.size() == predicates) {return new Satisfiable(model);}
        boolean isPositive = true;
        switch(strategy) {
            case INPUT:
            case SOS:       // there should be no clauses any more.
                for(Clause clause : secondaryClauses) {
                    if(statusInModel(clause,model) != 1) {
                        return new Erraneous(model,clause,symboltable);}}
                break;
            case NEGATIVE: isPositive = false;
            case POSITIVE:
                for(Clause clause : secondaryClauses) {
                    switch(statusInModel(clause,model)) {
                        case 1: continue;
                        case -1: return new Erraneous(model,clause,symboltable);}
                    boolean found = false;
                    for(CLiteral cliteral : clause) {
                        int literal = cliteral.literal;
                        if(model.status(literal) == 0 && ((isPositive && literal < 0) || (!isPositive && literal > 0))) {
                            model.add(literal); found = true; break;}}
                    if(!found) return new Erraneous(model,clause,symboltable);}}
        return new Satisfiable(model);}


    /** counts the number of clauses in the resolution solver */
    private int clauseCounter = 0;

    /** inserts the clause into the local data structures.
     * - If the clause is a unit clause, it generates a trueLiteralTask<br>
     * - If it is an initial clause, ti generates a simplifyBackward task.<br>
     *   These tasks are sorted such that longer clauses are simplified first (subsumption and replacement resolution).
     *
     * @param clause  the clause to be inserted.
     * @param primary determines whether the clause is inserted into primaryClauses or secondaryClauses.
     */
    private void insertClause(Clause clause, boolean primary) {
        if(clause.size() == 1) {
            ++statistics.unitClauses;
            addTrueLiteralTask(clause.getLiteral(0));
            return;}
        ++clauseCounter;
        (primary ? primaryClauses : secondaryClauses).add(clause);
        for(CLiteral<Clause> cLiteral : clause) {literalIndex.add(cLiteral);}}


    private void removeClause(Clause clause, int ignoreLiteral) {
        if(clause.removed) {return;}
        --clauseCounter;
        if(primaryClauses.contains(clause)) {primaryClauses.remove(clause);}
        else {secondaryClauses.remove(clause);}
        for(CLiteral<Clause> cLiteral : clause) {
            if(cLiteral.literal != ignoreLiteral) {literalIndex.remove(cLiteral);}}
        clause.removed = true;}

    private void replaceClause(Clause primaryClause, Clause secondaryClause) {
        primaryClauses.remove(primaryClause);
        primaryClause.removed = true;
        for(CLiteral<Clause> cLiteral : primaryClause) {literalIndex.remove(cLiteral);}
        secondaryClauses.remove(secondaryClause);
        primaryClauses.add(secondaryClause);}

    /** removes the literal from its clause .
     * - If the clause is already marked 'removed' nothing happens. <br>
     * - If the shortened clause is a unit clause, it generates a trueLiteralTask, and is removed.
     *
     * @param cLiteral
     * @return true if the clause is still there.
     */
    private boolean removeLiteral(CLiteral<Clause> cLiteral) {
        Clause clause = cLiteral.clause;
        if(clause.removed) {return false;}
        clause.removeLiteral(cLiteral);
        if(clause.size() == 1) {
            addTrueLiteralTask( clause.getLiteral(0));
            removeClause(clause,0);
            return false;}
        return true;}

    /** checks if some of the literals in the clause became pure, i.e. there are no more literals of this polarity in the index.
     * The negation of these literals become new unit clauses.
     * They are added to the task queue
     *
     * @param clause a clause which has been removed from the index.
     */
    private void checkPurity(Clause clause) {
        for(CLiteral cliteral : clause) {
            if(literalIndex.isEmpty(cliteral.literal)) {
                addTrueLiteralTask(-cliteral.literal);}}}


    /** return the entire statistics information
     *
     * @return the entire statistice information for the resolution solver.
     */
    public Statistic getStatistics() {return statistics;}
}
