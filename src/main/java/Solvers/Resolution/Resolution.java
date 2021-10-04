package Solvers.Resolution;

import Coordinator.Tasks.Task;
import Coordinator.Tasks.TaskQueue;
import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseType;
import Datastructures.Literals.CLiteral;
import Datastructures.Literals.LitAlgorithms;
import Datastructures.Results.*;
import Datastructures.Statistics.Statistic;
import Datastructures.Symboltable;
import Datastructures.Theory.EquivalenceClasses;
import Management.ProblemSupervisor;
import Solvers.Solver;
import Utilities.Utilities;
import Utilities.BucketSortedList;
import Utilities.BucketSortedIndex;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.*;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Function;

/** The class implements a SAT-Solver with the resolution principle.
 * Created by ohlbach on 18.10.2018.
 *
 * Resolution is combined with various simplification techniques: <br>
 *     - Propagation of unit clauses <br>
 *     - forward and backward subsumption<br>
 *     - forward and backward replacement resolution<br>
 *        <br>
 * Four different strategies are available: <br>
 *     - INPUT:    only resolvents with input clauses are allowed (complete only for Horn clauses)
 *     - SOS:      a percentage of the input clauses make up the Set of Support (SOS)<br>
 *                 resolvents are put into the SOS<br>
 *     - POSITIVE: one parent clause must be a positive clause<br>
 *     - NEGATIVE: one parent clause must be a negative clause. <br>
 *  <br>
 *  Unit clauses are exchanged between different solvers. <br>
 *  Therefore various resolution solvers can operate in parallel and exchange unit clauses as intermediate results.
 */
public abstract class Resolution extends Solver {

    boolean checkConsistency = true;

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
        String strategiess = parameters.get("strategy");
        if(strategiess == null) {strategiess = "INPUT";}
        String percentages = parameters.get("percentageOfSOSClauses");
        if(percentages == null) {percentages = "50";}
        String limits = parameters.get("limit");
        if(limits == null) {limits = Integer.toString(Integer.MAX_VALUE);}
        String place           = "Resolution: ";
        ArrayList seed;
        if(seeds != null) {seed = Utilities.parseIntRange(place+"seed: ",seeds,errors);}
        else              {seed = new ArrayList(); seed.add(null);}
        ArrayList percentage   = Utilities.parseIntRange(place+"percentageOfSOSClauses: ",percentages,errors);
        ArrayList limit = Utilities.parseIntRange(place+"limit: ",limits,errors);
        ArrayList strategies   = ResolutionStrategy.parseStrategies(strategiess,place, warnings,errors);
        if(percentage == null || limit == null || strategies == null) {return null;}
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
            map.put("percentage",perc);
            map.put("limit",    limitpar);
            map.put("name","Resolution_" + ++counter);
            list.add(map);}
        return list;}

    /** gives a string with descriptions of the available parameters.
     *
     * @return a description of the available parameters.
     */
    public static String help() {
        return "Resolution parameters:\n" +  ResolutionStrategy.help() +
                "  seed:       for the random number generator \n" +
                "              default: the generator starts each time with a new seed.\n" +
                "  percentageOfSOSClauses: percentageOfSOSClauses of clauses in the set of support. (default 50)\n" +
                "  limit:      maximal number of resolvents = limit*clauses (default unlimited)\n"+
                "The format of seed, percentage of SOSClauses and limit is\n"+
                "Integer, or list of Integers (comma or blank separated), or a range: 'Integer to Integer.\n"+
                "Each combination of parameters causes a separate thread to work at the given problem.";}


    /** a random number generator, used for selecting the next resolution literas */
    private Random random;

    /** INPUT,SOS,POSITIVE or NEGATIVE */
    private ResolutionStrategy strategy;

    /** maintains the tasks to be executed */
    private TaskQueue taskQueue = null;

    /** collects statistical information */
    public ResolutionStatistics statistics;

    /** for forming the ids of the new clauses */
    private int[] id = new int[]{0};

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
     * 4. Resolution and simplification is started until <br>
     *     - a contradiction is formed or <br>
     *     - the primaryClauses became empty or<br>
     *     - the resolution limit is exceeded or <br>
     *     - the thread is interrupted.<br>
     *
     * @return the result of the resolution sequence.
     */
    public Result solve() {
        super.initialize();
        globalParameters.log(solverId + " for problem " + problemId + " started");
        long time = System.currentTimeMillis();
        initializeData();
        Result result = null;
        try{result = initializeClauses();
            if(result == null) {result = resolve();}}
        catch(InterruptedException ex) {
            globalParameters.log("Resolution " + combinedId + " interrupted after " + resolvents + " resolvents.\n");
            result = new Aborted("Resolution aborted after " + resolvents + " resolvents");}
        statistics.elapsedTime = System.currentTimeMillis() - time;
        System.out.println("RESULT " + result.toString());
        problemSupervisor.finished(this, result, "done");
        return result;}

    /** one resolution parent is always chosen from this list */
    private BucketSortedList<Clause> primaryClauses;

    /** the other resolution parent is chosen from both lists */
    private BucketSortedList<Clause> secondaryClauses;

    /** maps literals (numbers) to their occurrences in clauses */
    public BucketSortedIndex<CLiteral> literalIndex;

    /** If the strategy is SOS, then this number determines how many randomly chosen clauses are put into the primaryClauses list */
    private int percentageOfSOSClauses = 0;

    /** The search is stopped after the number of resolvent reaches this limit */
    private int resolutionLimit = 0;

    /** is set false after all initial clauses are integrated */
    private boolean initializing = true;

    /** for optimizing subsumption and replacement resolution operations */
    private int timestamp = 1;

    private int maxClauseLength = 3;


    /** initializes resolution specific data structures*/
    private void initializeData() {
        Object seed            = solverParameters.get("seed");
        random                 = (seed != null) ? new Random((Integer)seed) : new Random();
        strategy               = (ResolutionStrategy)solverParameters.get("strategy");
        statistics             = new ResolutionStatistics(combinedId);
        Object percent         = solverParameters.get("percentage");
        percentageOfSOSClauses = (percent != null) ? (Integer)percent : 0;
        primaryClauses         = new BucketSortedList<Clause>(clause->clause.size());
        secondaryClauses       = new BucketSortedList<Clause>(clause->clause.size());
        literalIndex           = new BucketSortedIndex<CLiteral>(predicates+1,
                                    (cLiteral->cLiteral.literal),
                                    (cLiteral->cLiteral.clause.size()));
        taskQueue = new TaskQueue(combinedId,monitor);}

    /** EquivalenceClasses manage equivalent literals.
     *  In each equivalence class the literals are mapped to their representatives,
     *  which is always the predicate with the smallest number.
     */
    private EquivalenceClasses equivalenceClasses = null;

    /** If an equivalence p = -p occurs or can be derived, this function is called.
     *  It adds an Unsatisfiable task to the task queue.
     */
    private BiConsumer<Integer,IntArrayList> contradictionHandler = ((reason,origin)->{
        taskQueue.add(new Task(0,(()-> new Unsatisfiable(reason.toString(),null
                )), (()->reason.toString())));});

    /** This function is called when a new disjunction is to be inserted.
     *  It generates a simplifyBackwards task.
     */
    private Consumer<Clause> insertHandler = (
            clause -> {insertClause(clause,isPrimary(clause,true),"Initial clause");
                if(clause.size() > 1) {
                    taskQueue.add(new Task(basicClauseList.maxClauseLength-clause.size()+3, // longer clauses should be
                        (()-> {simplifyBackwards(clause); return null;}),    // checked first for subsumption and replacement resolution
                        (()-> "Simplify initial clause " + clause.toString())));}});

    private int maxInputId = 0;
    /** This method translates all basic clauses into Clause data structures.
     *  Equivalent literals are replaced by their representatives.
     *
     * @return possibly Unsatisfiable
     * @throws InterruptedException
     */
    private Result initializeClauses() throws InterruptedException {/*
        if(basicClauseList.equivalences != null) {
            equivalenceClasses = Transformers.prepareEquivalences(basicClauseList,contradictionHandler,symboltable);
            if(!taskQueue.isEmpty()) {Result result = taskQueue.run(); if(result != null) {return result;}}}

        Transformers.prepareConjunctions(basicClauseList,equivalenceClasses,
                (literal-> addTrueLiteralTask(literal, true, "Initial Conjunction")));
        if(Thread.interrupted()) {throw new InterruptedException();}
        Transformers.prepareDisjunctions(basicClauseList,id,equivalenceClasses,insertHandler);
        Transformers.prepareXors     (basicClauseList,id,equivalenceClasses,insertHandler);
        Transformers.prepareDisjoints(basicClauseList,id,equivalenceClasses,insertHandler);
        int limit = (int)solverParameters.get("limit");
        resolutionLimit = (limit == Integer.MAX_VALUE) ? limit : limit * clauseCounter;
        initializing = false;
        if(Thread.interrupted()) {throw new InterruptedException();}
        if(checkConsistency) {check("initializeClauses");}
        maxInputId = id[0];
        purityAndElimination();*/

        return taskQueue.run();}



    /** counts the resolvents */
    private int resolvents = 0;

    private HashSet<String> clauseNames = new HashSet<>();

    /** performs the resolution search until a solution is found, or the number of resolvents exceeds the limit.
     * - two parent literals are chosen. <br>
     * - the resolvent is generated <br>
     * - the resolvent itself is simplified.<br>
     * - if it survives it causes forward subsumption and replacement resolution<br>
     * - further simplifications are put into the task queue.<br>
     * - the task queue is worked off.
     *
     * @return the result of the search
     * @throws InterruptedException
     */
    private Result resolve()  throws InterruptedException {
        Result result = null;
        CLiteral[] parentLiterals = new CLiteral[2];
        while(resolvents <= resolutionLimit) {
            if(Thread.interrupted()) {throw new InterruptedException();}
            purityAndElimination();
            result = taskQueue.run();
            if(result != null){return result;}
            if(primaryClauses.isEmpty()) {return completeModel();}
            selectParentLiterals(parentLiterals);
            System.out.printf("\n");
            if(checkConsistency) {
                if(parentLiterals[0].literal != -parentLiterals[1].literal) {
                    System.out.println("Error when selecting parent literals");
                    System.out.println("   "+parentLiterals[0].clause+"@"+parentLiterals[0].literal + " and");
                    System.out.println("   "+parentLiterals[1].clause+"@"+parentLiterals[1].literal );
                    System.exit(1);}}
            clauseNames.add(Integer.toString(parentLiterals[0].clause.id)+Integer.toString(parentLiterals[1].clause.id));
            Clause resolvent = LitAlgorithms.resolve(id,parentLiterals[0],parentLiterals[1]);
            ++statistics.resolvents;
            if(resolvent == null) {continue;}
            if(monitoring) {
                monitor.print(combinedId,"Resolution between\n" + parentLiterals[0].clause.toString() +
                "@"+parentLiterals[0].literal + " and \n" + parentLiterals[1].clause.toString() +
                        "@"+parentLiterals[1].literal + " yields\n"+resolvent.toString());}
            ++resolvents;
            simplifyBackwards(resolvent);
            if(resolvent.removed) {continue;}
            simplifyForward(resolvent);
            boolean isPrimary = (resolvent.cliterals.size() < parentLiterals[0].clause.cliterals.size() +
                    parentLiterals[1].clause.cliterals.size() -2) || isPrimary(resolvent,false)   ;
            insertClause(resolvent,isPrimary, "Resolvent");
            result = taskQueue.run();
            if(result != null) {break;}
            //System.out.println("After Resolution");
            //System.out.println(toString());
        }
        if(result == null) {
            System.out.println(toString());
            return new Aborted("Maximum Resolution Limit " + resolutionLimit + " exceeded");
            }
        if(result.getClass() == Satisfiable.class) {
            ArrayList<int[]> falseClauses = basicClauseList.falseClausesInModel(((Satisfiable)result).model);
            if(falseClauses == null) {return result;}
            System.out.println(toString());
            return new Erraneous(((Satisfiable)result).model,falseClauses,symboltable);}
        return result;}

    private HashSet<Integer> indices = new HashSet<>();
    /** The method chooses two parent literals for the next resolution step.
     * The first parent clauses is chosen randomly from the primary clauses.<br>
     * Shorter clauses are preferred (quadratically in size) <br>
     *     <br>
     * For the second parent clause the literals of the first parent clauses are searched for:
     *  - a shorter second parent clause <br>
     *  - a parent clause with a literal that merges with a literal in the first clause <br>
     *  - if none such clause is found, the first one complementary to the last literal in the clause is chosen.
     *
     * @param parentLiterals to store the parent literals.
     */
    private void selectParentLiterals(CLiteral[] parentLiterals) {
        indices.clear();
        int i = 0;
        while(++i < 100) {
            int index = primaryClauses.getRandomIndex(random);
            if(indices.contains(index)) {continue;}
            indices.add(index);
            Clause parent1 = primaryClauses.getItem(index);
            if(parent1 == null) {
                System.out.println("Error: no parent clause found");
                System.out.println(toString());
                System.exit(1);
            }
            System.out.printf(" T" +parent1.id);
            int size1 = parent1.size();
            String id1 = Integer.toString(parent1.id);
            for(CLiteral literal1 : parent1) {
                parentLiterals[0] = literal1;
                boolean first = true;
                Iterator<CLiteral> iterator = literalIndex.iterator(-literal1.literal);
                while(iterator.hasNext()) {
                    CLiteral literal2 = iterator.next();parentLiterals[1] = literal2;
                    Clause parent2 = literal2.clause;
                    String id2 = Integer.toString(parent2.id);
                    if(clauseNames.contains(id1+id2) || clauseNames.contains(id2+id1)) {continue;}
                    if(first) {parentLiterals[1] = literal2; first = false;}
                    if(literal2.clause.size() < size1) {parentLiterals[1] = literal2; return;}
                    for(CLiteral lit2 : literal2.clause) {
                        if(parent1.contains(lit2.literal) == 1) {parentLiterals[1] = literal2; return;}}}}}
        System.out.println("\nNothing found");
        System.out.println(toString());}

    /** checks if the clause ought to be a primary clause or a secondary clause.
     *  It depends on the strategy and the situation (processing input clauses or resolvents)<br>
     *      Strategy: <br>
     *          INPUT:    all input clauses are primary, the resolvents are secondary<br>
     *          POSITVE:  positive clauses are primary, all others are secondary<br>
     *          NEGATIVE: negative clauses are primary, all others are secondary<br>
     *          SOS:      input = true: a randomly chosen percentage (percentageOfSOSClauses) is primary <br>
     *                    all resolvents are primary.
     *
     * @param clause a clause to be checked
     * @param input   true if the clause is one of the input clauses
     * @return        true if the clause is to be inserted into the primary clauses.
     */
    private boolean isPrimary(Clause clause, boolean input) {
        switch(strategy) {
            case INPUT:    return input;
            case POSITIVE: return clause.isPositive();
            case NEGATIVE: return clause.isNegative();
            case SOS:      return input ? (random.nextInt(101) <= percentageOfSOSClauses) : true;}
        return true;}



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
     *    If the resulting clause is a unit clause, it generates a importTrueLiteral task.
     *    <br>
     *    Removing clauses may produce pure literals, whose negation can be made true and therefore
     *    generates a importTrueLiteral task.<br>
     *  - Shortened clauses may trigger forward subsumptions and forward replacement resolutions.
     *    Therefore thy generate a corresponding task.
     *
     * @param clause
     */
    private void simplifyBackwards(Clause clause) {
        if(clause.removed) {return;}
        timestamp += maxClauseLength +1;
        Clause subsumer = LitAlgorithms.isSubsumed(clause,literalIndex,timestamp);
        if(subsumer != null) {
            ++statistics.backwardSubsumptions;
            if(primaryClauses.contains(clause) && !primaryClauses.contains(subsumer)) {replaceClause(clause,subsumer);}
            else {removeClause(clause,0);}
            if(monitoring) {
                monitor.print(combinedId,"Clause \n  " + clause.toString() + " is subsumed by \n  " + subsumer.toString());}
            return;}

        timestamp += maxClauseLength +1;
        Object[] replacements = LitAlgorithms.replacementResolutionBackwards(clause,literalIndex,timestamp);

        while(replacements != null) { // several literals may become resolved away
             CLiteral cLiteral = (CLiteral)replacements[0];
            ++statistics.backwardReplacementResolutions;
            if(monitoring) {
                monitor.print(combinedId,
                        "Literal " + cLiteral.literal + " in clause \n  " + clause.toString() + " resolved away by clause \n  "
                        + ((Clause)replacements[1]).toString());}
            if(removeLiteral(cLiteral)) {
                timestamp += maxClauseLength +1;
                replacements = LitAlgorithms.replacementResolutionBackwards(clause,literalIndex,timestamp);}}}



    /** just used in simplifyForward */
    private ArrayList<Clause> clauseList = new ArrayList<>();
    /** just used in simplifyForward */
    private ArrayList<CLiteral> literalList = new ArrayList<>();

    /** does forward subsumption and replacement resolution
     *
     * @param clause
     */
    private void simplifyForward(Clause clause) {
        if(clause.removed) {return;}
        clauseList.clear();
        timestamp += maxClauseLength +1;
        LitAlgorithms.subsumes(clause,literalIndex,timestamp,clauseList);
        for(Clause subsumedClause : clauseList) {
            ++statistics.forwardSubsumptions;
            if(monitoring) {monitor.print(combinedId,"Clause \n  " + clause.toString() + "  subsumes \n  " + subsumedClause.toString());}
            removeClause(subsumedClause,0);}

        literalList.clear();
        timestamp += maxClauseLength +1;
        LitAlgorithms.replacementResolutionForward(clause,literalIndex,timestamp,literalList);
        for(CLiteral cLiteral : literalList) {
            ++statistics.forwardReplacementResolutions;
            if(monitoring) {
                monitor.print(combinedId,"Literal " + cLiteral.literal + " in clause \n  " + cLiteral.clause.toString() +
                        " resolved away by resolvent \n  " +clause.toString());}
            removeLiteral(cLiteral);
            Clause otherClause = cLiteral.clause;
            if(!otherClause.removed) {
                taskQueue.add(new Task(otherClause.size()+3,
                        (()->{simplifyForward(otherClause);return null;}),
                        (()->"Forward Simplification for shortened clause " + otherClause.toString())));}}}



    /** This method is called be the problemSupervisor, when another solver derives a true literal.
         * It generates a trueLiteral task.
         *
         * @param literal a new true literal
         */
    public Result importTrueLiteral(int literal) {
        ++statistics.importedUnitClauses;
        --statistics.derivedUnitClauses;
        addTrueLiteralTask(literal, false,"Imported from another solver");
        return null;}


    /** turns the literal into a trueLiteralTask.
     * If it is a unit resolvent then it is forwarded to the problem supervisor.
     *
     * @reason  for monitoring the tasks
     * @param literal a unit literal.
     */
    private void addTrueLiteralTask(int literal, boolean forward, String reason) {
        taskQueue.add(new Task(1,
                (()->processTrueLiteral(literal)),
                (()->reason + ": " + (symboltable == null ? literal : symboltable.toString(literal)))));
        ++statistics.derivedUnitClauses;
        if(forward && !initializing) problemSupervisor.forwardTrueLiteral(this,literal);}

    /** computes the consequences of a new true literal
     * - all clauses with this literal are removed <br>
     * - pure literals cause new true literals <br>
     * - all occurrences of the negated literal are removed<br>
     * - for each shortened clause which became a unit clause, a new task is created.<br>
     * - if the primary clauses became empty, the model is completed
     *
     * @param literal a new true literal
     * @return the result of a model completion or null
     */
    private Result processTrueLiteral(int literal) {
        //System.out.println("PL START " + literal);
        //System.out.println(toString());
        switch(model.status(literal)) {
            case -1: //return new Unsatisfiable(null,null); //model,null); //literal);
            case +1: return null;}
        model.addImmediately(literal);
        if(false) {
            ArrayList<int[]> falseClauses = basicClauseList.falseClausesInModel(model);
            if(falseClauses != null) {
                System.out.println("ErrorCheck: the following basic clauses are false in the model");
                System.out.println(model.toString());
                for(int[] clause : falseClauses) {
                    System.out.println(basicClauseList.clauseToString(clause));}
                System.exit(1);}}
        Iterator<CLiteral> iterator = literalIndex.iterator(literal);
        while(iterator.hasNext()) {
            Clause clause = iterator.next().clause;
            removeClause(clause,literal);}

        for(CLiteral cLiteral : literalIndex.getAllItems(-literal)) {
            removeLiteral(cLiteral);}
        literalIndex.clearBoth(Math.abs(literal));
        //System.out.println("PL END " + literal);
        //System.out.println(toString());
        return null;}


    private ArrayList<Object[]> eliminatedLiterals = new ArrayList<>();

    private void processElimination(int eliminateLiteral) {
        int size01p = literalIndex.size01(eliminateLiteral);
        int size01n = literalIndex.size01(-eliminateLiteral);
        if(size01p != 1 || size01n == 0) {return;}
        //System.out.println("Start Elimination " + eliminateLiteral );
        //System.out.println(toString());
        Clause clause  = literalIndex.getAllItems(eliminateLiteral).get(0).clause;
        ArrayList<CLiteral> literals = clause.cliterals;
        boolean inPrimary = primaryClauses.contains(clause);
        for(CLiteral otherCliteral : literalIndex.getAllItems(-eliminateLiteral)) {
            boolean tautology = false;
            Clause otherClause = otherCliteral.clause;
            Clause newClause = new Clause(++id[0], ClauseType.OR);
            ArrayList<CLiteral> newLiterals = new ArrayList<>();
            for(CLiteral literal : literals) {
                if(literal.literal != eliminateLiteral) {newLiterals.add(new CLiteral(literal.literal,newClause,newLiterals.size()));}}
            for(CLiteral literal : otherClause.cliterals) {
                if(literal.literal == -eliminateLiteral) {continue;}
                int contained = LitAlgorithms.contains(newLiterals,literal.literal);
                if(contained > 0) {continue;}
                if(contained < 0) {tautology = true; break;}
                newLiterals.add(new CLiteral(literal.literal,newClause,newLiterals.size()));}
            if(tautology) {removeClause(otherClause,0); continue;}
            newClause.cliterals = newLiterals;
            newClause.setStructure();
            simplifyBackwards(newClause);
            if(newClause.removed) {removeClause(otherClause,0); continue;}
            boolean inp = inPrimary || primaryClauses.contains(otherClause);
            removeClause(otherClause,0);
            insertClause(newClause,inp,"Literal " + eliminateLiteral + " eliminated");}
        eliminatedLiterals.add(new Object[]{clause.cliterals,eliminateLiteral});
        removeClause(clause,0);
        literalIndex.clearBoth(Math.abs(eliminateLiteral));
        if(checkConsistency) {check("processElimination");}
        //System.out.println("End Elimination " + eliminateLiteral + "@" + clause.toString());
        //System.out.println(toString());
    }

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
        System.out.println("Completing Model\n"+toString());
        if(model.size() == predicates) {return new Satisfiable(model);}
        boolean isPositive = true;
        switch(strategy) {
            case INPUT:
            case SOS:       // there should be no clauses any more.
                for(Clause clause : secondaryClauses) {
                    if(!trueInModel(clause,model)) {
                        return new Erraneous(model,clause,symboltable);}}
                break;
            case NEGATIVE: isPositive = false;
            case POSITIVE:
                for(Clause clause : secondaryClauses) {
                    if(trueInModel(clause,model)) {continue;}
                    boolean found = false;
                    for(CLiteral cliteral : clause) {
                        int literal = cliteral.literal;
                        if(model.status(literal) == 0 && ((isPositive && literal < 0) || (!isPositive && literal > 0))) {
                            model.addImmediately(literal); found = true; break;}}
                    if(!found) return new Erraneous(model,clause,symboltable);}}
        completeEliminations();
        Result result = equivalenceClasses.completeModel();
        if(result != null) {return result;}
        result = checkModel(model);
        if(result != null) {return result;}
        return new Satisfiable(model);}

    private void completeEliminations() {
        for(int i = eliminatedLiterals.size()-1; i >= 0; --i) {
            Object[] els = eliminatedLiterals.get(i);
            ArrayList<CLiteral> literals = (ArrayList<CLiteral>)els[0];
            int literal = (int)els[1];
            if(model.status(literal) != 0) {continue;}
            boolean satisfied = false;
            for(CLiteral  cliteral : literals) {
                int lit = cliteral.literal;
                if(lit != literal && model.status(lit) == 1) {satisfied = true; break;}}
            model.addImmediately(satisfied ? -literal : literal);}}


    /** counts the number of clauses in the resolution solver */
    private int clauseCounter = 0;

    /** inserts the clause into the local data structures.
     * - If the clause is a unit clause, it generates a trueLiteralTask<br>
     * - If it is an initial clause, it generates a simplifyBackwards task.<br>
     *   These tasks are sorted such that longer clauses are simplified first (subsumption and replacement resolution).
     *
     * @param clause  the clause to be inserted.
     * @param primary determines whether the clause is inserted into primaryClauses or secondaryClauses.
     */
    private void insertClause(Clause clause, boolean primary, String reason) {
        switch(clause.size()) {
            case 1: addTrueLiteralTask(clause.getLiteral(0),true,reason); return;
            case 2: findEquivalence(clause);}
        ++clauseCounter;
        maxClauseLength = Math.max(maxClauseLength,clause.size());
        (primary ? primaryClauses : secondaryClauses).add(clause);
        for(CLiteral cLiteral : clause) {literalIndex.add(cLiteral);}
        if(checkConsistency) {check("insertClause");}}


    /** removes a clause from primary/secondary clauses and from the literal index (except ignoreLiteral)
     *
     * @param clause        the clause to be removed
     * @param ignoreLiteral the literal not to remove from the index
     */
    private void removeClause(Clause clause, int ignoreLiteral) {
        if(clause.removed) {return;}
        --clauseCounter;
        if(primaryClauses.contains(clause)) {primaryClauses.remove(clause);}
        else {secondaryClauses.remove(clause);}
        for(CLiteral cLiteral : clause) {
            if(cLiteral.literal != ignoreLiteral) {
                literalIndex.remove(cLiteral);}}
        clause.removed = true;
        if(checkConsistency) {check("removeClause");}}

    /** This method is called when secondary clause subsumes a primary clause.
     * In this case the subsumer must be moved to the primary clauses.
     * @param primaryClause   a primary clause
     * @param secondaryClause a secondary clause which subsumes a primary clause.
     */
    private void replaceClause(Clause primaryClause, Clause secondaryClause) {
        primaryClauses.remove(primaryClause);
        primaryClause.removed = true;
        for(CLiteral cLiteral : primaryClause) {literalIndex.remove(cLiteral);}
        secondaryClauses.remove(secondaryClause);
        primaryClauses.add(secondaryClause);
        if(checkConsistency) {check("replaceClause");}}

    /** removes the literal from its clause and the literal index.
     * - If the clause is already marked 'removed' nothing happens. <br>
     * - If the shortened clause is a unit clause, it generates a trueLiteralTask, and is removed.
     *
     * @param cLiteral the literal to be removed
     * @return true if the clause is still there.
     */
    private boolean removeLiteral(CLiteral cLiteral) {
        Clause clause = cLiteral.clause;
        if(clause.removed) {return false;}
        boolean isOld = clause.getPosition() >= 0;
        if(isOld) {
            (primaryClauses.contains(clause) ? primaryClauses : secondaryClauses).remove(clause);
            for(CLiteral cliteral : clause) literalIndex.remove(cliteral);}
        clause.remove(cLiteral);
        if(clause.size() == 1) {
            addTrueLiteralTask( clause.getLiteral(0),true,
                    "New true literal derived from clause " + clause.toString());
            removeClause(clause,0);
            return false;}
        if(isOld) {
            for(CLiteral cliteral : clause) literalIndex.add(cliteral); // literals are inserted into different buckets
            switch(strategy) {
                case INPUT:
                case SOS:      primaryClauses.add(clause); break;
                case POSITIVE: (clause.isPositive() ? primaryClauses : secondaryClauses).add(clause); break;
                case NEGATIVE: (clause.isNegative() ? primaryClauses : secondaryClauses).add(clause); break;}}
        if(checkConsistency) {check("removeLiteral");}
        clause.id = ++id[0];
        return true;}



    private ArrayList<Integer> zeros = new ArrayList<>();
    private ArrayList<Integer> ones = new ArrayList<>();

    /** This method checks all predicates for purity and elimination.
     * A literal p is pure if there are no clauses with p any more. <br>
     * In this case -p can be made true. <br>
     * A literal p can be eliminated if it occurs only once in the clauses, say in clause C.
     * In this case all clauses with -p can be replaced with their resolvent with C.
     */
    private void purityAndElimination() {
        while(literalIndex.size01(predicates,zeros,ones)) {
            for(int literal : zeros) {
                int sizep = literalIndex.size01(literal);
                if(sizep == 0) {
                    if(monitoring) {monitor.print(combinedId, "Eliminating pure literal " + -literal);}
                    processTrueLiteral(-literal);}}
            for(int literal : ones)  {processElimination(literal);}}}

    /** checks if the clause is part of an equivalence like (p,q) and (-p,-q)
     *
     * @param clause a clause to be checked (p,q)
     * @return true if there is another clause (-p,-q)
     */
    private boolean isEquivalence(Clause clause) {
        if(clause.size() != 2) {return false;}
        ArrayList<CLiteral> clits1 = literalIndex.getItems(-clause.getCLiteral(0).literal,2);
        ArrayList<CLiteral> clits2 = literalIndex.getItems(-clause.getCLiteral(1).literal,2);
        if(clits1 == null || clits2 == null) {return false;}
        timestamp += maxClauseLength +1;
        for(CLiteral clit : clits1) {clit.clause.timestamp = timestamp;}
        for(CLiteral clit : clits2) {if(clit.clause.timestamp == timestamp) {return true;}}
        return false;}

    /** This method checks if the clause is part of an equivalence (p,q) (-p,-q)
     * If this is the case: <br>
     *     - -p == q is inserted into the equivalence classes <br>
     *     - a processEquivalence task is generated
     *
     * @param clause the clause to be checked
     * @return true if an equivalence has been found.
     */
    private boolean findEquivalence(Clause clause) {
        if(!isEquivalence(clause)) {return false;}
        int literal1 = -clause.getCLiteral(0).literal;
        int literal2 =  clause.getCLiteral(1).literal;
        if(literal1 < 0) {literal1 = -literal1; literal2 = -literal2;}
        int fromliteral = literal1; int toliteral = literal2;
        equivalenceClasses.addDerivedEquivalence(fromliteral,toliteral,null);
       // taskQueue.add(new Task(2,(()->processEquivalence(fromliteral,toliteral)),
       //         (()-> "Replacing equivalent literal: "+ fromliteral + " -> " + toliteral)));
        ++statistics.equivalences;
        return true;}

    private ArrayList<Clause> replacedClauses = new ArrayList<>();

    /** This method replaces all occurrences of fromLiteral by toLiteral.
     *  Generated tautologies are ignored.<br>
     *  Double literals are avoided. <br>
     *  The new clauses are backwards simplified.
     *
     * @param fromLiteral antecedent
     * @param toLiteral   succedent
     * @return            null
     */
    private Result processEquivalence(int fromLiteral, int toLiteral) throws Unsatisfiable{
        //System.out.println("START EQUIVALENCE " + fromLiteral + " -> " + toLiteral);
        //System.out.println(toString());
        int fromStatus = model.status(fromLiteral);
        int toStatus   = model.status(toLiteral);
        if(fromStatus != 0 && toStatus != 0 && fromStatus != toStatus) {
            return null;} //new Unsatisfiable(null,null);} //model,toLiteral);}
        if(fromStatus != 0) {
            addTrueLiteralTask((fromStatus == 1 ? toLiteral : -toLiteral),true,
                    "equivalent literals " + fromLiteral + " " + toLiteral);
            return null;}
        if(toStatus != 0) {
            addTrueLiteralTask((toStatus == 1 ? fromLiteral : -fromLiteral),true,
                    "equivalent literals " + fromLiteral + " " + toLiteral);
            return null;}
        replacedClauses.clear();
        for(CLiteral cliteral : literalIndex.getAllItems(fromLiteral)) {
            Clause clause = cliteral.clause;
            Clause newClause = new Clause(++id[0],ClauseType.OR, clause.size());
            boolean tautology = false;
            for(CLiteral cLiteral : clause.cliterals) {
                int literal = cLiteral.literal;
                if(literal == toLiteral) {continue;}
                if(literal == -toLiteral) {tautology = true; break;}
                if(literal == fromLiteral) {literal = toLiteral;}
                newClause.add(new CLiteral(literal));}
            removeClause(clause,0);
            if(!tautology) {replacedClauses.add(newClause);}}

        for(CLiteral cliteral : literalIndex.getAllItems(-fromLiteral)) {
            Clause clause = cliteral.clause;
            Clause newClause = new Clause(++id[0],ClauseType.OR, clause.size());
            boolean tautology = false;
            for(CLiteral cLiteral : clause.cliterals) {
                int literal = cLiteral.literal;
                if(literal == -toLiteral) {continue;}
                if(literal == toLiteral) {tautology = true; break;}
                if(literal == -fromLiteral) {literal = -toLiteral;}
                newClause.add(new CLiteral(literal));}
            removeClause(clause,0);
            if(!tautology) {replacedClauses.add(newClause);}}

        literalIndex.clearBoth(fromLiteral);
        for(Clause clause : replacedClauses) {
            simplifyBackwards(clause);
            if(!clause.removed) {
                insertClause(clause,isPrimary(clause,false),"Literal " + fromLiteral + " -> " + toLiteral);}}
        for(Clause clause : replacedClauses) {if(!clause.removed) {simplifyForward(clause);}}

        //System.out.println("END EQUIVALENCE " + fromLiteral + " -> " + toLiteral);
        //System.out.println(toString());
        return null;}




    /** return the entire statistics information
     *
     * @return the entire statistics information for the resolution solver.
     */
   // public Statistic getStatistics() {return statistics;}

    /** lists the clauses and the literal index as a string.
     *
     * @return the clauses and the literal index as a string.
     */
    public String toString() {return toString(null);}

    /** lists the clauses and the literal index as a string.
     *
     * @param symboltable a symboltable or null
     * @return the clauses and the literal index as a string.
     */
    public String toString(Symboltable symboltable) {
        Function<Clause,String> clauseString = (clause->clause.toString(0,symboltable));
        Function<CLiteral,String> literalString = (cliteral->cliteral.toString(symboltable,clause->Integer.toString(clause.id)));
        StringBuilder st = new StringBuilder();
        st.append("Resolution:\n");
        if(!primaryClauses.isEmpty()) {
            st.append("Primary Clauses:\n").append(primaryClauses.toString(clauseString)).append("\n");}
        if(!secondaryClauses.isEmpty()) {
            st.append("Secondary Clauses:\n").append(secondaryClauses.toString(clauseString)).append("\n");}
        if(model != null && !model.isEmpty()) {
            st.append("Model:\n").append(model.toString()).append("\n\n");}
        st.append("Literal Index:\n").append(literalIndex.toString(literalString));
        if(!taskQueue.isEmpty()) {
            st.append("\nTask Queue:\n").append(taskQueue.toString());}
        if(!eliminatedLiterals.isEmpty()) {
            st.append("Eliminated Literals:\n");
            for(Object[] elms : eliminatedLiterals) {
                st.append("  "+elms[1].toString() + " from " + ((ArrayList<CLiteral>)elms[0]).toString()+"\n");}}
        return st.toString();}

    /** collects information about the control parameters
     *
     * @return a string with information about the control parameters.
     */
    public String parameters() {
        StringBuilder st = new StringBuilder();
        Object seed = solverParameters.get("seed");
        st.append("Resolution        " + combinedId + "\n");
        st.append("Strategy:         " + strategy.toString()).append("\n");
        st.append("Resolution Limit: " +  resolutionLimit).append("\n");
        if(seed != null) {
            st.append("Seed:             " + seed.toString()).append("\n");}
        if(strategy == ResolutionStrategy.SOS){
            st.append("SOS percentage:   " + percentageOfSOSClauses).append("\n");}
        return st.toString();
    }

    public void check(String info) {
        primaryClauses.check(info + ":'primary clauses'");
        secondaryClauses.check(info+"':secondary clauses'");
        literalIndex.check(info+":'literal index'");

        for(Clause clause : primaryClauses) {
            for(CLiteral cLiteral : clause) {
                if(!literalIndex.contains(cLiteral)) {
                    System.out.println("Error: "+info+ " literal " + cLiteral.literal + " in clause " + clause.toString() + " is not in the index.");}}}
        for(Clause clause : secondaryClauses) {
            for(CLiteral cLiteral : clause) {
                if(!literalIndex.contains(cLiteral)) {
                    System.out.println("Error: "+info+" literal " + cLiteral.literal + " in clause " + clause.toString() + " is not in the index.");}}}
    }
}
