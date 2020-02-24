package Solvers.Resolution;

import Coordinator.Tasks.Task;
import Coordinator.Tasks.TaskQueue;
import Datastructures.Clauses.BasicClauseList;
import Datastructures.Clauses.Clause;
import Datastructures.Literals.CLiteral;
import Datastructures.Literals.LitAlgorithms;
import Datastructures.Results.*;
import Datastructures.Symboltable;
import Datastructures.Theory.DisjointnessClasses;
import Datastructures.Theory.EquivalenceClasses;
import Datastructures.Theory.Model;
import Management.GlobalParameters;
import Management.Monitor;
import Management.ProblemSupervisor;
import Utilities.BucketSortedIndex;
import Utilities.BucketSortedList;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.*;
import java.util.function.Function;


public class Preparer {

    boolean checkConsistency = true;

    private static HashSet<String> keys = new HashSet<>(); // contains the allowed keys in the specification.
    static { // these are the allowed keys in the specification.
        for(String key : new String[]{"name", "type", "solver"}) {
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
            if(!keys.contains(key)) {warnings.append("Reduction: unknown key in parameters: " + key + "\n");}}
        ArrayList<HashMap<String,Object>> list = new ArrayList<>();
        HashMap<String,Object> map = new HashMap<>();
        map.put("name","Reduction");
        list.add(map);
        return list;}

    ProblemSupervisor problemSupervisor;

    /** gives a string with descriptions of the available parameters.
     *
     * @return a description of the available parameters.
     */
    public static String help() {
        return "There are no parameters";}


    /** constructs a new Resolution solver.
     *
     * @param problemSupervisor    coordinates several solvers.
     */
    public Preparer(ProblemSupervisor problemSupervisor) {
        this.problemSupervisor = problemSupervisor;}


    /** contains all the clauses, sorted according to the clause length*/
    private BucketSortedList<Clause> clauses;
    BucketSortedIndex<CLiteral> literalIndex = null;
    TaskQueue taskQueue = null;
    PreparerStatistics statistics = null;
    int maxClauseLength = 3;
    int timestamp = 1;
    int predicates;
    boolean monitoring = false;
    Monitor monitor;
    GlobalParameters globalParameters;
    String problemId;
    BasicClauseList basicClauseList;
    EquivalenceClasses equivalenceClasses = null;
    DisjointnessClasses disjointnessClasses = null;
    Symboltable symboltable = null;
    Model model = null;
    int maxInputId = 0;
    int[] ids = new int[]{0};
    boolean trackReasoning;
    static final int priorityResult      = 0;
    static final int priorityUnity       = 1;
    static final int priorityEquivalence = 2;
    static final int priorityElimination = 3;
    static final int priorityBinary      = 4;
    static final int priorityShift       = 5;

    public Result prepare() {
        initializeData();
        Result result = initializeClauses();
        if(result != null) {return result;}
        return simplify();}

    /** initializes resolution specific data structures*/
    void initializeData() {
        basicClauseList = problemSupervisor.basicClauseList;
        predicates   = basicClauseList.predicates;
        symboltable = basicClauseList.symboltable;
        globalParameters = problemSupervisor.globalParameters;
        monitor                    = globalParameters.monitor;
        monitoring                 = monitor.monitoring;
        problemId                  = problemSupervisor.problemId;
        clauses      = new BucketSortedList<Clause>(clause->clause.size());
        literalIndex = new BucketSortedIndex<CLiteral>(predicates+1,
                            (cLiteral->cLiteral.literal),
                            (cLiteral->cLiteral.clause.size()));
        taskQueue    = new TaskQueue(problemId,monitor);
        trackReasoning = globalParameters.trackReasoning;
        model = new Model(predicates,trackReasoning);
        statistics = new PreparerStatistics(problemId);}


    /** This method translates all basic clauses into Clause data structures.
     *  Equivalent literals are replaced by their representatives.
     *
     * @return possibly Unsatisfiable
     * @throws InterruptedException
     */
    Result initializeClauses() {
        Result result = null;
        if(basicClauseList.equivalences != null) {
            result = prepareEquivalences(basicClauseList.equivalences);
            if(result != null) {return result;}}

        if(basicClauseList.conjunctions != null) {
            result = prepareConjunctions(basicClauseList.conjunctions);
            if(result != null) {return result;}}

        if(basicClauseList.disjunctions != null) {
            result = prepareDisjunctions(basicClauseList.disjunctions);
            if(result != null) {return result;}}

        if(basicClauseList.disjoints != null || basicClauseList.xors != null) {
            disjointnessClasses = new DisjointnessClasses(symboltable,model,
                    ((literal,origins) -> addTrueLiteralTask(literal,origins, "derived from disjointness clause")),
                    ((literal1, literal2, origins) -> insertBinaryClause(literal1,literal2,origins)));}

        if(basicClauseList.disjoints != null) {prepareDisjoints(basicClauseList.disjoints);}

        if(basicClauseList.xors != null) {prepareXors(basicClauseList.xors);}

        if(checkConsistency) {check("initializeClauses");}
        maxInputId = ids[0];
    return null;}

    Result simplify() {
        Result result = runTaskQueue();
        if(result != null) {return result;}
        subsumeForward();
        Iterator<Clause> iterator = clauses.popIterator();
        while(iterator.hasNext()) {
            Clause clause = iterator.next();
            taskQueue.add(new Task(priorityShift+clause.size(),
                    () -> simplifyClause(clause),
                    () -> "simplifying clause " + clause.id));}
        clauses.pushIterator(iterator);
        result = runTaskQueue();
        if(result != null) {return result;}
        result = purityAndElimination();
        if(clauses.isEmpty()) {return completeModel();}
        return result;
    }
    /** This method checks all predicates for purity and elimination.
     * A literal p is pure if there are no clauses with p any more. <br>
     * In this case -p can be made true. <br>
     * A literal p can be eliminated if it occurs only once in the clauses, say in clause C. <br>
     * In this case all clauses with -p can be replaced with their resolvent with C.
     *
     * @result a result or null
     */
    Result purityAndElimination(){
        boolean simplified = true;
        while(simplified) { // iterate until no simplifications are possible any more
            simplified = false;
            Result result = removePureLiterals();  // all currently pure literals are eliminated now
            if(result != null) return result;
            int elimLiteral = literalIndex.oneOccurrence(predicates);
            if(elimLiteral != 0) {
                ++statistics.eliminations;
                if(monitoring) {monitor.print(problemId, "Eliminating single literal " + literalName(elimLiteral));}
                processElimination(elimLiteral);
                result = runTaskQueue();
                if(result != null) {return result;}
                simplified = true;}}
        return null;}

    /** This list collects the eliminated literals for completing the model at the end.
     *  For each clause (p,q,r,...) where p occurs only once it contains <br>
     *  (the clause, the eliminated literal)
     */
    private ArrayList<Object[]> eliminatedLiterals = new ArrayList<>();

    private ArrayList<Clause> replacedClauses = new ArrayList<>();


    /** The method eliminates all occurrences of a literal p and -p, where p occurs only once.<br>
     *  The clauses with -p are replaced by their resolvents with the clause with p.
     *  The resolvents are backward-simplified
     *
     * @param eliminateLiteral the literal which occurs only once.
     */
    void processElimination(int eliminateLiteral) {
        CLiteral parentCLiteral = literalIndex.getAllItems(eliminateLiteral).get(0);
        Clause clause  = parentCLiteral.clause;  // the clause with the literal to be eliminated
        replacedClauses.clear();
        for(CLiteral otherCliteral : literalIndex.getAllItems(-eliminateLiteral)) { // all literals with -p
            Clause otherClause = otherCliteral.clause;
            Clause resolvent = LitAlgorithms.resolve(ids,parentCLiteral,otherCliteral); // double literals and tautologies tested
            removeClause(otherClause);
            if(resolvent != null) {
                Clause sumbsumer = LitAlgorithms.isSubsumed(resolvent,literalIndex,timestamp);
                timestamp += maxClauseLength + 1;
                if(sumbsumer != null) continue;
                IntArrayList origins = new IntArrayList();
                origins.addAll(clause.origins); origins.addAll(otherClause.origins);
                resolvent.origins = origins;
                replacedClauses.add(resolvent);
                insertClause(resolvent);}}

        removeClause(clause);
        eliminatedLiterals.add(new Object[]{clause.cliterals,eliminateLiteral});
        literalIndex.clearBoth(Math.abs(eliminateLiteral));
        // all original clauses are removed

        for(Clause resolvent : replacedClauses) { // subsumption within the replaced clauses may not be recognized
            simplifyClause(resolvent);}
        if(checkConsistency) {check("processElimination");}
    }


    private IntArrayList zeros = new IntArrayList();

    /** removes all pure literals (literals which occur with one polarity only)
     *  by making the other polarity true.
     *
     * @return null or a final result of simplifications
     * @throws InterruptedException
     */
    Result removePureLiterals() {
        boolean purities = false;
        while(literalIndex.zeroes(predicates,zeros)) {
            purities = true;
            for(int literal : zeros) {
                if(model.status(literal) != 0) {continue;}
                if(monitoring) {monitor.print(problemId, "Making pure literal true: " + literalName(-literal));}
                ++statistics.purities;
                model.add(-literal,null);
                for(CLiteral cLiteral : literalIndex.getAllItems(-literal)) {
                    removeClause(cLiteral.clause);}}}
        if(purities && clauses.isEmpty()) {return completeModel();}
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
    Result completeModel() {
        System.out.println("Completing Model\n"+toString());
        Result result = null;
        for(int i = 1; i <= 3; ++i) {
            if(model.size() == predicates) {return new Satisfiable(model);}
            result = equivalenceClasses.completeModel(model);
            if(result != null) {return result;}
            completeEliminationsInModel();
            result = checkModel(model);
            if(result != null) {return result;}}
        return new Satisfiable(model);}

    /** completes a partial model by inserting the value for eliminated literals */
    void completeEliminationsInModel() {
        for(int i = eliminatedLiterals.size()-1; i >= 0; --i) {
            Object[] els = eliminatedLiterals.get(i);
            ArrayList<CLiteral> literals = (ArrayList<CLiteral>)els[0];
            int literal = (int)els[1];
            if(model.status(literal) != 0) {continue;}
            boolean satisfied = false;
            for(CLiteral cliteral : literals) {
                int lit = cliteral.literal;
                if(lit != literal && model.status(lit) == 1) {satisfied = true; break;}}
            model.add((satisfied ? -literal : literal),null);}}


    /** The method checks if the model satisfies the basic clauses.
     *
     * @return null or an Erraneous Result
     */
    public Result checkModel(Model model) {
        ArrayList<int[]> falseClauses = basicClauseList.falseClauses(model);
        if(falseClauses != null) {return new Erraneous(model,falseClauses,symboltable);}
        else {return null;}}

    Result runTaskQueue() {
        try {return taskQueue.run();} catch (InterruptedException ex) {}
        return null;}


    /** turns the literal into a trueLiteralTask.
     *
     * @reason  for monitoring the tasks
     * @param literal a unit literal.
     */
    void addTrueLiteralTask(int literal, IntArrayList origins, String reason) {
        ++statistics.derivedUnitClauses;
        taskQueue.add(new Task(priorityUnity,
                (()->processTrueLiteral(literal,origins)),
                (()->reason + ": " + literalName(literal))));}


    /** turns all equivalences in the basicClauseList into equivalence classes and sets the variable 'equivalenceClasses'.
     *
     * @return either null or Unsatisfiable if p == -p was encountered
     */
    public Unsatisfiable prepareEquivalences(ArrayList<int[]> basicClauses) {
        Unsatisfiable[] result = new Unsatisfiable[]{null};
        equivalenceClasses = new EquivalenceClasses(symboltable,
                ((literal, origin) -> {
                    result[0] = new Unsatisfiable(""+literal+" == "+-literal + " in equivalence clause " + origin.getInt(0));}));
        for(int[] clause : basicClauses) {
            equivalenceClasses.addEquivalenceClass(clause);
            if(result[0] != null) {return result[0];}}
        return null;}

    /** The handler is applied to all conjuncts in the basicClauseList.
     * If there are equivalence classes then the literals are mapped to the representative of the equivalence class.
     *
     * @param conjunctions         the list of input conjunctions.
     * @return                     null or an Unsatisfiable object.
     */
    public Unsatisfiable prepareConjunctions(ArrayList<int[]> conjunctions) {
        for(int[] basicClause : conjunctions) {
            for(int i = 2; i < basicClause.length; ++i) {
                int originalLiteral =  basicClause[i];
                int literal = originalLiteral;
                IntArrayList origin = null;
                if(equivalenceClasses != null) {
                    literal = equivalenceClasses.mapToRepresentative(originalLiteral);
                    if(literal != originalLiteral) origin = equivalenceClasses.mapToOrigins(originalLiteral);}
                IntArrayList origins = IntArrayList.wrap(new int[]{basicClause[0]});
                if(origin != null) {origins.addAll(origin);}
                if(model.isFalse(literal)) {
                    return new Unsatisfiable(model,literal,symboltable,origins);}
                else {model.add(literal,origins);}}}
        return null;}

    /** transforms all disjunctions in the basic clause list into clauses and applies the handler to the new clauses.
     * If there are equivalence classes then the literals are mapped to the representative of the equivalence class.
     * Double literals are removed.<br>
     * Tautologies are ignored. <br>
     * A resulting clause may be a unit clause.
     *
     * @param disjunctions       the list of input clauses.
     * @return Unsatisfiable if the clause is empty, otherwise null
     */
    public Unsatisfiable prepareDisjunctions(ArrayList<int[]> disjunctions) {
        for(int[] basicClause : disjunctions) {
            Unsatisfiable result = prepareDisjunction(basicClause);
            if(result != null) return result;}
        return null;}


    /** turns a single basic clause into a Clause datastructure and applies the handler to it.
     * Literals are replaced by their representative in the equivalence classes.<br>
     * Double literals are removed.<br>
     * Tautologies are ignored.
     *
     * @param basicClause [id,type,lit1,...] (the type is ignored)
     * @return Unsatisfiable if the clause is empty, otherwise null
     */
    private Unsatisfiable prepareDisjunction(int[] basicClause) {
        Clause clause = new Clause(++ids[0],basicClause.length-2);
        IntArrayList origins = trackReasoning ? IntArrayList.wrap(new int[]{basicClause[0]}) : null;
        for(int i = 2; i < basicClause.length; ++i) {
            int originalLiteral =  basicClause[i];
            int literal = originalLiteral;
            if(equivalenceClasses != null) {
                literal = equivalenceClasses.mapToRepresentative(originalLiteral);
                if(trackReasoning && literal != originalLiteral) {
                    IntArrayList origin = equivalenceClasses.mapToOrigins(originalLiteral);
                    if(origin != null) {origins.addAll(origin);}}}
            if(model.isFalse(literal)) {
                if(trackReasoning) {origins.addAll(model.getOrigin(literal));}
                continue;}
            clause.add(new CLiteral(literal,clause,i-2));}
        if(clause.isEmpty()) return new Unsatisfiable(model,basicClause,symboltable,origins);
        if(!clause.hasComplementaries()) {
            clause.removeDoubles();
            if(clause.size() == 1) {
                addTrueLiteralTask(clause.getLiteral(0),origins, "initial clause " + clause.id +
                        " simpified to one-literal clause");}
            clause.setStructure();
            clause.origins = origins;
            insertClause(clause);}
        return null;}

    void insertBinaryClause(int literal1, int literal2, IntArrayList origins) {
        Clause clause = new Clause(++ids[0],2);
        clause.add(new CLiteral(literal1,clause,0));
        clause.add(new CLiteral(literal2,clause,1));
        clause.origins = origins;
        insertClause(clause);}

    /** inserts the clause into the local data structures.
     *
     * @param clause  the clause to be inserted.
     */
    void insertClause(Clause clause) {
        ++statistics.clauses;
        maxClauseLength = Math.max(maxClauseLength,clause.size());
        clauses.add(clause);
        insertIntoIndex(clause);
        if(clause.isPositive()) {++statistics.positiveClauses;}
        else {if(clause.isNegative()) {++statistics.negativeClauses;}}
        if(checkConsistency) {check("insertClause");}}

    /** removes a clause from primary/secondary clauses and from the literal index (except ignoreLiteral)
     *
     * @param clause        the clause to be removed
     */
    void removeClause(Clause clause) {
        if(clause.removed) {return;}
        if(clause.isPositive()) {--statistics.positiveClauses;}
        else {if(clause.isNegative()) {--statistics.negativeClauses;}}
        --statistics.clauses;
        clauses.remove(clause);
        for(CLiteral cLiteral : clause) {literalIndex.remove(cLiteral);}
        clause.removed = true;
        if(checkConsistency) {check("removeClause");}}

    /** removes the literal from its clause and the literal index.
     * - If the clause is already marked 'removed' nothing happens. <br>
     * - If the shortened clause is a binary clause, it is entirely removed from the lists and the index.<br>
     *   The clause itself becomes a unit clause.
     *
     * @param cLiteral the literal to be removed
     * @param origins the basic Clauses Ids which caused the removal of the literal.
     * @return true if the clause became a unit clause
     */
    boolean removeLiteral(CLiteral cLiteral, IntArrayList origins) {
        Clause clause = cLiteral.clause;
        if(clause.removed) {return false;}
        clause.origins.addAll(origins);
        if(clause.size() == 2) {
            removeClause(clause);
            clause.remove(cLiteral);
            return true;}

        if(clause.isPositive()) {--statistics.positiveClauses;}
        else {if(clause.isNegative()) {--statistics.negativeClauses;}}
        clauses.remove(clause);
        removeFromIndex(clause);
        clause.remove(cLiteral);
        clauses.add(clause);
        insertIntoIndex(clause);
        if(clause.isPositive()) {++statistics.positiveClauses;}
        else {if(clause.isNegative()) {++statistics.negativeClauses;}}
        if(checkConsistency) {check("removeLiteral");}
        return false;}


    /** inserts a clause into the literal index
     *
     * @param clause  the clause to be inserted
     */
    void insertIntoIndex(Clause clause) {
        for(CLiteral cliteral : clause) {literalIndex.add(cliteral);}}

    /** removes a clause from the literal index
     *
     * @param clause  the clause to be inserted
     */
    void removeFromIndex(Clause clause) {
        for(CLiteral cliteral : clause) {literalIndex.remove(cliteral);}}

    /** transforms all disjoint clauses in the basicClauseList into normal clauses and applies the handler to them.
     * Disjoints are like XORs, except that all literals may be false.
     *
     * @param disjoints    the input clauses
     */
    public void prepareDisjoints(ArrayList<int[]> disjoints) {
        for(int[] basicClause : disjoints) {
            disjointnessClasses.addDisjointnessClass(basicClause,equivalenceClasses,model);}}

    /** transforms all exclusive-or clauses in the xors into normal clauses and applies the handler to them.
     *  Example: p xor q xor r yields the clauses: <br>
     *  1. p,q,r   (one of the literals must be true)<br>
     *  2. -p,-q   (if p ist true then q must be false and vice versa)<br>
     *  3. -p,-r   (if p ist true then r must be false and vice versa)<br>
     *  4. -q,-r   (if q ist true then r must be false and vice versa)
     *
     * @param xors    the input clauses
     */
    public void prepareXors(ArrayList<int[]> xors) {
        for(int[] basicClause : xors) {
            prepareDisjunction(basicClause);
            disjointnessClasses.addDisjointnessClass(basicClause,equivalenceClasses,model);}}


    /** computes the consequences of a new true literal
     * - all clauses with this literal are removed <br>
     * - all occurrences of the negated literal are removed<br>
     * - for each shortened clause which became a unit clause, a new task is created.<br>
     * - if the primary clauses became empty, the model is completed
     *
     * @param trueLiteral a new true literal
     * @return the result of a model completion or null
     */
    Result processTrueLiteral(int trueLiteral, IntArrayList origins) {
        int literal = equivalenceClasses.mapToRepresentative(trueLiteral);
        if(literal != trueLiteral) {origins.addAll(equivalenceClasses.mapToOrigins(trueLiteral));}
        switch(model.status(literal)) {
            case -1: return new Unsatisfiable(model,literal,symboltable,origins);
            case +1: return null;}
        model.add(literal,origins);
        for(CLiteral cLiteral : literalIndex.getAllItems(literal)) {
            removeClause(cLiteral.clause);}
        for(CLiteral cLiteral : literalIndex.getAllItems(-literal)) {
            Clause clause = cLiteral.clause;
            removeLiteral(cLiteral,origins);
            analyseShortenedClause(clause, "clause " + clause.id + " simplified by true literal " + literalName(trueLiteral));}
        if(checkConsistency) {check("processTrueLiteral");}
        return null;}


    /** just used in simplifyForward */
    private ArrayList<Clause> subsumedClauses = new ArrayList<>();

    /** This method iterates over the clauses and eliminates all subsumed clauses*/
    void subsumeForward() {
        subsumedClauses.clear();
        int startIndex = 0;
        Iterator<Clause> iterator = clauses.popIterator();
        while(iterator.hasNext()) {
            Clause clause = iterator.next();
            if(clause.removed) {continue;}
            LitAlgorithms.subsumes(clause,literalIndex,timestamp, subsumedClauses);
            timestamp += maxClauseLength +1;
            int size = subsumedClauses.size();
            for(int i = startIndex; i < size; ++i) {
                Clause subsumedClause = clauses.getItem(i);
                clause.removed = true;
                ++statistics.forwardSubsumptions;
                if(monitoring) {
                    monitor.print(problemId,"Clause \n  " + clause.toString(symboltable) +
                            "  subsumes \n  " + subsumedClause.toString(symboltable));}}
            startIndex = size;}
        clauses.pushIterator(iterator);
        for(Clause clause :subsumedClauses) {clause.removed = false; removeClause(clause);}}

    /** simplifies the clause be backward replacement resultion and ur resolution.
     *  A simplified clause triggers forward subsumption, forward replacement resolution and a true literal task.
     *  If the clause becomes a unit clause, it is removed.
     *
     * @param clause the clause to be simplified
     * @return  null, because a clause cannot become empty by this operations
     */
    Result simplifyClause(Clause clause) {
        if(clause.removed) {return null;}
        clause = urResolveClause(clause);
        if(clause == null) {return null;}
        if(clause.size() == 1) {
            addTrueLiteralTask(clause.getLiteral(0),clause.origins,"clause " + clause.id + " simplified");
            removeClause(clause);
            return null;}
        forwardSubsumption(clause);
        forwardReplacementResolution(clause);
        return null;}




    /** removes all clauses which are subsumed by the given clause.
     *
     * @param clause a clause
     */
    void forwardSubsumption(Clause clause) {
        subsumedClauses.clear(); //timestamp += maxClauseLength +1;
        LitAlgorithms.subsumes(clause,literalIndex,timestamp, subsumedClauses);
        timestamp += maxClauseLength +1;
        for(Clause subsumedClause : subsumedClauses) {
            ++statistics.forwardSubsumptions;
            if(monitoring) {
                monitor.print(problemId,"Clause \n  " + clause.toString(symboltable) +
                        "  subsumes \n  " + subsumedClause.toString(symboltable));}
            removeClause(subsumedClause);}
        if(checkConsistency) check("forwardSubsumption");}

    /** just used in simplifyForward */
    private ArrayList<CLiteral> resolvedLiterals = new ArrayList<>();

    /** simplifies other clauses by replacement resolution.
     *  The simplified clauses trigger new simplification tasks
     *
     * @param clause a clause which might trigger replacement resolution.
     */
    void forwardReplacementResolution(Clause clause) {
        resolvedLiterals.clear();
        LitAlgorithms.replacementResolutionForward(clause,literalIndex,timestamp, resolvedLiterals);
        timestamp += maxClauseLength +1;
        for(CLiteral cLiteral : resolvedLiterals) {
            ++statistics.forwardReplacementResolutions;
            Clause literalClause = cLiteral.clause;
            String clauseString = null;
            if(monitoring) {clauseString = literalClause.toString(symboltable);}
            removeLiteral(cLiteral,clause.origins);
            if(monitoring) {
                monitor.print(problemId,"\nLiteral " + cLiteral.toString(symboltable) + " in clause \n  " +
                        clauseString + " resolved away by clause\n  " +clause.toString(symboltable) + " to\n  " +
                        literalClause.toString());}
            analyseShortenedClause(literalClause,"clause " + clause.id + " simplified by UR-Resolution");
            if(checkConsistency) check("forwardReplacementResolution");}}

    void analyseShortenedClause(Clause clause, String reason) {
        if(clause.size() == 1) {
            addTrueLiteralTask(clause.getLiteral(0),clause.origins,reason);}
        else {taskQueue.add(new Task(priorityShift+clause.size(),
            () -> {forwardSubsumption(clause); forwardReplacementResolution(clause); return null;},
            () -> "simplify clause " + clause.toString(symboltable)));}}


    static IntArrayList joinOrigins(ArrayList<Clause> clauses, Clause clause) {
        IntArrayList origins = new IntArrayList();
        origins.addAll(clause.origins);
        for(Clause clause1 : clauses) {
            origins.addAll(clause1.origins);}
        return origins;}

    private Clause urResolveClause(Clause clause) {
        if(clause.removed) {return null;}
        Object result = LitAlgorithms.urResolution(clause,literalIndex,timestamp,maxClauseLength,usedClauses);
        timestamp += (maxClauseLength +1) * clause.size();
        if(result == null) {return null;}
        ++statistics.reductions;
        IntArrayList origins = joinOrigins(usedClauses,clause);
        if(result.getClass() == Integer.class) {
            int literal = (int)result;
            if(monitoring) {monitorUsedClauses("Derived unit literal " + literalName(literal) + " by UR-Resolution using clauses:");}
            processTrueLiteral(literal,origins); // this causes simplification of the clause
            return null;}

        if(result.getClass() == int[].class) {
            int[] literals = (int[])result;
            Clause resolvent = new Clause(++ids[0],literals.length);
            for(int literal : literals) {resolvent.add(new CLiteral(literal));}
            resolvent.setStructure();
            insertClause(resolvent);
            resolvent.origins = origins;
            if(monitoring) {monitorUsedClauses("Derived new clause\n   " + resolvent.toString(symboltable) + " by UR-Resolution using clauses:");}
            return resolvent;}

        CLiteral cliteral = (CLiteral)result;
        if(monitoring) {monitorUsedClauses("removing literal " + cliteral.toString(symboltable) + " from clause\n   " +
                clause.toString(symboltable) + " by UR-Resolution using clauses ");}
        removeLiteral(cliteral,origins);
        if(checkConsistency) {check("urResolveClause");}
         return clause;}

    private void monitorUsedClauses(String info) {
        StringBuilder st = new StringBuilder();
        st.append(info);
        ArrayList<Integer> ids = new ArrayList<>();
        for(Clause clause : usedClauses) {
            if(ids.contains(clause.id)) {continue;}
            ids.add(clause.id);
            st.append("\n   ").append(clause.toString(symboltable));}
        monitor.print(problemId,st.toString());}

    ArrayList<Clause> usedClauses = new ArrayList<>();





    /** lists the clauses and the literal index as a string.
     *
     * @param symboltable a symboltable or null
     * @return the clauses and the literal index as a string.
     */
    public String toString(Symboltable symboltable) {
        Function<Clause,String> clauseString = (clause->clause.toString(symboltable));
        Function<CLiteral,String> literalString = (cliteral->cliteral.toString(symboltable,clause->Integer.toString(clause.id)));
        StringBuilder st = new StringBuilder();
        st.append("Reduction:\n");
        if(!clauses.isEmpty()) {
            st.append("Clauses:\n").append(clauses.toString(clauseString)).append("\n");}
        if(model != null && !model.isEmpty()) {
            st.append("Model:\n").append(model.toString(symboltable)).append("\n\n");}
        st.append("Literal Index:\n").append(literalIndex.toString(literalString));
        if(!taskQueue.isEmpty()) {
            st.append("\nTask Queue:\n").append(taskQueue.toString());}
        if(!eliminatedLiterals.isEmpty()) {
            st.append("Eliminated Literals:\n");
            for(Object[] elms : eliminatedLiterals) {
                st.append("  "+elms[1].toString() + " from " + ((ArrayList<CLiteral>)elms[0]).toString()+"\n");}}
        if(!equivalenceClasses.isEmpty()) {st.append(equivalenceClasses.toString());}
        return st.toString();}

    /** turns the literal into a string, either via the symboltable, or directly
     *
     * @param literal the literal
     * @return its name
     */
    String literalName(int literal) {
        return (symboltable == null) ? Integer.toString(literal) :
                symboltable.getLiteralName(literal);}


    /** collects information about the control parameters
     *
     * @return a string with information about the control parameters.
     */
    public String parameters() {return "";}

    public void check(String info) {
        clauses.check(info + ": 'clauses'");
        literalIndex.check(info+":'literal index'");

        for(Clause clause : clauses) {
            for(CLiteral cLiteral : clause) {
                if(!literalIndex.contains(cLiteral)) {
                    System.out.println("Error: "+info+ " literal " + cLiteral.literal + " in clause "
                            + clause.toString() + " is not in the index.");
                    new Exception().printStackTrace();
                System.exit(1);}}}
    }
}
