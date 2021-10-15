package Datastructures.Clauses;

import Datastructures.Literals.CLiteral;
import Datastructures.Literals.LitAlgorithms;
import Datastructures.Results.Result;
import Datastructures.Results.Satisfiable;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Datastructures.Task;
import Datastructures.Theory.DisjointnessClasses;
import Datastructures.Theory.EquivalenceClasses;
import Datastructures.Theory.Model;
import Datastructures.TwoLiteral.TwoLitClause;
import Datastructures.TwoLiteral.TwoLitClauses;
import InferenceSteps.*;
import Management.Monitor;
import Management.ProblemSupervisor;
import Utilities.BucketSortedIndex;
import Utilities.BucketSortedList;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.concurrent.PriorityBlockingQueue;

import static Utilities.Utilities.isSubset;
import static Utilities.Utilities.multisetEquals;

public class AllClauses {

    /** supervises the problem solution */
    public final ProblemSupervisor problemSupervisor;
    private final String problemId;
    private final Thread thread;

    protected final Model model;
    public BasicClauseList     basicClauseList;
    public EquivalenceClasses  equivalenceClasses;
    public DisjointnessClasses disjointnessClasses;
    public TwoLitClauses       twoLitClauses;

    private final boolean monitoring;
    protected final Monitor monitor;
    protected final String monitorId;
    protected final boolean trackReasoning;

    private int maxClauseLength = 0;
    private final BucketSortedList<Clause> clauses;
    private final BucketSortedIndex<CLiteral> literalIndex;
    private final AllClausesStatistics statistics;
    private int timestamp = 1;
    private boolean clausesFinished;
    private final Symboltable symboltable;
    private final MRMatrices mrMatrices;

    private final ClauseType clauseType = ClauseType.OR;

    private enum TaskType {
        TRUELITERAL, EQUIVALENCE, DISJOINTNESS, INSERTCLAUSE, TWOLITCLAUSE, SIMPLIFYALL, SIMPLIFYOTHERS,
        MRESOLUTION
    }



    /** A queue of newly derived unit literals, newly derived binary equivalences and disjointness clauses
     * The unit literals are automatically put at the beginning of the queue.
     */
    private final PriorityBlockingQueue<Task<TaskType>> queue =
            new PriorityBlockingQueue<>(10, Comparator.comparingInt(this::getPriority));

    /** gets the priority for the objects in the queue.
     *
     * @param task the objects in the queue
     * @return the priority of the objects in the queue.
     */
    private int getPriority(Task<TaskType> task) {
        switch(task.taskType) {
            case TRUELITERAL:  return 0;
            case INSERTCLAUSE: return ((Clause)task.a).size();
            case EQUIVALENCE:  return 100;
            case DISJOINTNESS: return 101;
            case SIMPLIFYALL:  return 102;
            case SIMPLIFYOTHERS: return 103 + ((Clause)task.a).size();
            case MRESOLUTION: return 200;
            }
        return -1;}


    /** constructs a new AllClauses instance
     *
     * @param problemSupervisor    coordinates several solvers.
     */
    public AllClauses(ProblemSupervisor problemSupervisor) {
        this.problemSupervisor = problemSupervisor;
        problemSupervisor.allClauses = this;
        problemId = problemSupervisor.problemId;
        thread = Thread.currentThread();
        model = problemSupervisor.model;
        symboltable = model.symboltable;
        basicClauseList = problemSupervisor.basicClauseList;
        equivalenceClasses  = problemSupervisor.equivalenceClasses;
        disjointnessClasses = problemSupervisor.disjointnessClasses;
        twoLitClauses       = problemSupervisor.twoLitClauses;
        monitor = problemSupervisor.globalParameters.monitor;
        monitoring = monitor != null;
        monitorId = problemId+"AC";
        trackReasoning = problemSupervisor.globalParameters.trackReasoning;
        clauses      = new BucketSortedList<Clause>(Clause::size);
        literalIndex = new BucketSortedIndex<CLiteral>(model.predicates+1,
                (cLiteral->cLiteral.literal),
                (cLiteral->cLiteral.clause.size()));
        statistics = new AllClausesStatistics(problemId);
        mrMatrices = new MRMatrices((this));

        model.addObserver(thread,this::addTrueLiteral);
        equivalenceClasses.addObserver(this::addEquivalence);
        disjointnessClasses.addObserver(this::addDisjointness);
        twoLitClauses.addObserver(this::addTwoLitClause);
    }

    public void initialize() throws Result {
        initializeDisjoints();
        initializeXors();
        initializeDisjunctions();
        clausesFinished = true;
        twoLitClauses.addDisjointnessFinder();
        checkPurity();
    }

    /** This method puts the disjointness clauses into the disjointness classes
     */
    private void initializeDisjoints() {
        for(int[] clause : basicClauseList.disjoints) {
            disjointnessClasses.addDisjointnessClause(clause);
            integrateBasicDisjointnessClause(clause);}}

    /** This method puts the xor clauses into the disjointness classes and the clauses
     *
     * @throws Result if a contradiction occurs.
     */
    private void initializeXors() throws Result {
        for(int[] basicClause : basicClauseList.xors) {
            disjointnessClasses.addDisjointnessClause(basicClause);
            integrateBasicDisjointnessClause(basicClause);
            Clause clause = new Clause(basicClause[0],basicClause);
            if(trackReasoning) clause.inferenceStep = new ClauseCopy(basicClause,clause);
            integrateClause(clause);}}

    /** This method puts the disjunctions clauses into the clauses
     *
     * @throws Result if a contradiction occurs.
     */
    private void initializeDisjunctions() throws Result {
        for(int[] basicClause : basicClauseList.disjunctions) {
            Clause clause = new Clause(basicClause[0],basicClause);
            if(trackReasoning) clause.inferenceStep = new ClauseCopy(basicClause,clause);
            integrateClause(clause);}}


    /** works off the queue
     * new true literals <br>
     * new equivalences <br>
     * new disjointnesses <br>
     */
    public void run() {
        while(!Thread.interrupted()) {
            try {
                if(monitoring) {monitor.print(monitorId,"Queue is waiting:\n"+Task.queueToString(queue));}
                Task<TaskType> task = queue.take(); // waits if the queue is empty
                if(monitoring) System.out.println("Next Task: " + task.toString());
                switch (task.taskType) {
                    case TRUELITERAL:
                        integrateTrueLiteral((Integer)task.a);
                        break;
                    case EQUIVALENCE:
                        integrateEquivalence((Clause)task.a);
                        break;
                    case DISJOINTNESS:
                        integrateDerivedDisjointnessClause((Clause)task.a);
                        break;
                    case INSERTCLAUSE:
                        integrateClause((Clause)task.a);
                        break;
                    case TWOLITCLAUSE:
                        integrateTwoLitClause((TwoLitClause)task.a);
                        break;
                    case MRESOLUTION:
                        //multiResolution((Clause[]) task.a);
                        break;
                }
                if(clausesFinished && queue.isEmpty()) {
                    if(clauses.isEmpty()) throw new Satisfiable(model);
                    checkSatisfiabiliy();
                    checkPurity();}}
            catch(InterruptedException ex) {return;}
            catch(Result result) {problemSupervisor.setResult(result,"AllClauses"); return;}}}

    /** puts a true literal into the queue.
     *
     * @param literal a true literal
     * @param inference for making the literal true
     */
    public void addTrueLiteral(int literal, InferenceStep inference){
        if(monitoring) {
            monitor.print(monitorId,"In:   Unit literal " +
                    Symboltable.toString(literal,model.symboltable));}
        queue.add(new Task<>(AllClauses.TaskType.TRUELITERAL, literal, inference));}

    /** puts an equivalence into the queue
     *
     * @param eClause an equivalence clause
     */
    public void addEquivalence(Clause eClause) {
        if(monitoring) {
            monitor.print(monitorId,"In:   equivalence " + eClause.toString(0,model.symboltable));}
        queue.add(new Task<>(AllClauses.TaskType.EQUIVALENCE,eClause,null));}

    /** puts a disjointness into the queue
     *
     * @param dClause  a disjointness class
     */
    public void addDisjointness(Clause dClause) {
        if(monitoring) {
            monitor.print(monitorId,"In:   disjointness " +
                    dClause.toString(0,model.symboltable));}
        queue.add(new Task<>(TaskType.DISJOINTNESS, dClause, null));}

    private void addTwoLitClause(TwoLitClause clause) {
        if(monitoring) {
            monitor.print(monitorId,"In:   two-lit clause " +
                    clause.toString("",model.symboltable));}
        queue.add(new Task<>(TaskType.TWOLITCLAUSE, clause, null));}


    /** a not-integrated clause is simplified and integrated into the internal data structures.
     *  - equivalent literals are replaced <br>
     *  - true and false literals are taken care of <br>
     *  - double literals are removed <br>
     *  - tautologies are recognised <br>
     *  - a subsumed clause is deleted <br>
     *  - backward and forward replacement resolution is done <br>
     *  - an empty clause is thrown<br>
     *  - a unit clause is put into the model <br>
     *  - a two-literal clause is send to the Two-Lit module.
     *
     * @param clause  a new clause
     * @throws Result if a contradiction is found.
     */
    protected void integrateClause(Clause clause) throws Result {
        clause = equivalenceClasses.replaceEquivalences(clause);             // can be a new clause
        if((clause = replaceDoublesAndTautologies(clause)) == null) return;  // tautology, not needed any more
        if((clause = replaceTruthValues(clause)) == null) return;            // true clause, not needed any more

        switch(clause.size()) {
            case 0: throw new Unsatisfiable("Clause " + clause.id + " became empty", clause.inferenceStep);
            case 1: model.add(clause.getLiteral(0), clause.inferenceStep, null); // back to this
                    return;}

        if(isSubsumed(clause)) return;
        if((clause = replacementResolutionBackwards(clause)) == null) return;
        if(clause.size() == 2) twoLitClauses.addDerivedClause(clause);

        removeSubsumedClauses(clause);
        replacementResolutionForward(clause);
        insertClause(clause);
    }

    /** replaces all double literals and checks for tautology.
     *
     * @param clause an unintegrated clause
     * @return null (tautology) or the clause without double literals.
     */
    protected Clause replaceDoublesAndTautologies(Clause clause) {
        for(int i = 0; i < clause.size(); ++i) {
            int literal = clause.getLiteral(i);
            for(int j = i+1; j < clause.size(); ++j) {
                int otherLiteral = clause.getLiteral(j);
                if(literal == otherLiteral) {clause.removeAtPosition(j--); continue;}
                if(literal == -otherLiteral) {
                    if(monitoring) {
                        monitor.print(monitorId, "Clause " + clause.toString(0,symboltable) +
                                " is a tautology.");}
                    return null;}}}
        return clause;}

    /** a true literal causes the clause to be deleted, a false literal is removed.
     *
     * @param clause the original not integrated clause
     * @return null (true clause) or the unchanged clause or a new shortened clause
     */
    protected Clause replaceTruthValues(Clause clause) {
        Clause newClause = clause;
        for(int i = 0; i < clause.size(); ++i) {
            int literal = clause.getLiteral(i);
            switch(model.status(literal)) {
                case +1:
                    if(monitoring) {
                        monitor.print(monitorId, "Clause " + clause.toString(0,symboltable) +
                                " has a true literal " +
                                Symboltable.toString(literal,symboltable) + " and is deleted.");}
                    return null;
                case -1:
                    newClause = clause.clone(problemSupervisor.nextClauseId(),i);
                    if(trackReasoning) {
                        newClause.inferenceStep = new UnitResolution(clause,-literal,newClause,model.getInferenceStep(-literal));
                        if(monitoring) {monitor.print(monitorId,newClause.inferenceStep.toString(symboltable));}}
                    clause = newClause;
                    --i;}}
        return newClause;}

    /** performs all possible replacement resolutions on the given clause
     *
     * @param clause a not yet integrated clause
     * @return null, or the resolvent or the original clause
     * @throws Unsatisfiable if a contradiction is detected.
     */
    protected Clause replacementResolutionBackwards(Clause clause) throws Unsatisfiable {
        timestamp += maxClauseLength + 2;
        int size = clause.size();
        setTimestamps(clause,false);
        for(CLiteral cliteral : clause) {
            int literal = -cliteral.literal;
            BucketSortedList<CLiteral>.BucketIterator iterator = literalIndex.popIteratorTo(literal,size);
            try{
                while(iterator.hasNext()) {
                    Clause otherClause = iterator.next().clause;
                    int otherTimestamp = otherClause.timestamp;
                    if(otherTimestamp >= timestamp && otherTimestamp - timestamp == otherClause.size()-2) {
                        literalIndex.pushIterator(literal,iterator);
                        Clause resolvent = replacementResolveBackwards(clause,cliteral,otherClause,null);
                        timestamp += maxClauseLength + 2;
                        return resolvent == null ? null : replacementResolutionBackwards(resolvent);}}}
            finally {literalIndex.pushIterator(literal,iterator);}

            ArrayList<TwoLitClause> clauses2 = twoLitClauses.literalIndex.get(literal);
            if(clauses2 == null) continue;
            for(TwoLitClause clause2 : clauses2) {
                int literal2 = clause2.literal1 == literal ? clause2.literal2 : clause2.literal1;
                iterator = literalIndex.popIteratorTo(-literal2,size);
                try{
                    while(iterator.hasNext()) {
                        Clause otherClause = iterator.next().clause;
                        int otherTimestamp = otherClause.timestamp;
                        if(otherTimestamp >= timestamp && otherTimestamp - timestamp == otherClause.size()-2) {
                            Clause resolvent = replacementResolveBackwards(clause,cliteral,otherClause,clause2);
                            timestamp += maxClauseLength + 2;
                            return resolvent == null ? null : replacementResolutionBackwards(resolvent);}}}
                finally {literalIndex.pushIterator(literal,iterator);}}}
        timestamp += maxClauseLength + 2;
        return clause;}

    /** performs the replacement resolution
     *
     * @param clause    the clause to be shortened
     * @param cliteral  the literal of this clause
     * @param otherClause the other parent clause
     * @param clause2     null or an auxiliary two-literal clause
     * @return            null (unit clause) of the resolvent
     * @throws Unsatisfiable if the unit clause generated a contradiction
     */
    private Clause replacementResolveBackwards(Clause clause, CLiteral cliteral, Clause otherClause, TwoLitClause clause2) throws Unsatisfiable {
        ++statistics.replacementResolutionBackwards;
        Clause resolvent = clause.clone(problemSupervisor.nextClauseId(),cliteral.clausePosition);
        if(trackReasoning) {
            resolvent.inferenceStep = new ReplacementResolution(clause,otherClause, clause2, cliteral.literal,resolvent);
            if(monitoring) {monitor.print(monitorId,resolvent.inferenceStep.toString(symboltable));}}
        if(resolvent.size() == 1) {
            model.add(resolvent.getLiteral(0),resolvent.inferenceStep,null);
            return null;}
        return resolvent;}

    /** sets and increments the timestamps of the clauses with the same literals as in the clause
     *
     * @param clause a clause
     */
    private void setTimestamps(Clause clause, boolean from) {
        int size = clause.size();
        for(CLiteral cliteral : clause) {
            int literal = cliteral.literal;
            BucketSortedList<CLiteral>.BucketIterator iterator =
                     from ? literalIndex.popIteratorFrom(literal,size) :
                            literalIndex.popIteratorTo(literal,size);
            while(iterator.hasNext()) {
                CLiteral otherLiteral = iterator.next();
                Clause otherClause = otherLiteral.clause;
                if(clause == otherClause) {continue;}
                if(otherClause.timestamp < timestamp) {otherClause.timestamp = timestamp;}
                else {++otherClause.timestamp;}}
            literalIndex.pushIterator(literal,iterator);}}

    /** applies a true literal to all clauses.
     * Clauses containing the literal are removed.<br>
     * In clauses containing the negated literal, this literal is removed.<br>
     * For the resulting clause a INSERTCLAUSE Task is generated.
     *
     * @param literal a true literal
     * @throws Result if a contradiction is found.
     */
    private void integrateTrueLiteral(int literal) throws Result {
        // remove all clauses with the literal
        BucketSortedList<CLiteral>.BucketIterator iterator = literalIndex.popIterator(literal);
        while(iterator.hasNext()) {removeClause(iterator.next(),iterator,true);}
        literalIndex.pushIterator(literal,iterator);

        // remove all false literals
        iterator = literalIndex.popIterator(-literal);
        while(iterator.hasNext()) {
            CLiteral cliteral = removeClause(iterator.next(),iterator,false);
            Clause resolvent = cliteral.clause.clone(problemSupervisor.nextClauseId(),cliteral.clausePosition);
            ++statistics.unitResolutions;
            UnitResolution inf = null;
            if(trackReasoning) {
                inf = new UnitResolution(cliteral.clause, cliteral.literal,resolvent, model.getInferenceStep(literal));
                resolvent.inferenceStep = inf;
                if(monitoring) monitor.print(monitorId,inf.toString(symboltable));}
            if(resolvent.size() == 1) {
                ++statistics.derivedUnitClauses;
                model.add(resolvent.getLiteral(0),inf,null);}
            else queue.add(new Task<>(TaskType.INSERTCLAUSE,resolvent,null));}
        literalIndex.pushIterator(-literal,iterator);}

    /** generates for all clauses with the literal an INSERTCLAUSE task which does the replacements
     *
     * @param eClause       an equivalence clause
     */
    protected void integrateEquivalence(Clause eClause) throws Result {
        BucketSortedList<CLiteral>.BucketIterator iterator;
        for(int j = 1; j < eClause.size(); ++j) {
            int literal = eClause.getLiteral(j);
            for(int i = 1; i <= 2; ++i) {
                iterator = literalIndex.popIterator(literal);
                while(iterator.hasNext()) { // the replacements is done in insertClause
                    ++statistics.equivalenceReplacements;
                    queue.add(new Task<>(TaskType.INSERTCLAUSE,
                            removeClause(iterator.next(),iterator,false).clause,null));}
                literalIndex.pushIterator(literal,iterator);
                literal = -literal;}}}

    /** turns a disjointness class into the corresponding list of two-literal clauses.
     *
     * @param dClause a disjointness class.
     */
    protected void integrateDerivedDisjointnessClause(Clause dClause)  {
        if(dClause.isRemoved()) return;
        ArrayList<CLiteral> cliterals = dClause.cliterals;
        int size = cliterals.size();
        for(int i = 0; i < size; ++i) {
            int literal1 = -cliterals.get(i).literal;
            for(int j = i+1; j < size; ++j) {
                Clause clause = new Clause(problemSupervisor.nextClauseId(),clauseType,literal1,-cliterals.get(j).literal);
                if(trackReasoning) clause.inferenceStep = new DisjointnessClause2Clause(dClause,clause);
                queue.add(new Task<>(TaskType.INSERTCLAUSE, clause,null));}}}

    /** turns a basic disjointness clause into a list of two-literal clauses
     *
     * @param basicClause a basic disjointness clause.
     */
    protected void integrateBasicDisjointnessClause(int[] basicClause) {
        int size = basicClause.length;
        for(int i = 2; i < size; ++i) {
            int literal1 = -basicClause[i];
            for(int j = i+1; j < size; ++j) {
                Clause clause = new Clause(problemSupervisor.nextClauseId(),clauseType,literal1,-basicClause[j]);
                if(trackReasoning) clause.inferenceStep = new DisjointnessClause2Clause(basicClause,clause);
                queue.add(new Task<>(TaskType.INSERTCLAUSE, clause,null));}}}

    private final boolean[] keepClause = new boolean[1];

    /** forward subsumption and replacement resolution with a derived two-literal clause (usually a resolvent)
     * The new two-literal clause may subsume old clauses (the new clause must be kept in this case) <br>
     * It may also replacement resolve old clauses:  p,q and -p,q,... yields q,...<br>
     * Usually the new two-literal clause is not kept in AllClauses.
     *
     * @param clause a two-literal clause (usually a resolvent)
     */
    protected void integrateTwoLitClause(TwoLitClause clause) throws Result {
        int literal1 = clause.literal2;
        int literal2 = clause.literal1;
        keepClause[0] = false;

        for(int i = 1; i <= 2; ++i) {
            literalIndex.withIterator(literal1,iterator -> iterator.next().clause.timestamp = timestamp);

            if(i == 1) { // test for forward subsumption
                BucketSortedList<CLiteral>.BucketIterator iterator = literalIndex.popIterator(literal2);
                while(iterator.hasNext()) {
                    CLiteral cliteral = iterator.next();
                    Clause otherClause = cliteral.clause;
                    if(otherClause.timestamp == timestamp) {
                        if(otherClause.size() == 2) return; // backward subsumption
                        ++statistics.forwardSubsumptions;
                        keepClause[0] = true;  // because subsumed clause is removed.
                        if(monitoring) {monitor.print(monitorId,
                                "Clause " + otherClause.toString(0,symboltable) + " is subsumed by "
                        + clause.toString("",symboltable));}
                        removeClause(cliteral,iterator,false);}}
                literalIndex.pushIterator(literal2,iterator);}

            BucketSortedList<CLiteral>.BucketIterator iterator = literalIndex.popIterator(-literal2);
            while(iterator.hasNext()) {              // test for replacement resolution
                CLiteral cliteral = iterator.next();
                Clause otherClause = cliteral.clause;
                if(otherClause.timestamp == timestamp) {
                    removeClause(cliteral,iterator,false);
                    ++statistics.replacementResolutionForward;
                    Clause resolvent = otherClause.clone(problemSupervisor.nextClauseId(),cliteral.clausePosition);
                    ReplacementResolution step = null;
                    if(trackReasoning) {
                            step = new ReplacementResolution(clause,otherClause,cliteral.literal,resolvent);
                            resolvent.inferenceStep = step;
                            if(monitoring) {monitor.print(monitorId,step.toString());}}
                    if(resolvent.size() == 1) {
                        model.add(resolvent.getLiteral(0),step,null);}
                    else {synchronized(this) {queue.add(new Task(TaskType.INSERTCLAUSE, resolvent,null));}}
                }}
            literalIndex.pushIterator(-literal2,iterator);
            timestamp += 2;
            literal1 = clause.literal1;
            literal2 = clause.literal2;}

        replacementResolutionTwo(clause);

        if(keepClause[0]) {
            Clause newClause = new Clause(clause.id,clauseType,literal1,literal2);
            newClause.inferenceStep = clause.inferenceStep;
            insertClause(newClause);}}

    /** checks if the clause is subsumed by another clause
     *
     * @param clause a clause
     * @return true if the clause is subsumed
     */
    protected boolean isSubsumed(Clause clause) {
        timestamp += maxClauseLength + 2;
        Clause subsumer = LitAlgorithms.isSubsumed(clause,literalIndex,timestamp);
        timestamp += clause.size() + 2;
        if(subsumer != null) {
            ++statistics.forwardSubsumptions;
            if(monitoring)
                monitor.print(monitorId, "Clause " + clause.toString(0,model.symboltable) +
                        " is subsumed by clause " + subsumer.toString(0,model.symboltable));
            return true;}
        return false;}

    private final ArrayList<Clause> subsumedClauses = new ArrayList<>();

    /** removes all clauses subsumed by the given clause
     *
     * @param clause a clause
     */
    protected void removeSubsumedClauses(Clause clause){
        subsumedClauses.clear();
        timestamp += 2 + maxClauseLength;
        LitAlgorithms.subsumes(clause,literalIndex,timestamp,subsumedClauses);
        for(Clause subsumed : subsumedClauses) {
            ++statistics.backwardSubsumptions;
            if(monitoring)
                monitor.print(monitorId, "Clause " + subsumed.toString(0,model.symboltable) +
                        " is subsumed by clause " + clause.toString(0,model.symboltable));
            removeClause(subsumed);}
        timestamp += 2 + maxClauseLength;}



    private final ArrayList<Object[]> resolvents = new ArrayList<>();

    /** removes literals in other clauses by replacement resolution
     *
     * @param clause         a clause to be used for simplifying other clauses
     * @throws Unsatisfiable if a contradiction is found.
     */
    protected void replacementResolutionForward(Clause clause) throws Unsatisfiable {
        resolvents.clear();
        replacementResolutionForwardFind(clause);
        replacementResolutionResolveForward();}

    private void replacementResolutionResolveForward() throws Unsatisfiable{
        for(Object[] object : resolvents) {
            Clause clause = (Clause)object[0];
            CLiteral cliteral = (CLiteral)object[1];
            TwoLitClause twoClause = (TwoLitClause)object[2];
            ++statistics.replacementResolutionForward;
            Clause parentClause = cliteral.clause;
            Clause resolvent = parentClause.clone(problemSupervisor.nextClauseId(),cliteral.clausePosition);
            if(trackReasoning) {
                resolvent.inferenceStep = new ReplacementResolution(clause,parentClause,twoClause,cliteral.literal,resolvent);
                if(monitoring) {monitor.print(monitorId,resolvent.inferenceStep.toString(symboltable));}}
            removeClause(parentClause);
            if(resolvent.size() == 1) {
                model.add(resolvent.getLiteral(0),resolvent.inferenceStep,null);}
            else {queue.add(new Task<>(TaskType.INSERTCLAUSE,resolvent,null));}}}



    /** This method searches literals in other clauses to be removed by replacement resolution with the given clause.
     * The results are put into the list resolvents: [cLiteral,two-literal clause]
     *
     * @param clause        the clause to be checked
     */
    private void replacementResolutionForwardFind(Clause clause) {
        timestamp += maxClauseLength + 2;
        int size = clause.size();
        int difference = size-2;
        for(CLiteral cliteral : clause) { // all other clauses are timestamped
            int literal = cliteral.literal;
            BucketSortedList<CLiteral>.BucketIterator iterator = literalIndex.popIteratorFrom(literal,size);
            while(iterator.hasNext()) {
                CLiteral otherLiteral = iterator.next();
                Clause otherClause = otherLiteral.clause;
                if(clause == otherClause) {continue;}
                if(otherClause.timestamp < timestamp) {otherClause.timestamp = timestamp;}
                else {++otherClause.timestamp;}}
            literalIndex.pushIterator(literal,iterator);}

        // now we look for direct replacement resolvents
        // p,q,r  and -p,q,r,s -> q,r,s
        for(CLiteral cliteral : clause) {
            int literal = -cliteral.literal;
            BucketSortedList<CLiteral>.BucketIterator iterator = literalIndex.iteratorFrom(literal,size);
            while(iterator.hasNext()) {
                CLiteral otherLiteral = iterator.next();
                Clause otherClause = otherLiteral.clause;
                int otherTimestamp = otherClause.timestamp;
                if(otherTimestamp >= timestamp && otherTimestamp - timestamp == difference) {
                    resolvents.add(new Object[]{clause,otherLiteral,null});
                    otherClause.timestamp = 0;}}
            literalIndex.pushIterator(literal,iterator);

            // now we look for indirect replacement resolvents via two-literal clauses
            // p,q,r and -q,-s  and s,q,r,t -> q,r,t
            ArrayList<TwoLitClause> twoClauses = twoLitClauses.literalIndex.get(literal);
            if(twoClauses == null) continue;  // no available two-literal clauses
            for(TwoLitClause twoClause : twoClauses) {
                int literal2 = (literal == twoClause.literal1) ? twoClause.literal2 : twoClause.literal1;
                iterator = literalIndex.iteratorFrom(-literal2,size);
                while(iterator.hasNext()) {
                    CLiteral otherLiteral = iterator.next();
                    Clause otherClause = otherLiteral.clause;
                    int otherTimestamp = otherClause.timestamp;
                    if(otherTimestamp >= timestamp && otherTimestamp - timestamp == difference) {
                        resolvents.add(new Object[]{clause,otherLiteral,twoClause});
                        otherClause.timestamp = 0;}}
                literalIndex.pushIterator(literal,iterator);}}
        timestamp += maxClauseLength + 2;
    }


    /** performs indirect replacement resolution triggered by a newly derived two-literal clause
     * Example:<br>
     * p,q,r<br>
     * -p,-s<br>
     * s,q,r<br>
     * -----<br>
     * q,r
     *
     * @param twoClause a newly derived two-literal clause
     * @throws Unsatisfiable if a contradiction is encountered
     */
    protected void replacementResolutionTwo(TwoLitClause twoClause) throws Unsatisfiable {
        resolvents.clear();
        timestamp += maxClauseLength + 2;
        int twoLiteral1 = twoClause.literal1;
        int twoLiteral2 = twoClause.literal2;
        for(int i = 0; i <= 1; ++i) {
            BucketSortedList<CLiteral>.BucketIterator iterator1 = literalIndex.popIterator(-twoLiteral1);
            while(iterator1.hasNext()) {
                CLiteral cliteral = iterator1.next();
                Clause clause = cliteral.clause;
                setTimestamps(clause,true);
                int size = clause.size();
                int difference = size-2;
                BucketSortedList<CLiteral>.BucketIterator iterator2 = literalIndex.popIteratorFrom(-twoLiteral2,size+i);
                while(iterator2.hasNext()) {
                    CLiteral otherLiteral = iterator2.next();
                    Clause otherClause = otherLiteral.clause;
                    int otherTimestamp = otherClause.timestamp;
                    if(otherTimestamp >= timestamp && otherTimestamp - timestamp == difference) {
                     resolvents.add(new Object[]{clause,otherLiteral,twoClause});
                     otherClause.timestamp = 0;}}
                literalIndex.pushIterator(-twoLiteral2,iterator2);
                timestamp += maxClauseLength + 2;}
            literalIndex.pushIterator(-twoLiteral1,iterator1);
            twoLiteral1 = twoClause.literal2;
            twoLiteral2 = twoClause.literal1;}
        replacementResolutionResolveForward();
        timestamp += maxClauseLength + 2;
    }

    private ArrayList<Clause[]>     mrDClauses      = new ArrayList<>();
    private ArrayList<Clause>       mrClauses       = new ArrayList<>();
    private ArrayList<TwoLitClause> mrTwoLitClauses = new ArrayList<>();

    /** performs all multi-resolutions involving the given disjointness clause
     *
     * @param dClause       a disjointness clause
     * @throws Unsatisfiable if a contradiction is discovered
     */
    private void multiResolution(Clause dClause) throws Unsatisfiable {
        mrDClauses.clear();
        mrTwoLitClauses.clear();
        linkDisjointnesses(dClause, mrDClauses);
        for(Clause[] dClauses : mrDClauses) {
            mrClauses.clear();
            collectClauses(dClauses,mrClauses);
            if(!mrClauses.isEmpty()) {mrMatrices.mrResolve(dClauses,mrClauses,mrTwoLitClauses);}}
        for(TwoLitClause twoClause : mrTwoLitClauses) {twoLitClauses.addDerivedClause(twoClause);}}

    /** collects for a combination of disjointness clauses those clauses which may become part of a mult-resolution
     *
     * @param dClauses a combination of disjointness clauses
     * @param clauses for collecting the partners for a multi-resolution
     */
    private void collectClauses(Clause[] dClauses, ArrayList<Clause> clauses) {
        int minSize = Integer.MAX_VALUE;
        for(Clause dClause : dClauses) minSize = Math.min(minSize,dClause.size());
        int dLength = dClauses.length;
        for(Clause dClause : dClauses) {
            for(CLiteral dLiteral : dClause) {
                BucketSortedList<CLiteral>.BucketIterator iterator = literalIndex.popIteratorFrom(dLiteral.literal,dLength);
                while(iterator.hasNext()) {
                    Clause clause = iterator.next().clause;
                    if(clause.size() <= dLength+1 &&!clauses.contains(clause)) clauses.add(clause);}
                literalIndex.pushIterator(dLiteral.literal,iterator);}}
        if(clauses.size() < minSize) clauses.clear();}


    /** adds the disjointenss clause to the clause's aux attribute
     *
     * @param dClause    a disjointness clause
     * @param mrDClauses for collecting the full disjointness combinations (no null's in the array)
     */
    private void linkDisjointnesses(Clause dClause, ArrayList<Clause[]> mrDClauses) {
        int dSize = dClause.size();
        for(CLiteral dliteral : dClause.cliterals) {
            BucketSortedList<CLiteral>.BucketIterator iterator = literalIndex.popIteratorTo(dliteral.literal,dSize);
            while(iterator.hasNext()) {addDClause(iterator.next(),dClause,mrDClauses); }
            literalIndex.pushIterator(dliteral.literal,iterator);}}

    /** adds a disjointness clause to the clause's aux-attribute.
     * Example: a clause:
     * 1,  2,   3 may be linked to disjointness clauses: <br>
     * D1, D2, D3  (eq. D1 = 1,5,6, D2 = 2,7,8 and D3 = 3,9,10,11)  <br>
     * D4,null,D5 <br>
     * A new disjointness clause D6 with literal 2 is now added as follows:<br>
     *
     * 1,  2,   3 <br>
     * D1, D2, D3  <br>
     * D4, D6, D5 <br>
     * D1, D6, D3  <br>
     *
     * @param cLiteral   a literal of a normal clause
     * @param dClause    a disjointness clause with the same literal
     * @param mrDClauses for collecting the full disjointness combinations (no null's in the array)
     */
    private void addDClause(CLiteral cLiteral, Clause dClause, ArrayList<Clause[]> mrDClauses) {
        Clause clause = cLiteral.clause;
        int position = cLiteral.clausePosition;
        ArrayList<Clause[]> dClauses = (ArrayList<Clause[]>)clause.aux;
        if(dClauses == null) {dClauses = new ArrayList<>(); clause.aux = dClauses;}
        if(dClauses.isEmpty()) {
            Clause[] dcl = new Clause[clause.size()];
            dClauses.add(dcl);
            dcl[position] = dClause;
            return;}
        int size = dClauses.size();
        for(int i = 0; i < size; ++i) {
            Clause[] dcl = dClauses.get(i);
            if(dcl[position] == null) {dcl[position] = dClause;}
            else {dcl = Arrays.copyOf(dcl,dcl.length);
                  dcl[position] = dClause;
                  dClauses.add(dcl);}
            if(isFull(dcl)) addFullDClause(mrDClauses,dcl);}}

    /** adds a full combination of disjointness clauses to the list.
     *  However, it checks first if there is already a multiset equal list.
     *
     * @param mrDClauses a list of full combinations of disjointness clauses
     * @param dClauses   a new full combination of disjointness clauses
     */
    private void addFullDClause(ArrayList<Clause[]> mrDClauses, Clause[] dClauses) {
        for(Clause[] dcls : mrDClauses) {
            if(multisetEquals(dcls,dClauses)) return;}
        mrDClauses.add(dClauses);}

    /** checks if there are no nulls anymore in the array
     *
     * @param dClauses an array of Disjointness clauses
     * @return true if there are no nulls anymore in the array
     */
    private boolean isFull(Clause[] dClauses) {
        for(Clause dClause : dClauses) {if(dClause == null) return false;}
        return true;}



    /** checks if the clause is a unit clause.
     * In this case the clause is put into the model.
     *
     * @param clause a clause
     * @return true if the clause is a unit clause
     * @throws Unsatisfiable if a contradiction is found
     */
    private boolean checkUnitClause(Clause clause) throws Unsatisfiable {
        if(clause.size() == 1) {
            model.add(clause.getLiteral(0), clause.inferenceStep, null); // back to this
            return true;}
        return false;}

    /** checks if the literal is pure (there are no further occurrences).
     * If literal is pure then -literal can be made true.
     *
     * @param literal        a literal to be checked
     * @throws Result if a contradiction is found.
     */
    private void checkPurity(int literal) throws Result {
        assert clausesFinished;
        if(literalIndex.isEmpty(literal)) {
            if(monitoring) {
                monitor.print(monitorId,"Literal " + Symboltable.toString(literal,model.symboltable) +
                        " became pure");}
            ++statistics.purities;
            model.add(-literal,null,null);}}

    /** performs purity checks for all literals in the clause
     *
     * @param clause  a just removed clause.
     * @throws Result may throw Satisfiable
     */
    private void checkPurity(Clause clause) throws Result {
        assert clausesFinished;
        for(CLiteral cliteral : clause.cliterals) checkPurity(cliteral.literal);}

    /** checks all literals for purity.
     *
     * @throws Result may throw Satisfiable
     */
    private void checkPurity() throws Result {
        assert clausesFinished;
        for(int literal = 1; literal <= model.predicates; ++literal) {
            if(model.status(literal) == 0) {
                checkPurity(literal);
                checkPurity(-literal);}}}

    /** Checks the clause set for satisfiability.
     * If the clauses contain no positive clauses, a model is generated from the negative and mixed clauses.<br>
     * If the clauses contain no negative clauses, a model is generated from the positive and mixed clauses.<br>
     * In this case the model will be completed as far as necessary, and the clause set becomes empty.
     *
     * @throws Result should be just Satisfiable
     */
    private void checkSatisfiabiliy() throws Result {
        assert clausesFinished;
        if(statistics.negativeClauses == 0) {
            while(!clauses.isEmpty()) makeLiteralTrue(+1);
            throw new Satisfiable(model);}

        if(statistics.positiveClauses == 0) {
            while(!clauses.isEmpty()) {
                makeLiteralTrue(-1);}
            throw new Satisfiable(model);}}

    /** finds the first literal which can be made true:
     * sign = +1: the first positive literal<br>
     * sign = -1: the first negative literal. <br>
     * The literal is made true.
     *
     * @param sign   +1 or -1
     * @throws Result should not happen
     */
    private void makeLiteralTrue(int sign) throws Result {
        int literal = 0;
        for(Clause clause : clauses) {
            for(CLiteral cliteral : clause.cliterals) {
                if(sign*cliteral.literal > 0) {literal = cliteral.literal; break;}}
            if(literal != 0) break;}
        if(monitoring) {
            monitor.print(monitorId, "Making literal " +
                    Symboltable.toString(literal,model.symboltable) +
                    " true because clauses contain only positive/negative and mixed clauses." );}
        model.add(literal,null,thread);
        integrateTrueLiteral(literal);}

    /** inserts the clause into the local data structures.
     *
     * @param clause  the clause to be inserted.
     */
    protected void insertClause(Clause clause) {
        ++statistics.clauses;
        maxClauseLength = Math.max(maxClauseLength,clause.size());
        clause.setStructure();
        clauses.add(clause);
        for(CLiteral cliteral : clause) {literalIndex.add(cliteral);}
        switch(clause.structure) {
            case NEGATIVE: ++statistics.negativeClauses; break;
            case POSITIVE: ++statistics.positiveClauses; break;}}

    /** removes the clause and does purity checks
     *
     * @param clause a clause to be removed.
     */
    private void removeClause(Clause clause) {
        clause.removed = true;
        switch(clause.structure) {
            case NEGATIVE: --statistics.negativeClauses; break;
            case POSITIVE: --statistics.positiveClauses; break;}
        --statistics.clauses;
        for(CLiteral cliteral : clause) {literalIndex.remove(cliteral);}
        clauses.remove(clause);}


    /** removes the iterator's next clause from the index and the clauses
     *
     * @param cliteral iterator's next cliteral;
     * @param iterator an iterator over the literal index.
     * @param really if true then the clause will not be used any more
     * @return cliteral;
     */
    private CLiteral removeClause(CLiteral cliteral,BucketSortedList<CLiteral>.BucketIterator iterator, boolean really)  {
        Clause clause = cliteral.clause;
        switch(clause.structure) {
            case NEGATIVE: --statistics.negativeClauses; break;
            case POSITIVE: --statistics.positiveClauses; break;}
        --statistics.clauses;
        iterator.remove();
        for(CLiteral clit : clause) if(clit != cliteral) literalIndex.remove(clit);
        clauses.remove(clause);
        return cliteral;}

    /** removes the cliteral and performs a purity check
     *
     * @param cliteral the literal to be removed
     * @param origins the basic clause ids for removing the literal.
     * @return true if the clause survived
     */
    private boolean removeLiteral(CLiteral cliteral, IntArrayList origins) throws Result {
        Clause clause = cliteral.clause;
        switch(clause.structure) {
            case NEGATIVE: --statistics.negativeClauses; break;
            case POSITIVE: --statistics.positiveClauses; break;}
        --statistics.clauses;
        clause.remove(cliteral);
        clause.setStructure();
        literalIndex.remove(cliteral);
        boolean alive = updateClause(clause,origins);
        if(clausesFinished) checkPurity(cliteral.literal);
        return alive;}


    /** removes the iterator's next literal and performs a purity check
     *
     * @param iterator an iterator over the literal index.
     * @param origins the basic clause ids for removing the literal.
     * @return true if the clause survived
     */
    private boolean removeLiteral(BucketSortedList<CLiteral>.BucketIterator iterator, IntArrayList origins) throws Result {
        CLiteral cliteral = iterator.next();
        Clause clause = cliteral.clause;
        switch(clause.structure) {
            case NEGATIVE: --statistics.negativeClauses; break;
            case POSITIVE: --statistics.positiveClauses; break;}
        --statistics.clauses;
        clause.remove(cliteral);
        clause.setStructure();
        iterator.remove();
        boolean alive = updateClause(clause,origins);
        if(clausesFinished) checkPurity(cliteral.literal);
        return alive;}

    /** updates the status of a clause after literal removal
     *
     * @param clause   a clause whose literal has been removed
     * @param origins basic clause ids for the literal removal
     * @return true if the clause survived
     * @throws Result if a contradiction is found, Satisfiable if the clause set became empty.
     */
    private boolean updateClause(Clause clause, IntArrayList origins) throws Result{
        switch(clause.size()) {
            case 1:
                if(monitoring) {
                    monitor.print(monitorId,"Clause " + clause.toString(0, model.symboltable) +
                            " became a unit clause");}
                model.add(clause.getLiteral(0), null,null);
                       // trackReasoning ? joinIntArrays(clause.origins,origins) : null,null);
                literalIndex.remove(clause.getCLiteral(0));
                clauses.remove(clause);
                if(clausesFinished && clauses.isEmpty()) throw new Satisfiable(model);
                return false;
            case 2: twoLitClauses.addDerivedClause(clause);} // keep the two-literal clause

        switch(clause.structure) {
            case NEGATIVE: ++statistics.negativeClauses; break;
            case POSITIVE: ++statistics.positiveClauses; break;}
        ++statistics.clauses;
        return true;}

    /** checks if the clauses are empty.
     *
     * @return true if there are no clauses
     */
    public boolean isEmpty() {
        return clauses.isEmpty();}

    /** Lists all clauses as a string
     *
     * @return the clauses as string
     */
    public String toString() {
        return toString(model.symboltable);}


    /** Lists all clauses as a string of nombers
     *
     * @return the clauses as string
     */
    public String toNumbers() {
        return toString(null);}


    /** Lists all clauses as a string
     *
     * @param symboltable null or a symboltable
     * @return the clauses as string
     */
    public String toString(Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        st.append("All Clauses of Problem " + problemId+":");
        int size = Integer.toString(problemSupervisor.clauseCounter).length()+2;
        for(Clause clause : clauses) {
            st.append("\n").append(clause.toString(size,symboltable));}
        return st.toString();}

    /** lists all clauses and the literal index as string
     *
     * @param symboltable null or a symboltable
     * @return the info as string.
     */
    public String infoString(Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        st.append("All Clauses of Problem "+problemId+":\n");
        int size = Integer.toString(problemSupervisor.clauseCounter).length()+2;
        for(Clause clause : clauses) {
            st.append(clause.infoString(size,symboltable)).append("\n");}
        st.append(literalIndex.toString(cliteral -> cliteral.toString(symboltable)+"@"+cliteral.clause.id));
        if(!queue.isEmpty()) {
            st.append("All Clauses Queue of Problem "+problemId+":").append(Task.queueToString(queue));}
        return st.toString();}

}