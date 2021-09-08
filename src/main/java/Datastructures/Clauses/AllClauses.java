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
import Management.Monitor;
import Management.ProblemSupervisor;
import Utilities.BucketSortedIndex;
import Utilities.BucketSortedList;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.concurrent.PriorityBlockingQueue;

import static Utilities.Utilities.joinIntArrays;

public class AllClauses {

    /** supervises the problem solution */
    private final ProblemSupervisor problemSupervisor;
    private final String problemId;
    private final Thread thread;

    private final Model model;
    private final BasicClauseList     basicClauseList;
    private final EquivalenceClasses  equivalenceClasses;
    private final DisjointnessClasses disjointnessClasses;
    private final TwoLitClauses       twoLitClauses;

    private final boolean monitoring;
    private final Monitor monitor;
    private final String monitorId;
    private final boolean trackReasoning;

    private int counter = 0;
    private int maxClauseLength = 0;
    private final BucketSortedList<Clause> clauses;
    private final BucketSortedIndex<CLiteral> literalIndex;
    private final AllClausesStatistics statistics;
    private int timestamp = 1;
    private final boolean clausesFinished;

    private final ClauseType clauseType = ClauseType.OR;

    private enum TaskType {
        TRUELITERAL, EQUIVALENCE, DISJOINTNESS, INSERTCLAUSE, TWOLITCLAUSE, SIMPLIFYALL, SIMPLIFYOTHERS,
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
            case EQUIVALENCE:  return 1;
            case DISJOINTNESS: return 2;
            case SIMPLIFYALL:  return 3;
            case SIMPLIFYOTHERS: return 2 + ((Clause)task.a).size();
            }
        return -1;}


    /** constructs a new AllClauses instance
     *
     * @param problemSupervisor    coordinates several solvers.
     */
    public AllClauses(ProblemSupervisor problemSupervisor) throws Result {
        this.problemSupervisor = problemSupervisor;
        problemSupervisor.allClauses = this;
        problemId = problemSupervisor.problemId;
        thread = Thread.currentThread();
        model = problemSupervisor.model;
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

        model.addObserver(thread,this::addTrueLiteral);
        equivalenceClasses.addObserver(this::addEquivalence);
        disjointnessClasses.addObserver(this::addDisjointness);
        twoLitClauses.addObserver(this::addTwoLitClause);

        initializeDisjoints();
        initializeXors();
        initializeDisjunctions();
        clausesFinished = true;
        checkPurity();
    }

    /** This method puts the disjointness clauses into the disjointness classes
     */
    private void initializeDisjoints() {
        for(int[] clause : basicClauseList.disjoints) {
            disjointnessClasses.addDisjointnessClause(clause);
            integrateDisjointnessClause(clause);}}

    /** This method puts the xor clauses into the disjointness classes and the clauses
     *
     * @throws Result if a contradiction occurs.
     */
    private void initializeXors() throws Result {
        for(int[] clause : basicClauseList.xors) {
            disjointnessClasses.addDisjointnessClause(clause);
            integrateDisjointnessClause(clause);
            integrateDisjunction(clause);}}

    /** This method puts the disjunctions clauses into the clauses
     *
     * @throws Result if a contradiction occurs.
     */
    private void initializeDisjunctions() throws Result {
        for(int[] clause : basicClauseList.disjunctions) {
            integrateDisjunction(clause);}}


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
                switch (task.taskType) {
                    case TRUELITERAL:
                        integrateTrueLiteral((Integer)task.a,task.origins);
                        break;
                    case EQUIVALENCE:
                        integrateEquivalence((Integer)task.a);
                        break;
                    case DISJOINTNESS:
                        integrateDisjointnessClass((Clause)task.a);
                        break;
                    case INSERTCLAUSE:
                        integrateClause((Clause)task.a);
                        break;
                    case TWOLITCLAUSE:
                        integrateTwoLitClause((TwoLitClause)task.a);
                        break;
                }}
            catch(InterruptedException ex) {return;}
            catch(Result result) {problemSupervisor.setResult(result,"AllClauses"); return;}}}

    /** puts a true literal into the queue.
     *
     * @param literal a true literal
     * @param origins the basic clause ids causing the derivation of the true literal.
     */
    public void addTrueLiteral(int literal, IntArrayList origins){
        if(monitoring) {
            monitor.print(monitorId,"In:   Unit literal " +
                    Symboltable.toString(literal,model.symboltable));}
        queue.add(new Task<>(AllClauses.TaskType.TRUELITERAL, origins, literal, null));}

    /** puts an equivalence into the queue
     *
     * @param clause an equivalence clause
     */
    public void addEquivalence(Clause clause) {
        if(monitoring) {
            monitor.print(monitorId,"In:   equivalence " + clause.toString(0,model.symboltable));}
        queue.add(new Task<>(AllClauses.TaskType.EQUIVALENCE,null, clause,null));}

    /** puts a disjointness into the queue
     *
     * @param disjoints  a disjointness class
     */
    public void addDisjointness(Clause disjoints) {
        if(monitoring) {
            monitor.print(monitorId,"In:   disjointness " +
                    disjoints.toString(0,model.symboltable));}
        queue.add(new Task<>(TaskType.DISJOINTNESS, null, disjoints, null));}

    private void addTwoLitClause(TwoLitClause clause) {
        if(monitoring) {
            monitor.print(monitorId,"In:   two-lit clause " +
                    clause.toString("",model.symboltable));}
        queue.add(new Task<>(TaskType.TWOLITCLAUSE, null, clause, null));}


    /** a not-integrated clause is simplified and integrated into the internal data structures.
     *  - equivalent literals are replaced <br>
     *  - true and false literals are taken care of <br>
     *  - double literals are removed <br>
     *  - tautologies are recognised <br>
     *  - a subsumed clause is deletec <br>
     *  - backward and froward replacement resolution is done <br>
     *  - an empty clause is thrown<br>
     *  - a unit clause is put into the model <br>
     *  - a two-literal clause is send to the Two-Lit module.
     *
     * @param clause  a new clause
     * @throws Result if a contradiction is found.
     */
    private void integrateClause(Clause clause) throws Result {
        IntArrayList origins = clause.origins;
        ArrayList<CLiteral> cliterals = clause.cliterals;

        for(int i = 0; i < cliterals.size(); ++i) {
            CLiteral cliteral = cliterals.get(i);
            int oldLiteral = cliteral.literal;

            int literal = equivalenceClasses.getRepresentative(oldLiteral); // take care of equivalences
            if(trackReasoning && literal != oldLiteral) {
                origins = joinIntArrays(origins,equivalenceClasses.getOrigins(oldLiteral));}

            switch(model.status(literal)) {                                 // take care of the model
                case +1: return; // true clause
                case -1:         // false literal
                    if(trackReasoning) origins = joinIntArrays(origins,model.getOrigin(literal));
                    cliterals.remove(i--);
                    continue;}

            boolean doubel = false;                                        // take care of doubles and tautologies
            for(int j = 0; j < i; ++j) {
                int lit = cliterals.get(j).literal;
                if(lit == literal) {doubel = true; break;}
                if(lit == -literal) return;} // tautology
            if(doubel) {cliterals.remove(i--); continue;}

            cliteral.literal = literal;} // maybe changed

        switch(clause.size()) {
            case 0:
                throw new Unsatisfiable("Clause " + clause.id + " became empty", origins);
            case 1:
                if (monitoring) {
                    monitor.print(monitorId, "Clause " + clause.toString(0, model.symboltable) +
                            " became a unit clause");}
                model.add(clause.getLiteral(0), origins, null); // back to this
                return;}

        if(isSubsumed(clause)) return;

        Object[] result = LitAlgorithms.replacementResolutionBackwards(clause,literalIndex,timestamp);
        timestamp += 2;
        if(result != null) {
            CLiteral cliteral = (CLiteral) result[0];
            Clause otherClause = (Clause) result[1];
            ++statistics.forwardReplacementResolutions;
            if(trackReasoning) origins = joinIntArrays(origins,otherClause.origins);
            if(monitoring) {
                monitor.print(monitorId, "Literal " + cliteral.toString(model.symboltable) +
                        " in  clause \n" + cliteral.clause.toString(3,model.symboltable) +
                        " will be removed by replacement resolution with clause\n" +
                        otherClause.toString(3,model.symboltable));}
            clause.remove(cliteral);
            clause.origins = origins;
            if(checkUnitClause(clause)) return;}

        clause.origins = origins;

        if(clause.size() == 2)
            twoLitClauses.addDerivedClause(clause.getLiteral(0), clause.getLiteral(1), origins);

        simplifyOtherClauses(clause);
        insertClause(clause);
    }

    /** turns a single basic clause into a Clause datastructure.
     * The clause is simplified in integrateClause
     *
     * @param basicClause [id,type,lit1,...]
     * @throws Result if the clause is empty, otherwise null
     */
    private void integrateDisjunction(int[] basicClause) throws Result {
        Clause clause = new Clause(++counter,basicClause);
        clause.origins = trackReasoning ? IntArrayList.wrap(new int[]{basicClause[0]}) : null;
        integrateClause(clause);}


    /** applies a true literal to all clauses.
     * Clauses containing the literal are removed.<br>
     * In clauses containing the negated literal, this literal is removed.<br>
     * For the resulting clause a INSERTCLAUSE Task is generated.
     *
     * @param literal a true literal
     * @param origins the basic clause ids for the truth of the literal
     * @throws Result if a contradiction is found.
     */
    private void integrateTrueLiteral(int literal, IntArrayList origins) throws Result {
        literalIndex.withIterator(literal,iterator -> removeClause(iterator.next(),iterator)); // remove all clauses with the literal

        BucketSortedList<CLiteral>.BucketIterator iterator = literalIndex.popIterator(-literal);
        while(iterator.hasNext()) {
            CLiteral cliteral = removeClause(iterator.next(),iterator);
            Clause clause = cliteral.clause;
            clause.remove(cliteral);
            if(trackReasoning) clause.origins = joinIntArrays(clause.origins,origins);
            if(checkUnitClause(clause)) continue;
            queue.add(new Task<>(TaskType.INSERTCLAUSE,null,clause,null));}
        literalIndex.pushIterator(-literal,iterator);}

    /** generates for all clauses with the literal an INSERTCLAUSE task which does the replacements
     *
     * @param literal       a literal equal to some representative
     */
    private void integrateEquivalence(int literal) {
        for(int i = 1; i <= 2; ++i) {
            literalIndex.withIterator(literal,iterator -> { // the replacements is done in insertClause
                queue.add(new Task<>(TaskType.INSERTCLAUSE,null,
                        removeClause(iterator.next(),iterator).clause,null));});
            literal = -literal;}}

    /** turns a disjointness class into the corresponding list of two-literal clauses.
     *
     * @param disjoints a disjointness class.
     */
    private void integrateDisjointnessClass(Clause disjoints)  {
        IntArrayList origins  = disjoints.origins;
        ArrayList<CLiteral> cliterals = disjoints.cliterals;
        int size = cliterals.size();
        for(int i = 0; i < size; ++i) {
            int literal1 = -cliterals.get(i).literal;
            for(int j = i+1; j < size; ++j) {
                Clause clause = new Clause(++counter,clauseType,literal1,-cliterals.get(j).literal,origins);
                queue.add(new Task<>(TaskType.INSERTCLAUSE,null, clause,null));}}}

    /** turns a disjointness clause into a list of two-literal clauses
     *
     * @param basicClause a basic disjointness clause.
     */
    private void integrateDisjointnessClause(int[] basicClause) {
        IntArrayList origins  = trackReasoning ? IntArrayList.wrap(new int[]{basicClause[0]}) : null;
        int size = basicClause.length;
        for(int i = 2; i < size; ++i) {
            int literal1 = -basicClause[i];
            for(int j = i+1; j < size; ++j) {
                Clause clause = new Clause(++counter,clauseType,literal1,-basicClause[j],origins);
                queue.add(new Task<>(TaskType.INSERTCLAUSE,null, clause,null));}}}

    private final boolean[] keepClause = new boolean[1];

    /** forward subsumption and replacement resolution with a derived two-literal clause (usually a resolvent)
     * The new two-literal clause may subsume old clauses (the new clause must be kept in this case) <br>
     * It may also replacement resolve old clauses:  p,q and -p,q,... yields q,...<br>
     * Usually the new two-literal clause is not kept in AllClauses.
     *
     * @param clause a two-literal clause (usually a resolvent)
     */
    private void integrateTwoLitClause(TwoLitClause clause) throws Unsatisfiable {
        int literal1 = clause.literal2;
        int literal2 = clause.literal1;
        keepClause[0] = false;

        for(int i = 1; i <= 2; ++i) {
            literalIndex.withIterator(literal1,iterator -> iterator.next().clause.timestamp = timestamp);

            if(i == 1) { // test for forward subsumption
                literalIndex.withIterator(literal2, iterator -> {
                    CLiteral cliteral = iterator.next();
                    if(cliteral.clause.timestamp == timestamp) { // forward subsumption
                        ++statistics.forwardSubsumptions;
                        keepClause[0] = true;  // because subsumed clause is removed.
                        removeClause(cliteral,iterator);}});}

            BucketSortedList<CLiteral>.BucketIterator iterator = literalIndex.popIterator(-literal2);
            while(iterator.hasNext()) {              // test for replacement resolution
                CLiteral cliteral = iterator.next();
                Clause otherClause = cliteral.clause;
                if(otherClause.timestamp == timestamp) {
                    if(monitoring) {
                        monitor.print(monitorId,"replacement resolution between clause " +
                                clause.toString("",model.symboltable) + " and " +
                                otherClause.toString(0,model.symboltable));}
                    removeClause(cliteral,iterator);
                    otherClause.remove(cliteral);
                    ++statistics.forwardReplacementResolutions;
                    if(trackReasoning) otherClause.origins = joinIntArrays(clause.origins,otherClause.origins);
                    if(!checkUnitClause(otherClause))
                        queue.add(new Task<>(TaskType.INSERTCLAUSE,null,otherClause,null));}}
            literalIndex.pushIterator(-literal2,iterator);

            timestamp += 2;
            literal1 = clause.literal1;
            literal2 = clause.literal2;}

        if(keepClause[0]) {
            Clause newClause = new Clause(++counter,clauseType,2);
            newClause.add(new CLiteral(literal1,newClause,0));
            newClause.add(new CLiteral(literal2,newClause,1));
            newClause.origins = clause.origins;
            insertClause(newClause);}
    }

    /** checks if the clause is subsumed by another clause
     *
     * @param clause a clause
     * @return true if the clause is subsumed
     */
    private boolean isSubsumed(Clause clause) {
        Clause subsumer = LitAlgorithms.isSubsumed(clause,literalIndex,timestamp);
        timestamp += clause.size() + 2;
        if(subsumer != null) {
            ++statistics.forwardSubsumptions;
            if(monitoring)
                monitor.print(monitorId, "Clause\n" + clause.toString(4,model.symboltable) +
                        " is subsumed by clause\n" + subsumer.toString(4,model.symboltable));
            return true;}
        return false;}

    private final ArrayList<Clause> subsumedClauses = new ArrayList<>();

    /** removes all clauses subsumed by the given clause
     *
     * @param clause a clause
     * @throws Result if a contradiction is found
     */
    private void removeSubsumedClauses(Clause clause) throws Result{
        subsumedClauses.clear();
        LitAlgorithms.subsumes(clause,literalIndex,timestamp,subsumedClauses);
        for(Clause subsumed : subsumedClauses) {
            ++statistics.backwardSubsumptions;
            if(monitoring)
                monitor.print(monitorId, "Clause\n" + subsumed.toString(4,model.symboltable) +
                        " is subsumed by clause\n" + clause.toString(4,model.symboltable));
            removeClause(clause);}
        timestamp += 2 + maxClauseLength;
    }



    private final ArrayList<CLiteral> resolvents = new ArrayList<>();

    /** removes subsumed clauses and literals in other clauses by replacement resolution
     *
     * @param clause         a clause to be used for simplifying other clauses
     * @throws Result if a contradiction is found.
     */
    private void simplifyOtherClauses(Clause clause) throws Result {
        removeSubsumedClauses(clause);
        resolvents.clear();
        LitAlgorithms.replacementResolutionForward(clause,literalIndex,timestamp,resolvents);
        timestamp += 2 + maxClauseLength;
        for(CLiteral cliteral : resolvents) {
            ++statistics.forwardReplacementResolutions;
            Clause resolvent = cliteral.clause;
            if(monitoring) {
                monitor.print(monitorId, "Literal " + cliteral.toString(model.symboltable) +
                        " in  clause \n" + resolvent.toString(3,model.symboltable) +
                        " will be removed by replacement resolution with clause\n" +
                        clause.toString(3,model.symboltable));}
            removeClause(resolvent);
            resolvent.remove(cliteral);
            if(trackReasoning) resolvent.origins = joinIntArrays(resolvent.origins,clause.origins);
            if(!checkUnitClause(resolvent))
                queue.add(new Task<>(TaskType.INSERTCLAUSE,null,resolvent,null));}}

    /** checks if the clause is a unit clause.
     * In this case the clause is put into the model.
     *
     * @param clause a clause
     * @return true if the clause is a unit clause
     * @throws Unsatisfiable if a contradiction is found
     */
    private boolean checkUnitClause(Clause clause) throws Unsatisfiable {
        if(clause.size() == 1) {
            if (monitoring) {
                monitor.print(monitorId, "Clause " + clause.toString(0, model.symboltable) +
                        " became a unit clause");}
            model.add(clause.getLiteral(0), clause.origins, null); // back to this
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
    private void checkSatisfiablity() throws Result {
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
        integrateTrueLiteral(literal,null);}

    /** inserts the clause into the local data structures.
     *
     * @param clause  the clause to be inserted.
     */
    private void insertClause(Clause clause) {
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
    private void removeClause(Clause clause) throws Result {
        switch(clause.structure) {
            case NEGATIVE: --statistics.negativeClauses; break;
            case POSITIVE: --statistics.positiveClauses; break;}
        --statistics.clauses;
        for(CLiteral cliteral : clause) {literalIndex.remove(cliteral);}
        clauses.remove(clause);
        if(clausesFinished) {
            if(clauses.isEmpty()) throw new Satisfiable(model);
            checkPurity(clause);
            checkSatisfiablity();}}


    /** removes the iterator's next clause from the index and the clauses
     *
     * @param cliteral iterator's next cliteral;
     * @param iterator an iterator over the literal index.
     * @return cliteral;
     */
    private CLiteral removeClause(CLiteral cliteral,BucketSortedList<CLiteral>.BucketIterator iterator)  {
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
                model.add(clause.getLiteral(0),
                        trackReasoning ? joinIntArrays(clause.origins,origins) : null,null);
                literalIndex.remove(clause.getCLiteral(0));
                clauses.remove(clause);
                if(clausesFinished && clauses.isEmpty()) throw new Satisfiable(model);
                return false;
            case 2: twoLitClauses.addDerivedClause(clause.getLiteral(0),clause.getLiteral(2),
                    trackReasoning ? joinIntArrays(clause.origins,origins) : null);} // keep the two-literal clause

        switch(clause.structure) {
            case NEGATIVE: ++statistics.negativeClauses; break;
            case POSITIVE: ++statistics.positiveClauses; break;}
        ++statistics.clauses;
        return true;}


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
        st.append("All Clauses of Problem " + problemId+":\n");
        int size = Integer.toString(counter).length()+2;
        for(Clause clause : clauses) {
            st.append(clause.toString(size,symboltable)).append("\n");}
        return st.toString();}

    /** lists all clauses and the literal index as string
     *
     * @param symboltable null or a symboltable
     * @return the info as string.
     */
    public String infoString(Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        st.append("All Clauses of Problem "+problemId+":\n");
        int size = Integer.toString(counter).length()+2;
        for(Clause clause : clauses) {
            st.append(clause.infoString(size,symboltable)).append("\n");}
        st.append(literalIndex.toString(cliteral -> cliteral.toString(symboltable)+"@"+cliteral.clause.id));
        if(!queue.isEmpty()) {
            st.append("All Clauses Queue of Problem "+problemId+":").append(Task.queueToString(queue));}
        return st.toString();}

}