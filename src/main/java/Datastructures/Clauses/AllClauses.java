package Datastructures.Clauses;

import Datastructures.Literals.CLiteral;
import Datastructures.Literals.LitAlgorithms;
import Datastructures.Results.Result;
import Datastructures.Results.Satisfiable;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Datastructures.Theory.DisjointnessClass;
import Datastructures.Theory.DisjointnessClasses;
import Datastructures.Theory.EquivalenceClasses;
import Datastructures.Theory.Model;
import Datastructures.TwoLiteral.TwoLitClauses;
import Management.Monitor;
import Management.ProblemSupervisor;
import Utilities.BucketSortedIndex;
import Utilities.BucketSortedList;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.concurrent.PriorityBlockingQueue;

import static Utilities.Utilities.joinIntArraysSorted;

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
    private final Symboltable symboltable;

    private int counter = 0;
    private int maxClauseLength = 0;
    private final BucketSortedList<Clause> clauses;
    private final BucketSortedIndex<CLiteral> literalIndex;
    private final AllClausesStatistics statistics;
    private int timestamp = 1;
    private final boolean clausesFinished;

    private enum TaskType {
        TRUELITERAL, EQUIVALENCE, DISJOINTNESS,SIMPLIFYALL, SIMPLIFYOTHERS,
    }

    private static class Task {
        TaskType taskType;
        IntArrayList origins;
        Object a;
        Object b;
        Task(TaskType taskType, IntArrayList origins, Object a, Object b) {
            this.taskType = taskType;
            this.origins = origins;
            this.a = a;
            this.b = b;
        }
    }


    /** A queue of newly derived unit literals, newly derived binary equivalences and disjointness clauses
     * The unit literals are automatically put at the beginning of the queue.
     */
    private final PriorityBlockingQueue<Task> queue =
            new PriorityBlockingQueue<>(10, Comparator.comparingInt(this::getPriority));

    /** gets the priority for the objects in the queue.
     *
     * @param task the objects in the queue
     * @return the priority of the objects in the queue.
     */
    private int getPriority(Task task) {
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
        symboltable = model.symboltable;
        clauses      = new BucketSortedList<Clause>(Clause::size);
        literalIndex = new BucketSortedIndex<CLiteral>(model.predicates+1,
                (cLiteral->cLiteral.literal),
                (cLiteral->cLiteral.clause.size()));
        statistics = new AllClausesStatistics(problemId);

        model.addObserver(Thread.currentThread(),(literal, origins) ->
                queue.add(new Task(TaskType.TRUELITERAL,origins, literal,null)));

        equivalenceClasses.addObserver((representative,literal,origins) ->
                queue.add(new Task(TaskType.EQUIVALENCE,origins,representative,literal)));

        disjointnessClasses.addObserver((disjoints) ->
                queue.add(new Task(TaskType.DISJOINTNESS,null, disjoints, null)));

        initializeDisjoints();
        initializeXors();
        initializeDisjunctions();
        clausesFinished = true;
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
            addClause(clause);
            integrateDisjointnessClause(clause);}}

    /** This method puts the disjunctions clauses into the clauses
     *
     * @throws Result if a contradiction occurs.
     */
    private void initializeDisjunctions() throws Result {
        for(int[] clause : basicClauseList.disjunctions) {
            addClause(clause);}}


    /** works off the queue
     * new true literals <br>
     * new equivalences <br>
     * new disjointnesses <br>
     */
    public void run() {
        while(!Thread.interrupted()) {
            try {
                if(monitoring) {monitor.print(monitorId,"Queue is waiting");}
                Task task = queue.take(); // waits if the queue is empty
                switch (task.taskType) {
                    case TRUELITERAL:
                        integrateTrueLiteral((Integer)task.a,task.origins);
                        break;
                    case EQUIVALENCE:
                        integrateEquivalence((Integer)task.a,(Integer)task.b,task.origins);
                        break;
                    case DISJOINTNESS:
                        integrateDisjointness((DisjointnessClass)task.a);
                        break;
                    case SIMPLIFYALL:
                        simplifyAllClauses();
                        break;
                    case SIMPLIFYOTHERS:
                        simplifyOtherClauses((Clause)task.a);
                        break;
                }}
            catch(InterruptedException ex) {return;}
            catch(Result result) {problemSupervisor.setResult(result,"AllClauses"); return;}}}



    /** turns a single basic clause into a Clause datastructure. <br>
     * Literals are replaced by their representative in the equivalence classes.<br>
     * True or false literlas are eliminated<br>
     * Double literals are removed.<br>
     * Tautologies are ignored.<br>
     * Empty clauses cause throw of Result <br>
     * Two-literal clauses are put into twoLitClauses.
     *
     * @param basicClause [id,type,lit1,...]
     * @throws Result if the clause is empty, otherwise null
     */
    private void addClause(int[] basicClause) throws Result {
        Clause clause = new Clause(++counter,basicClause.length-2);
        IntArrayList origins = trackReasoning ? IntArrayList.wrap(new int[]{basicClause[0]}) : null;
        int position = -1;
        for(int i = 2; i < basicClause.length; ++i) {
            int originalLiteral =  basicClause[i];
            int literal = equivalenceClasses.getRepresentative(originalLiteral);
            if(trackReasoning && literal != originalLiteral) {
                origins = joinIntArraysSorted(origins,equivalenceClasses.getOrigins(originalLiteral));}

            switch(model.status(literal)) {
                case +1: return; // true literal:  ignore clause
                case -1:         // false literal: ignore literal
                    if(trackReasoning) {origins = joinIntArraysSorted(origins,model.getOrigin(literal));}
                    continue;}

            switch(clause.contains(literal)) {
                case +1: continue; // double literal
                case -1:
                    ++statistics.tautologies;
                    if(monitoring) {
                        monitor.print(monitorId, "Clause " +
                                BasicClauseList.clauseToString(0,basicClause,symboltable) + " became a tautology");}
                    return;}   // tautology

            clause.add(new CLiteral(literal,clause,++position));}

        switch(clause.size()) {
            case 0:
                throw new Unsatisfiable("Clause " + BasicClauseList.clauseToString(0,basicClause,symboltable) +
                    " became empty",origins);
                case 1:
                    ++statistics.derivedUnitClauses;
                    model.add(clause.getLiteral(0),origins,null); // back to this process
                    return;}

        if(isSubsumed(clause)) return;
        if(clause.size() == 2) twoLitClauses.addDerivedClause(clause.getLiteral(0),
                                                              clause.getLiteral(1),origins);
        clause.setStructure();
        clause.origins = origins;
        insertClause(clause);}


    /** applies a true literal to all clauses.
     * Clauses containing the literal are removed.<br>
     * In clauses containing the negated literal, this literal is removed.<br>
     * If the resulting clause is a unit clause, it is added to the model and removed from the clauses.<br>
     * If the resulting clause is a two-literal clause, it is added to the twoLitClauses and  not removed
     * from the clauses.
     *
     * @param literal a true literal
     * @param origins the basic clause ids for the truth of the literal
     * @throws Result if a contradiction is found.
     */
    private void integrateTrueLiteral(int literal, IntArrayList origins) throws Result {
        BucketSortedList<CLiteral>.BucketIterator iterator = literalIndex.popIterator(literal);
        while(iterator.hasNext()) {   // remove all clauses with the literal
            removeClause(iterator);}
        literalIndex.pushIterator(literal,iterator);

        iterator = literalIndex.popIterator(-literal);
        while(iterator.hasNext()) {               // remove from all clauses the negated literal
            removeLiteral(iterator,origins);}
        literalIndex.pushIterator(-literal,iterator);}

    /** replaces all occurrences of literal by representative
     * resulting tautologies are deleted <br>
     * unit literals are put into the model <br>
     * two-literal clauses are kept and put into the twoLiteral module.
     *
     * @param representative for representative = literal
     * @param literal       a literal
     * @param origins      the basic clause ids for the equivalence.
     * @throws Result if a contradiction is found
     */
    private void integrateEquivalence(int representative, int literal, IntArrayList origins) throws Result {
        BucketSortedList<CLiteral>.BucketIterator iterator;
        for(int i = 1; i <= 2; ++i) {
            iterator = literalIndex.popIterator(literal);
            while(iterator.hasNext()) {
                Clause clause = replaceLiteral(iterator,representative,origins);
                if(clause != null) removeSubsumedClauses(clause);}
            literalIndex.pushIterator(literal,iterator);
            literal = -literal;
            representative = -representative;}
    }

    /** turns a disjointness class into the corresponding list of two-literal clauses.
     *
     * @param disjoints a disjointness class.
     */
    private void integrateDisjointness(DisjointnessClass disjoints)  {
        IntArrayList origins  = disjoints.origins;
        IntArrayList literals = disjoints.literals;
        int size = literals.size();
        for(int i = 0; i < size; ++i) {
            int literal1 = -literals.getInt(i);
            for(int j = i+1; j < size; ++j) {
                Clause clause = new Clause(++counter,literal1,-literals.getInt(j),origins);
                if(!isSubsumed(clause)) {insertClause(clause);}}}}

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
                Clause clause = new Clause(++counter,literal1,-basicClause[j],origins);
                if(!isSubsumed(clause)) {insertClause(clause);}}}}

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
                monitor.print(monitorId, "Clause\n" + clause.toString(4,symboltable) +
                        " is subsumed by clause\n" + subsumer.toString(4,symboltable));
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
                monitor.print(monitorId, "Clause\n" + subsumed.toString(4,symboltable) +
                        " is subsumed by clause\n" + clause.toString(4,symboltable));
            removeClause(clause);}
        timestamp += 2;
    }

    /** performs all replacement resolutions and all purity checks
     *
     * @throws Result if a contradiction is found.
     */
    private void simplifyAllClauses() throws Result{
        ArrayList<Object[]> results = new ArrayList<>();
        for(Clause clause : clauses) {
            Object[] result = LitAlgorithms.replacementResolutionBackwards(clause,literalIndex,timestamp);
            timestamp += 2;
            if(result != null) results.add(result);}

        for(Object[] result : results) {
            CLiteral cliteral = (CLiteral)result[0];
            Clause otherClause = (Clause)result[1];
            ++statistics.backwardReplacementResolutions;
            if(monitoring) {
                monitor.print(monitorId, "Literal " + cliteral.toString(symboltable) +
                        " in  clause \n" + cliteral.clause.toString(3,symboltable) +
                        " will be removed by replacement resolution with clause\n" +
                        otherClause.toString(3,symboltable));}
            if(removeLiteral(cliteral,trackReasoning ? joinIntArraysSorted(cliteral.clause.origins,otherClause.origins) : null))
                queue.add(new Task(TaskType.SIMPLIFYOTHERS,null,cliteral.clause,null));
            purityCheck();}
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
        for(CLiteral cliteral : resolvents) {
            ++statistics.forwardReplacementResolutions;
            if(monitoring) {
                monitor.print(monitorId, "Literal " + cliteral.toString(symboltable) +
                        " in  clause \n" + cliteral.clause.toString(3,symboltable) +
                        " will be removed by replacement resolution with clause\n" +
                        clause.toString(3,symboltable));}
            if(removeLiteral(cliteral,trackReasoning ? joinIntArraysSorted(cliteral.clause.origins,clause.origins) : null))
                queue.add(new Task(TaskType.SIMPLIFYOTHERS,null,cliteral.clause,null));}
        }

    /** checks if the literal is pure (there are no further occurrences).
     * If literal is pure then -literal can be made true.
     *
     * @param literal        a literal to be checked
     * @throws Result if a contradiction is found.
     */
    private void purityCheck(int literal) throws Result {
        assert clausesFinished;
        if(literalIndex.isEmpty(literal)) {
            if(monitoring) {
                monitor.print(monitorId,"Literal " + Symboltable.toString(literal,symboltable) +
                        " became pure");}
            ++statistics.purities;
            model.add(-literal,null,null);}}

    /** performs purity checks for all literals in the clause
     *
     * @param clause  a just removed clause.
     * @throws Result may throw Satisfiable
     */
    private void purityCheck(Clause clause) throws Result {
        assert clausesFinished;
        for(CLiteral cliteral : clause.cliterals) purityCheck(cliteral.literal);}

    /** checks all literals for purity.
     *
     * @throws Result may throw Satisfiable
     */
    private void purityCheck() throws Result {
        assert clausesFinished;
        for(int literal = 1; literal <= model.predicates; ++literal) {
            if(model.status(literal) == 0) {
                purityCheck(literal);
                purityCheck(-literal);}}}

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
                    Symboltable.toString(literal,symboltable) +
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
            purityCheck(clause);
            checkSatisfiablity();}}


    /** removes the iterator's next clause and does purity checks
     *
     * @param iterator an iterator over the literal index.
     * @throws Result if a contradiction is found.
     */
    private void removeClause(BucketSortedList<CLiteral>.BucketIterator iterator) throws Result {
        CLiteral cliteral = iterator.next();
        Clause clause = cliteral.clause;
        switch(clause.structure) {
            case NEGATIVE: --statistics.negativeClauses; break;
            case POSITIVE: --statistics.positiveClauses; break;}
        --statistics.clauses;
        iterator.remove();
        for(CLiteral clit : clause) {
            if(clit != cliteral) literalIndex.remove(clit);}
        clauses.remove(clause);
        if(clausesFinished) {
            if(clauses.isEmpty()) throw new Satisfiable(model);
            purityCheck(clause);
            checkSatisfiablity();}}

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
        if(clausesFinished) purityCheck(cliteral.literal);
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
        if(clausesFinished) purityCheck(cliteral.literal);
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
                model.add(clause.getLiteral(0),
                        trackReasoning ? joinIntArraysSorted(clause.origins,origins) : null,null);
                literalIndex.remove(clause.getCLiteral(0));
                clauses.remove(clause);
                if(clausesFinished && clauses.isEmpty()) throw new Satisfiable(model);
                return false;
            case 2: twoLitClauses.addDerivedClause(clause.getLiteral(0),clause.getLiteral(2),
                    trackReasoning ? joinIntArraysSorted(clause.origins,origins) : null);} // keep the two-literal clause

        switch(clause.structure) {
            case NEGATIVE: ++statistics.negativeClauses; break;
            case POSITIVE: ++statistics.positiveClauses; break;}
        ++statistics.clauses;
        return true;}

    /** replaces the oldLiteral by the newLiteral
     *
     * @param iterator    the next() yields the oldLiteral
     * @param newLiteral  a new literal
     * @param origins     null or the clause ids for the replacement
     * @return            null (tautology) or the changed clause
     * @throws Result if a contradiction has been found.
     */
    private Clause replaceLiteral(BucketSortedList<CLiteral>.BucketIterator iterator,
                                int newLiteral, IntArrayList origins) throws Result {
        CLiteral cliteral = iterator.next();
        Clause clause = cliteral.clause;
        IntArrayList orig = trackReasoning ? joinIntArraysSorted(clause.origins,origins) : null;
        switch(clause.contains(newLiteral)) {
            case +1 : removeLiteral(iterator,orig); return clause;  // double oldLiteral
            case -1 : removeClause(iterator);       return null;}   // tautology

        switch(clause.structure) {
            case NEGATIVE: --statistics.negativeClauses; break;
            case POSITIVE: --statistics.positiveClauses; break;}
        iterator.remove();
        cliteral.literal = newLiteral;
        literalIndex.add(cliteral);
        clause.origins = orig;
        clause.setStructure();
        switch(clause.structure) {
            case NEGATIVE: ++statistics.negativeClauses; break;
            case POSITIVE: ++statistics.positiveClauses; break;}
        return clause;
    }

    /** Lists all clauses as a string
     *
     * @return the clauses as string
     */
    public String toString() {
        return toString(symboltable);}


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
            st.append("All Clauses Queue of Problem "+problemId+":").append(queue.toString());}
        return st.toString();}

}