package Datastructures;

import Datastructures.Clauses.Quantifier;
import Datastructures.Results.Result;
import Datastructures.Results.Satisfiable;
import Datastructures.Results.UnsatClause;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Theory.Model;
import InferenceSteps.InferenceStep;
import Management.ErrorReporter;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.concurrent.PriorityBlockingQueue;
import java.util.function.Consumer;

public class ClauseList {
    /** the number of predicates */
    private int predicates;

    private LinkedItemList<Clause> clauses;

    private LiteralIndex<Literal> literalIndex;

    public ClauseListStatistics statistics = null;

    boolean allClausesInserted = false;
    boolean trackReasoning = false;
    Model model = null;
    String solverId = "ClauseList";
    String problemId;
    Consumer<String> monitor = null;
    Symboltable symboltable = null;
    boolean verify = false;
    int timestamp = 0;

    /** contains pairs singleton-literal,clause. To be used when a model has to be completed. */
    private final ArrayList<Object> singletons = new ArrayList<>();

    /** A queue of newly derived unit predicates and binary equivalences.
     * The unit predicates are automatically put at the beginning of the queue.
     */
    private final PriorityBlockingQueue<Task> queue =
            new PriorityBlockingQueue<>(100, Comparator.comparingInt(task->task.priority));


    public ClauseList(boolean trackReasoning, boolean verify) {
        this.trackReasoning = trackReasoning;
        this.verify = verify;
    }

    public void initialize(Model model, String problemId,
                           Consumer<String> monitor, Symboltable symboltable) {
        this.model = model;
        this.problemId = problemId;
        this.predicates = model.predicates;
        this.monitor = monitor;
        this.symboltable = symboltable;
        if(clauses != null) clauses = new LinkedItemList<>("Clauses");
        else                clauses.clear();
        if(literalIndex != null) literalIndex = new LiteralIndex<>(predicates);
        else                     literalIndex.ensureCapacity(predicates);
        allClausesInserted = false;
        timestamp = 0;
        statistics = new ClauseListStatistics();
        model.addObserver(Thread.currentThread(),
                (literal,inferenceStep) -> {
                        synchronized(this) {queue.add(new Task(Task.TaskType.TRUELITERAL, literal,inferenceStep));}});}

    public void allClausesInserted() throws Result {
        IntArrayList trueLiterals = model.model; // derived true literals are automatically appended
        for(int i = 0; i < trueLiterals.size(); ++i) {
            int literal = trueLiterals.getInt(i);
            applyTrueLiteral(literal,model.getInferenceStep(literal));}

        Clause clause = clauses.firstLinkedItem;
        while(clause != null) {
            removeSubsumedClauses(clause);
            clause = (Clause)clause.nextItem;}

        this.allClausesInserted = true;
        removePureLiterals(); // new true literals generate tasks

        processTasks();
        if(clauses.isEmpty()) throw new Satisfiable(problemId,solverId,model);
    }

    public void processTasks() throws Result {
        while (!queue.isEmpty()) {
            Task task = queue.poll();
            switch (task.taskType) {
                case TRUELITERAL:
                    applyTrueLiteral(task.literal, task.inferenceStep);
                    break;
                case SHORTENED_CLAUSE:
                    Clause clause = task.clause;
                    if(clause.isInList) {
                        removeSubsumedClauses(task.clause);
                        mergeResolution(task.clause);}
                    break;
                case PURITY:
                    removePurePredicate(Math.abs(task.literal));
                    break;
                default:
                    break;
            }
        }
    }

    public void addClause(Clause clause) {
        if(allClausesInserted) addSubsumptionTask(clause);
        clauses.addToBack(clause);
        addClauseToIndex(clause);}

    public void removeClause(Clause clause) {
        clauses.remove(clause);}

    public void addClauseToIndex(Clause clause) {
        for(Datastructures.Literal literalObject : clause.literals) {
            literalIndex.addToBack(literalObject);}}

    public void removeClauseFromIndex(Clause clause) {
        for(Literal literalObject : clause.literals) {
            literalIndex.remove(literalObject);}}

    public void removeLiteralFromIndex(Literal literalObject) {
        literalIndex.remove(literalObject);
        if(allClausesInserted) addPurityTask(literalObject);}

    /** applies the true literal to all clauses containing the literal.
     *
     * @param literal       a true literal
     * @throws Unsatisfiable if a contradiction is discovered.
     */
    void applyTrueLiteral(int literal, InferenceStep inferenceStep) throws Unsatisfiable {
        for(int sign = 1; sign >= -1; sign -=2) {
            Literal literalObject = literalIndex.getFirstLiteral(sign*literal);
            while(literalObject != null) {
                Clause clause = literalObject.clause;
                switch(clause.applyTrueLiteral(literal,sign == 1, inferenceStep, trackReasoning,
                        monitor,this::removeLiteralFromIndex,this::addTrueLiteralTask, symboltable)){
                    case -1: throw new UnsatClause(problemId,solverId, clause);
                    case 1: clauses.remove(clause); continue;}
                addSubsumptionTask(clause);
                literalObject = (Literal)literalObject.nextItem;
            }}}

    void addTrueLiteralTask(int literal, InferenceStep inferenceStep) throws Unsatisfiable{
        model.add(null,literal,inferenceStep);
        if(allClausesInserted) queue.add(new Task(Task.TaskType.TRUELITERAL, literal,inferenceStep));}

    void addSubsumptionTask(Clause clause) {
        queue.add(new Task(Task.TaskType.SHORTENED_CLAUSE, clause));}

    void addPurityTask(int literal) {
        queue.add(new Task(Task.TaskType.PURITY, literal, null));}

    void addPurityTask(Literal literalObject) {
        queue.add(new Task(Task.TaskType.PURITY, literalObject.literal, null));}

    /**
     * Removes all clauses subsumed by the given clause clauses from the clause list.
     * <br>
     * A subsumer subsumes a subsumee iff the subsumee clause is true in all models of the subsumer clause.
     * <br>
     * A necessary condition is that the subsumer's literals are a subset of the subsumee's literals.
     * This is checked with a timestamp mechanism for efficiently finding the subsumee candidates.
     * <br>
     * All subsumed clauses are removed from the datastructures.
     *
     * @param subsumer the clause that potentially subsumes other clauses.
     */
    public void removeSubsumedClauses (Clause subsumer) {
        ++timestamp;
        int size = subsumer.literals.size();
        Literal subsumerLiteral = subsumer.literals.get(0);
        Literal subsumeeLiteral = literalIndex.getFirstLiteral(subsumerLiteral.literal);
        while(subsumeeLiteral != null) {
            Clause subsumee = subsumeeLiteral.clause;
            if (subsumee != subsumer) subsumee.timestamp = timestamp;
            subsumeeLiteral = (Literal)subsumeeLiteral.nextItem;}

        for(int i = 1; i < subsumer.literals.size(); ++i) {
            subsumerLiteral = subsumer.literals.get(i);
            subsumeeLiteral = literalIndex.getFirstLiteral(subsumerLiteral.literal);
            while(subsumeeLiteral != null) {
                Clause subsumee = subsumeeLiteral.clause;
                if (subsumee.timestamp - timestamp == size-1 && subsumes(subsumer,subsumee)) {
                    if(verify) verifySubsumption(subsumer,subsumee);
                    removeClause(subsumee);
                    removeClauseFromIndex(subsumee);
                    if(monitor != null) {
                        monitor.accept("Clause " + subsumer.toString(symboltable,0) +
                                " subsumes " + subsumee.toString(symboltable,0));}}
                else ++subsumee.timestamp;
                subsumeeLiteral = (Literal)subsumeeLiteral.nextItem;}}
        timestamp += size;}

    /**
     * Determines whether the given clause is subsumed by some of the clauses in the list.
     *
     * @param subsumee The clause to be checked for subsumption.
     * @return the subsumer clause if the subsumee clause is subsumed by some clause in the list, otherwise null.
     */
    public Clause isSubsumed(Clause subsumee) {
        ++timestamp;
        for(Literal subsumeeLiteral : subsumee.literals) {
            Literal subsumerLiteral = literalIndex.getFirstLiteral(subsumeeLiteral.literal);
            while(subsumerLiteral != null) {
                Clause subsumer = subsumerLiteral.clause;
                int subsumerTimestamp = subsumer.timestamp;
                if(subsumerTimestamp < timestamp) {subsumer.timestamp = timestamp; continue;}
                if(subsumerTimestamp == subsumer.literals.size()-1 && subsumes(subsumer,subsumee)) {
                    timestamp += subsumee.literals.size();
                    if(verify) verifySubsumption(subsumer,subsumee);
                    return subsumer;}
                ++subsumee.timestamp;
                subsumerLiteral = (Literal)subsumerLiteral.nextItem;}}
        timestamp += subsumee.literals.size();
        return null;}

    /**
     * Determines whether a subsumer clause subsumes a subsumee clause.
     * <br>
     * Precondition:  The subsumer's literals must be a subset of the subsumee's literals
     * <br>
     * A subsumer clause subsumes a subsumee clause if the subsumee clause is true in all models of the subsumer clause.
     * In some cases this expensive check can be avoided by checking syntactic conditions on the clause.
     * In particular if the clauses are both OR-clauses then the subset condition is sufficient.
     * <br>
     * If both clauses have no multiple literals then the subsumer-interval must be smaller or equal the subsumee-interval.
     *
     * @param subsumer The clause that potentially subsumes other clauses
     * @param subsumee The clause to be potentially subsumed
     * @return true if the subsumer subsumes the subsumee, false otherwise
     */
    protected boolean subsumes(Clause subsumer, Clause subsumee) {
        if(subsumer.quantifier == Quantifier.OR && subsumee.quantifier == Quantifier.OR) {return true;}
        if(!subsumer.hasMultipleLiterals) {
            if(!subsumee.hasMultipleLiterals) // the subsumer-interval must be smaller or equal the subsumee-interval
                return subsumee.min <= subsumer.min && subsumee.max >= subsumer.max;

            int extraLiterals = 0;
            boolean subsumeeMultiples = false; // check multiples in the subsumer-part
            for(Literal subsumeeLiteral : subsumee.literals) {
                if(subsumer.findLiteral(subsumeeLiteral.literal) == null)
                    extraLiterals += subsumeeLiteral.multiplicity;
                else {
                    if(subsumeeLiteral.multiplicity > 1) {
                        subsumeeMultiples = true;}}}

            // the extraLiterals in the subsumee may all be false or true in the subsumer's models.
            if(!subsumeeMultiples) {
                return subsumee.min <= subsumer.min && subsumee.max >= subsumer.max + extraLiterals;}}

        return subsumesByModels(subsumer, subsumee);}


    /**
     * Determines whether a subsumer clause subsumes a subsumee clause by checking if the subsumer's models satisfy the subsumee.
     * <br>
     * The subsumer's literals must be a subset of the subsumee's literals.<br>
     * In principle there are no other conditions, but by efficiency reasons the method should only be called
     * when there are multiple literals in the clause.
     *
     * @param subsumer The clause that potentially subsumes other clauses
     * @param subsumee The clause to be potentially subsumed
     * @return true if the subsumer subsumes the subsumee, false otherwise
     */
    protected boolean subsumesByModels(Clause subsumer, Clause subsumee) {
        IntArrayList predicates = subsumee.predicates();
        for(int model : subsumer.getModels(monitor,symboltable)) { // monitor and symboltable should not be necessary for simplified clauses
            if(!subsumee.isTrue(model,predicates)) return false;}
        return true;}

    protected boolean verifySubsumption(Clause subsumer, Clause subsumee ) {
        if(subsumesByModels(subsumer,subsumee)) return true;
        if(monitor!=null) {
            monitor.accept("Error: Subsumption Check: Clause " + subsumer.toString(symboltable,0) +
                    "is not subsumed by " + subsumee.toString(symboltable,0));}
        return false;}

    /** searches pure predicates and process them.
     * <br>
     *  A literal p is pure in clauses which are NOT interval- or exactly-clauses if it occurs either <br>
     *  - only positively or only negatively in OR- or ATLEAST-clauses or<br>
     *  - only positively or only negatively in ATMOST-clauses<br>
     *  The literal p is singleton pure if it occurs only once in an interval- or exactly-clause.<br>
     *  A pure literal in  OR- or ATLEAST-clauses can be made true if occurs only positively,<br>
     *  or made false if it occurs only negatively. <br>
     *  In atmost clauses it is the other way round.<br>
     *  A singleton pure literal in an interval- or exactly-clause can be removed. <br>
     *  Its truth value can only be determined after a model for the entire clause set has been found.
     *
     * @throws Unsatisfiable if a contradiction is discovered.
     * */
    void removePureLiterals()  throws Unsatisfiable{
        boolean purityFound = true;
        while(purityFound) {
            purityFound = false;
            for(int predicate = 1; predicate <= predicates; ++predicate) {
                purityFound |= removePurePredicate(predicate);
                if(clauses.isEmpty()) return;}}}

    /** checks if the predicate or its negation is pure.
     * <br>
     *  A literal p is pure in clauses which are NOT interval- or exactly-clauses if it occurs either <br>
     *  - only positively or only negatively in OR- or ATLEAST-clauses or<br>
     *  - only positively or only negatively in ATMOST-clauses<br>
     *  The literal p is singleton pure if it occurs only once in an interval- or exactly-clause.<br>
     *  A pure literal in  OR- or ATLEAST-clauses can be made true if occurs only positively,<br>
     *  or made false if it occurs only negatively. <br>
     *  In atmost clauses it is the other way round.<br>
     *  A singleton pure literal in an interval- or exactly-clause can be removed. <br>
     *  Its truth value can only be determined after a model for the entire clause set has been found.
     *
     * @param predicate a predicate.
     * @throws Unsatisfiable if a contradiction is discovered.
     * */
    protected boolean removePurePredicate(int predicate) throws Unsatisfiable{
        if(literalIndex.isBothEmpty(predicate)) return false;
        if(isPositivelyPure(predicate)) {
            if(monitor!=null) {monitor.accept("Literal " + Symboltable.toString(predicate,symboltable) +
                    " is positively pure");}
            ++statistics.pureLiterals;
            model.add(null,predicate,null);
            applyTrueLiteral(predicate,null); // pure literals will never be part of a contradiction.
            return true;}
        if(isNegativelyPure(predicate)) {
            if(monitor!=null) {monitor.accept("Predicate " + Symboltable.toString(predicate,symboltable) +
                    " is negatively pure");}
            ++statistics.pureLiterals;
            model.add(null,-predicate,null);
            applyTrueLiteral(-predicate,null); // pure literals will never be part of a contradiction.
            return true;}

        int literal = isSingletonPure(predicate);
        if(literal != 0) {
            if(monitor != null) {monitor.accept("Literal " + Symboltable.toString(literal,symboltable) +
                    " is singleton pure");}
            ++statistics.singletonLiterals;
            Clause clause = literalIndex.getFirstLiteral(literal).clause;
            singletons.add(literal); singletons.add(clause.clone());
            switch(clause.removeLiteral(literal,trackReasoning,0,this::removeLiteralFromIndex, this::addTrueLiteralTask, monitor,symboltable)) {
                case -1: clauses.remove(clause); throw new UnsatClause(problemId,solverId, clause);
                case +1: clauses.remove(clause); return true;}
            return true;}
        return false;
    }

    /**
     * Determines whether a given predicate is positively pure in the clause list.
     * <br>
     * A literal p is positively pure if it can be made true without affecting the satisfiability or unsatisfiability of a clause set.<br>
     * This is the case when it occurs positively in OR- and Atleast clauses and negatively in Atmost clauses<br>
     * Examples: <br>
     * p,q,r
     * atleast p,a,b
     * atmost -p,c,d
     * <br>
     * Add a true literal to a true OR- or Atleast-clause does not change the truth of the clause. <br>
     * Adding a false literal to an Atmost-clause does not change the truth of the clause.
     *
     * @param predicate the predicate to check for positive purity
     * @return true if the predicate is positively pure, false otherwise
     */
    protected boolean isPositivelyPure(int predicate) {
        Literal positiveLiterals  = literalIndex.getFirstLiteral(predicate);
        Literal negativeLiterals = literalIndex.getFirstLiteral(-predicate);
        Clause clause;
        while (negativeLiterals != null) { // only negative ATMOST-literals are allowed
            clause = negativeLiterals.clause;
            if(clause.quantifier != Quantifier.ATMOST) return false;
            negativeLiterals = (Literal)negativeLiterals.nextItem;}

        while (positiveLiterals != null) {
            clause = positiveLiterals.clause; // only positive Or- or Atleast clauses are allowed.
            if(!(clause.quantifier == Quantifier.OR && clause.quantifier == Quantifier.ATLEAST)) return false;
            positiveLiterals = (Literal)positiveLiterals.nextItem;}

        return true;}

    /**
     * Determines whether a given predicate is positively pure in the clause list.
     * <br>
     * A literal p is positively pure if it can be made true without affecting the satisfiability or unsatisfiability of a clause set.<br>
     * This is the case when it occurs positively in OR- and Atleast clauses and negatively in Atmost clauses<br>
     * Examples: <br>
     * p,q,r
     * atleast p,a,b
     * atmost -p,c,d
     * <br>
     * Add a true literal to a true OR- or Atleast-clause does not change the truth of the clause. <br>
     * Adding a false literal to an Atmost-clause does not change the truth of the clause.
     *
     * @param predicate the predicate to check for positive purity
     * @return true if the predicate is positively pure, false otherwise
     */
    protected boolean isNegativelyPure(int predicate) {
        Literal positiveLiterals = literalIndex.getFirstLiteral(predicate);
        Literal negativeLiterals = literalIndex.getFirstLiteral(-predicate);
        Clause clause;
        while (positiveLiterals != null) { // only positive ATMOST-literals are allowed
            clause = positiveLiterals.clause;
            if(clause.quantifier != Quantifier.ATMOST) return false;
            positiveLiterals = (Literal)positiveLiterals.nextItem;}

        while (negativeLiterals != null) {
            clause = negativeLiterals.clause; // only negative Or- or Atleast clauses are allowed.
            if(!(clause.quantifier == Quantifier.OR && clause.quantifier == Quantifier.ATLEAST)) return false;
            negativeLiterals = (Literal)negativeLiterals.nextItem;}

        return true;}

    /** checks if the predicate p is singleton pure.
     * <br>
     * If p occurs only once in an interval clause p is singleton pure.<br>
     * If -p occurs only once in an interval clause -p is singleton pure.
     *
     * @param predicate a predicate.
     * @return the predicate or its negation if it is singleton pure
     */
    protected int isSingletonPure(int predicate) {
        Literal positiveLiterals = literalIndex.getFirstLiteral(predicate);
        Literal negativeLiterals = literalIndex.getFirstLiteral(-predicate);
        if(positiveLiterals != null) {
            return (positiveLiterals.clause.quantifier == Quantifier.INTERVAL && positiveLiterals.nextItem == null) ? predicate : 0;}
        if(negativeLiterals != null) {
            return (negativeLiterals.clause.quantifier == Quantifier.INTERVAL && negativeLiterals.nextItem == null) ? -predicate : 0;}
        return 0;}

    protected void mergeResolution(Clause shorterParent) throws Unsatisfiable {
        ++timestamp;
        for(Literal shorterLiteral1 : shorterParent.literals) {
            int resolutionLiteral = shorterLiteral1.literal;
            Literal longerLiteral = literalIndex.getFirstLiteral(-resolutionLiteral);
            while(longerLiteral != null) {
                longerLiteral.clause.timestamp = timestamp;
                longerLiteral = (Literal)longerLiteral.nextItem;}
            for(Literal shorterLiteral2 : shorterParent.literals) {
                if(shorterLiteral1 == shorterLiteral2) continue;
                longerLiteral = literalIndex.getFirstLiteral(shorterLiteral2.literal);
                while(longerLiteral != null) {
                    Clause longerParent = longerLiteral.clause;
                    int longerTimestamp = longerParent.timestamp;
                    if(longerTimestamp < timestamp) {longerParent.timestamp = timestamp; continue;}
                    if(longerTimestamp == longerParent.literals.size()-1) {
                        resolve(shorterParent,resolutionLiteral,longerParent);
                        timestamp += shorterParent.literals.size();}
                    ++shorterParent.timestamp;
                longerLiteral = (Literal)longerLiteral.nextItem;}}}
        timestamp += shorterParent.literals.size();}


    protected void resolve(Clause parent1, int literal, Clause parent2) throws Unsatisfiable {
         if(parent1.quantifier == Quantifier.OR && parent2.quantifier == Quantifier.OR) {
             if(parent1.literals.size() == parent2.literals.size()) {
                  parent1.removeLiteral(literal,false,0,null,this::addTrueLiteralTask,monitor,symboltable);
             }
         }
    }


    /** extends the model by determining the truth-value of singleton pure predicates in interval- and exactly-clauses.
     *
     * @throws Unsatisfiable should not happen.
     */
    public void extendModel() throws Unsatisfiable {
        for(int i = singletons.size()-2; i >= 0; i -=2) {
            Clause clause = (Clause)singletons.get(i+1);
            int trueLiterals = clause.trueLiterals(model::isTrue);
            if(clause.min <= trueLiterals &&  trueLiterals <= clause.max) { // enough true literals. All unsigned literals can become false.
                for(Datastructures.Literal literalObject : clause.literals) {
                    int unsignedLiteral = literalObject.literal;
                    if(model.status(unsignedLiteral) == 0) {
                        if(monitor!=null) monitor.accept("Extending model with " + Symboltable.toString(-unsignedLiteral,symboltable) +
                                " for clause " + clause.toString(symboltable,0));
                        model.add(null,-unsignedLiteral,null);}}
                continue;}

            if(trueLiterals > clause.max) { // this should not happen
                ErrorReporter.reportErrorAndStop("Normalizer.extendModel: too many true literals in clause " +
                        clause.toString(symboltable,0) + "\nnumber of true literal: " + trueLiterals +
                        "\nModel: " + model.toString(symboltable));}

            for(Datastructures.Literal literalObject : clause.literals) {
                int unsignedLiteral = literalObject.literal;
                if(model.status(unsignedLiteral) == 0) { // filling up the true literals up to clause.max
                    if(trueLiterals >= clause.max) unsignedLiteral = -unsignedLiteral;
                    if(monitor!=null) monitor.accept("Extending model with " + Symboltable.toString(unsignedLiteral,symboltable) +
                            " for clause " + clause.toString(symboltable,0));
                    model.add(null,unsignedLiteral,null);
                    if(model.status(literalObject.literal) == 1) trueLiterals += literalObject.multiplicity;}}


            if(trueLiterals < clause.min)  // should not happen
                ErrorReporter.reportErrorAndStop("Normalizer.extendModel: not enough true literals in clause " +
                        clause.toString(symboltable,0) + "\nnumber of true literals: " + trueLiterals  +
                        "\nModel: " + model.toString(symboltable));}}

    /** lists the singletons as string
     *
     * @param symboltable null or a symboltable.
     * @return the singletons as a string.*/
    public String singletonsToString(Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        st.append("Singleton Literals:\n");
        for(int i = 0; i < singletons.size(); i += 2) {
            st.append(Symboltable.toString((int) singletons.get(i), symboltable)).append(" in clause ");
            st.append(((Clause) singletons.get(i+1)).toString(symboltable,0)).append("\n");}
        return st.toString();}
}







