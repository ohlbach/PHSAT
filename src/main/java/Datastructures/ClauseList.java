package Datastructures;

import Datastructures.Clauses.Quantifier;
import Datastructures.Results.Result;
import Datastructures.Results.Satisfiable;
import Datastructures.Results.UnsatClause;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Theory.Model;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.Comparator;
import java.util.concurrent.PriorityBlockingQueue;
import java.util.function.Consumer;

public class ClauseList {
    /** the number of predicates */
    private int predicates;

    private LinkedItemList<Clause> clauses;

    private LiteralIndex<Literal> literalIndex;

    public ClauseListStatistics statistic = null;

    boolean allClausesInserted = false;
    boolean trackReasoning = false;
    Model model = null;
    String solverId = "ClauseList";
    String problemId;
    Consumer<String> monitor = null;
    Symboltable symboltable = null;
    boolean verify = false;
    int timestamp = 0;

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
        statistic = new ClauseListStatistics();
    }

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
                case SUBSUMPTION:
                    Clause clause = task.clause;
                    if(clause.isInList) removeSubsumedClauses(task.clause);
                    break;
                case PURITY:
                    break;
                default:
                    break;
            }
        }
    }

    public void addClause(Clause clause) {
        if(allClausesInserted) addSubsumptionTask(clause);
        clauses.addToBack(clause);
        addToIndex(clause);}

    public void removeClause(Clause clause) {
        clauses.remove(clause);}

    public void addToIndex(Clause clause) {
        for(Datastructures.Literal literalObject : clause.literals) {
            literalIndex.addToBack(literalObject);}}

    public void removeClauseFromIndex(Clause clause) {
        for(Literal literalObject : clause.literals) {
            literalIndex.remove(literalObject);}}

    public void removeLiteralFromIndex(Literal literalObject) {
        literalIndex.remove(literalObject);
        if(allClausesInserted) addPurityTask(literalObject.literal);}

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
                    case -1: throw new UnsatClause(null,null, clause);
                    case 1: clauses.remove(clause); continue;}
                addSubsumptionTask(clause);
                literalObject = (Literal)literalObject.nextItem;
            }}}

    void addTrueLiteralTask(int literal, InferenceStep inferenceStep) throws Unsatisfiable{
        model.add(null,literal,inferenceStep);
        if(allClausesInserted) queue.add(new Task(Task.TaskType.TRUELITERAL, literal,inferenceStep));}

    void addSubsumptionTask(Clause clause) {
        queue.add(new Task(Task.TaskType.SUBSUMPTION, clause));}

    void addPurityTask(int literal) {
        if (literalIndex.isEmpty(literal)) {
            queue.add(new Task(Task.TaskType.PURITY, literal, null));}}


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
                ++subsumer.timestamp;
                subsumerLiteral = (Literal)subsumerLiteral.nextItem;}}
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


    public void removePureLiterals() {

    }
    }





