package Datastructures;

import Datastructures.Results.UnsatClause;
import Datastructures.Results.Unsatisfiable;
import InferenceSteps.InferenceStep;

import java.util.Comparator;
import java.util.concurrent.PriorityBlockingQueue;
import java.util.function.Consumer;

public class ClauseList {
    /** the number of predicates */
    private int predicates;

    private LinkedItemList<Clause> clauses;

    private LiteralIndex literalIndex;

    boolean allClausesInserted = false;

    /** A queue of newly derived unit predicates and binary equivalences.
     * The unit predicates are automatically put at the beginning of the queue.
     */
    private final PriorityBlockingQueue<Task> queue =
            new PriorityBlockingQueue<>(100, Comparator.comparingInt(task->task.priority));


    public ClauseList(int predicates) {
        this.predicates = predicates;
        clauses = new LinkedItemList<Clause>("Clauses");
        literalIndex = new LiteralIndex(predicates);}

    public void reinitialize(int predicates) {
        this.predicates = predicates;
        clauses.clear();
        literalIndex.ensureCapacity(predicates);
    }

    public void addClause(Clause clause) {
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
        if(allClausesInserted) checkPurity(literalObject.literal);}

    /** applies the true literal to all clauses containing the literal.
     *
     * @param literal       a true literal
     * @throws Unsatisfiable if a contradiction is discovered.
     */
    void applyTrueLiteral(int literal, InferenceStep inferenceStep, boolean trackReasoning,
                          Consumer<String> monitor, Symboltable symboltable) throws Unsatisfiable {
        for(int sign = 1; sign >= -1; sign -=2) {
            Literal literalObject = literalIndex.getFirstLiteral(sign*literal);
            while(literalObject != null) {
                Clause clause = literalObject.clause;
                switch(clause.applyTrueLiteral(literal,sign == 1, inferenceStep, trackReasoning,
                        monitor,this::removeLiteralFromIndex,this::addTrueLiteralTask, symboltable)){
                    case -1: throw new UnsatClause(null,null, clause);
                    case 1: clauses.remove(clause); continue;}
                literalObject = (Literal)literalObject.nextItem;
            }}}

    void addTrueLiteralTask(int literal, InferenceStep inferenceStep) {
        queue.add(new Task(Task.TaskType.TRUELITERAL, literal,inferenceStep));
    }

    void checkPurity(int literal) {
        if (literalIndex.isEmpty(literal)) {
            queue.add(new Task(Task.TaskType.PURITY, literal, null));
        }
    }



}
