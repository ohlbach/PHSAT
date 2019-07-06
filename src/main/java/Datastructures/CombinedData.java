package Datastructures;

import Algorithms.Algorithms;
import Coordinator.Tasks.TaskQueue;
import Coordinator.Tasks.TrueLiteral;
import Datastructures.Clauses.BasicClauseList;
import Datastructures.Clauses.Clause;
import Datastructures.Literals.LiteralIndex;
import Datastructures.Results.Result;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Theory.*;
import Management.Monitor;

/**
 * Created by ohlbach on 03.07.2019.
 */
public class CombinedData {
    public BasicClauseList basicClauseList;
    public Model model;
    public Disjunctions disjunctions;
    public ImplicationDAG implicationDAG;
    public EquivalenceClasses equivalenceClasses;
    public DisjointnessClasses disjointnessClasses;
    public LiteralIndex<Clause> literalIndex;
    public TaskQueue taskQueue;
    public Monitor monitor;

    public CombinedData(BasicClauseList basicClauseList, Model model, Disjunctions disjunctions, ImplicationDAG implicationDAG, EquivalenceClasses equivalenceClasses,
                        DisjointnessClasses disjointnessClasses, LiteralIndex<Clause> literalIndex, TaskQueue taskQueue, Monitor monitor) {
        this.basicClauseList     = basicClauseList;
        this.model               = model;
        this.disjunctions        = disjunctions;
        this.implicationDAG      = implicationDAG;
        this.equivalenceClasses  = equivalenceClasses;
        this.disjointnessClasses = disjointnessClasses;
        this.literalIndex        = literalIndex;
        this.taskQueue           = taskQueue;
        this.monitor             = monitor;
    }

    /** computes the consequences of a new unit clause in the clauses and the implicationDAG.<br>
     *  Equivalence classes need not be changed because it is assumed that all their literals<br>
     *  are already replaced by their representatives<br>
     *  Disjointness classes need not be changed because the information is also in the implicationDAG
     *
     * @param literal a newly derived unit clause.
     * @return Unsatisfiable if a contradiction has been discovered, otherwise null
     */
    public Result handleTrueLiteral(int literal) {
        int status = model.add(literal);
        if(status == -1) {return new Unsatisfiable(model,literal);}
        if(status == 1) {return null;}
        disjunctions.newTrueLiteral(literal);
        implicationDAG.newTrueLiteral(literal,false);
        disjointnessClasses.newTrueLiteral(literal);
        return null;}


    /** simplifies all clauses with the new two-literal clause and inserts it into the implicationDAG<br>
     * This may cause new unit clauses, binary clauses and other shortened causes to be generated
     * and put into the task queue.
     *
     * @param literal1 a literal
     * @param literal2 a literal
     * @return null
     */
    public Result handleTwoLiteralClause(int literal1, int literal2, String sourceId){
        literal1 = equivalenceClasses.mapToRepresentative(literal1);
        literal2 = equivalenceClasses.mapToRepresentative(literal2);
        if(literal1 == -literal2) {return null;}
        if(literal1 == literal2) {
            taskQueue.addTask(new TrueLiteral(literal1,monitor,sourceId,(literal->handleTrueLiteral(literal))));
            return null;}
        Algorithms.simplifyWithImplication(-literal1,literal2,literalIndex,implicationDAG);
        implicationDAG.addClause(literal1,literal2);
        return null;}

}
