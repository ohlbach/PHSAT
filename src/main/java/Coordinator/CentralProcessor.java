package Coordinator;

import Datastructures.Clauses.ClauseList;
import Datastructures.Results.Satisfiable;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Theory.DisjointnessClasses;
import Datastructures.Theory.EquivalenceClasses;
import Datastructures.Theory.ImplicationDAG;
import Datastructures.Theory.Model;

/**
 * Created by ohlbach on 10.10.2018.
 */
public class CentralProcessor extends Processor {

    public CentralProcessor(Preprocessor preprocessor) {
        clauses        = preprocessor.clauses;
        model          = preprocessor.model;
        implicationDAG = preprocessor.implicationDAG;
        equivalences   = preprocessor.equivalences;
        disjointnesses = preprocessor.disjointnesses;
    }

    public void newUnsatifiability(Unsatisfiable unsatisfiable) {
        addTask(new Task.Unsatisfiability(unsatisfiable,this));}

    public void newSatisfiability(Satisfiable satisfiable) {
        addTask(new Task.Satisfiability(satisfiable,this));
    }

    public void newUnitClause(int literal) {
        addTask(new Task.OneLiteral(literal,this));}

    public void newTwoLiteralClause(int literal1, int literal2) {
        addTask(new Task.TwoLiteral(literal1,literal2,this));}
}
