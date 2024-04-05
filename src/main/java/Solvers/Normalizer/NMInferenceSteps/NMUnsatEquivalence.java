package Solvers.Normalizer.NMInferenceSteps;

import Datastructures.Clauses.InputClauses;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;

public class NMUnsatEquivalence extends Unsatisfiable {
    int[] clause;
    int literal1,literal2;

    public NMUnsatEquivalence(String problemId, String solverId, long startTime, int[] clause, int literal1, int literal2) {
        super(problemId,solverId,startTime);
        this.clause = clause;
        this.literal1 = literal1;
        this.literal2 = literal2;
    }
    @Override
    public String description(Symboltable symboltable) {
        return "Equivalence " + InputClauses.toString(0,clause,symboltable) +
                " has literals " + Symboltable.toString(literal1,symboltable) + " and " +
                Symboltable.toString(literal2,symboltable) + " with different truth values.";}
}
