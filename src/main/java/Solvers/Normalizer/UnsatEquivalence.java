package Solvers.Normalizer;

import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;

public class UnsatEquivalence extends Unsatisfiable {
    InferenceStep inferenceStep;
    int literal1,literal2;

    public UnsatEquivalence(String problemId, String solverId, int literal1, int literal2, InferenceStep inferenceStep) {
        super(problemId,solverId);
        this.literal1 = literal1;
        this.literal2 = literal2;
        this.inferenceStep = inferenceStep;}

    @Override
    public String description(Symboltable symboltable) {
        return "Equivalence " + Symboltable.toString(literal1,symboltable) + " == " + Symboltable.toString(literal2,symboltable) +
                " has literals with different truth values.";}
}
