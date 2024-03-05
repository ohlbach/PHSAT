package Solvers.Normalizer.NMInferenceSteps;

import Datastructures.Clauses.InputClauses;
import Datastructures.Symboltable;

public class NMISTrueLiteralToEquivalence extends NMInferenceStep{

    int oldTrueLiteral;
    int newTrueLiteral;
    int[] equivalence;
    public NMISTrueLiteralToEquivalence(int oldTrueLiteral, int[] equivalence, int newTrueLiteral) {
        super("True Literal", null);
        this.oldTrueLiteral = oldTrueLiteral;
        this.equivalence = equivalence;
        this.newTrueLiteral = newTrueLiteral;
    }

    public String toString(Symboltable symboltable) {
        return "True Literal: true(" + symboltable.toString(oldTrueLiteral,symboltable) + ") and equivalence " +
                InputClauses.toString(0,equivalence,symboltable) + " => true(" +
                symboltable.toString(newTrueLiteral,symboltable) + ")";}

}
