package Solvers.Normalizer;

import Datastructures.Results.Unsatisfiable;
import InferenceSteps.InfInputClause;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class Equivalence {
    int[] inputClause;
    int representative;
    IntArrayList literals;
    ArrayList<InferenceStep> inferenceSteps = new ArrayList<>();

    public Equivalence(int[] inputClause, int representative) {
        this.inputClause = inputClause;
        this.representative = representative;
        this.literals = new IntArrayList();
    }

    public void addLiteral(int literal) throws Unsatisfiable {
        if(literal == representative || literals.contains(literal)) return;
        InferenceStep step = new InfInputClause(inputClause,representative,literal);
        if(literal == -representative || literals.contains(-literal))
            throw new UnsatEquivalence(null,null,literal,-literal,step);
        literals.add(literal);
        inferenceSteps.add(step);
    }
}
