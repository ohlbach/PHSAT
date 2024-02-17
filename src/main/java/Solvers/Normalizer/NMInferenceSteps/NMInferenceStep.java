package Solvers.Normalizer.NMInferenceSteps;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import Solvers.Normalizer.Clause;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public abstract class NMInferenceStep extends InferenceStep {

    Clause clause;


    public abstract boolean verify(Clause deducedClause, Symboltable symboltable, StringBuilder errors);
    @Override
    public String title() {
        return null;
    }

    @Override
    public String rule() {
        return null;
    }

    @Override
    public String toString(Symboltable symboltable) {
        return null;
    }

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps, IntArrayList ids) {

    }
}
