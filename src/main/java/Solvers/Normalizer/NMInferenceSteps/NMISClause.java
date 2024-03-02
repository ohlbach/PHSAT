package Solvers.Normalizer.NMInferenceSteps;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import Solvers.Normalizer.Clause;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class NMISClause extends InferenceStep {
    Clause clause;
    public NMISClause(Clause clause) {
        this.clause = clause;}

    @Override
    public String title() {
        return "NormalizerClause";
    }

    @Override
    public String rule() {
        return null;
    }

    @Override
    public String toString(Symboltable symboltable) {
        return title() + ": " + clause.toString(symboltable,0);
    }

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps, IntArrayList ids) {

    }
}
