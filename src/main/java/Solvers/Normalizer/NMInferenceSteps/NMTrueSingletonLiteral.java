package Solvers.Normalizer.NMInferenceSteps;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import Solvers.Normalizer.Clause;

import java.util.ArrayList;

public class NMTrueSingletonLiteral extends InferenceStep {
    Clause clause;
    int trueLiteral;

    public NMTrueSingletonLiteral(Clause clause, int trueLiteral) {
        this.clause = clause;
        this.trueLiteral = trueLiteral;}

    @Override
    public String title() {
        return "Singleton True Literal";
    }

    @Override
    public String rule() {
        return null;
    }

    @Override
    public String toString(Symboltable symboltable) {
        return title() + ": " + Symboltable.toString(trueLiteral,symboltable) + " in clause " + clause.toString(symboltable,0);
    }

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps, IntArrayList ids) {

    }
}
