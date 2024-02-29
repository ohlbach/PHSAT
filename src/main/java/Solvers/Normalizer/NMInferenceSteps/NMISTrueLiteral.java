package Solvers.Normalizer.NMInferenceSteps;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import Solvers.Normalizer.Clause;
import it.unimi.dsi.fastutil.ints.IntArrayList;

public class NMISTrueLiteral extends NMInferenceStep{

    int trueLiteral;
    InferenceStep inferenceStep;

    public NMISTrueLiteral(String title, int trueLiteral, InferenceStep inferenceStep, Clause clause) {
        super(title, clause);
        this.trueLiteral = trueLiteral;
        this.inferenceStep = inferenceStep;}

    public String toString(Clause deducedClause, Symboltable symboltable) {
        String status = deducedClause.isTrue ? " (true)": (deducedClause.isFalse ? " (false)" : "");
        return title() + ": " + clause.toString(symboltable,0) + " and true(" +
                Symboltable.toString(trueLiteral,symboltable) +") => " + deducedClause.toString(symboltable,0) + status;}

    public boolean isTrue(int i, int literal, IntArrayList literals) {
        if(literal ==  trueLiteral) return true;
        if(literal == -trueLiteral) return false;
        return super.isTrue(i,literal,literals);}

}
