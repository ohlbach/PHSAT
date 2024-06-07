package InferenceSteps;

import Datastructures.Symboltable;
import Solvers.Normalizer.Clause;
import Solvers.Normalizer.NMInferenceSteps.NMInferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

public class NMISEquivalentLiteral extends NMInferenceStep {
    int representative;
    int equivalentLiteral;
    InferenceStep inferenceStep;

    public NMISEquivalentLiteral(String title, int representative, int equivalentLiteral, InferenceStep inferenceStep, int[] clause) {
        super(title, clause);
        this.representative = representative;
        this.equivalentLiteral = equivalentLiteral;
        this.inferenceStep = inferenceStep;}

    public String toString(Clause deducedClause, Symboltable symboltable) {
        return title() + ": " + Clause.toString(clauseBefore, symboltable) + " and literal " +
                Symboltable.toString(equivalentLiteral,symboltable) +
                " replaced by literal " + Symboltable.toString(representative,symboltable) +
                " => " + deducedClause.toString(symboltable,0);}


    public boolean isTrue(int i, int literal, IntArrayList predicates) {
        if(literal == representative) return isTrue(i,equivalentLiteral, predicates);
        if(literal == -representative) return !isTrue(i,equivalentLiteral, predicates);;
        return isTrue(i,literal, predicates);}
}
