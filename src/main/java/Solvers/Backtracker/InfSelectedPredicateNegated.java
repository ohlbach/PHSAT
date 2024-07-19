package Solvers.Backtracker;

import Datastructures.Clause;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfSelectedPredicateNegated  extends InferenceStep {
    @Override
    public String title() {
        return "Negated Selected Predicate";
    }

    @Override
    public String rule() {
        return "";
    }

    int negatedPredicate;
    ArrayList<int[]> usedClauses;

    public InfSelectedPredicateNegated(int negatedPredicate, ArrayList<Clause> usedClauses) {
        this.negatedPredicate = negatedPredicate;
        this.usedClauses = new ArrayList<>(usedClauses.size());
        for(Clause clause : usedClauses) { this.usedClauses.add(clause.simpleClone());}}

    @Override
    public String toString(Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        st.append(title()).append("\n");
        for(int[] clause : usedClauses) st.append(Clause.toString(clause,symboltable)).append("\n");
        st.append(" -> ").append(Symboltable.toString(negatedPredicate,symboltable));
        return st.toString();}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps, IntArrayList ids) {

    }
}
