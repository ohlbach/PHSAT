package Solvers.Backtracker;

import Datastructures.Clause;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;

import java.util.ArrayList;

public class InfClauseInference extends InferenceStep {
    @Override
    public String title() {
        return "Clause Inference";
    }

    @Override
    public String rule() {
        return "true(literal1) and clauses => true(literal2)";
    }

    private int selectedLiteral;
    private InferenceStep selectedStep;
    private int literal;
    private ArrayList<int[]> usedClauses;

    public InfClauseInference(int selectedLiteral, InferenceStep selectedStep, int literal, ArrayList<Clause> usedClauses) {
        this.selectedLiteral = selectedLiteral;
        this.selectedStep = selectedStep;
        this.literal = literal;
        if(usedClauses != null) {
            this.usedClauses = new ArrayList<>(usedClauses.size());
            for(Clause clause : usedClauses) {
                this.usedClauses.add(clause.simpleClone());}}}

    @Override
    public String toString(Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        st.append(title()).append(": true(").append(Symboltable.toString(selectedLiteral,symboltable)).append(") ");
        if(usedClauses != null) {
            st.append("and clauses\n");
            for (int[] clause : usedClauses) {
                st.append(Clause.toString(clause, symboltable)).append("\n");}}
        st.append("causes true(").append(Symboltable.toString(literal,symboltable)).append(")");
        return st.toString();
    }
}
