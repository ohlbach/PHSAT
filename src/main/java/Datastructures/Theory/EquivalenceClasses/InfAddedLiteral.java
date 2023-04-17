package Datastructures.Theory.EquivalenceClasses;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

/** This class describes the Inference step where a new equivalence p = q
 * is added to an existing equivalence class which contains already p.
 */
public class InfAddedLiteral  extends InferenceStep {
    @Override
    public String title() {
        return "Literal Added To Equivalence Class";
    }

    @Override
    public String rule() {
        return title() + "\n  'p = q = ... = x = ... = s' and 'x = y'\n" +
                "  -------------------------------------\n" +
                "     p = q = ... = x = ... = s = y'";
    }

    int representative,newLiteral,oldLiteral;
    IntArrayList literals;
    InferenceStep newInferenceStep,oldInferenceStep;

    public InfAddedLiteral(int representative, IntArrayList literals,
            int newLiteral,int oldLiteral, InferenceStep newInferenceStep, InferenceStep oldInferenceStep) {
        this.representative = representative;
        this.literals = literals.clone(); // literals may be changed in later steps.
        this.newLiteral = newLiteral;
        this.oldLiteral = oldLiteral;
        this.newInferenceStep = newInferenceStep;
        this.oldInferenceStep = oldInferenceStep;}


    @Override
    public String toString(Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        st.append(title()).append("\n");
        st.append(Symboltable.toString(representative,symboltable));
        for(int literal : literals)
            st.append(" = ").append(Symboltable.toString(literal,symboltable));
        String clause = st.toString();
        st.append("\n").append(Symboltable.toString(oldLiteral,symboltable)).append(" = ").
                append(Symboltable.toString(newLiteral,symboltable)).append("\n");
        int length = st.length();
        for(int i = 0; i < length; ++i) st.append("-");
        st.append("\n");
        st.append(clause).append(" = ").append(Symboltable.toString(newLiteral,symboltable));
        return st.toString();}

    @Override
    public IntArrayList inputClauseIds() {
        IntArrayList inputClauses1 = (oldInferenceStep == null) ? null: oldInferenceStep.inputClauseIds();
        IntArrayList inputClauses2 = (newInferenceStep == null) ? null: newInferenceStep.inputClauseIds();
        if(inputClauses1 == null) return inputClauses2;
        if(inputClauses2 == null) return inputClauses1;
        inputClauses1.addAll(inputClauses1.size(),inputClauses2);
        return inputClauses1;}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        if(oldInferenceStep != null && !steps.contains(oldInferenceStep)) steps.add(oldInferenceStep);
        if(newInferenceStep != null && !steps.contains(newInferenceStep)) steps.add(newInferenceStep);
        }
}
