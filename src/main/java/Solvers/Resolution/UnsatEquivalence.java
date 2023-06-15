package Solvers.Resolution;

import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;

public class UnsatEquivalence extends Unsatisfiable {

    private int literal1,literal2;
    public UnsatEquivalence(int literal1, int literal2, InferenceStep... steps) {
        super(null,null);
        this.literal1 = literal1;
        this.literal2 = literal2;
        for(InferenceStep step : steps) {if(step != null) inferenceSteps.add(step);}}

    @Override
    public String description(Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        st.append("Contradictory Equivalent Literals: ").append(Symboltable.toString(literal1,symboltable)).append(" == ").
                append(Symboltable.toString(literal2,symboltable));
        if(!inferenceSteps.isEmpty()) {
            st.append("\nInference Steps:\n");
            for(InferenceStep step : inferenceSteps) st.append(step.toString(symboltable)).append("\n");}
        return st.toString();}
}
