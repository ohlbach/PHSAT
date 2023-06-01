package Solvers.Resolution;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfList extends InferenceStep {
    @Override
    public String title() {
        return "List of Inference Steps";}
    private InferenceStep[] inferenceSteps;
    @Override
    public String rule() {
        StringBuilder st = new StringBuilder();
        st.append(inferenceSteps[0].rule());
        for(int i = 1; i < inferenceSteps.length; ++i) {
            st.append("\n").append(inferenceSteps[i].rule());}
        return st.toString();}

    public InfList(InferenceStep[] inferenceSteps) {
        this.inferenceSteps = inferenceSteps;}

    public static InferenceStep makeInfList(InferenceStep... steps) {
        ArrayList<InferenceStep> inferenceSteps = new ArrayList<>();
        for(InferenceStep step: steps) {
            if(step != null) {
                if(step instanceof InfList) {
                    for(InferenceStep st : ((InfList) step).inferenceSteps) {
                        if(!inferenceSteps.contains(st)) inferenceSteps.add(st);}}
                else {if(!inferenceSteps.contains(step)) inferenceSteps.add(step);}}}
        if(inferenceSteps.size() == 1) return inferenceSteps.get(0);
        steps = new InferenceStep[inferenceSteps.size()];
        return new InfList(inferenceSteps.toArray(steps));}

    @Override
    public String toString(Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        st.append(inferenceSteps[0].toString(symboltable));
        for(int i = 1; i < inferenceSteps.length; ++i) {
            st.append("\n").append(inferenceSteps[i].toString(symboltable));}
        return st.toString();}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps, IntArrayList ids) {
        if(steps.contains(this)) return;
        for(InferenceStep step: inferenceSteps) step.inferenceSteps(steps,ids);}
}
