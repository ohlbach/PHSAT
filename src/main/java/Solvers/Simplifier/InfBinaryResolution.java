package Solvers.Simplifier;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfBinaryResolution extends InferenceStep {

    private final static String title = "Binary Resolution";
    @Override
    public String title() {return title;}

    private final static String rule = title + "\np,q and -p,r -> q,r";
    @Override
    public String rule() {return rule;}

    private String clause1,clause2,resolvent;
    private InferenceStep step1,step2;
    public InfBinaryResolution(Clause clause1, Clause clause2, Clause resolvent, Symboltable symboltable) {
        this.clause1   = clause1.toString(symboltable,0);
        this.clause2   = clause2.toString(symboltable,0);
        this.resolvent = resolvent.toString(symboltable,0);
        step1 = clause1.inferenceStep;
        step2 = clause2.inferenceStep;
    }

    @Override
    public String toString(Symboltable symboltable) {
        return title + "\n" + clause1 + " and " + clause2 + " -> " + resolvent;}

    @Override
    public IntArrayList inputClauseIds() {
        IntArrayList ids = step1.inputClauseIds().clone();
        for(int id : step2.inputClauseIds()) {if(!ids.contains(id)) ids.add(id);}
        return ids;}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        step1.inferenceSteps(steps);
        step2.inferenceSteps(steps);
        steps.add(this);}
}
