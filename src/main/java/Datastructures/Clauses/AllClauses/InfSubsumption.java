package Datastructures.Clauses.AllClauses;

import Datastructures.Clauses.Clause;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfSubsumption extends InferenceStep {
    public static final String title = "Subsumption";
    public static final String rule = title+"\n" +
            "atleast n: phi subsumes alteast n-k: phi' psi iff\n"+
            "phi' subset phi and |phi'| = |phi|-k";

    private Clause subsumer, subsumee;

    public InfSubsumption(Clause subsumer, Clause subsumee) {
        this.subsumer = subsumer;
        this.subsumee = subsumee;}

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        return title+":\n" + subsumer.toString(0,symboltable) + " subsumes " +
                subsumee.toString(0,symboltable);}

    @Override
    public IntArrayList origins() {
        InferenceStep step = subsumer.inferenceStep;
        return step != null ? step.origins() : null;}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        InferenceStep step = subsumer.inferenceStep;
        if(step != null) step.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}
}
