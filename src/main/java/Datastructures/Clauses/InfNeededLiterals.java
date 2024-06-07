package Datastructures.Clauses;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfNeededLiterals extends InferenceStep {
    public static final String title = "Needed Literals";
    public static final String rule = title + "\n"+
            "Literals which must be true.\n"+
            "Example: >= 4: p^2,q^2,r\n"+
            "        In order to get 4 true predicates, p and q must be true.";

    private final Clause atleastClause;
    private final Clause andClause;

    public InfNeededLiterals(Clause atleastClause,Clause andClause) {
        this.atleastClause = atleastClause;
        this.andClause = andClause;}

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        return title + "\n" +
                atleastClause.toString(0,symboltable) + " -> " + andClause.toString(0,symboltable);}


    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps, IntArrayList ids) {
        if(steps.contains(this)) return;
        InferenceStep step = atleastClause.inferenceStep;
        if(step != null) step.inferenceSteps(steps,ids);
        if(!steps.contains(this)) steps.add(this);}
}
