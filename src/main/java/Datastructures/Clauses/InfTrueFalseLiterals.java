package Datastructures.Clauses;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfTrueFalseLiterals extends InferenceStep {
    public static final String title = "Removed Literals with Truth Value";

    public static final String rule =
            "m: p^n,phi and true(p) -> m-n: phi\n"+
                    "m: p^n,phi and false(p) -> m: phi";

    private final Clause oldClause,newClause;
    private final IntArrayList trueLiterals;
    private final IntArrayList falseLiterals;

    public InfTrueFalseLiterals(Clause oldClause, Clause newClause, IntArrayList trueLiterals,IntArrayList falseLiterals) {
        this.oldClause = oldClause;
        this.newClause = newClause;
        this.trueLiterals = trueLiterals;
        this.falseLiterals = falseLiterals;}

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return title + ":\n" + rule;}

    @Override
    public String toString(Symboltable symboltable) {
        String literals = "";
        if(!trueLiterals.isEmpty()) {
            literals += "true(";
            for(int i = 0; i < trueLiterals.size(); ++i) {
                literals += Symboltable.toString(trueLiterals.getInt(i),symboltable);
                if(i < trueLiterals.size()-1) literals += ",";}
            literals += ")";
            if(!falseLiterals.isEmpty()) literals += " and ";}
        if(!falseLiterals.isEmpty()) {
            literals += "false(";
            for(int i = 0; i < falseLiterals.size(); ++i) {
                literals += Symboltable.toString(falseLiterals.getInt(i),symboltable);
                if(i < falseLiterals.size()-1) literals += ",";}
            literals += ")";}
        return title + ":\n"+oldClause.toString(0,symboltable) + " and " + literals +
                " -> " + newClause.toString(0,symboltable);}

    @Override
    public IntArrayList origins() {
        InferenceStep step = oldClause.inferenceStep;
        return (step != null) ? step.origins() : null;}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        InferenceStep step = oldClause.inferenceStep;
        if(step != null) step.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}
}
