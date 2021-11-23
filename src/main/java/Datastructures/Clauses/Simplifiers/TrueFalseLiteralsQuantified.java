package Datastructures.Clauses.Simplifiers;

import Datastructures.Clauses.ClauseOld;
import Datastructures.Symboltable;
import Datastructures.Theory.Model;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

import static Utilities.Utilities.joinIntArrays;

/** This class documents the results of true and false literals in a quantified clause */

public class TrueFalseLiteralsQuantified extends InferenceStep {
    public static final String title = "True Literal in Quantified Clauses";
    public static final String rule = title + ":\n"+
            "Q m p,l_1,...,l_n and true(p)  -> Q (m-1) l_1,...,l_n\n"+
            "Q m p,l_1,...,l_n and false(p) -> Q m l_1,...,l_n\n"+
            "where Q is one of atleast, atmost, exactly";

    private final ClauseOld oldClause;
    private final ClauseOld newClause;
    private final IntArrayList trueLiterals;
    private final IntArrayList falseLiterals;
    private final Model model;

    public TrueFalseLiteralsQuantified(ClauseOld oldClause, ClauseOld newClause,
                                       IntArrayList trueLiterals, IntArrayList falseLiterals, Model model) {
        this.oldClause = oldClause;
        this.newClause = newClause;
        this.trueLiterals = trueLiterals;
        this.falseLiterals = falseLiterals;
        this.model = model;}

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        String tLiterals = "";
        String fLiterals = "";
        if(!trueLiterals.isEmpty()) {
            for(int i = 0; i < trueLiterals.size(); ++i) {
                tLiterals += Symboltable.toString(trueLiterals.getInt(i),symboltable);
                if(i < trueLiterals.size()-1) tLiterals += ",";}
            tLiterals = "true(" + tLiterals + ")";
            if(!falseLiterals.isEmpty()) tLiterals += " and ";}
        if(!falseLiterals.isEmpty()) {
            for(int i = 0; i < falseLiterals.size(); ++i) {
                fLiterals += Symboltable.toString(falseLiterals.getInt(i),symboltable);
                if(i < falseLiterals.size()-1) fLiterals += ",";}
            fLiterals = "false(" + fLiterals + ")";}

        return oldClause.toString(0,symboltable) + " and " + tLiterals + fLiterals + " -> " +
                newClause.toString(0,symboltable);}

    @Override
    public IntArrayList origins() {
        InferenceStep step = oldClause.inferenceStep;
        IntArrayList origins = step == null ? null : step.origins();
        for(int literal : trueLiterals) {
            step = model.getInferenceStep(literal);
            if(step != null) joinIntArrays(origins,step.origins());}
        for(int literal : falseLiterals) {
            step = model.getInferenceStep(literal);
            if(step != null) joinIntArrays(origins,step.origins());}
        return origins;}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        InferenceStep step = oldClause.inferenceStep;
        if(step != null)  steps.add(step);
        for(int literal : trueLiterals) {
            step = model.getInferenceStep(literal);
            if(step != null) step.inferenceSteps(steps);}
        for(int literal : falseLiterals) {
            step = model.getInferenceStep(literal);
            if(step != null) step.inferenceSteps(steps);}
        if(steps.contains(this)) steps.add(this);}
}
