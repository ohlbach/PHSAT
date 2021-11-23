package InferenceSteps;

import Datastructures.Clauses.ClauseOld;
import Datastructures.Symboltable;
import Datastructures.Theory.Model;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

import static Utilities.Utilities.joinIntArrays;

/** Documents removal or true/false literals in atleast-clauses */

public class NumericTrueFalse extends InferenceStep{

    public static final String title = "True/False Literals in Numeric Clauses";

    public static final String rule = title + ": Examples:\n" +
            "atleast 2 p,q,r and true(p)  -> atleast 1 q,r\n" +
            "atleast 2 p,q,r and false(p) -> atleast 2 q r\n" +
            "similar with atmost and exactly";

    private final ClauseOld oldClause;
    private final ClauseOld newClause;
    private final IntArrayList trueLiterals;
    private final IntArrayList falseLiterals;
    private final Model model;

    public NumericTrueFalse(ClauseOld oldClause, ClauseOld newClause,
                            IntArrayList trueLiterals, IntArrayList falseLiterals, Model model) {
        this.oldClause     = oldClause;
        this.newClause     = newClause;
        this.trueLiterals  = trueLiterals;
        this.falseLiterals = falseLiterals;
        this.model         = model;

    }

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        String trues = "";
        int size;
        if(trueLiterals != null) {
            size = trueLiterals.size();
            trues = "true(";
            for(int i = 0; i < size; ++i) {
                trues += Symboltable.toString(oldClause.getLiteral(trueLiterals.getInt(i)),symboltable);
                if(i < size - 1) trues += ",";}
            trues += ")";}
        String falses = "";
        if(falseLiterals != null) {
            size = falseLiterals.size();
            falses = "false(";
            for(int i = 0; i < size; ++i) {
                falses += Symboltable.toString(oldClause.getLiteral(falseLiterals.getInt(i)),symboltable);
                if(i < size - 1) falses += ",";}
            falses += ")";}
        String separator = "";
        if(trueLiterals != null && falseLiterals != null) separator = " and ";
        return title + ":\n" + oldClause.toString(0,symboltable) + " and " +
                trues + separator + falses + " -> " +
                newClause.toString(0,symboltable);
    }

    @Override
    public IntArrayList origins() {
        InferenceStep step = oldClause.inferenceStep;
        IntArrayList origins = (step == null) ? null : step.origins();
        if(trueLiterals != null) {
            for(int position : trueLiterals) {
                step = model.getInferenceStep(oldClause.getLiteral(position));
                if(step != null) joinIntArrays(origins, step.origins());}}
        if(falseLiterals != null) {
            for(int position : falseLiterals) {
                step = model.getInferenceStep(oldClause.getLiteral(position));
                if(step != null) joinIntArrays(origins, step.origins());}}
        return origins;}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        InferenceStep step = oldClause.inferenceStep;
        if(step != null) step.inferenceSteps(steps);
        if(trueLiterals != null) {
            for(int position : trueLiterals) {
                step = model.getInferenceStep(oldClause.getLiteral(position));
                if(step != null) step.inferenceSteps(steps);}}
        if(falseLiterals != null) {
            for(int position : falseLiterals) {
                step = model.getInferenceStep(oldClause.getLiteral(position));
                if(step != null) step.inferenceSteps(steps);}}}

}
