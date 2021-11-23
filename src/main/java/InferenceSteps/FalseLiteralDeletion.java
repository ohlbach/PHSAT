package InferenceSteps;

import Datastructures.Clauses.ClauseOld;
import Datastructures.Symboltable;
import Datastructures.Theory.Model;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

import static Utilities.Utilities.joinIntArrays;

/** documents the deletion of false literals in disjunctions */

public class FalseLiteralDeletion extends InferenceStep{
    private static final String title = "False Literal Deletion";

    private static final String rule =
            title + ": Example:\n" +
            "A,p,B,q,C\n"+
            "false(p,q)\n" +
            "----------\n"+
               "A,B,C";

    private ClauseOld oldClause;
    private ClauseOld newClause;
    private IntArrayList positions;
    private Model model;

    public FalseLiteralDeletion(ClauseOld oldClause, ClauseOld newClause, IntArrayList positions, Model model) {
        this.oldClause = oldClause;
        this.newClause = newClause;
        this.positions = positions;
        this.model = model;}

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        String falses = "false(";
        int size =positions.size();
        for(int i = 0; i < size; ++i) {
            int position = positions.getInt(i);
            falses += Symboltable.toString(oldClause.getLiteral(position),symboltable);
            if(i < size - 1) falses += ",";}
        falses += ")";
        return title +":\n" + oldClause.toString(0,symboltable) + " and " + falses +
                " -> " + newClause.toString(0,symboltable);}

    @Override
    public IntArrayList origins() {
        IntArrayList origins = null;
        InferenceStep step = oldClause.inferenceStep;
        if(step != null) origins = step.origins();
        for(int position : positions) {
            step = model.getInferenceStep(oldClause.getLiteral(position));
            if(step != null) {origins = joinIntArrays(origins,step.origins());}}
        return origins;}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        InferenceStep step = oldClause.inferenceStep;
        if(step != null) step.inferenceSteps(steps);
        for(int position : positions) {
            step = model.getInferenceStep(oldClause.getLiteral(position));
            if(step != null) {step.inferenceSteps(steps);}}
        if(!steps.contains(this)) steps.add(this);}
}
