package Datastructures.Clauses.Simplifiers;

import Datastructures.Clauses.Clause;
import Datastructures.Symboltable;
import Datastructures.Theory.EquivalenceClasses;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

import static Utilities.Utilities.joinIntArrays;

/** for documenting replacements of literals by equivalent literals
 */
public class EquivalenceReplacements extends InferenceStep {

    public static final String title = "Equivalence Replacements";

    public static final String rule = title + ":\n" +
            "...,p,.......,q,...\n"+
            "  p = x ... q = y\n"+
            "-------------------\n" +
            "...,x,.......,y,...";

    private final Clause oldClause;
    private final Clause newClause;
    private final IntArrayList positions;
    private final EquivalenceClasses equivalenceClasses;

    public EquivalenceReplacements(Clause oldClause, Clause newClause, IntArrayList positions,
                                   EquivalenceClasses equivalenceClasses) {
        this.oldClause = oldClause;
        this.newClause = newClause;
        this.positions = positions;
        this.equivalenceClasses = equivalenceClasses;
    }

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        String equations = "";
        int size =positions.size();
        for(int i = 0; i < size; ++i) {
            int position = positions.getInt(i);
            equations += Symboltable.toString(oldClause.getLiteral(position),symboltable) + "="+
                    Symboltable.toString(newClause.getLiteral(position),symboltable);
            if(i < size - 1) equations += ",";}
        return title +":\n" + oldClause.toString(0,symboltable) + " and " + equations +
                " -> " + newClause.toString(0,symboltable);}

    @Override
    public IntArrayList origins() {
        IntArrayList origins = (oldClause.inferenceStep != null) ? oldClause.inferenceStep.origins() : null;
        for(int position : positions) {
            InferenceStep step = equivalenceClasses.getEClause(oldClause.getLiteral(position)).inferenceStep;
            if(step != null) joinIntArrays(origins,step.origins());}
        return origins;}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        if(oldClause.inferenceStep != null) oldClause.inferenceStep.inferenceSteps(steps);
        for(int position : positions) {
            InferenceStep step = equivalenceClasses.getEClause(oldClause.getLiteral(position)).inferenceStep;
            if(step != null) step.inferenceSteps(steps);}
        if(!steps.contains(this)) steps.add(this);}
}
