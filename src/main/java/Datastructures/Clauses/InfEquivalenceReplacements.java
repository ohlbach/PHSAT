package Datastructures.Clauses;

import Datastructures.Clauses.Clause;
import Datastructures.Symboltable;
import Datastructures.Theory.EquivalenceClasses;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

import static Utilities.Utilities.joinIntArrays;

/** for documenting replacements of literals by equivalent literals
 */
public class InfEquivalenceReplacements extends InferenceStep {

    public static final String title = "Equivalence Replacements";

    public static final String rule = title + ":\n" +
            "...,p,.......,q,...\n"+
            "  p -> x ... q -> y\n"+
            "-------------------\n" +
            "...,x,.......,y,...";

    private final Clause oldClause;
    private final Clause newClause;
    private final ArrayList<Object> literals;
    private final EquivalenceClasses equivalenceClasses;

    public InfEquivalenceReplacements(Clause oldClause, Clause newClause, ArrayList<Object> literals,
                                      EquivalenceClasses equivalenceClasses) {
        this.oldClause = oldClause;
        this.newClause = newClause;
        this.literals = literals;
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
        int size = literals.size();
        for(int i = 0; i < size; i+=3) {
            int oldLiteral = (Integer)literals.get(i);
            int newLiteral = (Integer)literals.get(i+1);
            equations += Symboltable.toString(oldLiteral,symboltable) + "->"+Symboltable.toString(newLiteral,symboltable);
            if(i < size - 2) equations += " ,";}
        return title +":\n" + oldClause.toString(0,symboltable) + " and " + equations +
                " -> " + newClause.toString(0,symboltable);}

    @Override
    public IntArrayList origins() {
        IntArrayList origins = (oldClause.inferenceStep != null) ? oldClause.inferenceStep.origins() : null;
        for(int i = 0; i < literals.size(); i+=3) {
            InferenceStep step = (InferenceStep)literals.get(i+2);
            if(step != null) origins = joinIntArrays(origins,step.origins());
        }
        return origins;}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        if(oldClause.inferenceStep != null) oldClause.inferenceStep.inferenceSteps(steps);
        for(int i = 0; i < literals.size(); i+=3) {
            int oldLiteral = (Integer)literals.get(i);
            InferenceStep step = equivalenceClasses.getEClause(oldLiteral).inferenceStep;
            if(step != null) step.inferenceSteps(steps);}
        if(!steps.contains(this)) steps.add(this);}
}

