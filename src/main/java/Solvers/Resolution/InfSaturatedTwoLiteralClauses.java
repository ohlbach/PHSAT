package Solvers.Resolution;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfSaturatedTwoLiteralClauses extends InferenceStep {

    private static String title = "Saturated 2-Literal clauses";
    @Override
    public String title() {
        return title;}

    private static String rule = title +"\n  In a saturated set of 2-literal clauses any literal can be made true.";
    @Override
    public String rule() {
        return rule;}

    int literal;
    InferenceStep step;
    int id;

    public InfSaturatedTwoLiteralClauses(Clause clause) {
        literal = clause.literals.get(0).literal;
        step    = clause.inferenceStep;
        id      = clause.identifier;}

    @Override
    public String toString(Symboltable symboltable) {
        return title + "\n  In the saturated set of 2-literal clauses, the literal " +
                Symboltable.toString(literal,symboltable) + " is set to true";
    }


    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps, IntArrayList ids) {
        if(steps.contains(this)) return;
        steps.add(this);
        if(!ids.contains(id)) ids.add(id);}
}
