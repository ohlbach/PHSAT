package Solvers.Resolution;

import Datastructures.Symboltable;
import Datastructures.Theory.Model;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfTrueLiterals extends InferenceStep {

    private static final String title = "True Literals Applied to Clause";
    @Override
    public String title() {return title;}

    private static final String rule =
            "C: l1,...,ln and true(p1,...,pk) -> pi removed from C and C is simplified\n" +
            "Example: >= 2 p^2,q,r. and false(q) -> >= 2 p^2,r -> p must be true.\n"+
            "Example: >= 2 p,q^2,r^2,s and false(p) -> >= 2 q^2,r^2,s -> q,r";
    @Override
    public String rule() {return rule;}

    String clauseBefore;
    String clauseAfter = null;
    IntArrayList oldTrueLiterals = new IntArrayList();

    IntArrayList newTrueLiterals = null;
    ArrayList<InferenceStep> inferenceSteps = new ArrayList<>();

    public InfTrueLiterals (Clause clause, Model model, Symboltable symboltable) {
        clauseBefore = clause.toString(symboltable,0);
        inferenceSteps.add(clause.inferenceStep);
        for(Literal literalObject : clause.literals) {
            int literal = literalObject.literal;
            int status = model.status(literal);
            if(status != 0) {
                InferenceStep step = model.getInferenceStep(literal);
                if(step == null) step = new InfExternal(literal);
                inferenceSteps.add(step);
                oldTrueLiterals.add(status*literal);}}}

    public InfTrueLiterals addResults(Clause clause, byte status, IntArrayList derivedLiterals, Symboltable symboltable) {
        if(status == 0) clauseAfter = clause.toString(symboltable,0);
        newTrueLiterals = derivedLiterals.clone();
        return this;}

    public String result(Symboltable symboltable) {
        String result = (clauseAfter != null) ? clauseAfter : "";
        if(newTrueLiterals != null && !newTrueLiterals.isEmpty()) {
            if(!result.isEmpty()) result += " and ";
            result += "true(" + Symboltable.toString(newTrueLiterals,symboltable) + ")";}
        if(result.isEmpty()) result = "empty clause.";
        return result;}

    @Override
    public String toString(Symboltable symboltable) {
        return title + "\n  " +clauseBefore + " and true(" + Symboltable.toString(oldTrueLiterals,symboltable) + ") -> "
                + result(symboltable);}


    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps, IntArrayList ids) {
        if(steps.contains(this)) return;
        for(InferenceStep step : inferenceSteps) step.inferenceSteps(steps,ids);
        steps.add(this);}
}
