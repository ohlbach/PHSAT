package Datastructures.Clauses.QuantifiedToCNF;

import Datastructures.Clauses.Clause;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

/** This class documents the transformation of atleast-clauses to CNF.
 */
public class AtleastToCNF extends InferenceStep {

    public static final String title = "Atleast-Clause to Conjunctive Normal Form";

    public static final String rule = title+":\n"+
            "atleast m p_1,...,p_n ->" +
            "(n over (n-m+1)) clauses with all combinations of n-m+1 literals.\n"+
            "Example:\n"+
            "atleast 2: 1,2,3,4,5,6 ->\n"+
            "1: 1,2,3,4,5\n"+
            "2: 1,2,3,4,6\n"+
            "3: 1,2,3,5,6\n"+
            "4: 1,2,4,5,6\n"+
            "5: 1,3,4,5,6\n"+
            "6: 2,3,4,5,6";

    private final Clause atleastClause;
    private final Clause orClause;
    public AtleastToCNF(Clause atleastClause, Clause orClause) {
        this.atleastClause = atleastClause;
        this.orClause = orClause;}

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        return title+"\n"+ atleastClause.toString(0,symboltable) + " -> " +
                orClause.toString(0,symboltable);}

    @Override
    public IntArrayList origins() {
        InferenceStep step = atleastClause.inferenceStep;
        return (step == null) ? null : step.origins();}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        InferenceStep step = atleastClause.inferenceStep;
        if(step != null) step.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}
}
