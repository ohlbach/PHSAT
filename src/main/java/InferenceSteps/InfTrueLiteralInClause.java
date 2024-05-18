package InferenceSteps;

import Datastructures.Clause;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfTrueLiteralInClause extends InferenceStep{
    @Override
    public String title() {
        return "True Literal in Clause";
    }

    @Override
    public String rule() {
        return "";
    }
    int[] clauseBefore;
    int literal;

    public InfTrueLiteralInClause(int[] clauseBefore, int literal) {
        this.clauseBefore = clauseBefore;
        this.literal = literal;
    }

    @Override
    public String toString(Symboltable symboltable) {
        return title() + " " + Clause.toString(clauseBefore, symboltable) + " -> " + Symboltable.toString(literal, symboltable);}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps, IntArrayList ids) {

    }
}
