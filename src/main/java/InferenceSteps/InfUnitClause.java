package InferenceSteps;

import Datastructures.Clause;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfUnitClause extends InferenceStep {
    @Override
    public String title() {
        return "Unit Clause";
    }

    @Override
    public String rule() {
        return "";
    }

    Clause clause;
    public InfUnitClause(Clause clause) {
        super();
        this.clause = clause;}

    @Override
    public String toString(Symboltable symboltable) {
        return title() + " " + clause.toString(symboltable,0);
    }

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps, IntArrayList ids) {

    }
}
