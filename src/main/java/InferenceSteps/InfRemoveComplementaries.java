package InferenceSteps;

import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfRemoveComplementaries extends InferenceStep{

    int[] oldClause;
    int[] newClause;

    public InfRemoveComplementaries(int[] oldClause, int[] newClause) {
        this.oldClause = oldClause;
        this.newClause = newClause;
    }

    @Override
    public String title() {
        return null;
    }

    @Override
    public String rule() {
        return null;
    }

    @Override
    public String toString(Symboltable symboltable) {
        return null;
    }

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps, IntArrayList ids) {

    }
}
