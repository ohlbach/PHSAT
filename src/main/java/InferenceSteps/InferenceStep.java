package InferenceSteps;

import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

public abstract class InferenceStep {
    public abstract String rule();

    public abstract String toString(Symboltable symboltable);

    public abstract Object input();

    public abstract Object output();

    /** collects the basicClause ids of all clauses causing the current inference
     *
     * @return the list of basic clause ids of all clauses causing the current inference
     */
    public abstract IntArrayList origins();

}
