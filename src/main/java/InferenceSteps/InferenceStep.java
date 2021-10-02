package InferenceSteps;

import Datastructures.Symboltable;

public abstract class InferenceStep {
    public abstract String rule();

    public abstract String toString(Symboltable symboltable);

    public abstract Object input();

    public abstract Object output();

}
