package InferenceSteps;

import Datastructures.Symboltable;

public class InfPureLiteral extends InferenceStep{
    @Override
    public String title() {
        return "Pure Literal";
    }

    @Override
    public String rule() {
        return "Literal occurs only positively/negatively";
    }

    private int literal;
    public InfPureLiteral(int literal) {
        this.literal = literal;}

    @Override
    public String toString(Symboltable symboltable) {
        return title() + " " + Symboltable.toString(literal,symboltable);}
}
