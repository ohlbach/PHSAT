package InferenceSteps;

import Datastructures.Clause;
import Datastructures.Symboltable;

public class InfMergeResolution extends InferenceStep {
    @Override
    public String title() {
        return "Merge Resolution";}

    @Override
    public String rule() {
        return "p,phi and -p,phi,psi => phi,psi";}

    int[] parent1; int[] parent2; int resolvent[];

    public InfMergeResolution(int[] parent1, int[] parent2, Clause resolvent) {
        this.parent1 = parent1;
        this.parent2 = parent2;
        this.resolvent = resolvent.simpleClone();}

    @Override
    public String toString(Symboltable symboltable) {
        return title() + ": " + Clause.toString(parent1,symboltable) + " and " + Clause.toString(parent2,symboltable) +
                " => " + Clause.toString(resolvent,symboltable);}
}
