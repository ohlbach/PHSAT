package InferenceSteps;

import Datastructures.Clause;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.function.Consumer;

public class InfMergeResolution extends InferenceStep {
    @Override
    public String title() {
        return link == null ? "Merge Resolution" : "Linked Merge Resolution";}

    @Override
    public String rule() {
        return link == null ? "p,phi and -p,phi,psi => phi,psi" :
                "p,q and -p,phi and -q,phi,psi => phi,psi";}

    int[] link;
    int[] parent1; int[] parent2; int resolvent[];

    public InfMergeResolution(int[] parent1, int[] parent2, Clause resolvent) {
        this.parent1 = parent1;
        this.parent2 = parent2;
        this.resolvent = resolvent.simpleClone();}

    public InfMergeResolution(Clause link, int[] parent1, int[] parent2, Clause resolvent) {
        this.link = link.simpleClone();
        this.parent1 = parent1;
        this.parent2 = parent2;
        this.resolvent = resolvent.simpleClone();
    }

    public boolean verify(Consumer<String> monitor, Symboltable symboltable) {
        IntArrayList predicates = Clause.predicates(parent1,parent2);
        for(int model : Clause.getModels(parent2,predicates)) {
            if(Clause.isTrue(parent1, model,predicates) && (link == null || Clause.isTrue(link,model,predicates))) {
                if (!Clause.isTrue(resolvent, model, predicates)) {
                    monitor.accept("Error: " + toString(symboltable) +
                            "\n Resolution failed for model:  " + Clause.modelString(model, predicates, symboltable));
                    return false;}}}
        return true;}

    @Override
    public String toString(Symboltable symboltable) {
        if(link == null)
            return title() + ": " + Clause.toString(parent1,symboltable) + " and " + Clause.toString(parent2,symboltable) +
                " => " + Clause.toString(resolvent,symboltable);
            return title() + ": " + Clause.toString(link,symboltable) + " and " + Clause.toString(parent1,symboltable) + " and " +
                    Clause.toString(parent2,symboltable) +
                    " => " + Clause.toString(resolvent,symboltable);
    }
}
