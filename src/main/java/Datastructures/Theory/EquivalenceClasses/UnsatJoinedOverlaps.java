package Datastructures.Theory.EquivalenceClasses;

import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import Utilities.Utilities;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class UnsatJoinedOverlaps extends Unsatisfiable {

    EquivalenceClass eqClass1;
    EquivalenceClass eqClass2;

    public UnsatJoinedOverlaps(EquivalenceClass eqClass1, EquivalenceClass eqClass2) {
        this.eqClass1 = eqClass1;
        this.eqClass2 = eqClass2;

    }
    @Override
    public String description(Symboltable symboltable) {
        return "Overlapping equivalence classes with complementary literals:\n"+
                eqClass1.toString(symboltable)+"\n"+
                eqClass2.toString(symboltable);}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        if(eqClass1.inferenceSteps != null) steps.add(eqClass1.inferenceSteps.get(0));
        if(eqClass2.inferenceSteps != null) steps.add(eqClass2.inferenceSteps.get(0));}

    @Override
    public IntArrayList inputClauseIds() {
        if(eqClass1.inferenceSteps == null) return null;
        return Utilities.unionIntArrayLists(eqClass1.inferenceSteps.get(0).inputClauseIds(),
                eqClass2.inferenceSteps.get(0).inputClauseIds());}
}
