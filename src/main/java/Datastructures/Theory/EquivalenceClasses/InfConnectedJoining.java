package Datastructures.Theory.EquivalenceClasses;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import static Utilities.Utilities.joinIntArrays;

public class InfConnectedJoining extends InferenceStep {

    private final EquivalenceClass eqClass1;
    private final EquivalenceClass eqClass2;
    private final int literal1;
    private final int literal2;
    private final EquivalenceClass joinedClass;
    private final InferenceStep[] inferenceSteps;

    public static final String title = "Joining of Connected Equivalences";

    public static final String rule = title + ":\n"+
            "     p = q = ... = x = ... = s\n"+
            "     a = b = ... = y = ... = k and x = y\n"+
            "-------------------------------------------\n"+
            "p = ... = x = ... = s = a = ... = y = ... k";


    public InfConnectedJoining(EquivalenceClass eqClass1, EquivalenceClass eqClass2, int literal1, int literal2,
                               EquivalenceClass joinedClass, InferenceStep... inferenceSteps) {
        this.eqClass1 = eqClass1;
        this.eqClass2 = eqClass2;
        this.literal1 = literal1;
        this.literal2 = literal2;
        this.joinedClass = joinedClass.clone(); // the class may be extended later.
        this.inferenceSteps = inferenceSteps;}

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        String st1 = eqClass1.toString(symboltable);
        String st2 = eqClass2.toString(symboltable) + " and " + Symboltable.toString(literal1,symboltable) + " = " +
                Symboltable.toString(literal2,symboltable);
        String st3 = Utilities.Utilities.concatenateString("-",st2.length());
        return title + ":\n" + st1 + "\n" + st2 + "\n"+ st3 + "\n" + joinedClass.toString(symboltable);}

    @Override
    public IntArrayList inputClauseIds() {
        IntArrayList inputIds = null;
        for(InferenceStep step : inferenceSteps) joinIntArrays(inputIds,step.inputClauseIds());
        return inputIds;}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        for(InferenceStep step : inferenceSteps) {
        if(step != null && !steps.contains(step)) steps.add(step);}}
}
