package Solvers.Resolution;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class Equivalence {

    int triggerLiteral;
    int representative;
    IntArrayList literals = new IntArrayList();

    ArrayList<InferenceStep> inferenceSteps = new ArrayList<>();

    public Equivalence(int triggerLiteral, int representative, int literal, InferenceStep inferenceStep) {
        this.triggerLiteral = triggerLiteral;
        this.representative = representative;
        literals.add(literal);
        inferenceSteps.add(inferenceStep);}

    public String toString() {
        return toString(null);}
    public String toString(Symboltable symboltable) {
        String trigger = triggerLiteral != 0 ? Symboltable.toString(triggerLiteral,symboltable) + " -> " : "";
        String lits = Symboltable.toString(literals.getInt(0),symboltable);
        for(int i = 1; i < literals.size(); ++i) lits += ","+Symboltable.toString(literals.getInt(i),symboltable);
        return trigger + Symboltable.toString(representative,symboltable) + " = " + lits;}
}
