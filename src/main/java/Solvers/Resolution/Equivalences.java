package Solvers.Resolution;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;

import java.util.ArrayList;
import java.util.HashMap;

public class Equivalences {

    HashMap<Integer,ArrayList<Equivalence>> equivalences = new HashMap<>();

    void addEquivalence(int triggerLiteral, int literal1, int literal2, InferenceStep inferenceStep) {
        if(Math.abs(literal1) > Math.abs(literal2)) {
            int dummy = literal1;
            literal1 = literal2;
            literal2 = dummy;}
        if(literal1 < 0) {literal1 *= -1; literal2 *= -1;}

        ArrayList<Equivalence> equivalenceList = equivalences.get(triggerLiteral);
        if(equivalenceList != null) {
            for(Equivalence equivalence: equivalenceList) {
                if(equivalence.representative == literal1) {
                    equivalence.literals.add(literal2);
                    equivalence.inferenceSteps.add(inferenceStep);
                    return;}}
            equivalenceList.add(new Equivalence(triggerLiteral,literal1,literal2,inferenceStep));
            return;}
        equivalenceList = new ArrayList<>();
        equivalenceList.add(new Equivalence(triggerLiteral,literal1,literal2,inferenceStep));
        equivalences.put(triggerLiteral,equivalenceList);}

    ArrayList<Equivalence> get(int triggerLiteral) {
        return equivalences.get(triggerLiteral);}
    public String toString() {
        return toString(null);}
    public String toString(Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        equivalences.forEach((Integer triggerLiteral,ArrayList<Equivalence> equivalenceList) -> {
            for(Equivalence equivalence: equivalenceList) {
                st.append(equivalence.toString(symboltable)).append("\n");}});
        return st.toString();}
}
