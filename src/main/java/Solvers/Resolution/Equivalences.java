package Solvers.Resolution;

import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import Utilities.TriConsumer;

import java.util.ArrayList;
import java.util.HashMap;

public class Equivalences {

    HashMap<Integer,ArrayList<Equivalence>> equivalences = new HashMap<>();

    void add(int triggerLiteral, int literal1, int literal2, InferenceStep inferenceStep) throws Unsatisfiable {
        Equivalence equivalence1 = getEquivalence(triggerLiteral,literal1);
        Equivalence equivalence2 = getEquivalence(triggerLiteral,literal2);
        int representative1 = literal1; int representative2 = literal2;
        InferenceStep step1 = null; InferenceStep step2 = null;
        if(equivalence1 != null) {
            representative1 = equivalence1.getRepresentative(literal1);
            step1           = equivalence1.getInferenceStep(literal1);}
        if(equivalence2 != null) {
            representative2 = equivalence2.getRepresentative(literal2);
            step2           = equivalence2.getInferenceStep(literal2);}

        if(representative1 == representative2) return;
        if(representative1 == -representative2)
            throw new UnsatEquivalence(literal1, literal2, step1, step2, inferenceStep);

        if(Math.abs(representative1) > Math.abs(representative2)) {
            int dummy = representative1;
            representative1 = representative2;
            representative2 = dummy;}
        if(representative1 < 0) {representative1 *= -1; representative2 *= -1;}

        if(equivalence1 == null && equivalence2 == null) {
            ArrayList<Equivalence> equivalenceList = equivalences.get(triggerLiteral);
            if(equivalenceList == null) {
                equivalenceList = new ArrayList<>();
                equivalences.put(triggerLiteral,equivalenceList);}
            equivalenceList.add(new Equivalence(triggerLiteral,literal1,literal2,inferenceStep));
            return;}
        if(equivalence2 == null) { // equivalence1 != null
            equivalence1.add(representative2,inferenceStep);
            return;}
        if(equivalence1 == null) { // equivalence2 != null
            equivalence2.add(representative1,inferenceStep);
            return;}
        equivalence1.join(representative1 == equivalence1.representative ? 1:-1,equivalence2,inferenceStep);
        equivalences.get(triggerLiteral).remove(equivalence2);}

    void applyTrueLiteral(int literal, InferenceStep inferenceStep, TriConsumer<Integer,InferenceStep,InferenceStep> trueLiterals) {
        ArrayList<Equivalence> equivalenceList = equivalences.get(literal);
        if(equivalenceList != null) {
            for(Equivalence equivalence : equivalenceList) equivalence.triggerLiteral = 0;
            ArrayList<Equivalence> equivalenceZero = equivalences.get(0);
            if(equivalenceZero == null) equivalences.put(0,equivalenceList);
            else equivalenceZero.addAll(equivalenceList);
            equivalences.remove(literal);
            return;}
        equivalenceList = equivalences.get(-literal);
        if(equivalenceList != null) {equivalences.remove(-literal); return;}

        equivalences.forEach((triggerLiteral,equivList) -> {
            for(int i = 0; i < equivList.size(); ++i) {
                Equivalence equivalence = equivList.get(i);
                if(equivalence.applyTrueLiteral(literal,trueLiterals)) {
                    equivList.remove(i);
                    return;}}});}

    Equivalence getEquivalence(int triggerLiteral, int literal) {
        ArrayList<Equivalence> equivalenceList = equivalences.get(triggerLiteral);
        if(equivalenceList == null) return null;
        for(Equivalence equivalence : equivalenceList) {
            if(equivalence.contains(literal)) return equivalence;}
        return null;}

    int getRepresentative(int triggerLiteral, int literal) {
        Equivalence equivalence = getEquivalence(triggerLiteral,literal);
        if(equivalence != null) return equivalence.getRepresentative(literal);
        return literal;}

    InferenceStep getInferenceStep(int triggerLiteral,int literal) {
        Equivalence equivalence = getEquivalence(triggerLiteral,literal);
        if(equivalence != null) return equivalence.getInferenceStep(literal);
        return null;}

    public String toString() {
        return toString(null);}
    public String toString(Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        equivalences.forEach((Integer triggerLiteral,ArrayList<Equivalence> equivalenceList) -> {
            for(Equivalence equivalence: equivalenceList) {
                st.append(equivalence.toString(symboltable)).append("\n");}});
        return st.toString();}
}
