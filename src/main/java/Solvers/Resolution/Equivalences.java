package Solvers.Resolution;

import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.function.BiConsumer;

public class Equivalences {

    HashMap<Integer,ArrayList<Equivalence>> equivalences = new HashMap<>();

    void add(int triggerLiteral, int literal1, int literal2, InferenceStep inferenceStep) throws Unsatisfiable {
        if(Math.abs(literal1) > Math.abs(literal2)) {
            int dummy = literal1;
            literal1 = literal2;
            literal2 = dummy;}
        if(literal1 < 0) {literal1 *= -1; literal2 *= -1;}

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

        if(equivalence1 == null && equivalence2 == null) {
            ArrayList<Equivalence> equivalenceList = equivalences.computeIfAbsent(triggerLiteral, k -> new ArrayList<>());
            equivalenceList.add(new Equivalence(triggerLiteral,representative1,representative2,inferenceStep));
            return;}
        if(equivalence2 == null) { // equivalence1 != null
            equivalence1.add(representative2,InfList.makeInfList(step1 ,inferenceStep));
            return;}
        if(equivalence1 == null) { // equivalence2 != null
            equivalence2.add(representative1,InfList.makeInfList(step2 ,inferenceStep));
            return;}
        equivalence1.join((representative1 == equivalence1.representative ? 1:-1) *
                               (representative2 == equivalence2.representative ? 1:-1),
                equivalence2,inferenceStep);
        equivalences.get(triggerLiteral).remove(equivalence2);}

    void applyTrueLiteral(int literal, InferenceStep inferenceStep, BiConsumer<Integer,InferenceStep> trueLiterals) {
        ArrayList<Equivalence> equivalenceList = equivalences.get(literal);
        if(equivalenceList != null) {
            for(Equivalence equivalence : equivalenceList) equivalence.triggerLiteralZero(inferenceStep);
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
                if(equivalence.applyTrueLiteral(literal,inferenceStep, trueLiterals)) {
                    equivList.remove(i);
                    return;}}});}

    boolean isEmpty() {
        return equivalences.isEmpty();}

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
            if(!equivalenceList.isEmpty()) {
                st.append(equivalenceList.get(0).toString(symboltable));
                for(int i = 1; i < equivalenceList.size(); ++i) {
                    st.append("\n").append(equivalenceList.get(i).toString(symboltable));}}});
        return st.toString();}
}
