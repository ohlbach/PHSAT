package Solvers.Resolution;

import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.function.BiConsumer;

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

    void add(int literal, InferenceStep inferenceStep) throws Unsatisfiable {
        for(int i = 0; i < literals.size(); ++i) {
            int lit = literals.getInt(i);
            if(lit == literal) return;
            if(lit == -literal) throw new UnsatEquivalence(lit,literal,inferenceSteps.get(i), inferenceStep);
            literals.add(literal);
            inferenceSteps.add(inferenceStep);}}

    int getRepresentative(int literal) {
        if(literal == representative || literal == -representative) return literal;
        for(int lit: literals) {
            if(lit == literal) return representative;
            if(lit == -literal) return -representative;}
        return literal;}

    InferenceStep getInferenceStep(int literal) {
        if(literal == representative || literal == -representative) return null;
        for(int i = 0; i < literals.size(); ++i) {
            int lit = literals.getInt(i);
            if(lit == literal || lit == -literal) return inferenceSteps.get(i);}
        return null;}

    void join(int sign, Equivalence equivalence, InferenceStep inferenceStep) {
        literals.add(sign*equivalence.representative);
        inferenceSteps.add(inferenceStep);
        for(int i = 0; i < equivalence.literals.size(); ++i) {
            literals.add(equivalence.literals.getInt(i));
            inferenceSteps.add(equivalence.inferenceSteps.get(i));}}

    boolean applyTrueLiteral(int trueLiteral, BiConsumer<Integer,InferenceStep> trueLiterals) {
        for(int i = 0; i < literals.size(); ++i) {
            int literal = literals.getInt(i);
            if(literal == trueLiteral) {
                for(int j = 0; j < literals.size(); ++j) {
                    int lit = literals.getInt(j);
                    if(lit != trueLiteral) trueLiterals.accept(lit,inferenceSteps.get(i));}
                return true;}
            if(literal == -trueLiteral) {
                for(int j = 0; j < literals.size(); ++j) {
                    int lit = literals.getInt(j);
                    if(lit != -trueLiteral) trueLiterals.accept(-lit,inferenceSteps.get(i));}
                return true;}}
        return false;}


    boolean contains(int literal) {
        if(literal == representative || literal == -representative) return true;
        for(int lit : literals) {if(literal == lit || literal == -lit) return true;}
        return false;}


    public String toString() {
        return toString(null);}
    public String toString(Symboltable symboltable) {
        String trigger = triggerLiteral != 0 ? Symboltable.toString(triggerLiteral,symboltable) + " -> " : "";
        String lits = Symboltable.toString(literals.getInt(0),symboltable);
        for(int i = 1; i < literals.size(); ++i) lits += ","+Symboltable.toString(literals.getInt(i),symboltable);
        return trigger + Symboltable.toString(representative,symboltable) + " = " + lits;}
}
