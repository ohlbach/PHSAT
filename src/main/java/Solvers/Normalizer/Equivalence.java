package Solvers.Normalizer;

import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import InferenceSteps.InfInputClause;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

/** This class represents an equivalence class.*/
public class Equivalence {
    /** the original equivalence clause */
    int[] inputClause;
    /** the representative of the equivalence class */
    int representative;
    /** the list of equivalent literals */
    IntArrayList literals;
    /** for each equivalent literal an extra inference step.*/
    ArrayList<InferenceStep> inferenceSteps = new ArrayList<>();

    /**
     * Represents an equivalence class.
     *
     * <p>
     * An equivalence class is created by providing the inputClause and the representative literal.
     * The class also maintains a list of equivalent literals.
     * </p>
     *
     * @param inputClause the input clause representing an equivalence
     * @param representative the representative of the equivalence class
     */
    public Equivalence(int[] inputClause, int representative) {
        this.inputClause = inputClause;
        this.representative = representative;
        this.literals = new IntArrayList();}

    /**
     * Adds a literal to the equivalence class.
     * <p>
     * This method adds a literal to the equivalence class.
     * If the literal is already part of the class or if it is the representative literal, the method does nothing.
     * </p>
     *
     * @param literal the literal to be added to the equivalence class
     * @throws Unsatisfiable if the literal is equivalent to the negation of the representative literal
     */
    public void addLiteral(int literal) throws Unsatisfiable {
        if(literal == representative || literals.contains(literal)) return;
        InferenceStep step = new InfInputClause(inputClause,representative,literal, "Equivalence");
        if(literal == -representative || literals.contains(-literal)) throw new UnsatEquivalence(this);
        literals.add(literal);
        inferenceSteps.add(step);
    }

    /** checks if the equivalence class is empty.
     *
     * @return true if the class is emtpy.
     */
    public boolean isEmpty() {
        return literals.isEmpty();}

    /**
     * Returns a string representation of the Equivalence object.
     *
     * @param symboltable the symboltable used to map literals to names
     * @return a string representation of the Equivalence object
     */
    public String toString(Symboltable symboltable) {
        if(isEmpty()) return "";
        StringBuilder st = new StringBuilder();
        st.append(inputClause[0]).append(": ").append(Symboltable.toString(representative,symboltable));
        for(int literal : literals) st.append(" = ").append(Symboltable.toString(literal,symboltable));
        return st.toString();
    }
}
