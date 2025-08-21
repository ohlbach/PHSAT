package Solvers.Backtracker;

import Datastructures.Symboltable;

import java.util.Arrays;

/**
 * A class for managing a temporary model that tracks predicates and their truth values.
 * This class is typically used for temporary models in the backtrackers.
 */
public class TemporaryModel {
    private int[] model;
    byte[] truthValues;
    private int[] predictePositions;
    private int[] lastFlippable;
    private int firstFreePosition;



    /**
     * Creates a new temporary model.
     * @param predicates determines the size of the model.
     */
    public TemporaryModel(int predicates) {
        makeData(predicates);}

    /**
     * Reuses the given data structures of the temporary model for a new backtracker with a new global model.
     * @param predicates a new number of predicates
     */
    TemporaryModel reuseModel(int predicates) {
        if(predicates >= model.length) makeData(predicates);
        else {firstFreePosition = 0;
            Arrays.fill(model, 0);
            Arrays.fill(truthValues, (byte) 0);
            Arrays.fill(predictePositions, 0);
            Arrays.fill(lastFlippable, 0);}
        return this;}

    /**
     * Creates a new data structure for the temporary model.
     * @param predicates the number of predicates.
     */
   private void makeData(int predicates) {
        model             = new int[++predicates];
        truthValues       = new byte[predicates];
        predictePositions = new int[predicates];
        lastFlippable = new int[predicates];
        firstFreePosition = 0;}

    /**
     * Makes a literal temporarily true.
     * If the literal contradicts the truth value of the literal in the temporary model, then the truth value is overwritten
     * @param literal a literal which is supposed to be true.
     * @param flippableLiteral the literal on whose selection the truth of the literal depends. (maybe 0 for globally true literals)
     * @return false if a contradiction was detected, otherwise true.
     */
   synchronized boolean makeTemporarilyTrue(int literal, int flippableLiteral) {
        int predicate = Math.abs(literal);
        if(isTemporarilyTrue(literal)) return true;
        if(isTemporarilyFalse(literal))  {
            truthValues[predicate] = (byte)((literal > 0)? +1 : -1);
            return false;}
        truthValues[predicate] = (byte) (literal > 0 ? 1 : -1);
        predictePositions[predicate] = firstFreePosition;
        model[firstFreePosition++] = literal;
        lastFlippable[predicate] = flippableLiteral;
         return true;}

    /** backtracks the model to the given literal. Its truth-value is negated.
     *
     * All array entries after the literal's position are zeroed.
     * @param literal a literal to which the model is to be backtracked.
     */
    void clear(int literal) {
        int predicate = Math.abs(literal);
        int position = predictePositions[predicate]+1;
        Arrays.fill(model, position, firstFreePosition, 0);
        Arrays.fill(lastFlippable, position, firstFreePosition, 0);
        Arrays.fill(predictePositions, position, firstFreePosition, 0);
        Arrays.fill(truthValues, position, firstFreePosition, (byte)0);
        firstFreePosition = position;
        truthValues[predicate] *= (short)-1;
    }

    synchronized int status(int literal) {
        return (literal > 0) ? truthValues[literal] : -truthValues[-literal]; }

    /**
     * Checks if a literal is temporarily true.
     * @param literal a literal
     * @return true if the literal is temporarily true, otherwise false.
     */
    synchronized boolean isTemporarilyTrue(int literal) {
        return (literal > 0) ? truthValues[literal] == +1: truthValues[-literal] == -1; }


    /**
     * Checks if a literal is temporarily false.
     * @param literal a literal
     * @return true if the literal is temporarily false, otherwise false.
     */
    synchronized boolean isTemporarilyFalse(int literal) {
        return (literal > 0) ? truthValues[literal] == -1: truthValues[-literal] == +1; }

    /**
     * Checks if a literal is temporarily undefined.
     * @param literal a literal
     * @return true if the literal is temporarily undefined, otherwise false.
     */
    synchronized boolean isTemporarilyUndefined(int literal) {
        int predicate = Math.abs(literal);
        return truthValues[predicate] == 0 && predictePositions[predicate] != 0;}

    /**
     * Returns the literal with the higher position in the model
     * @param literal1 the first literal.
     * @param literal2 the second literal.
     * @return the literal with the higher position in the model.
     */
    synchronized int max(int literal1, int literal2) {
        if(literal1 == 0) return literal2;
        if(literal2 == 0) return literal1;
        if(predictePositions[Math.abs(literal1)] > predictePositions[Math.abs(literal2)]) return literal1;
        else return literal2;}

    synchronized int lastFlippableLiteral(int literal) {
        return lastFlippable[Math.abs(literal)];}

    /** Returns the String representation of the temporary model.
     * @return a String representation of the temporary model.
     */
   public String toString() {
        return toString(null);}

    /**
     * Returns the String representation of the temporary model.
     * @param symboltable a symboltable or null.
     * @return a String representation of the temporary model, given a symboltable (or null).
     */
   public String toString(Symboltable symboltable) {
        StringBuilder sb = new StringBuilder();
        for(int i = 0; i < firstFreePosition; ++i) {
            if(i > 0 && i < firstFreePosition-1) sb.append(",");
            int literal = model[i];
            int dependentLiteral = lastFlippable[Math.abs(literal)];
            if(dependentLiteral != 0) sb.append(":").append(Symboltable.toString(dependentLiteral,symboltable));
            sb.append(Symboltable.toString(literal,symboltable));}
        return sb.toString();}
}
